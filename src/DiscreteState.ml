(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: define discrete states (global locations)
 *
 * File contributors        : Étienne André
 * Created                  : 2010/03/10
 * Renamed from Automaton.ml: 2015/10/22
 *
 ************************************************************)
 

(************************************************************)
(* Modules *)
(************************************************************)
open OCamlUtilities
open Automaton
open AbstractProperty



(************************************************************)
(** {2 Types} *)
(************************************************************)

(** Unique identifier for each different global location *)
type global_location_index = int

(* Array automaton_index -> location_index *)
type locations = location_index array

(* Array discrete_index -> discrete_value *)
type discrete = AbstractValue.abstract_value array

(* Local variables table type *)
type local_variables_table = (variable_ref, AbstractValue.abstract_value) Hashtbl.t
type global_variables_table = (variable_ref, AbstractValue.abstract_value) Hashtbl.t

(* Global location: location for each automaton + value of the discrete *)
type global_location = locations * global_variables_table * local_variables_table

type discrete_valuation = variable_ref -> AbstractValue.abstract_value
type discrete_setter = variable_ref -> AbstractValue.abstract_value -> unit
type local_discrete_valuation = variable_ref -> AbstractValue.abstract_value
type local_discrete_setter = variable_ref -> AbstractValue.abstract_value -> unit
type discrete_access = discrete_valuation * discrete_setter * local_discrete_valuation * local_discrete_setter

exception NotEqual

let location_equal loc1 loc2 =
	let (locs1, discr1, _) = loc1 in
	let (locs2, discr2, _) = loc2 in
	(* can use polymorphic = here *)
	if not (locs1 = locs2) then false else (
		if not ((Hashtbl.length discr1) = (Hashtbl.length discr2)) then false else (
			try (
			    Hashtbl.iter (fun variable_ref d1 ->
			        let d2 = Hashtbl.find discr2 variable_ref in
			        if not (d1 = d2) then
			            raise NotEqual;
			    ) discr1;
			    true
			) with _ -> false
			(* all entries equal *)			
		) 
	)


(** Should the float be displaid using exact rationals or (possibly approximated) floats? *)
type rational_display =
	| Exact_display
	| Float_display


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Automata} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

type automaton_index = int
type automaton_name = string




(************************************************************)
(** Global variables *)
(************************************************************)

(* The minimum discrete_index *)
let min_discrete_index = ref 0

(* The number of discrete variables *)
let nb_discrete = ref 0

(* The number of automata *)
let nb_automata = ref 0

(************************************************************)
(** Useful functions *)
(************************************************************)

let get_locations (locations, _, _) =	locations

let get_discrete (_, global_variables_table, _) = global_variables_table

let get_local_variables (_, _, local_variables_table) = local_variables_table

(*let location_hash_code location =
	let locations = get_locations location in
	Array.fold_left (fun h loc -> 
		7919 * h + loc
	) 0 locations*)

let hash_code location =
	let locations, discrete, _ = location in
	let loc_hash = Array.fold_left (fun h loc -> 2*h + loc) 0 locations in
	let discr_hash = Hashtbl.fold (fun _ value acc ->
        2*acc + (AbstractValue.hash value)
	) discrete 0 in

	loc_hash + 3 * discr_hash

(* Replace a discrete variable by its name, considering the offset *)
let string_of_discrete names index =
	names (index + !min_discrete_index)


(************************************************************)
(** {2 Locations} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Initialization} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** 'initialize nb_automata min_discrete_index max_discrete_index' initializes the min and max discrete indexes and the number of automata. *)
let initialize nb_auto min_discrete max_discrete =
	min_discrete_index := min_discrete;
	nb_discrete := max_discrete - min_discrete + 1;
	nb_automata := nb_auto


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** 'make_location locations discrete_values' creates a new location. All automata should be given a location. Discrete variables may not be given a value (in which case they will be initialized to 0). *)
let make_location locations_per_automaton global_variables_table local_variables_table =
	(* Create an array for locations *)
	let locations = Array.make !nb_automata 0 in
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Return the new location *)
	locations, global_variables_table, local_variables_table

(* We have to copy discrete values of arrays and stacks *)
(* Because of array and stack are references in OCaml, if we don't copy their content *)
(* discrete values stay the same between previous location and new location leading to a misbehavior. *)
(* This is due to the fact that the update in-place of their values or their content will update old and new location *)
(* as it was the same references. *)
(* As it was possible to update content of array in IMITATOR via a[i] = x, or stack by stack_push(x, s) *)
(* List isn't concerned because we doesn't have ability to modify it's content in IMITATOR. *)
let copy_discrete_values_at_location location =
	(* Get discrete variables *)
	let global_variables_table = get_discrete location in
	(* Copy discrete variables *)
(*	let cpy_global_variables_table = Hashtbl.copy global_variables_table in*)
    (* Deep copy (recursive) of abstract values *)
(*	Hashtbl.filter_map_inplace (fun variable_ref value -> Some (AbstractValue.deep_copy value)) cpy_global_variables_table;*)
    let l = global_variables_table |> Hashtbl.to_seq |> List.of_seq in
    let cpy_l = List.map (fun (vr, v) -> vr, AbstractValue.deep_copy v) l in
    let cpy_global_variables_table = cpy_l |> List.to_seq |> Hashtbl.of_seq in
	(* Return copy array of discrete variables *)
	cpy_global_variables_table

(** 'copy_location location' creates a fresh location identical to location. *)
let copy_location location =
	(* Create an array for locations *)
	let cpy_locations = Array.copy (get_locations location) in
	(* Copy the array of discrete values *)
	let cpy_discrete = copy_discrete_values_at_location location in
	(* Copy the table of local variables and reinit values of local variables *)
	let local_variables_table = Hashtbl.copy (get_local_variables location) in
	(* Return the new location *)
	cpy_locations, cpy_discrete, local_variables_table


(** Side-effect version of 'update_location'. *)
let update_location_with locations_per_automaton (locations, _, _) =
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Get the location associated to some automaton *)
let get_location location automaton_index =
	let locations = get_locations location in
	locations.(automaton_index)

(** Get the value associated to some discrete variable *)
let get_discrete_value location variable_ref =
	let global_variables_table = get_discrete location in
	Hashtbl.find global_variables_table variable_ref

(** Get the value associated to some global discrete variable *)
let get_discrete_value_by_name location variable_name =
    let variable_ref = variable_name, 0 (* id = 0 mean global variable *) in
    get_discrete_value location variable_ref

(** Get the NumConst value associated to some discrete variable *)
let get_discrete_rational_value location discrete_index =
    let value = get_discrete_value location discrete_index in
    AbstractValue.numconst_value value

(** Get the NumConst value associated to some global discrete variable *)
let get_discrete_rational_value_by_name location variable_name =
    let variable_ref = variable_name, 0 (* id = 0 mean global variable *) in
    get_discrete_rational_value location variable_ref

(** Set the value associated to some discrete variable *)
let set_discrete_value location variable_ref value =
    let global_variables_table = get_discrete location in
	Hashtbl.replace global_variables_table variable_ref value

(* Get the value associated to some discrete local variable *)
let get_local_discrete_value location variable_ref =
    let local_variables_table = get_local_variables location in
    Hashtbl.find local_variables_table variable_ref

(* Set the value associated to some discrete local variable *)
let set_local_discrete_value location variable_ref value =
    let local_variables_table = get_local_variables location in
    Hashtbl.replace local_variables_table variable_ref value

(** Get a tuple of functions for reading / writing a global variable at a given location *)
(* A discrete access enable to read or write a value of a variable at a given discrete index *)
let discrete_access_of_location location =
    get_discrete_value location, set_discrete_value location, get_local_discrete_value location, set_local_discrete_value location


(************************************************************)
(* Check whether the global location is accepting *)
(************************************************************)

(** Check whether a global location is accepting according to the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
let is_accepting (locations_acceptance_condition : automaton_index -> location_index -> bool) (global_location : global_location) =
	(* Check whether a local location is accepting *)
	get_locations global_location |> Array.mapi locations_acceptance_condition |> Array.exists identity

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. *)
let string_of_location automata_names location_names discrete_names rational_display location =
	(* Get the locations per automaton *)
	let locations = get_locations location in
	(* Get the values for discrete variables *)
	let global_variables_table = get_discrete location in
	(* Convert the locations *)
	let string_array = Array.mapi (fun automaton_index location_index ->
		(automata_names automaton_index) ^ ": " ^ (location_names automaton_index location_index)
	) locations in
	let location_string = string_of_array_of_string_with_sep ", " string_array in
	(* Convert the discrete *)
	let discrete = global_variables_table |> Hashtbl.to_seq |> List.of_seq in

	let string_list = List.map (fun ((variable_name, _ (* id *)), value) ->
		variable_name
		^ " = "
		^ AbstractValue.string_of_value value
        ^ (
            (* Convert to float? *)
            match rational_display with
            | Exact_display -> ""
            | Float_display -> " (~ " ^ (string_of_float (AbstractValue.to_float_value value)) ^ ")"
        )
	) discrete in

	let discrete_string = string_of_list_of_string_with_sep ", " string_list in
	(* Return the string *)
	(* TODO benjamin NOW replace by length of table *)
	location_string ^ (if !nb_discrete > 0 then ", " else "") ^ discrete_string
	

	
