(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: define global locations
 * 
 * File contributors        : Étienne André
 * Created                  : 2010/03/10
 * Renamed from Automaton.ml: 2015/10/22
 * Last modified            : 2020/09/28
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

(* Array discrete_index -> NumConst.t *)
type discrete = DiscreteValue.discrete_value array

(* Global location: location for each automaton + value of the discrete *)
type global_location = locations * discrete


exception NotEqual

let location_equal loc1 loc2 =
	let (locs1, discr1) = loc1 in
	let (locs2, discr2) = loc2 in
	(* can use polymorphic = here *)
	if not (locs1 = locs2) then false else (
		if not ((Array.length discr1) = (Array.length discr2)) then false else (
			try (
				Array.iteri (fun i d1 -> 
					if not (discr2.(i) = d1) then raise NotEqual
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

let get_locations (locations, _) =	locations

let get_discrete (_, discrete) = discrete

(*let location_hash_code location =
	let locations = get_locations location in
	Array.fold_left (fun h loc -> 
		7919 * h + loc
	) 0 locations*)

let hash_code location =
	let locations, discrete = location in
	let loc_hash = Array.fold_left (fun h loc -> 2*h + loc) 0 locations in
	let discr_hash = Array.fold_left (fun h q -> 
		2*h + (DiscreteValue.hash q)
	) 0 discrete in
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
let make_location locations_per_automaton discrete_values =
	(* Create an array for locations *)
	let locations = Array.make !nb_automata 0 in
	(* Create an array for discrete *)
	let discrete = Array.make !nb_discrete DiscreteValue.rational_zero in
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values;
	(* Return the new location *)
	locations, discrete


(** 'copy_location location' creates a fresh location identical to location. *)
let copy_location location =
	(* Create an array for locations *)
	let locations = Array.copy (get_locations location) in
	(* Create an array for discrete *)
	let discrete = Array.copy (get_discrete location) in
	(* Return the new location *)
	locations, discrete


(** 'update_location locations discrete_values location' creates a new location from the original location, and update the given automata and discrete variables. *)
let update_location locations_per_automaton discrete_values location =
	(* Create an array for locations *)
	let locations = Array.copy (get_locations location) in
	(* Create an array for discrete *)
	let discrete = Array.copy (get_discrete location) in
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values;
	(* Return the new location *)
	locations, discrete


(** Side-effet version of 'update_location'. *)
let update_location_with locations_per_automaton discrete_values (locations, discrete) =
	(* Iterate on locations *)
	List.iter (fun (automaton_index, location_index) -> locations.(automaton_index) <- location_index) locations_per_automaton;
	(* Iterate on discrete *)
	List.iter (fun (discrete_index, value) -> discrete.(discrete_index - !min_discrete_index) <- value) discrete_values




(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Access} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** Get the location associated to some automaton *)
let get_location location automaton_index =
	let locations = get_locations location in
	locations.(automaton_index)

(** Get the value associated to some discrete variable *)
let get_discrete_value location discrete_index =
	let discrete = get_discrete location in
	(* Do not forget the offset *)
	discrete.(discrete_index - !min_discrete_index)

let get_discrete_rational_value location discrete_index =
    let value = get_discrete_value location discrete_index in
    DiscreteValue.numconst_value value

(************************************************************)
(* Check whether the global location is accepting *)
(************************************************************)

(** Check whether a global location is accepting according to the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
let is_accepting (locations_acceptance_condition : automaton_index -> location_index -> bool) (global_location : global_location) =
	
	let result = ref false in
	(* Check whether a local location is accepting *)
	
	(*** TODO: rewrite using Array.exists! ***)
	
	Array.iteri (fun automaton_index location_index ->
		result := !result || locations_acceptance_condition automaton_index location_index) (get_locations global_location);
	
	(* Return result *)
	!result



(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(*------------------------------------------------------------*)
(* Matching global_location predicates with a given global_location *)
(*------------------------------------------------------------*)

let match_loc_predicate loc_predicate global_location =
	match loc_predicate with
	| Loc_predicate_EQ (automaton_index, location_index) ->
		get_location global_location automaton_index = location_index
	| Loc_predicate_NEQ (automaton_index, location_index) ->
		get_location global_location automaton_index <> location_index

(*------------------------------------------------------------*)
(* Matching simple predicates with a given global_location *)
(*------------------------------------------------------------*)

let match_simple_predicate (locations_acceptance_condition : automaton_index -> location_index -> bool) simple_predicate global_location =
	match simple_predicate with

	(* Here convert the global_location to a variable valuation *)
	| Discrete_boolean_expression discrete_boolean_expression -> DiscreteExpressionEvaluator.check_discrete_boolean_expression (get_discrete_value global_location) discrete_boolean_expression
	
	| Loc_predicate loc_predicate -> match_loc_predicate loc_predicate global_location

	| State_predicate_true -> true
	
	| State_predicate_false -> false
	
	| State_predicate_accepting -> is_accepting locations_acceptance_condition global_location


(*------------------------------------------------------------*)
(* Matching state predicates with a given global_location *)
(*------------------------------------------------------------*)

(***TODO/NOTE: Might have been nicer to convert the acceptance condition during the ModelConverter phase :-/ ***)

let rec match_state_predicate_factor (locations_acceptance_condition : automaton_index -> location_index -> bool) state_predicate_factor global_location : bool =
	match state_predicate_factor with
	| State_predicate_factor_NOT state_predicate_factor_neg -> not (match_state_predicate_factor locations_acceptance_condition state_predicate_factor_neg global_location)
	| Simple_predicate simple_predicate -> match_simple_predicate locations_acceptance_condition simple_predicate global_location
	| State_predicate state_predicate -> match_state_predicate locations_acceptance_condition state_predicate global_location

and match_state_predicate_term (locations_acceptance_condition : automaton_index -> location_index -> bool) state_predicate_term global_location : bool =
	match state_predicate_term with
	| State_predicate_term_AND (state_predicate_term_1, state_predicate_term_2) ->
		match_state_predicate_term locations_acceptance_condition state_predicate_term_1 global_location
		&&
		match_state_predicate_term locations_acceptance_condition state_predicate_term_2 global_location
	| State_predicate_factor state_predicate_factor -> match_state_predicate_factor locations_acceptance_condition state_predicate_factor global_location

and match_state_predicate (locations_acceptance_condition : automaton_index -> location_index -> bool) state_predicate global_location : bool =
	match state_predicate with
	| State_predicate_OR (state_predicate_1, state_predicate_2) ->
		match_state_predicate locations_acceptance_condition state_predicate_1 global_location
		||
		match_state_predicate locations_acceptance_condition state_predicate_2 global_location
	| State_predicate_term state_predicate_term -> match_state_predicate_term locations_acceptance_condition state_predicate_term global_location




(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(** 'string_of_location automata_names location_names discrete_names location' converts a location to a string. *)
let string_of_location automata_names location_names discrete_names rational_display location =
	(* Get the locations per automaton *)
	let locations = get_locations location in
	(* Get the values for discrete variables *)
	let discrete = get_discrete location in
	(* Convert the locations *)
	let string_array = Array.mapi (fun automaton_index location_index ->
		(automata_names automaton_index) ^ ": " ^ (location_names automaton_index location_index)
	) locations in
	let location_string = string_of_array_of_string_with_sep ", " string_array in
	(* Convert the discrete *)
	let string_array = Array.mapi (fun discrete_index value ->
		(string_of_discrete discrete_names discrete_index) ^ " = " ^ (DiscreteValue.string_of_value value) ^ (
			(* Convert to float? *)
			match rational_display with
			| Exact_display -> ""
			| Float_display -> " (~ " ^ (string_of_float (DiscreteValue.to_float_value value)) ^ ")"
		)
	) discrete in
	let discrete_string = string_of_array_of_string_with_sep ", " string_array in
	(* Return the string *)
	location_string ^ (if !nb_discrete > 0 then ", " else "") ^ discrete_string
	

	
