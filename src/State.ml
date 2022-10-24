(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Description of a state_index, a symbolic state and sets of states indexes
 * 
 * File contributors : Étienne André, Laure Petrucci
 * Created           : 2016/05/04
 *
 ************************************************************)



(************************************************************)
(* Modules *)
(************************************************************)

open Exceptions
open OCamlUtilities
open ImitatorUtilities
open Automaton
open AbstractModel
open Statistics


(************************************************************)
(** Reachable states *)
(************************************************************)
type state_index = int

(** Concrete state: location and px-valuation *)
type concrete_state = {
	global_location: DiscreteState.global_location;
	px_valuation   : (Automaton.variable_index -> NumConst.t);
}

(** State: location and constraint *)
type state = {
	global_location: DiscreteState.global_location;
	px_constraint  : LinearConstraint.px_linear_constraint;
}

(** Abstract state: abstract location (index) and concrete constraint *)
type abstract_state = {
	global_location_index: DiscreteState.global_location_index;
	px_constraint        : LinearConstraint.px_linear_constraint;
}


(************************************************************)
(** Statistics *)
(************************************************************)
let statespace_dcounter_nb_constraint_comparisons = create_discrete_counter_and_register "number of constraints comparisons" States_counter Verbose_standard




(************************************************************)
(* Cache *)
(************************************************************)

(*** TODO (ÉA, 2022/10): move somewhere else; dedicated class?? ***)
(*** TODO: move to the class! (otherwise, if several algorithms in the row, cache will not be reinitialized… ***)

(* hash function for locations *)
let loc_hash locations =
	Array.fold_left (fun h l ->
		7919 * h + l
	) 0 locations


(*(* hash function for clock updates *)
let upd_hash clock_update =
	List.fold_left (fun h v ->
		7919 * h + v
	) 0 clock_update*)


(* Cache for computed invariants *)
let inv_cache = Cache.make loc_hash 200

(*(* Cache for clock updates *)
let upd_cache = Cache.make upd_hash 100*)



(************************************************************)
(** State comparison functions *)
(************************************************************)

(** Compare two states (generic version).
  * Arguments:
  * constraint_comparison_function
  * comparison_name
  * state1
  * state2
  * clocks_to_remove: some variables to remove before performing the comparison between states (typically used to remove the `global_time_clock` before comparing two states)
  *)
let states_compare (constraint_comparison_function : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint -> bool) (comparison_name : string) (state1 : state) (state2 : state) (clocks_to_remove : Automaton.clock_index list) : bool =
	let (loc1, constr1) = state1.global_location, state1.px_constraint in
	let (loc2, constr2) = state2.global_location, state2.px_constraint in
	if not (DiscreteState.location_equal loc1 loc2) then false else (
		(* Statistics *)
		print_message Verbose_high ("About to compare " ^ comparison_name ^ " between two constraints.");

		(* Statistics *)
		statespace_dcounter_nb_constraint_comparisons#increment;

		if verbose_mode_greater Verbose_high then(
			let nb_comparisons = statespace_dcounter_nb_constraint_comparisons#discrete_value in
			print_message Verbose_high ("Already performed " ^ (string_of_int nb_comparisons) ^ " constraint comparison" ^ (s_of_int nb_comparisons) ^ ".");
		);


		let constr1, constr2 =
		match clocks_to_remove with
			(* Nothing to do *)
			| [] -> constr1, constr2
			(* Nothing to do *)
			| _ ->
				(* Remove the clock_to_remove in both constraints *)
				(*** NOTE: expensive! ***)
				LinearConstraint.px_hide clocks_to_remove constr1
				,
				LinearConstraint.px_hide clocks_to_remove constr2
		in

		(* Perform the actual comparison *)
		constraint_comparison_function constr1 constr2
	) (* if distinct discrete locations *)



(** Concrete implementations *)
let state_equals      = states_compare LinearConstraint.px_is_equal "equality"
let state_included_in = states_compare LinearConstraint.px_is_leq   "inclusion"


(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(*let is_one_location_accepting (state : state) =
	
	(* Retrieve the model *)
	let model = Input.get_model() in
	(* Retrieve the locations *)
	let locations = DiscreteState.get_locations state.global_location in
	let result = ref false in
	(* Check whether a local location is accepting *)
	
	(*** TODO: rewrite using Array.exists! ***)
	
	Array.iteri (fun automaton_index location_index ->
		result := !result || model.is_accepting automaton_index location_index) locations;
	
	(* Return result *)
	!result
	
let match_state_predicate state_predicate state =
	(* Call dedicated function *)
	DiscreteState.match_state_predicate state_predicate state.global_location*)


(* Tests whether a state matches `state_predicate`; takes as argument the accepting condition of the model of the form `automaton_index -> location_index -> acceptance of location_index in automaton_index` *)
let match_state_predicate (model : AbstractModel.abstract_model) (locations_acceptance_condition : Automaton.automaton_index -> Automaton.location_index -> bool) (state_predicate : AbstractProperty.state_predicate) (state : state) : bool =
    let discrete_access = DiscreteState.discrete_access_of_location state.global_location in
	DiscreteExpressionEvaluator.match_state_predicate (Some model.variable_names) (Some model.functions_table) discrete_access locations_acceptance_condition state.global_location state_predicate



(************************************************************)
(** Constraints satisfaction *)
(*************************************************************)

(** Check whether a discrete non-linear constraint is satisfied by the discrete values in a location **)
let evaluate_d_nonlinear_constraint_in_location (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) (discrete_guard : AbstractModel.discrete_guard) : bool =
    let discrete_access = DiscreteState.discrete_access_of_location location in
    DiscreteExpressionEvaluator.check_nonlinear_constraint (Some model.variable_names) (Some model.functions_table) discrete_access discrete_guard

(** Check whether the discrete part of a guard is satisfied by the discrete values in a location *)
let is_discrete_guard_satisfied (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) (guard : AbstractModel.guard) : bool =
	match guard with
	| True_guard -> true
	| False_guard -> false
	| Discrete_guard discrete_guard -> evaluate_d_nonlinear_constraint_in_location model location discrete_guard
	| Continuous_guard _ -> true
	| Discrete_continuous_guard discrete_continuous_guard -> evaluate_d_nonlinear_constraint_in_location model location discrete_continuous_guard.discrete_guard

(** Check whether the discrete part of a guards are satisfied by the discrete values in a location *)
let is_discrete_guards_satisfied (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) (discrete_guards : AbstractModel.guard list) : bool =
    List.for_all (is_discrete_guard_satisfied model location) discrete_guards

(** Check whether the intersection between a pxd_constraint with an AbstractModel.guard if satisfiable (both inputs remain unchanged) *)
let is_constraint_and_continuous_guard_satisfiable (pxd_linear_constraint : LinearConstraint.pxd_linear_constraint) = function
	(* True: trivially satisfiable because we assume the original constraint was satisfiable *)
	| True_guard -> true

	(* False: trivially unsatisfiable *)
	| False_guard -> false

	(* Discrete guard: trivially satisfiable because no continuous part, and we assume the original constraint was satisfiable *)
	| Discrete_guard _ -> true

	(* Continuous guard: we have to intersect and check satisfiability *)
	| Continuous_guard continuous_guard ->
		LinearConstraint.pxd_is_satisfiable (LinearConstraint.pxd_intersection [pxd_linear_constraint; continuous_guard])

	(* Discrete + continuous guard: we have to intersect and check satisfiability *)
	| Discrete_continuous_guard discrete_continuous_guard ->
		LinearConstraint.pxd_is_satisfiable (LinearConstraint.pxd_intersection [pxd_linear_constraint; discrete_continuous_guard.continuous_guard])





(************************************************************)
(** Computation of invariants *)
(*************************************************************)

(*------------------------------------------------------------*)
(** Create a PXD constraint of the form D_i = d_i for the discrete variables *)
(*------------------------------------------------------------*)
let discrete_constraint_of_global_location (model : AbstractModel.abstract_model) (global_location : DiscreteState.global_location) : LinearConstraint.pxd_linear_constraint =
	let discrete_values = List.map (fun discrete_index -> discrete_index, (DiscreteState.get_discrete_value global_location discrete_index)) model.discrete in

    (* TODO check with étienne, maybe can use all numeric as constraint ??? *)
    (* Get only rational discrete for constraint encoding *)
    let only_discrete_rational_values = List.filter (fun (discrete_index, discrete_value) -> AbstractValue.is_rational_value discrete_value) discrete_values in
    (* map to num const *)
    let discrete_rational_numconst_values = List.map (fun (discrete_index, discrete_value) -> discrete_index, AbstractValue.numconst_value discrete_value) only_discrete_rational_values in

	(* Constraint of the form D_i = d_i *)
	LinearConstraint.pxd_constraint_of_point discrete_rational_numconst_values


(*------------------------------------------------------------*)
(** Compute the invariant associated to a location   *)
(*------------------------------------------------------------*)
let compute_plain_continuous_invariant (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : LinearConstraint.pxd_linear_constraint =
    (* construct invariant *)
	let invariants : AbstractModel.invariant list = AbstractModelUtilities.get_model_invariants model location in
	let _ (* discrete_invariants *), continuous_invariants = AbstractModelUtilities.split_guards_into_discrete_and_continuous invariants in
	(* Perform the intersection *)
	LinearConstraint.pxd_intersection continuous_invariants


(*------------------------------------------------------------*)
(** Compute the invariant I_l associated to a location, including renaming and time elapse. Uses cache.  *)
(*------------------------------------------------------------*)
let compute_invariant (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : LinearConstraint.pxd_linear_constraint =
	(* Strip off discrete for caching scheme  *)
	let locations = DiscreteState.get_locations location in
	(* check in cache *)
	let entry = Cache.find inv_cache locations in
	match entry with
		| Some inv -> inv
		| None -> (
			(* Build plain invariant I_l(X) *)
			let invariant = compute_plain_continuous_invariant model location in
			(* Store in cache *)
			Cache.store inv_cache locations invariant;
			invariant
		)



(*------------------------------------------------------------*)
(* Compute the invariant associated to a location and valuate the value of the discrete variables   *)
(*------------------------------------------------------------*)
let compute_valuated_invariant (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : LinearConstraint.px_linear_constraint =
	(* Compute the invariant with the discrete variables *)
	let invariant = compute_plain_continuous_invariant model location in

	(* Valuate the discrete variables *)
	let discrete_constraint = discrete_constraint_of_global_location model location in

	(* Perform intersection of C(X) and I_l0(X) and D_i = d_i *)
	print_message Verbose_high ("Performing intersection of I_l(X) and D_i = d_i");
	let current_constraint = LinearConstraint.pxd_intersection [invariant ; discrete_constraint ] in
	(* Print some information *)
	if verbose_mode_greater Verbose_total then
		print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names current_constraint);

	(* Eliminate the discrete variables *)
	print_message Verbose_high ("Hide discrete");
	LinearConstraint.pxd_hide_discrete_and_collapse current_constraint


(************************************************************)
(************************************************************)
(** Structure to define sets of state_index *)
(************************************************************)
(************************************************************)

(* state struct for constructing set type *)
module State = struct
	type t = state_index
	let compare = compare
end

(* set of states for efficient lookup *)
module StateIndexSet = Set.Make(State)




(**************************************************************)
(* Encapsulation in a class *)
(*** NOTE: technically, we could better create a *generic* class and instantiate it with StateIndexSet when needed ***)
(**************************************************************)
class stateIndexSet =
	object (self)

	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Initially, the set is empty *)
	val mutable the_set = StateIndexSet.empty
	

	
	(************************************************************)
	(* Access methods *)
	(************************************************************)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Is the set empty? *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method is_empty =
		StateIndexSet.is_empty the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Does the set contain a given element? *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method mem element =
		StateIndexSet.mem element the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Retrieve the number of elements *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method cardinal =
		StateIndexSet.cardinal the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Retrieve all elements in the form of a list *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method all_elements =
		StateIndexSet.elements the_set
	
	
	
	(************************************************************)
	(* Modification methods *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Empty the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method empty =
		(* Directly replace the set with a new empty set *)
		the_set <- StateIndexSet.empty
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add an element to the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add element =
		the_set <- StateIndexSet.add element the_set

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Remove an element; raises Not_found if the element was not in the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method remove element =
		(* First check *)
		if not (self#mem element) then raise Not_found;
		(* Then remove *)
		self#remove_or_do_nothing element

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Remove an element or do nothing if the element was not in the set *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method remove_or_do_nothing element =
		the_set <- StateIndexSet.remove element the_set
	

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
