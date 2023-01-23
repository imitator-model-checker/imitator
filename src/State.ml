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

(* Counter for updates of continuous variables (mostly PPL) *)
let counter_updates_assign = create_hybrid_counter_and_register "State.updates_assign" States_counter Verbose_experiments
(* Counter for updates of continuous variables (mostly PPL) *)
let counter_clock_updates_assign = create_discrete_counter_and_register "State.clock_updates_assign" States_counter Verbose_experiments



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

    (* Get discrete rational (can be encoded as constraint) *)
	let discrete_rational_values = List.map (fun discrete_index ->
	    (* Get variable name of by index *)
	    let variable_name = model.variable_names discrete_index in
	    (* Get value of GLOBAL variable variable_name *)
	    let value = DiscreteState.get_discrete_value_by_name global_location variable_name in
	    discrete_index, value
    ) model.discrete_rationals in
    (* Map to num const *)
    let discrete_rational_numconst_values = List.map (fun (discrete_index, discrete_value) -> discrete_index, AbstractValue.numconst_value discrete_value) discrete_rational_values in

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
(** Operations on states: computation of successors and predecessors *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Time-elapsing and time-past *)
(************************************************************)

(*------------------------------------------------------------*)
(* Generic function to apply the updates to a linear constraint (either by intersection with the updates, or by existential quantification) *)
(* time_direction   : if Forward, then apply updates; if Backward, apply 'inverted' updates *)
(* linear_constraint: the linear constraint (modified by this function) *)
(* clock_updates    : the list of clock updates to apply *)
(*------------------------------------------------------------*)
(*** TO OPTIMIZE: use cache (?) *)
let apply_updates_assign_gen (time_direction: LinearConstraint.time_direction) (model : AbstractModel.abstract_model) (linear_constraint : LinearConstraint.pxd_linear_constraint) (clock_updates : AbstractModel.clock_updates list) =

	(* Statistics *)
	counter_updates_assign#increment;
	counter_updates_assign#start;


	if clock_updates <> [] then(

		(* Statistics *)
		counter_clock_updates_assign#increment;


		(* Merge updates *)

		(*** TO OPTIMIZE: only create the hash if there are indeed some resets/updates ***)

		let clocks_hash = Hashtbl.create model.nb_clocks in

		(* Check wether there are some complex updates of the form clock' = linear_term *)
		let arbitrary_updates = ref false in
		(* Iterate on the lists of clocks for all synchronized automata *)
		List.iter (fun local_updates ->
			match local_updates with
			| No_update -> ()
			| Resets list_of_clocks ->
				(* Iterate on the clocks, for a given automaton *)
				List.iter (fun clock_id ->
					(* Assign this clock to true in the table *)
					Hashtbl.replace clocks_hash clock_id (LinearConstraint.make_pxd_linear_term [] NumConst.zero);
				) list_of_clocks;
			| Updates list_of_clocks_lt ->
				(* Set the flag *)
				arbitrary_updates := true;
				(* Iterate on the clocks, for a given automaton *)
				List.iter (fun (clock_id, linear_term) ->
					(* Update the update *)
					Hashtbl.replace clocks_hash clock_id linear_term;
				) list_of_clocks_lt;
		) clock_updates;

		(* THREE CASES: no updates, only resets (to 0) or updates (to linear terms) *)

		(* CASE 1: no update nor reset *)
		if Hashtbl.length clocks_hash = 0 then (

			(* do nothing! *)

			(* Print some information *)
			print_message Verbose_total ("\n -- No resets to handle here");

		(* CASE 2: only resets *)
		)else(if not !arbitrary_updates then(

			(* Print some information *)
			print_message Verbose_total ("\n -- Case only resets");


			(*** TODO : add "reset" function to LinearConstraint ***)


			(*** TO OPTIMIZE: Hashtbl.fold and List.map should be merged into one function ***)

			(* Compute the list of clocks to update from the hashtable *)
			let list_of_clocks_to_update = Hashtbl.fold (fun clock_id _ list_of_clocks -> clock_id :: list_of_clocks) clocks_hash [] in

			(* Hide clocks updated within the linear constraint, viz., exists X' : lc, for X' in rho(X) *)
			(* Print some information *)
			print_message Verbose_total ("\n -- Remove reset clocks");

			(* Eliminate variables *)
			LinearConstraint.pxd_hide_assign list_of_clocks_to_update linear_constraint;

			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names linear_constraint);
			);

			(* Case 2a: forward updates, i.e., add `X = 0` *)
			if time_direction = LinearConstraint.Time_forward then(
				(* Compute X = 0 for the variables appearing in resets *)
				print_message Verbose_total ("\n -- Computing resets of the form `X = 0`");
				let updates =
					(List.map (fun variable_index ->
						(* Consider cases for clocks *)
						match model.type_of_variables variable_index with
						(* Clocks: X = 0 *)
						| DiscreteType.Var_type_clock ->
							let x_lt = LinearConstraint.make_pxd_linear_term [
								NumConst.one, variable_index;
							] NumConst.zero in
							LinearConstraint.make_pxd_linear_inequality x_lt LinearConstraint.Op_eq
						| _ -> raise (InternalError "Only clocks can be updated.")
					) list_of_clocks_to_update) in
				(* Create the constraint *)
				let updates = LinearConstraint.make_pxd_constraint updates in
				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names updates);
				);

				(* Add the constraints X = 0 *)
				(* Print some information *)
				print_message Verbose_total ("\n -- Adding `X = 0` for reset clocks");

				(* Apply intersection *)
				LinearConstraint.pxd_intersection_assign linear_constraint [updates];

				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names linear_constraint);
				);
			); (* end if LinearConstraint.Time_forward *)

		(* CASE 3: updates to linear terms *)
		)else(

			print_message Verbose_total ("\n -- Case updates to linear terms");

			(*** TODO (not urgent) : add "update" function to LinearConstraint ***)

			(* Compute the pairs (x_i , linear_term) from the hashtable *)
			let updates : (Automaton.clock_index * LinearConstraint.pxd_linear_term) list = Hashtbl.fold (fun clock_id linear_term current_updates -> (clock_id, linear_term) :: current_updates) clocks_hash [] in

			(* Case 3a: existential quantification requires to create primed variables *)

			(** TO OPTIMIZE (?): could be performed statically (when converting the model).
				PRO: save time because no need to compute this for each constraint;
				CON: lose time & memory (but maybe not that much) at some point because operations on constraints will have all dimensions instead of just the updated prime variables
				TO OPTIMIZE (other option): merge all operations together, so that no need for hashtable
			*)

			(* CASE 3, step 1: Compute the correspondance between clocks X_i and renamed clocks X_i' *)
			let prime_of_variable = Hashtbl.create (List.length updates) in
			let variable_of_prime = Hashtbl.create (List.length updates) in
			let clock_prime_id = ref model.nb_ppl_variables in
			List.iter (fun (clock_id, _) ->
				Hashtbl.add prime_of_variable clock_id !clock_prime_id;
				Hashtbl.add variable_of_prime !clock_prime_id clock_id;
				(* Debug message *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total ("\nThe primed index of variable `" ^ (model.variable_names clock_id) ^ "` (index = " ^ (string_of_int clock_id) ^ ") is set to " ^ (string_of_int !clock_prime_id) ^ ".")
				);
				(* Increment the prime id for next variable *)
				clock_prime_id := !clock_prime_id + 1;
				()
			) updates;
			let new_max_dimension = !clock_prime_id in
			let extra_dimensions = new_max_dimension - model.nb_ppl_variables in
			print_message Verbose_total ("\nNew dimension for constraints: " ^ (string_of_int new_max_dimension) ^ "; extra dimensions : " ^ (string_of_int extra_dimensions) ^ ".");
			(* Extend the number of dimensions *)
			LinearConstraint.set_dimensions model.nb_parameters (model.nb_clocks + extra_dimensions) model.nb_rationals;
			LinearConstraint.pxd_add_dimensions extra_dimensions linear_constraint;

			(* Compute pairs (X_i', X_i) *)
			let clocks_and_primes = Hashtbl.fold (fun clock_id clock_prime_id pairs -> (clock_id, clock_prime_id) :: pairs) prime_of_variable [] in

			(* CASE 3, step 2: Create primed constraints *)
			let inequalities = List.map (fun (clock_id, linear_term) ->

				let possibly_primed_clock_index, possibly_primed_linear_term =
				(* Forward update: create `X_i' = linear_term` *)
				if time_direction = LinearConstraint.Time_forward then(
					(Hashtbl.find prime_of_variable clock_id),
					linear_term
				)else(
				(* Backward update: create `X_i = linear_term'` *)
					clock_id,
					(LinearConstraint.rename_pxd_linear_term clocks_and_primes linear_term)
				) in

				(* Build `linear_term - clock_id' = 0` or  `linear_term' - clock_id = 0` *)
				LinearConstraint.make_pxd_linear_inequality (
					LinearConstraint.add_pxd_linear_terms
						(* 1: The update linear term *)
						possibly_primed_linear_term
						(* 2: - clock_id' *)
						(LinearConstraint.make_pxd_linear_term [
								NumConst.minus_one, possibly_primed_clock_index;
							] NumConst.zero)
				) LinearConstraint.Op_eq
			) updates in

			(* Create the constraint *)
			let inequalities = LinearConstraint.make_pxd_constraint inequalities in

			(* Print some information *)
			let print_constraint c =
				if verbose_mode_greater Verbose_total then(
					let all_variable_names = fun variable_id ->
						if variable_id < model.nb_ppl_variables then
							model.variable_names variable_id
						else
							(model.variable_names (Hashtbl.find variable_of_prime variable_id)) ^ "'"
					in
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint all_variable_names c);
				)else(
					()
				)
			in

			print_constraint inequalities;

			(* Add the constraints X_i' = linear_term *)

			(* Print some information *)
			print_message Verbose_total ("\n -- Adding `X_i' = linear_term` for updated clocks");
			(* Apply intersection *)
			LinearConstraint.pxd_intersection_assign linear_constraint [inequalities];
			(* Print some information *)
			print_constraint linear_constraint;

			(* Remove the variables X_i *)
			let list_of_clocks_to_hide, _ = List.split updates in
			(* Hide clocks updated within the linear constraint, viz., exists X_i : lc, for X_i in rho(X) *)
			print_message Verbose_total ("\n -- Computing exists `X : lc` for updated clocks");
			LinearConstraint.pxd_hide_assign list_of_clocks_to_hide linear_constraint;
			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);

			(* Renames clock X_i' into X_i *)
			(** TO OPTIMIZE !! *)
			print_message Verbose_total ("\n -- Renaming clocks X_i' into X_i for updated clocks");
			LinearConstraint.pxd_rename_variables_assign clocks_and_primes linear_constraint;
			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);

			(* Go back to the original number of dimensions *)
			print_message Verbose_total ("\nGo back to standard dimension for constraints: " ^ (string_of_int model.nb_ppl_variables) ^ ".");
			LinearConstraint.set_dimensions model.nb_parameters model.nb_clocks model.nb_rationals;
			LinearConstraint.pxd_remove_dimensions extra_dimensions linear_constraint;
			(* Print some information *)
			if verbose_mode_greater Verbose_total then(
				print_constraint linear_constraint;
			);

(*			(* Case 3b: without existential quantification, just intersect with updates *)
			) else (

				(* Create constraints X_i = linear_term *)
				let inequalities = List.map (fun (clock_id, linear_term) ->
					(* Build linear_term - clock_id = 0 *)
					LinearConstraint.make_pxd_linear_inequality (
						LinearConstraint.add_pxd_linear_terms
							(* 1: The update linear term *)
							linear_term
							(* 2: - clock_id *)
							(LinearConstraint.make_pxd_linear_term [
									NumConst.minus_one, clock_id;
								] NumConst.zero)
					) LinearConstraint.Op_eq
				) updates in

				(* Create the constraint *)
				let inequalities = LinearConstraint.make_pxd_constraint inequalities in

				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total ("\nConstraint before intersection with update constraint…");
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names inequalities);
				);

				(* Apply intersection *)
				LinearConstraint.pxd_intersection_assign linear_constraint [inequalities];

				(* Print some information *)
				if verbose_mode_greater Verbose_total then(
					print_message Verbose_total ("\nConstraint after intersection with update constraint…");
					print_message Verbose_total (LinearConstraint.string_of_pxd_linear_constraint model.variable_names inequalities);
				);


			)*)

			(*** TODO: check what about discrete variables ?!! ***)
		) (* end CASE 3 *)
		)
	); (* end if some clock updates *)

	(* Statistics *)
	counter_updates_assign#stop;

	(* Bye bye *)
	()


(*------------------------------------------------------------*)
(* Apply the updates to a linear constraint by existential quantification (that is, when applying x := x+y+i+1, "x" is replaced with "x+y+i+1" *)
(*------------------------------------------------------------*)
let apply_updates_assign_forward = apply_updates_assign_gen LinearConstraint.Time_forward


(*------------------------------------------------------------*)
(* Apply the updates to a linear constraint by existential quantification and with backward direction (that is, when applying x := x+y+i+1, "x+y+i+1" is replaced with "x" *)
(*------------------------------------------------------------*)
let apply_updates_assign_backward = apply_updates_assign_gen LinearConstraint.Time_backward



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
