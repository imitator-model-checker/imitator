(************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: main virtual class to explore the state space: only defines post-related function, i.e., to compute the successor states of ONE state
 *
 * File contributors : Étienne André
 * Created           : 2015/12/02
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open ImitatorUtilities
open AlgoGeneric
open State



(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)

(** Type to define the state_index that have unexplored successors in case of premature termination *)
type unexplored_successors =
	(* Not defined (i.e., not yet defined, or no premature termination) *)
	| UnexSucc_undef
	(* A list of states with unexplored successors *)
	| UnexSucc_some of state_index list


(*** NOTE: made public only because used in AlgoEFOptQueue ***)
type exploration_limit_reached =
	(* No limit *)
	| Keep_going

	(* Termination due to time limit reached *)
	| Time_limit_reached

	(* Termination due to state space depth limit reached *)
	| Depth_limit_reached

	(* Termination due to a number of explored states reached *)
	| States_limit_reached

	(* Termination because a witness has been found *)
	| Witness_found


exception LimitDetectedException of exploration_limit_reached

(************************************************************)
(* Statistics *)
(************************************************************)

(*** NOTE: made public only because used in AlgoEFOptQueue ***)
val counter_explore_using_strategy : Statistics.hybridCounter


(**************************************************************)
(* Class-independent functions *)
(**************************************************************)

(*------------------------------------------------------------*)
(* Compute the list of stopped and elapsing clocks in a location *)
(* Returns a pair (stopped clocks, elapsing clocks)           *)
(*------------------------------------------------------------*)
(* val compute_stopwatches : DiscreteState.global_location -> (Automaton.clock_index list * Automaton.clock_index list) *)

(*------------------------------------------------------------------*)
(* Get the list of updates from a combined transition               *)
(* Function by Étienne André                                        *)
(* original_location  : the original location, needed to test the Boolean expressions*)
(* combined_transition: the combined_transition in which the updates are sought *)
(*------------------------------------------------------------------*)
(* Returns a pair of the list of clock updates and discrete updates *)
(*------------------------------------------------------------------*)
(*val get_updates_in_combined_transition : DiscreteState.global_location -> StateSpace.combined_transition -> AbstractModel.clock_updates * (DiscreteExpressions.discrete_update list)*)


(*------------------------------------------------------------------*)
(* Compute a new location for a combined_transition                 *)
(* combined_transition: the transition involved                     *)
(* source_location    : the source location                         *)
(*------------------------------------------------------------------*)
(* returns the new location, the discrete guards (a list of d_linear_constraint), the continuous guards (a list of pxd_linear_constraint) and the updates *)
(*------------------------------------------------------------------*)
(* val compute_new_location_guards_updates : AbstractModel.abstract_model -> DiscreteState.global_location -> StateSpace.combined_transition -> (DiscreteState.global_location * DiscreteExpressions.nonlinear_constraint list * LinearConstraint.pxd_linear_constraint list * AbstractModel.clock_updates list) *)


(*------------------------------------------------------------*)
(** Apply time elapsing in location to the_constraint (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
(* val apply_time_elapsing : DiscreteState.global_location -> LinearConstraint.pxd_linear_constraint -> unit *)

(*------------------------------------------------------------*)
(** Apply time past in location to the_constraint (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
val apply_time_past : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.pxd_linear_constraint -> unit


(*------------------------------------------------------------*)
(** Apply time elapsing in location to the_constraint (Answer might not be correct if PTA has stopwatches) *)
(*------------------------------------------------------------*)
val apply_time_elapsing_no_stopwatch : LinearConstraint.pxd_linear_constraint -> unit

(*------------------------------------------------------------*)
(** Apply time past in location to the_constraint (Answer might not be correct if PTA has stopwatches) *)
(*------------------------------------------------------------*)
val apply_time_past_no_stopwatch : LinearConstraint.pxd_linear_constraint -> unit

(*------------------------------------------------------------*)
(** Apply time elapsing in location to a concrete valuation (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
val apply_time_elapsing_to_concrete_valuation : AbstractModel.abstract_model -> DiscreteState.global_location -> NumConst.t -> LinearConstraint.px_valuation -> LinearConstraint.px_valuation


(*------------------------------------------------------------*)
(** Compute the list of successor states of a given state, and returns the list of new states *)
(*------------------------------------------------------------*)
(*** NOTE (ÉA, 2023/06/05): new class-independent version copied from method `post_from_one_state` from class `AlgoStateBased`, but with no dependency with anything, notably the state space ***)
val combined_transitions_and_states_from_one_state_functional : Options.imitator_options -> AbstractModel.abstract_model -> State.state -> (StateSpace.combined_transition * State.state) list


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(** Compute the initial state with the initial invariants and time elapsing; takes a boolean denoting whether we should abort whenever the initial state is unsatisfiable *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
val create_initial_state : Options.imitator_options -> AbstractModel.abstract_model -> bool -> State.state

(*------------------------------------------------------------*)
(** Given `Zn-1` and `Zn` such that `Zn` is the successor zone of `Zn-1` by guard `g-1` and updating variables in `Un-1` to some values (that we do not need to know as we know the zone), given `Zn+1` a set of concrete points (valuations) successor of zone `Zn` by elapsing of a set of variables `t` and non-elapsing of others `nont`, by guard `gn`, updates `Rn`, then `nnconvex_constraint_zone_predecessor_g_u(Zn-1, gn-1, Un-1, Zn, t, nont, gn, Un, Zn+1)` computes the subset of points in `Zn` that are predecessors of `Zn` (by updates of `Un`, guard `gn`, elapsing of `t`, non-elapsing of `nont`), and that are direct successors (without time elapsing) of `Zn-1` via `gn-1` and `Un-1`. *)
(*------------------------------------------------------------*)
(*** NOTE: no check is made that Zn is a successor of Zn-1, nor that Zn+1 is a subset of Zn ***)
(*** NOTE: no check is made that t and nont represent exactly the set of variables used in the polyhedra. ***)
(*------------------------------------------------------------*)
(*val constraint_zone_predecessor_g_u :
	(* Zn-1 *) LinearConstraint.px_linear_constraint ->
	(* gn-1 *) LinearConstraint.pxd_linear_constraint ->
	(* Un-1 *) AbstractModel.clock_updates list ->
	(* Zn *)   LinearConstraint.px_linear_constraint ->
	(* t *)    (Automaton.variable_index list) ->
	(* nont *) (Automaton.variable_index list) ->
	(* gn *)   LinearConstraint.pxd_linear_constraint ->
	(* Un *)   AbstractModel.clock_updates list ->
	(* Zn+1 *) LinearConstraint.px_linear_constraint ->
	LinearConstraint.px_linear_constraint*)



(*------------------------------------------------------------*)
(** Reconstruct a (valid) concrete run from a symbolic run *)
(*------------------------------------------------------------*)
(* val concrete_run_of_symbolic_run : AbstractModel.abstract_model -> StateSpace.stateSpace -> StateSpace.symbolic_run -> (Automaton.variable_index -> NumConst.t) -> StateSpace.concrete_run *)

(*------------------------------------------------------------*)
(** Reconstruct a whole counterexample from the initial state to a given target state. Return a list of pairs (valuation * absolute time) *)
(*------------------------------------------------------------*)
val reconstruct_counterexample : AbstractModel.abstract_model -> StateSpace.stateSpace -> state_index -> StateSpace.concrete_run



(*------------------------------------------------------------*)
(** Check if any limit is reached, and raises the necessary exception. Type `option` are used for algorithms which might not encode all such data. *)
(*------------------------------------------------------------*)
val check_limits : Options.imitator_options -> int option -> int option -> float option -> unit


(************************************************************)
(* Class-independent functions on constraints *)
(************************************************************)

(*(* Project a p-constraint on selected parameters if requested by the property, or return the constraint unchanged *)
val project_p_constraint_if_requested : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> LinearConstraint.p_linear_constraint -> LinearConstraint.p_linear_constraint*)
(** Project a p_nnconvex_constraint on selected parameters if requested by the property, or return the constraint unchanged *)
val project_p_nnconvex_constraint_if_requested : AbstractModel.abstract_model -> AbstractProperty.abstract_property -> LinearConstraint.p_nnconvex_constraint -> LinearConstraint.p_nnconvex_constraint


(*------------------------------------------------------------*)
(** Converts a timed interval into a linear constraint of the form `global_clock \in interval` (e.g., `global_clock > c` for an interval `(c, infinity)`, or `d1 <= global_clock < d2` for an interval `[d1, d2]` *)
(*------------------------------------------------------------*)
val px_linear_constraint_of_timed_interval : AbstractModel.abstract_model -> AbstractProperty.timed_interval -> LinearConstraint.px_linear_constraint

(*------------------------------------------------------------*)
(** Converts a timed interval into a linear constraint of the form `global_clock \leq term` where `term` is the upper bound of a timed interval, or returns None for intervals with no upper bound (e.g. (5, infinity) *)
(*------------------------------------------------------------*)
val upper_bound_px_linear_constraint_option_of_timed_interval : AbstractModel.abstract_model -> AbstractProperty.timed_interval -> LinearConstraint.px_linear_constraint option

(*------------------------------------------------------------*)
(** `intersect_with_timed_interval_constraint_option model c_option c` returns the same constraint `c` if `c_option = None`, and a new constraint `c ^ c_option` otherwise. The model is only used for pretty printing (variable names). *)
(*------------------------------------------------------------*)
val intersect_with_timed_interval_constraint_option : AbstractModel.abstract_model -> LinearConstraint.px_linear_constraint option -> LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint




(*
(************************************************************)
(************************************************************)
(* Class definition for state_index waiting lists *)
(************************************************************)
(************************************************************)
class waiting_list :
	object

		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Add a state to the waiting list *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method add : state_index -> unit

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)*)



(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoStateBased : AbstractModel.abstract_model -> Options.imitator_options ->
	object inherit algoGeneric

		(************************************************************)
		(* Class variables *)
		(************************************************************)

		(*** TODO: make private (while accessible to subclasses ***)
		val mutable state_space : StateSpace.stateSpace

		(* Function to be called from the distributed IMITATOR *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable patator_termination_function : (unit -> unit) option


		(** Status of the analysis *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable termination_status : Result.state_based_algorithm_termination option

		(** List of state_index that have unexplored successors in case of premature termination *)
		val mutable unexplored_successors : unexplored_successors

		(** Variable to denote whether the analysis may continue, or whether the analysis should terminate; useful to terminate, e.g., when a witness is found (at least for BFS algorithms) *)
		val mutable algorithm_keep_going : bool

		(** The current new state indexes *)
		val mutable new_states_indexes : state_index list

		(** Variable to remain of the termination *)
		(*** NOTE: public only for AlgoEFoptQueue ***)
		val mutable limit_reached : exploration_limit_reached

		(** Non-necessarily convex constraint storing the parameter synthesis result (for selected algorithm) *)
		val mutable synthesized_constraint : LinearConstraint.p_nnconvex_constraint

		(*** NOTE: only used for exemplification purpose ***)
		(** Positive examples spotted (positive examples: concrete runs to the target state) *)
		val mutable positive_examples : Result.valuation_and_concrete_run list

		(*** NOTE: only used for exemplification purpose ***)
		(** Negative examples spotted (negative examples: *impossible* concrete runs to the target state) *)
		val mutable negative_examples : Result.valuation_and_concrete_run list

		(*** NOTE: only used for exemplification purpose ***)
		val nb_POSITIVE_EXAMPLES_MAX : int
		val nb_NEGATIVE_EXAMPLES_MAX : int

		(************************************************************)
		(* Class methods *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Write a message preceded by "[algorithm_name]" *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_algo_message : verbose_mode -> string -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Write a message preceded by "\n[algorithm_name]" *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_algo_message_newline : verbose_mode -> string -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Methods to simplify the option handling *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method get_initial_px_constraint_or_die : LinearConstraint.px_linear_constraint
		method get_initial_p_constraint_or_die : LinearConstraint.p_linear_constraint
		method get_initial_p_nnconvex_constraint_or_die : LinearConstraint.p_nnconvex_constraint

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Set the PaTATOR termination function *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_patator_termination_function : (unit -> unit) -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the p-constraint only if it is not cached using the mini-cache system *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_p_constraint_with_minicache : (LinearConstraint.px_linear_constraint -> LinearConstraint.p_linear_constraint)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Reset the mini-cache *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method reset_minicache : unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the projection of a PX-constraint is included into the `synthesized_constraint` *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_whether_px_included_into_synthesized_constraint : LinearConstraint.px_linear_constraint -> bool


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Simple Boolean denoting whether we should abort if the initial state is unsatisfiable (basically, we should always abort, except for Validity-synthesis) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method abort_if_unsatisfiable_initial_state : bool

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the initial state with the initial invariants and time elapsing, and check whether it is satisfiable; if not, raise UnsatisfiableInitialState *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_initial_state_or_abort : State.state


		(*------------------------------------------------------------*)
		(* Add a new state to the state space (if indeed needed) *)
		(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
		(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
		(*------------------------------------------------------------*)
		(*** TODO: return the list of actually added states ***)
		method virtual add_a_new_state : state_index -> StateSpace.combined_transition -> State.state -> bool




		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*** NOTE: made public only for EFoptQueue ***)
		method post_from_one_state : state_index -> state_index list

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Add a transition to the state space *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method add_transition_to_state_space : (state_index * StateSpace.combined_transition * state_index) -> StateSpace.addition_result -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method try_termination_at_initial_state : Result.imitator_result option

(*		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
 		method virtual process_initial_state : State.state -> bool *)


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Generate counter-example(s) if required by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method construct_counterexamples : state_index -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Create 1 positive and (up to) 2 negative examples (of type `option` in case could not be exhibited) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method exhibit_3_counterexamples : State.state_index-> (Result.valuation_and_concrete_run * Result.valuation_and_concrete_run option * Result.valuation_and_concrete_run option)


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform when meeting a state with no successors: virtual method to be defined in subclasses *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual process_deadlock_state : state_index -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed) *)
		(*** NOTE: this is in fact a BFS function ***)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual process_post_n : state_index list -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
		(*** NOTE: this is in fact a BFS function ***)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual check_termination_at_post_n : bool

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the property is a #witness mode; if so, raise TerminateAnalysis *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method terminate_if_witness : AbstractProperty.abstract_property -> unit


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Main method to run the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : Result.imitator_result

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Packaging the result at the end of the exploration (to be defined in subclasses) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual compute_result : Result.imitator_result

		method explore_layer_bfs : state_index -> unit

		method explore_queue_bfs : state_index -> unit

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
