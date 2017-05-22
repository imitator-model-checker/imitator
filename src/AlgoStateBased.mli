(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: main virtual class to explore the state space: only defines post-related function, i.e., to compute the successor states of ONE state
 * 
 * File contributors : Étienne André
 * Created           : 2015/12/02
 * Last modified     : 2017/03/19
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

(* Type to define the state_index that have unexplored successors in case of premature termination *)
type unexplored_successors =
	(* Not defined (i.e., not yet defined, or no premature termination) *)
	| UnexSucc_undef
	(* A list of states with unexplored successors *)
	| UnexSucc_some of state_index list
	

(**************************************************************)
(* Class-independent functions *)
(**************************************************************)
val compute_initial_state_or_abort : unit -> State.state


(* val compute_plain_invariant : Location.global_location -> LinearConstraint.pxd_linear_constraint *)

(*------------------------------------------------------------*)
(* Compute the invariant associated to a location and valuate the value of the discrete variables   *)
(*------------------------------------------------------------*)
val compute_valuated_invariant : Location.global_location -> LinearConstraint.px_linear_constraint


(*------------------------------------------------------------*)
(** Apply time elapsing in location to the_constraint (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
(* val apply_time_elapsing : Location.global_location -> LinearConstraint.pxd_linear_constraint -> unit *)

(*------------------------------------------------------------*)
(** Apply time past in location to the_constraint (the location is needed to retrieve the stopwatches stopped in this location) *)
(*------------------------------------------------------------*)
val apply_time_past : Location.global_location -> LinearConstraint.pxd_linear_constraint -> unit



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
(************************************************************)



(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoStateBased :
	object inherit algoGeneric

		(************************************************************)
		(* Class variables *)
		(************************************************************)
		
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable state_space : StateSpace.state_space

		(* Nature of the state space according to a property *)
		val mutable statespace_nature : StateSpace.statespace_nature
		
		(* Function to be called from the distributed IMITATOR *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable patator_termination_function : (unit -> unit) option


		(* Status of the analysis *)
		(*** TODO: make private (while accessible to subclasses ***)
		val mutable termination_status : Result.bfs_algorithm_termination option

		(* Constraint of the initial state (used by some algorithms to initialize their variables) *)
		val mutable initial_constraint : LinearConstraint.px_linear_constraint option
		
		(* List of state_index that have unexplored successors in case of premature termination *)
		val mutable unexplored_successors : unexplored_successors
		
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Write a message preceeded by "[algorithm_name]" *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_algo_message : verbose_mode -> string -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Write a message preceeded by "\n[algorithm_name]" *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_algo_message_newline : verbose_mode -> string -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Variable initialization (to be defined in subclasses) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method initialize_variables : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set the PaTATOR termination function *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_patator_termination_function : (unit -> unit) -> unit
	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Update the nature of the trace set *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method update_statespace_nature : State.state -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Create a StateSpace.state_comparison from the options *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method state_comparison_operator_of_options : StateSpace.state_comparison

		
		(*------------------------------------------------------------*)
		(* Add a new state to the reachability_graph (if indeed needed) *)
		(* Side-effects: modify new_states_indexes *)
		(*** TODO: move new_states_indexes to a variable of the class ***)
		(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
		(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
		(*------------------------------------------------------------*)
		(*** TODO: simplify signature by removing the state_index list ref and the action_index, and by returning the list of actually added states ***)
		method virtual add_a_new_state : state_index -> state_index list ref -> Automaton.action_index -> Location.global_location -> LinearConstraint.px_linear_constraint -> bool
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Add a transition to the state space *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method add_transition_to_state_space : (state_index * Automaton.action_index * state_index) -> StateSpace.addition_result -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual process_initial_state : State.state -> bool
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Actions to perform when meeting a state with no successors: virtual method to be defined in subclasses *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual process_deadlock_state : state_index -> unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Compute the list of successor states of a given state, and update the state space; returns the list of new states' indexes actually added *)
		(** TODO: to get a more abstract method, should get rid of the state space, and update the state space from another function ***)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 		method post_from_one_state : StateSpace.state_space -> state_index -> state_index list *)

		
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
		(* Main method to run the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : unit -> Result.imitator_result
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual compute_result : Result.imitator_result

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
