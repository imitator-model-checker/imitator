(************************************************************
 *
 *                       IMITATOR
 * 
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: validity synthesis, i.e., return the set of valuations for which the initial state is defined
 * 
 * File contributors : Étienne André
 * Created           : 2022/10/07
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open AbstractProperty
open Result
open AlgoStateBased
open Statistics
open State




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoValid (model : AbstractModel.abstract_model) =
	object (self) inherit algoStateBased model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "Valid"

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Simple Boolean denoting whether we should abort if the initial state is unsatisfiable (basically, we should always abort, except for Validity-synthesis) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method abort_if_unsatisfiable_initial_state : bool = false


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
	(*** NOTE: this function is overridden here ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method try_termination_at_initial_state : Result.imitator_result option =

		(* Terminate in any case *)

		(* Set termination status *)
		termination_status <- Some (Result.Regular_termination);

		(* Immediate termination *)
		Some (Result.Single_synthesis_result {
			(* Good valuations *)
			result				= Good_constraint (self#get_initial_p_nnconvex_constraint_or_die, Result.Constraint_exact);

			(* English description of the constraint *)
			constraint_description= "constraint guaranteeing validity";

			(* Explored state space *)
			state_space			= state_space;

			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;

			(* Termination *)
			termination			= Result.Regular_termination;
		}
	)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state space (if indeed needed) *)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: return the list of actually added states ***)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state source_state_index combined_transition new_state =
		raise (InternalError "Function AlgoValid:add_a_new_state should never be called")
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result : Result.imitator_result =
		raise (InternalError "not implemented")


	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
