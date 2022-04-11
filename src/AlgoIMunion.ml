(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IMKunion algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/08
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open Ppl_ocaml
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoIMK
open State



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoIMunion (pval : PVal.pval) =
	object (self) inherit algoIMK pval as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* List of last states *)
(* 	val mutable last_states : StateSpace.state_index list = [] *)

	(* Non-necessarily convex parameter constraint *)
	val mutable result : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "IMunion"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
(* 		last_states <- []; *)

		result <- LinearConstraint.false_p_nnconvex_constraint ();
		
		(* The end *)
		()
		
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: add the deadlock state to the list of last states *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index =
		self#print_algo_message_newline Verbose_low ("found a state with no successor");
		
		(* Get the state *)
		let px_constraint = (StateSpace.get_state state_space state_index).px_constraint in
		(* Projet onto P *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
		(* Add the constraint to the result *)
		LinearConstraint.p_nnconvex_p_union_assign result p_constraint
		
(*		(* Add to the list of last states *)
		last_states <- state_index :: last_states*)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state that is on a loop *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_looping_state state_index =
		self#print_algo_message_newline Verbose_low ("found a state in a loop");
		
		(* Get the state *)
		let px_constraint = (StateSpace.get_state state_space state_index).px_constraint in
		(* Projet onto P *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
		(* Add the constraint to the result *)
		LinearConstraint.p_nnconvex_p_union_assign result p_constraint
		(* Add to the list of last states *)
(* 		last_states <- state_index :: last_states *)

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		
		(* IMunion: return the disjunction of all constraints AND the current constraint, viz., the constraint of the first state (necessary because the states accumulated may have been restricted with some neg J since they were added to "result") *)

		(*** NOTE: code copied from AlgoIMK ***)
		(*** NOTE: better not use just "0" as the initial state may have been merged with another state ***)
		let initial_state_index = StateSpace.get_initial_state_index state_space in
		let initial_state = StateSpace.get_state state_space initial_state_index in
		(* Retrieve the constraint of the initial state *)
		let px_constraint = initial_state.px_constraint in
		
		self#print_algo_message_newline Verbose_total ("projecting the initial state constraint onto the parameters...");
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in

		self#print_algo_message_newline Verbose_total ("adding the initial constraint to the result");
		LinearConstraint.p_nnconvex_intersection_assign result p_constraint;
		
		
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in IMunion.compute_result")
			| Some status -> status
		in

		(* The state space nature is good if 1) it is not bad, and 2) the analysis terminated normally *)
		(*** NOTE: unsure of this computation (if it has any meaning for this algorithm anyway) ***)
		let statespace_nature =
			if statespace_nature = StateSpace.Unknown && termination_status = Regular_termination then StateSpace.Good
			(* Otherwise: unchanged *)
			else statespace_nature
		in

		(* Constraint is exact if termination is normal, unknown otherwise (on the one hand, pi-incompatible inequalities (that would restrain the constraint) may be missing, and on the other hand union of good states (that would enlarge the constraint) may be missing too) *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_invalid in

		let result = match statespace_nature with
			| StateSpace.Good | StateSpace.Unknown -> Good_constraint(result, soundness)
			| StateSpace.Bad -> Bad_constraint(result, soundness)
		in

		(* Return result *)
		Point_based_result
		{
			(* Reference valuation *)
			reference_val		= self#get_reference_pval;
			
			(* Result of the algorithm *)
			result				= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= termination_status;
		}

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
