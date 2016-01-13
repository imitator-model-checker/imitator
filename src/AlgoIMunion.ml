(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IMKunion algorithm [AS11]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/08
 * Last modified     : 2016/01/11
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
open AlgoBFS
open AlgoIMK



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoIMunion =
	object (self) inherit algoIMK as super
	
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
	(* Add a new state to the state_space (if indeed needed) *)
	(* Also update tile_nature and slast *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint =
		super#add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: add the deadlock state to the list of last states *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index =
		self#print_algo_message Verbose_low ("\nfound a state with no successor");
		
		(* Get the state *)
		let _, px_constraint = StateSpace.get_state state_space state_index in
		(* Projet onto P *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
		(* Add the constraint to the result *)
		LinearConstraint.p_nnconvex_union result p_constraint
		
(*		(* Add to the list of last states *)
		last_states <- state_index :: last_states*)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state that is on a loop: nothing to do for this algorithm, but can be defined in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_looping_state state_index =
		self#print_algo_message Verbose_low ("\nfound a state in a loop");
		
		(* Get the state *)
		let _, px_constraint = StateSpace.get_state state_space state_index in
		(* Projet onto P *)
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
		(* Add the constraint to the result *)
		LinearConstraint.p_nnconvex_union result p_constraint
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
		let (_ , px_constraint ) = initial_state in
		
		self#print_algo_message Verbose_total ("\nprojecting the initial state constraint onto the parameters...");
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in

		self#print_algo_message Verbose_total ("\nadding the initial constraint to the result");
		LinearConstraint.p_nnconvex_intersection result p_constraint;
		
		
		IMNonconvex_result
		{
			(* Result of the algorithm *)
			nonconvex_constraint= result;
			
			(* Explored state space *)
			state_space			= state_space;
			
			(* Nature of the state space (needed??) *)
		(* 	tile_nature			: AbstractModel.tile_nature; *)
			
			(* Number of random selections of pi-incompatible inequalities performed *)
			nb_random_selections= nb_random_selections;
	
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= 
				match termination_status with
				| None -> raise (InternalError "Termination status not set in IMunion.compute_result")
				| Some status -> status
			;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
