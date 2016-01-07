(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: IM algorithm [ACEF09]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/06
 * Last modified     : 2016/01/07
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
open Result
open AlgoBFS
open AlgoIMK



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoIM =
	object (self) inherit algoIMK as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "IM"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		(* The end *)
		()
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state_space (if indeed needed) *)
	(* Also update tile_nature and slast *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint =
		super#add_a_new_state state_space orig_state_index new_states_indexes action_index location final_constraint
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
	
		(* IMK: return only the current constraint, viz., the constraint of the first state *)
		(*** NOTE: better not use just "0" as the initial state may have been merged with another state ***)
		
		
		
		
		(*** TODO ***)
		
		let initial_state_index = StateSpace.get_initial_state_index state_space in
		let initial_state = StateSpace.get_state state_space initial_state_index in
		(* Retrieve the constraint of the initial state *)
		let (_ , px_constraint ) = initial_state in

		(*** TODO ***)
		
		
		
		
		print_message Verbose_total ("\nAlgorithm " ^ self#algorithm_name ^ ": projecting the initial state constraint onto the parameters...");
		
		let p_constraint = LinearConstraint.px_hide_nonparameters_and_collapse px_constraint in
(* 		Convex_constraint (LinearConstraint.px_hide_nonparameters_and_collapse px_constraint , !tile_nature)  *)
	
		IMConvex_result
		{
			(* Result of IM *)
			convex_constraint	= p_constraint;
			
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
				| None -> raise (InternalError "Termination status not set in IM.compute_result")
				| Some status -> status
			;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
