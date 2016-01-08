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
 * Last modified     : 2016/01/08
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
	val mutable last_states : StateSpace.state_index list = []
	
	
	
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
		
		last_states <- [];
		
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
		print_message Verbose_low ("\nAlgorithm " ^ self#algorithm_name ^ ": found a state with no successor");
		(* Add to the list of last states *)
		last_states <- state_index :: last_states
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state that is on a loop: nothing to do for this algorithm, but can be defined in subclasses *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_looping_state state_index =
		print_message Verbose_low ("\nAlgorithm " ^ self#algorithm_name ^ ": found a state in a loop");
		(* Add to the list of last states *)
		last_states <- state_index :: last_states

		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
	
	
	
		(*** Test ***)
		let test = ppl_new_Pointset_Powerset_NNC_Polyhedron_from_space_dimension 88 Universe in
(* 		: pointset_powerset_nnc_polyhedron -> pointset_powerset_nnc_polyhedron *)
	
	
		(* Method used here: intersection of all p-constraints *)
		(* Alternative methods would have been: 1) on-the-fly intersection (everytime a state is met) or 2) intersection of all final states, i.e., member of a loop, or deadlock states *)

		(* Create the result *)
		let p_constraint = LinearConstraint.p_true_constraint() in
		
		print_message Verbose_low ("\nAlgorithm " ^ self#algorithm_name ^ ": performing the intersection of all p-constraints...");
		
		(* Iterate on all states *)
(* 		val iterate_on_states : (state_index -> abstract_state -> unit) -> state_space -> unit *)
		StateSpace.iterate_on_states (fun state_index abstract_state ->
			(* Retrieve the px-constraint *)
			let _, px_linear_constraint = abstract_state in
			(* Project onto the parameters *)
			let projection = LinearConstraint.px_hide_nonparameters_and_collapse px_linear_constraint in
			(* Intersect with the result *)

			(*** TODO: check if only one intersection with the list of all projections gathered would be more efficient ??? ***)
			
			LinearConstraint.p_intersection_assign p_constraint [projection];
		) state_space;
		
	
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
