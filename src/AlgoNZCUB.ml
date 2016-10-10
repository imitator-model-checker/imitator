(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA)
 * 
 * File contributors : Étienne André
 * Created           : 2016/10/10
 * Last modified     : 2016/10/10
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
open AlgoLoopSynth



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNZCUB =
	object (self) inherit algoLoopSynth as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "NZCUB"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when found a loop, after updating the state space *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_loop_after_state_space_update loop_starting_point_state_index =
		(* Retrieve the model *)
		let model = Input.get_model () in

		(* Compute the SCC (after the transitions were updated, of course) *)
		if verbose_mode_greater Verbose_low then(
			let scc = StateSpace.reconstruct_scc state_space loop_starting_point_state_index in
			(* Print it *)
			let verbose_string_of_state_index state_index =
				let state = StateSpace.get_state state_space state_index in
				"\n\ts_" ^ (string_of_int state_index) ^ "{" ^ (ModelPrinter.string_of_state model state) ^ "}"
			in
			print_message Verbose_low ("\nSCC: [" ^ (string_of_list_of_string_with_sep "\n\t- " (List.map verbose_string_of_state_index scc)) ^ "\n]");
			
			
			(*** TODO ***)
			
			(* Retrieve the extra clock *)
			let reset_clock = match model.special_reset_clock with
				| None -> raise (InternalError("A special reset clock should have been defined in AlgoNZCUB"))
				| Some clock_index -> clock_index
			in
			
			(* Find transitions *)
			
			(* Measure time elapsing *)
			
			(* Look for clock that must be reset *)
			
			()
			
			
		);
		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in LoopSynth.compute_result")
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the existence of at least one loop *)
			result				= Good_constraint (loop_constraint, soundness);
			
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
