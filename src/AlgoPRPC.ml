(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Parametric reachability preservation-based cartography (PRPC) [ALNS15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/29
 * Last modified     : 2016/01/29
 *
 ************************************************************)


   !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!
 WARNING !!! THIS FILE IS NOW UNPLUGGED FROM THE IMITATOR SOURCE CODE (as of 2016/03/17)
 This paragraph should raise a compiling error (syntax error) if by any chance this file was linked from another file.
  !!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!***!!!

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
open AlgoBCCover




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoPRPC =
	object (self) inherit algoBCCover as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PRPC"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(*	method initialize_variables =
		super#initialize_variables;
		
		(* The end *)
		()*)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_instance =
		(* Create a new instance of PRP *)
		new AlgoPRP.algoPRP

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in PRPC.compute_result")
			| Some status -> status
		in

		(* Coverage is... *)
		(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
		let coverage =
			(* INTEGER COMPLETE if termination is regular and all tiles are valid *)
			if termination_status = BC_Regular_termination && (List.for_all (fun abstract_im_result -> match abstract_im_result.soundness with
					| Constraint_maybe_under | Constraint_exact | Constraint_maybe_over -> true
					| Constraint_maybe_invalid -> false
				) im_results)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in
		
		(* Return result *)
		BC_result {
			(* Number of points in V0 *)
			size_v0				= nb_points;
			
			(* List of tiles *)
			(*** NOTE: reverse as each im_result was added as first element ***)
			tiles				= List.rev im_results;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Computation time to look for points *)
			find_point_time		= find_next_point_counter#value;
			
			(* Number of points on which IM could not be called because already covered *)
			nb_unsuccessful_points = nb_unsuccessful_points;
			
			(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
			coverage			= coverage;
			
			(* Termination *)
			termination			= termination_status;
		}


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
