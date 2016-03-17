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
(* Modules *)
(************************************************************)
open AlgoBCCover



(************************************************************)
(* Class definition *)
(************************************************************)
class algoPRPC :
	object inherit algoBCCover
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
(*		method initialize_variables : unit*)
		
		(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
		method algorithm_instance : AlgoIMK.algoIMK

(*		(* Create the initial point for the analysis *)
		method get_initial_point : more_points

		(* Find the next point *)
		method find_next_point : more_points

		(* Processing the result of IM *)
		method process_result : Result.im_result -> PVal.pval -> unit*)

		method compute_result : Result.imitator_result
end