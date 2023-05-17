(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Parametric deadlock-freeness
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/08
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoPostStar

(************************************************************)
(* Class definition *)
(************************************************************)
class algoDeadlockFree : AbstractModel.abstract_model ->
	object inherit algoPostStar
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		method initialize_variables : unit
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_termination_at_post_n : bool

		
		method compute_result : Result.imitator_result
end
