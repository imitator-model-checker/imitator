(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Random Behavioral Cartography with a maximum number of consecutive failed attempts to find a non-integer point not covered by any tile, then followed by a sequential check point by point. Described in the distributed setting in [ACE14,ACN15]
 * Note: the algorithm does NOT track points already computed randomly but not kept because covered by some tile.
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2020/08/28
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoCartoGeneric



(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCRandomSeq : HyperRectangle.hyper_rectangle -> NumConst.t -> int -> (PVal.pval -> AlgoStateBased.algoStateBased) -> tiles_storage ->
	object inherit algoCartoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		
		method initialize_variables : unit
		
		(* Create the initial point for the analysis *)
		method get_initial_point : more_points

		(* Find the next point *)
		method find_next_point : more_points

		(* Processing the result of IM *)
(* 		method process_result : Result.im_result -> PVal.pval -> unit *)

		method compute_bc_result : Result.imitator_result
end