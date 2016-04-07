(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Random Behavioral Cartography with a maximum number of consecutive failed attempts to find a non-integer point not covered by any tile, then followed by a sequential check point by point. Described in the distributed setting in [ACE14,ACN15]
 * Note: the algorithm does NOT track points already computed randomly but not kept because covered by some tile.
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2016/03/16
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoCartoGeneric



(************************************************************)
(* Class definition *)
(************************************************************)
(*** NOTE: this function cannot have max_tries as a parameter, as it it inherits algoCartoGeneric which has none ***)
class algoBCRandomSeq : (*int ->*)
	object inherit algoCartoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set the maximum number of tries (must be done right after creating the algorithm object!) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_max_tries : int -> unit
		
		method initialize_variables : unit
		
		(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
		method algorithm_instance : AlgoIMK.algoIMK

		(* Create the initial point for the analysis *)
		method get_initial_point : more_points

		(* Find the next point *)
		method find_next_point : more_points

		(* Processing the result of IM *)
(* 		method process_result : Result.im_result -> PVal.pval -> unit *)

		method compute_bc_result : Result.imitator_result
end