(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Generic class for cartography-style algorithms
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 * Last modified     : 2016/01/29
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)
(*** NOTE: no use of the "option" type, as we may want to add more values later (e.g., "Maybe more points but could not find any" for the random cartography) ***)
type more_points =
	(* No more uncovered parameter valuations *)
	| No_more
	(* Some more uncovered parameter valuations *)
	| Some_pval of PVal.pval

	


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)
(* Convert an 'im_result' into an 'abstract_im_result' *)
val abstract_im_result_of_im_result : Result.im_result -> PVal.pval -> Result.abstract_im_result


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoCartoGeneric :
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Current point *)
		val mutable current_point : more_points

		(* Number of dimensions *)
		val mutable nb_dimensions : int
		
		(* Number of points in V0 (slightly approximated) *)
		val mutable nb_points : NumConst.t

		(* Min & max bounds for the parameters *)
		val mutable min_bounds : NumConst.t array
		val mutable max_bounds : NumConst.t array

		(* List of im_results *)
		val mutable im_results : Result.abstract_im_result list
		
(*		(* Initial p-constraint (needed to check whether points satisfy it) *)
		val mutable init_p_constraint = LinearConstraint.p_true_constraint ()
*)

		(* Counts the points actually member of an existing constraint for information purpose *)
		val mutable nb_unsuccessful_points : int

		(* Counter tracking the computation time to look for points *)
		val find_next_point_counter : Counter.counter
		
		(* Status of the analysis *)
		val mutable termination_status : Result.bc_algorithm_termination option

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		(* Create the initial point for the analysis *)
		method virtual get_initial_point : more_points

		
		(* Find the next point *)
		method virtual find_next_point : more_points

		
		(* Variable initialization (to be improved in subclasses) *)
		method initialize_variables : unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Generic function returning true if a computed pi0 belongs to none of the tiles, and satisfies the init constraint. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method test_pi0_uncovered : PVal.pval -> bool

		
		(* Main method to run the algorithm: virtual method to be defined in subclasses *)
		method run : unit -> Result.imitator_result
		
		
		(* Processing the result of IM *)
		method virtual process_result : Result.im_result -> PVal.pval -> unit

(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
(* 		method virtual compute_result : Result.imitator_result *)

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
