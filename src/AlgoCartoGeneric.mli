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
 * Last modified     : 2016/01/27
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
val abstract_im_result_of_im_result : Result.im_result -> Result.abstract_im_result


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
		
		(* Min & max bounds for the parameters *)
		val mutable min_bounds : NumConst.t array
		val mutable max_bounds : NumConst.t array

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		(* Create the initial point for the analysis *)
		method virtual get_initial_point : more_points

		(* Find the next point *)
		method virtual find_next_point : more_points

		(* Variable initialization (to be improved in subclasses) *)
		method initialize_variables : unit
		
		(* Main method to run the algorithm: virtual method to be defined in subclasses *)
		method run : unit -> Result.imitator_result
		
		(* Processing the result of IM *)
		method virtual process_result : Result.im_result -> unit

(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
(* 		method virtual compute_result : Result.imitator_result *)

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
