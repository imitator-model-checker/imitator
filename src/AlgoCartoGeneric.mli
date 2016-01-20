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
 * Last modified     : 2016/01/20
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
(* Class definition *)
(************************************************************)
class virtual algoCartoGeneric :
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Current point *)
(* 		val mutable current_point : more_points *)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		(* Create the initial point for the analysis *)
		method virtual get_initial_point : more_points

		(* Find the next point *)
		method virtual find_next_point : more_points

		(* Variable initialization (to be defined in subclasses) *)
(* 		method virtual initialize_variables : unit *)
		
		(* Main method to run the algorithm: virtual method to be defined in subclasses *)
(* 		method virtual run : unit -> Result.imitator_result *)
		
		(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
(* 		method virtual compute_result : Result.imitator_result *)

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
