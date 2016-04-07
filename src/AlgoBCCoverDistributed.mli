(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/04
 * Last modified     : 2016/04/01
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributed :
	object
	inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods to simulate class parameters *)
		(************************************************************)
		
		(* Sets the function creating a new instance of the algorithm to call (typically IM or PRP) *)
		method set_algo_instance_function : (unit -> AlgoIMK.algoIMK) -> unit

		(* Get the function creating a new instance of the algorithm to call (typically IM or PRP) *)
		method get_algo_instance_function : (unit -> AlgoIMK.algoIMK)

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method virtual algorithm_name : string
		
		method virtual initialize_variables : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Run IM and return an abstract_im_result. Parameters are the reference valuation and the termination function to be set in IM (for PaTATOR). *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run_im : PVal.pval -> (unit -> unit) option -> Result.abstract_im_result
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Processing the result of IM *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual run : unit -> Result.imitator_result


end