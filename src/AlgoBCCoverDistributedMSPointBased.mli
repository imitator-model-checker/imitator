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
 * Last modified     : 2016/03/17
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric
(* open AlgoCartoMaster *)


(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributedMSPointBased :
	object
	inherit algoGeneric
(* 	inherit algoMaster *)
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
		(* Processing the result of IM *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual run : unit -> Result.imitator_result


end