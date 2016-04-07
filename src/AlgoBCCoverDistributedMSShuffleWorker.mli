(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with shuffle distribution of points. [ACN15]
 * Worker algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2016/03/17
 *
 ************************************************************)


	
(************************************************************)
(* Modules *)
(************************************************************)


(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverDistributedMSShuffleWorker :
	object
	inherit AlgoBCCoverDistributedMSPointBasedWorker.algoBCCoverDistributedMSPointBasedWorker
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string

		
(*		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method im_instance : AlgoIMK.algoIMK*)

end