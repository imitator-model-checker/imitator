(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with sequential distribution of points. [ACE14]
 * Worker algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/10
 * Last modified     : 2020/07/16
 *
 ************************************************************)


	
(************************************************************)
(* Modules *)
(************************************************************)


(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverDistributedMSSeqWorker : HyperRectangle.hyper_rectangle -> (PVal.pval -> AlgoStateBased.algoStateBased) -> AlgoCartoGeneric.tiles_storage ->
	object inherit AlgoBCCoverDistributedMSPointBasedWorker.algoBCCoverDistributedMSPointBasedWorker

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