(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with random pi0 and n retries before switching to sequential mode. [ACE14]
 * Worker algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2016/03/17
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open AlgoBCCoverDistributedMSPointBasedWorker

(************************************************************)
(************************************************************)
(* Internal exceptions *)
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverDistributedMSRandomSeqWorker =
	object (self)
	inherit AlgoBCCoverDistributedMSPointBasedWorker.algoBCCoverDistributedMSPointBasedWorker as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr MS random+seq WORKER#" ^ (string_of_int worker_rank) ^ ""

	
(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method im_instance : AlgoIMK.algoIMK =
		(* Create a new instance of IM *)
		new AlgoIM.algoIM*)


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
