(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with random pi0 and n retries before switching to sequential mode. [ACE14]
 * Master algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open ImitatorUtilities
open AlgoBCCover


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
class algoBCCoverDistributedMSRandomSeqMaster (v0 : HyperRectangle.hyper_rectangle) (step : NumConst.t) (max_tries: int) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
	object (self) inherit AlgoBCCoverDistributedMSPointBasedMaster.algoBCCoverDistributedMSPointBasedMaster v0 step algo_instance_function tiles_manager_type as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr MS random(" ^ (string_of_int max_tries) ^ ")+seq MASTER"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method bc_instance =
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Creating BC instance…");
		
		let algo_bcrandomseq = new AlgoBCRandomSeq.algoBCRandomSeq v0 step max_tries algo_instance_function tiles_manager_type in
		let myalgo :> AlgoCartoGeneric.algoCartoGeneric = algo_bcrandomseq in
		myalgo
		
		

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
