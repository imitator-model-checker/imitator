(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with shuffle distribution of points. [ACN15]
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
class algoBCCoverDistributedMSShuffleMaster (v0 : HyperRectangle.hyper_rectangle) (step : NumConst.t) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
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
	method algorithm_name = "BC (full cov) distr MS shuffle MASTER"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method bc_instance =
		let myalgo :> AlgoCartoGeneric.algoCartoGeneric = new AlgoBCShuffle.algoBCShuffle v0 step algo_instance_function tiles_manager_type in
		myalgo


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
