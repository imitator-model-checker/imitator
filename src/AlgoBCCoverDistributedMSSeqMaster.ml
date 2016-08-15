(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with sequential distribution of points. [ACE14]
 * Master algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/10
 * Last modified     : 2016/08/15
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
class algoBCCoverDistributedMSSeqMaster =
	object (self)
	inherit AlgoBCCoverDistributedMSPointBasedMaster.algoBCCoverDistributedMSPointBasedMaster as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr MS seq MASTER"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method bc_instance =
		let myalgo :> AlgoCartoGeneric.algoCartoGeneric = new AlgoBCCover.algoBCCover in
		(* Important: set now the parameters *)
		(* Set the instance of IM / PRP that was itself set from the current cartography class *)
		myalgo#set_algo_instance_function self#get_algo_instance_function;
		myalgo#set_tiles_manager_type self#get_tiles_manager_type;
		myalgo


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
