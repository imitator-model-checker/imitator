(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with shuffle distribution of points. [ACN15]
 * Master algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2016/08/16
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
class algoBCCoverDistributedMSShuffleMaster =
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
	method algorithm_name = "BC (full cov) distr MS shuffle MASTER"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method bc_instance =
		let myalgo :> AlgoCartoGeneric.algoCartoGeneric = new AlgoBCShuffle.algoBCShuffle in
		(* Important: set now the parameters *)
		(* Set the instance of IM / PRP that was itself set from the current cartography class *)
		(*** NOTE: in fact: not necessary as the master will never call itself IM/PRP ***)
(* 		myalgo#set_algo_instance_function self#get_algo_instance_function; *)
		myalgo#set_tiles_manager_type self#get_tiles_manager_type;
		myalgo


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
