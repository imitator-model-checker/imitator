(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/10
 * Last modified     : 2016/03/17
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributedMSPointBasedMaster :
	object
	inherit AlgoBCCoverDistributed.algoBCCoverDistributed
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method virtual algorithm_name : string
		
		method initialize_variables : unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual bc_instance : AlgoCartoGeneric.algoCartoGeneric

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Run the master algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : unit -> Result.imitator_result


end