(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-slave with sequential distribution of points. [ACE14]
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/03
 * Last modified     : 2016/03/04
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoBCCoverDistributedMSPointBased


(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverDistributedMSSeq :
	object
	inherit algoBCCoverDistributedMSPointBased
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Return a new instance of the algorithm to be iteratively called (typically BCrandom or BCcover) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method algorithm_instance : AlgoCartoGeneric.algoCartoGeneric

end