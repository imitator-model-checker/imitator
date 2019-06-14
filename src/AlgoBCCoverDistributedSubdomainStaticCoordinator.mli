(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain with static distribution. [ACN15]
 * Coordinator algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2016/08/15
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric

(************************************************************)
(* Class definition *)
(************************************************************)
class algoBCCoverDistributedSubdomainStaticCoordinator :
	object
	inherit AlgoBCCoverDistributedSubdomainStatic.algoBCCoverDistributedSubdomainStatic

		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method initialize_variables : unit
		
		
(*		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Initialization method (only non-empty for coordinator) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method initialize : unit*)
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Finalization method to process results communication to the coordinator *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method finalize : Result.cartography_result -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Method packaging the result output by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_bc_result : Result.imitator_result
end