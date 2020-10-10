(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain with static distribution. [ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2020/08/28
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric

(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributedSubdomainStatic : HyperRectangle.hyper_rectangle -> NumConst.t -> (PVal.pval -> AlgoStateBased.algoStateBased) -> AlgoCartoGeneric.tiles_storage ->
	object inherit AlgoBCCoverDistributedSubdomain.algoBCCoverDistributedSubdomain
	
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method virtual algorithm_name : string
		
		method initialize_variables : unit
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Finalization method to process results communication to the coordinator *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual finalize : Result.cartography_result -> unit

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Generic algorithm for all collaborators (including the coordinator) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method run : unit -> Result.imitator_result


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Method packaging the result output by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual compute_bc_result : Result.imitator_result
end