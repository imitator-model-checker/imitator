(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain. [ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2016/03/24
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric

(************************************************************)
(* Class definition *)
(************************************************************)
class virtual algoBCCoverDistributedSubdomain :
	object
	inherit AlgoBCCoverDistributed.algoBCCoverDistributed

		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Rank of the collaborator *)
		val collaborator_rank : DistributedUtilities.rank

		(* Number of collaborators (including the coordinator) *)
		val nb_collaborators : int
	
		(* The current algorithm instance *)
(* 		val mutable current_bc_algo_instance : AlgoBCCover.algoBCCover *)

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method virtual algorithm_name : string
		
		method initialize_variables : unit
		
			
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the initial subdomains (with the v0 and the number of collaborators as arguments) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_initial_subdomains_with : HyperRectangle.hyper_rectangle -> int -> HyperRectangle.hyper_rectangle list

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the initial subdomains (retrieving automatically v0 and the number of collaborators) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_initial_subdomains : HyperRectangle.hyper_rectangle list
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Generic algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual run : unit -> Result.imitator_result


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Method packaging the result output by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual compute_bc_result : Result.imitator_result
end