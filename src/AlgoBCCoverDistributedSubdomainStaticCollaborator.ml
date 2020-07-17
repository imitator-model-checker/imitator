(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain with static distribution. [ACN15]
 * Collaborator (non-coordinator) algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/17
 * Last modified     : 2020/07/17
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoGeneric
open DistributedUtilities



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverDistributedSubdomainStaticCollaborator (v0 : HyperRectangle.hyper_rectangle) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
	object (self) inherit AlgoBCCoverDistributedSubdomainStatic.algoBCCoverDistributedSubdomainStatic v0 algo_instance_function tiles_manager_type as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr StaticSubdomain collaborator#" ^ (string_of_int collaborator_rank)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialization method (only non-empty for coordinator) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method initialize = () *)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Finalize: send all tiles to the coordinator *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Send tiles to the coordinator *)
	method finalize cartography_result =
		self#print_algo_message Verbose_low ("About to send all my tiles to the coordinator...");

(*		(* Retrieve the abstract_im_results *)
		let abstract_im_result_list = current_bc_algo_instance#get_abstract_im_result_list in
		
		(* Send all tiles to the coordinator *)
		send_abstract_im_result_list abstract_im_result_list;*)
		
		(* Send the cartography to the coordinator *)
		send_cartography_result cartography_result;

		(* Print some information *)
		self#print_algo_message Verbose_low ("Tiles sent! The end for me.");
		
		(* The end *)
		()



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm (useless method for regular collaborator) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		Distributed_worker_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
