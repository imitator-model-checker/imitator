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
class virtual algoBCCoverDistributedSubdomainStatic (model : AbstractModel.abstract_model) (v0 : HyperRectangle.hyper_rectangle) (step : NumConst.t) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
	object (self) inherit AlgoBCCoverDistributedSubdomain.algoBCCoverDistributedSubdomain model v0 step algo_instance_function tiles_manager_type as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_name : string

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Compute the subdomain for oneself *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		
	(*** NOTE: for now, brute force method: compute ALL subdomains, then select the subdomain # rank, where rank is the rank of the collaborator ***)
	method private compute_own_subdomain =

		(* Compute all subdomains *)
		let subdomains = self#compute_initial_subdomains in
		
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			(* Retrive the model *)
			let model = Input.get_model() in
			
			self#print_algo_message Verbose_low ("I computed the following " ^ (string_of_int (List.length subdomains)) ^ " subdomain" ^ (s_of_int (List.length subdomains)) ^ ".");
			
			List.iteri (fun i subdomain ->
				print_message Verbose_low("Subdomain #" ^ (string_of_int (i+1)) ^ ":\n" ^ (ModelPrinter.string_of_v0 model subdomain));
			) subdomains;
		);

		
		(* Select subdomain # rank *)
		let subdomain =
		try(
			List.nth subdomains collaborator_rank
		)with
		Failure f -> (
			raise (InternalError("Error when accessing element #" ^ (string_of_int collaborator_rank) ^ " in the list of subdomains."))
		)
		in
		(* Print some verbose information *)
		self#print_algo_message Verbose_low ("I picked up subdomain #" ^ (string_of_int collaborator_rank) ^ ".");
		(* Return subdomain *)
		subdomain



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialization method (only non-empty for coordinator) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method virtual initialize : unit *)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Finalization method to process results communication to the coordinator *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual finalize : Result.cartography_result -> unit
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm for all collaborators (including the coordinator) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
(*		(* Initialize: will do nothing for regular collaborators, and a few things (including a check that the number of nodes is a power of 2) for the coordinator) *)
		self#initialize;*)
		
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Computing own static subdomain…");
		
		(* Compute subdomain for this collaborator only *)
		let subdomain = self#compute_own_subdomain in

		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_low("Own static subdomain: " ^ (ModelPrinter.string_of_v0 (Input.get_model()) subdomain));
		);
		
		(* Create an instance of the sequential cartography *)
		(*** NOTE: in static distribution mode, since each collaborator is responsible for its own cartography, the sequential cartography is perfectly suited ***)
		let bc_instance = new AlgoBCCover.algoBCCover model subdomain step algo_instance_function tiles_manager_type in
		
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Running cartography on own static subdomain…");
		
		(* Launch the cartography *)
		let imitator_result = bc_instance#run () in

		(* Retrieve the result *)
		let cartography_result =
		match imitator_result with
			| Cartography_result cartography_result -> cartography_result
			| _ -> raise (InternalError ("The result of BC should be 'BC_result cartography_result'"))
		in
		
		(* Send the results to the coordinator (if collaborator), or collect results from collaborators (if coordinator) *)
		self#finalize cartography_result;
		
		(* Return the result *)
		self#compute_bc_result
		
		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_bc_result : Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
