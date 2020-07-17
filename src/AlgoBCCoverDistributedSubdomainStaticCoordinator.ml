(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: subdomain with static distribution. [ACN15]
 * Coordinator algorithm
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
open AlgoBCCoverDistributedSubdomain



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCCoverDistributedSubdomainStaticCoordinator (v0 : HyperRectangle.hyper_rectangle) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
	object (self) inherit AlgoBCCoverDistributedSubdomainStatic.algoBCCoverDistributedSubdomainStatic v0 algo_instance_function tiles_manager_type as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Number of collaborators (excluding the coordinator) *)
	val nb_other_collaborators = DistributedUtilities.get_nb_nodes () - 1
	
	(* List of bc_results received from the collaborators *)
	val mutable bc_results : Result.cartography_result list = []

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC (full cov) distr StaticSubdomain coordinator"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
(* 		super#initialize_variables; *)
		
		(* The end *)
		()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Finalization method to process results communication to the coordinator *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* At the end, the coordinator (collaborator #0) receives all tiles, and handles the result *)
	method finalize cartography_result =
		self#print_algo_message Verbose_standard ( "Coordinator starting finalization");
		
		(* Create the manager *)
		
		
		(* First add its own bc_result to the list *)
		bc_results <- [cartography_result];
		
		(* Then collect the others collaborators results *)
		let nb_collaborators_done = ref 0 in

		(* Print some information *)
		self#print_algo_message Verbose_low ("Expecting to receive " ^ (string_of_int nb_other_collaborators) ^ " results from my collaborators.");
		
		while !nb_collaborators_done < nb_other_collaborators do
			(* Print some information *)
			self#print_algo_message Verbose_low ("" ^ ( string_of_int ( nb_other_collaborators - !nb_collaborators_done )) ^ " collaborators left." );

			(* Receive *)
			let collaborator_rank , cartography_result = DistributedUtilities.receive_cartography_result () in
			
			(* Print some information *)
			self#print_algo_message Verbose_standard ("Received a result with " ^ (string_of_int (List.length cartography_result.tiles)) ^ " tile" ^ (s_of_int (List.length cartography_result.tiles)) ^ " from collaborator " ^ (string_of_int collaborator_rank ) ^ ".");
			
			(* Add to list *)
			bc_results <- cartography_result :: bc_results;

			(* Increment the number of collaborators that finished their job *)
			nb_collaborators_done := !nb_collaborators_done + 1;
			
		done;
		
		(* Print some information *)
		self#print_algo_message Verbose_standard ("All collaborators done" );
		
		(* The end *)
		()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
	
		(* Now append all bc_results to make a unique cartography_result *)
		
		(* First create a manager *)
		let tiles_manager = match tiles_manager_type with
			| AlgoCartoGeneric.Tiles_list -> new TilesManagerList.tilesManagerList
			| AlgoCartoGeneric.Tiles_good_bad_constraint -> raise (NotImplemented "not implemented yet")
		in
		(* Now initialize the tiles manager *)
		tiles_manager#initialize;

		let computation_time = ref 0. in
(* 		let find_point_time = ref 0. in *)
		let nb_unsuccessful_points = ref 0 in
		(* Best possible value for coverage *)
		let coverage = ref Coverage_full in
		(* Best possible value for termination *)
		let termination = ref BC_Regular_termination in
		
		(* Iterate on all received results *)
		List.iter (fun (cartography_result : Result.cartography_result) ->
			(* Add all tiles to the manager *)
			List.iter (fun tile -> tiles_manager#process_tile tile) cartography_result.tiles;
			
			(* Update computation_time *)
			computation_time := !computation_time +. cartography_result.computation_time;
			
			(* Update find_point_time *)
			
			(*** TODO: try to find a way to add it back (as it is likely lost for now) ***)
			
(* 			find_point_time := !find_point_time +. bc_result.find_point_time; *)
			
			(* Update nb_unsuccessful_points *)
			nb_unsuccessful_points := !nb_unsuccessful_points + cartography_result.nb_unsuccessful_points;
			
			(* Update coverage to worst one *)
			let new_coverage = match !coverage, cartography_result.coverage with
				| Coverage_full, other -> other
				| Coverage_empty, Coverage_empty -> Coverage_empty
				| _, Coverage_empty -> Coverage_unknown
				| Coverage_empty, _ -> Coverage_unknown
				| Coverage_integer_complete, Coverage_full -> Coverage_integer_complete
				| Coverage_integer_complete, other -> other
				| Coverage_unknown, _ -> Coverage_unknown
			in coverage := new_coverage;
			
			(* Update termination to worst one *)
			let new_termination = match !termination, cartography_result.termination with
				| BC_Regular_termination, other -> other
				| other, BC_Regular_termination -> other
				| BC_Tiles_limit, BC_Tiles_limit -> BC_Tiles_limit
				| BC_Time_limit, BC_Time_limit -> BC_Time_limit
				| _, _ -> BC_Mixed_limit
			in termination := new_termination;
		) bc_results;
		
		(* Number of points in the original v0 (before splitting for the own exploration of the coordinator) *)
		let original_nb_points : NumConst.t = v0#get_nb_points (Input.get_options())#step in

		(* Ask the tiles manager to process the result itself, by passing the appropriate arguments *)
		tiles_manager#process_result start_time v0 original_nb_points !nb_unsuccessful_points !termination (Some !coverage)


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
