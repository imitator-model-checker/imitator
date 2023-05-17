(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Generic class for cartography-style algorithms
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoGeneric


(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)
(*** NOTE: no use of the "option" type, as we may want to add more values later (e.g., "Maybe more points but could not find any" for the random cartography) ***)
type more_points =
	(* No more uncovered parameter valuations *)
	| No_more
	(* Some more uncovered parameter valuations *)
	| Some_pval of PVal.pval

	
(*** NOTE: this should be a class parameter (but would require much work in other classes… so TODO later) ***)
type tiles_storage =
	(* List of constraints, as in the legacy cartography [AF10] *)
	| Tiles_list
	(* A good/bad representation of the tiles, when only the parameter valuations are of interest *)
	| Tiles_good_bad_constraint


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)
(** Convert a 'single_synthesis_result' into an 'abstract_point_based_result' *)
val abstract_point_based_result_of_single_synthesis_result : Result.single_synthesis_result -> PVal.pval -> Result.abstract_point_based_result

(** Convert a 'point_based_result' into an 'abstract_point_based_result' *)
val abstract_point_based_result_of_point_based_result : Result.point_based_result -> PVal.pval -> Result.abstract_point_based_result

(*------------------------------------------------------------*)
(* Print warning(s) depending on a Result.bc_algorithm_termination *)
(*------------------------------------------------------------*)
val print_warnings_limit_for : Result.bc_algorithm_termination -> unit


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoCartoGeneric : AbstractModel.abstract_model -> HyperRectangle.hyper_rectangle -> NumConst.t -> (PVal.pval -> AlgoStateBased.algoStateBased) -> tiles_storage ->
	object inherit algoGeneric
		(************************************************************)
		(* Class variables *)
		(************************************************************)
		(* Current point *)
		val mutable current_point : more_points
		
		(* Current iteration (number of times IM is called); used for printing only *)
		val mutable current_iteration : int

		(* Number of dimensions *)
		val mutable nb_dimensions : int
		
		(* Number of points in V0 (slightly approximated) *)
		val mutable nb_points : NumConst.t

		(* Min & max bounds for the parameters *)
		val mutable min_bounds : NumConst.t array
		val mutable max_bounds : NumConst.t array

		(* The current algorithm instance *)
		val mutable current_algo_instance : AlgoStateBased.algoStateBased
		
		(* List of im_results *)
(* 		val mutable im_results : Result.abstract_im_result list *)
		
		(* Counts the points actually member of an existing constraint for information purpose *)
		val mutable nb_unsuccessful_points : int

		(* Counter tracking the computation time to look for points *)
(* 		val find_next_point_counter : Counter.counter *)
		
		(* Status of the analysis *)
		val mutable termination_status : Result.bc_algorithm_termination option

		
		
		(************************************************************)
		(* Class methods: methods used in subclasses as building blocks *)
		(************************************************************)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Return the current_point; raises InternalError if current_point was not initialized *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method get_current_point_option : PVal.pval
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the smallest point (according to the min bounds) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_smallest_point : PVal.pval


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the sequential successor of a given point. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_next_sequential_pi0 : PVal.pval ->  more_points
		

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the sequential uncovered successor of a given point. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_next_sequential_uncovered_pi0_from : PVal.pval -> more_points

	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Compute the sequential uncovered successor of the current point. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_next_sequential_uncovered_pi0 : more_points

	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Methods on random generation of a pi0 *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method one_random_pi0 : PVal.pval
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Try to generate a random pi0; after unsuccessful max_tries (because the randomly generated point was always covered by a tile), return No_more *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method try_random_pi0 : int -> more_points
			
		
		(************************************************************)
		(* Class methods *)
		(************************************************************)
		(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
(* 		method virtual algorithm_instance : AlgoIMK.algoIMK *)

		
		(* Create the initial point for the analysis *)
		method virtual get_initial_point : more_points

		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Find the next uncovered point *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual find_next_point : more_points

		
		(* Variable initialization (to be improved in subclasses) *)
		method initialize_variables : unit
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Generic function returning true if a computed pi0 belongs to none of the tiles, and satisfies the init constraint. *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method test_pi0_uncovered : PVal.pval -> bool

		
		(* Main method to run the algorithm: virtual method to be defined in subclasses *)
		method run : unit -> Result.imitator_result
		


		(************************************************************)
		(* Class methods to be called by distributed algorithms *)
		(************************************************************)
		(*** TODO: add a new interface AlgoCartoDistributed ? ***)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Initializing cartography algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method initialize_cartography : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get all tiles *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 		method get_abstract_im_result_list : Result.abstract_im_result list *)
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set all tiles, i.e., replace the list of abstract_im_result by that given in argument (used when the collaborator creates a new AlgoCartoGeneric, and wants to add the previously computed tiles) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 		method set_abstract_im_result_list : Result.abstract_im_result list -> unit *)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Get the tiles manager *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method get_tiles_manager : TilesManager.tilesManager
		
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Set the tiles manager, i.e., replace the list of tiles as managed by the manager with that given in argument (used when the collaborator creates a new AlgoCartoGeneric, and wants to add the previously computed tiles) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method set_tiles_manager : TilesManager.tilesManager -> unit
	
	
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Iteration condition to keep computing new tiles *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_iteration_condition : bool
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Create auxiliary files generated by one instance of IM *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method create_auxiliary_files : Result.imitator_result -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Process one result of an abstract version of an instance of IM *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method process_result : Result.abstract_point_based_result -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Compute the next point, store it; return it if new point exists and limits not reached *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method compute_and_return_next_point : more_points
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Call the algorithm on the current point (typically call IM or PRP) *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method call_point : PVal.pval -> Result.imitator_result

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Update termination condition, depending on the limit reached *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method update_termination_condition : unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Print warning(s) if the limit of an exploration has been reached, according to the analysis options *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_warnings_limit : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Method packaging the result output by the algorithm *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
		method virtual compute_bc_result : Result.imitator_result

(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)
