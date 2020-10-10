(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Random Behavioral Cartography with a maximum number of consecutive failed attempts to find a non-integer point not covered by any tile [AF10]
 * Note: the algorithm does NOT track points already computed randomly but not kept because covered by some tile.
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/02
 * Last modified     : 2020/08/28
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
open AlgoCartoGeneric




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoBCRandom (v0 : HyperRectangle.hyper_rectangle) (step : NumConst.t) (max_tries: int) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : tiles_storage) =
	object (self) inherit algoCartoGeneric v0 step algo_instance_function tiles_manager_type as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Current number of failed attempts to find an integer point not covered by any tile *)
(* 	val mutable nb_failed_attempts = 0 *)

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "BC random(" ^ (string_of_int max_tries) ^ ")"

			
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(* The end *)
		()

		
(*	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_instance =
		(* Create a new instance of IM *)
		new AlgoIM.algoIM*)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Create the initial point for the analysis *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method get_initial_point =
		(* Return a random point *)
		Some_pval (self#one_random_pi0)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Find the next point *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method find_next_point =
		(* Print some information *)
		self#print_algo_message Verbose_low ("Trying to randomly find a fresh pi0 with " ^ (string_of_int max_tries) ^ " tries.");

		self#try_random_pi0 max_tries


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result =
		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in BCRandom.compute_bc_result")
			| Some status -> status
		in

		(* Force coverage to Coverage_unknown  *)
		(*** TODO: could perhaps check manually (!) if the coverage is integer complete, or even complete ***)
		let forced_coverage_option = Some Coverage_unknown in

		(* Retrieve the manager *)
		let tiles_manager = self#get_tiles_manager in
		
		(* Ask the tiles manager to process the result itself, by passing the appropriate arguments *)
		tiles_manager#process_result start_time v0 nb_points nb_unsuccessful_points termination_status forced_coverage_option


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
