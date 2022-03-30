(************************************************************
 *
 *                       IMITATOR
 *
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 *
 * File contributors : Étienne André
 * Created           : 2016/03/04
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
(* Internal exceptions *)
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual algoBCCoverDistributed (v0 : HyperRectangle.hyper_rectangle) (step : NumConst.t) (algo_instance_function : (PVal.pval -> AlgoStateBased.algoStateBased)) (tiles_manager_type : AlgoCartoGeneric.tiles_storage) =
	object (self) inherit algoGeneric as super


	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* The current algorithm instance *)
	(*** NOTE: this initialiation is useless (and time consuming?), as a new instance will be overwritten when needed ***)
	val mutable current_algo_instance : AlgoStateBased.algoStateBased =
		let dummy_pval = new PVal.pval in
		let myalgo :> AlgoStateBased.algoStateBased = new AlgoIMK.algoIMK dummy_pval in myalgo
	

	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize_variables : unit


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Run IM and return an abstract_point_based_result *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run_im pi0 patator_termination_function_option =

		(* Print some messages *)
		if verbose_mode_greater Verbose_low then(
			self#print_algo_message Verbose_medium ("**************************************************");
			self#print_algo_message Verbose_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
			self#print_algo_message Verbose_low ("Running IM for the following reference valuation:" (*^ (string_of_int !current_iteration)*));
			self#print_algo_message Verbose_low (ModelPrinter.string_of_pval model pi0);
		);

		(* Save verbose mode *)
		let global_verbose_mode = get_verbose_mode() in

		(* Prevent the verbose messages (except in verbose modes high or total) *)
		(*------------------------------------------------------------*)
		if not (verbose_mode_greater Verbose_high) then
				set_verbose_mode Verbose_mute;

		(* Call IM *)

		(*** NOTE: the initial state is computed again and again for each new instance of IM; TO OPTIMIZE? ***)

(* 		let imitator_result = algo#run() in *)
		
		(* Call the algorithm to be iterated on (typically IM or PRP) *)
		(*** NOTE: the bc time limit is NOT checked inside one execution of the algorithm to be iterated (but also note that the max execution time of the algorithm to be iterated is set to that of BC, in the Options pre-processing) ***)
		current_algo_instance <- algo_instance_function pi0;

		(* Set up the termination function for PaTATOR *)
		begin
		match patator_termination_function_option with
			| None -> ()
			| Some f -> current_algo_instance#set_patator_termination_function f;
		end;

		let imitator_result : imitator_result = current_algo_instance#run() in

		
		(* Get the verbose mode back *)
		set_verbose_mode global_verbose_mode;
		(*------------------------------------------------------------*)

		self#print_algo_message Verbose_low ("Finished a computation of " ^ (current_algo_instance#algorithm_name) ^ ".");

		(* Checking the result type, and computing abstraction *)
		let abstract_point_based_result = match imitator_result with
			(* Result for IM, IMK, IMunion *)
			| Single_synthesis_result single_synthesis_result -> AlgoCartoGeneric.abstract_point_based_result_of_single_synthesis_result single_synthesis_result pi0
			(* Result for IM, IMK, IMunion *)
			| Point_based_result point_based_result -> AlgoCartoGeneric.abstract_point_based_result_of_point_based_result point_based_result pi0
			(* Other *)
			| _ -> raise (InternalError("A point_based_result is expected as an output of the execution of " ^ current_algo_instance#algorithm_name ^ "."))
		in

		(* Return the abstract result *)
		abstract_point_based_result


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual run : unit -> Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
