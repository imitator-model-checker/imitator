(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-slave with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/04
 * Last modified     : 2016/03/04
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
class virtual algoBCCoverDistributedMSPointBased =
	object (self)
	inherit algoGeneric as super
	
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
	(* Global method on pi0 *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the algorithm to be iteratively called (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual bc_instance : AlgoCartoGeneric.algoCartoGeneric

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* Fork between master and worker *)
		if DistributedUtilities.get_rank() = DistributedUtilities.masterrank then self#run_as_master
		else self#run_as_worker


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Algorithm for the master *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run_as_master : Result.imitator_result =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)
		(* Retrieve the input options *)
(* 		let options = Input.get_options () in *)

		(* Create an object responsible to handle everything linked to the cartography *)
		let bc = self#bc_instance in

		(* Factoring initialization *)
		bc#initialize_cartography;
		

		
		(*** TODO : check that initial pi0 is suitable!! (could be incompatible with initial constraint) ***)
		
		

		(* While there is another point to explore *)
		while bc#check_iteration_condition do
		
			(************************************************************)
			(*** BEGIN USELESS: to be carried out by workers ***)
(*			(* Get the point *)
			let pi0 = self#get_current_point_instance in
			
			(* Print some messages *)
			(*** HACK: only print if non-distributed ***)
(* 			if options#distribution_mode = Options.Non_distributed then( *)
			print_message Verbose_standard ("\n**************************************************");
			print_message Verbose_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: " ^ (string_of_int current_iteration) ^ "");
			print_message Verbose_standard ("Considering the following pi" ^ (string_of_int current_iteration));
			print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
(* 			); *)
			
			
			(* Print some information *)
			self#print_algo_message Verbose_low ("Setting new pi0...");

			(* Set the new pi0 *)
			Input.set_pi0 (pi0);
			
			(* Save the verbose mode as it may be modified *)
			let global_verbose_mode = get_verbose_mode() in
			
			(* Prevent the verbose messages (except in verbose medium, high or total) *)
			(*------------------------------------------------------------*)
			if not (verbose_mode_greater Verbose_medium) then
				set_verbose_mode Verbose_mute;
						
			(* Call the algorithm to be iterated on (typically IM or PRP) *)
			(*** NOTE: the bc time limit is NOT checked inside one execution of the algorithm to be iterated (but also note that the max execution time of the algorithm to be iterated is set to that of BC, in the Options pre-processing) ***)
			algo_instance <- self#algorithm_instance;
			let imitator_result : imitator_result = algo_instance#run() in*)
			(*** END USELESS: to be carried out by workers ***)
			(************************************************************)

			(*** TODO here: call the worker! ***)
			
			let abstract_result = raise (InternalError("todo !! ")) in
			
			
			(** Create auxiliary files with the proper file prefix, if requested *)
			(*** NOTE: cannot create files, as the real state space is on the worker machine ***)
(* 			bc#create_auxiliary_files imitator_result; *)

			(* Get the verbose mode back *)
(* 			set_verbose_mode global_verbose_mode; *)
			(*------------------------------------------------------------*)

			(* Process result *)
			bc#process_result abstract_result;
			
			(* Update limits *)
			bc#update_limit;

		done; (* end while more points *)

		(* Update termination condition *)
		bc#update_termination_condition;
	
		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		bc#print_warnings_limit;
		
		(*** TODO: wait for remaining workers (except when limits reached?) ***)
		
		(* Return the algorithm-dependent result *)
		bc#compute_result

	
	
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Algorithm for the worker *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run_as_worker : Result.imitator_result =
		raise (InternalError("not implemented"))


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		raise (InternalError("not implemented"))
(*		self#print_algo_message_newline Verbose_standard (
			"Successfully terminated " ^ (after_seconds ()) ^ "."
		);

		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in BCCover.compute_result")
			| Some status -> status
		in

		(* Coverage is... *)
		(*** NOTE: this is only true for the original behavioral cartography; for variants this may not hold ***)
		let coverage =
			(* INTEGER COMPLETE if termination is regular and all tiles are exact or under-approximations *)
			if termination_status = BC_Regular_termination && (List.for_all (fun abstract_im_result -> match abstract_im_result.soundness with
					| Constraint_exact | Constraint_maybe_under -> true
					| Constraint_maybe_over | Constraint_maybe_invalid -> false
				) im_results)
				then Coverage_integer_complete
			(* UNKNOWN otherwise *)
			else Coverage_unknown
		in
		
		(* Return result *)
		BC_result {
			(* Number of points in V0 *)
			size_v0				= nb_points;
			
			(* List of tiles *)
			(*** NOTE: reverse as each im_result was added as first element ***)
			tiles				= List.rev im_results;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Computation time to look for points *)
			find_point_time		= find_next_point_counter#value;
			
			(* Number of points on which IM could not be called because already covered *)
			nb_unsuccessful_points = nb_unsuccessful_points;
			
			(* Evaluation of the coverage of V0 by tiles computed by the cartography *)
			coverage			= coverage;
			
			(* Termination *)
			termination			= termination_status;
		}*)


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
