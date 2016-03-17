(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/10
 * Last modified     : 2016/03/17
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
class virtual algoBCCoverDistributedMSPointBasedWorker =
	object (self)
	inherit AlgoBCCoverDistributedMSPointBased.algoBCCoverDistributedMSPointBased as super
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Shortcut to avoid repeated calls *)
	val worker_rank = DistributedUtilities.get_rank()
	
	
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
	(** Return a new instance of the algorithm to be iteratively called (typically IM or PRP) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* 	method virtual im_instance : AlgoIMK.algoIMK *)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
	
		(* Counters *)
		(*** TODO: set as class variables? ***)
		let counter_waiting = new Counter.counter in
		let counter_working = new Counter.counter in
		counter_waiting#init;
		counter_working#init;

		let model = Input.get_model() in

		(*** TODO: disable generation of graphics and files in any case ***)

		(* Flag tracking termination request from the master *)
		let finished = ref false in

		(* Ask the master for work *)
		send_work_request();
		
		(* While more work… *)
		while not !finished do
			(* Receive work assignment or termination signal *)
			counter_waiting#start;
			let assignment = receive_work () in
			counter_waiting#stop;
			
			(* Check whether the assignment is work or termination *)
			match assignment with
			| DistributedUtilities.Work pi0 ->
				counter_working#start;

				self#print_algo_message Verbose_medium("Received work.");

				(* Create instance of the algorithm to be called *)
				let algo = self#get_algo_instance_function () in
				
				(* Set up the pi0 *)
				(*** NOTE/BADPROG: a bit ugly… pi0 could have been a parameter of the algorithm! ***)
				Input.set_pi0 pi0;
				
				(* Print some messages *)
				self#print_algo_message Verbose_medium ("**************************************************");
				self#print_algo_message Verbose_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
				self#print_algo_message Verbose_low ("Launching IM for the following pi:" (*^ (string_of_int !current_iteration)*));
				self#print_algo_message Verbose_low (ModelPrinter.string_of_pi0 model pi0);

				(* Save verbose mode *)
				let global_verbose_mode = get_verbose_mode() in 
				
				(* Prevent the verbose messages (except in verbose modes high or total) *)
				if not (verbose_mode_greater Verbose_high) then
						set_verbose_mode Verbose_mute;

				(* Call IM *)
				
				(*** NOTE: the initial state is computed again and again for each new instance of IM; TO OPTIMIZE? ***)
				
				let imitator_result = algo#run() in

				(* Get the verbose mode back *)
				set_verbose_mode global_verbose_mode;
				
				self#print_algo_message Verbose_low ("Finished a computation of " ^ (algo#algorithm_name) ^ ".");
				
				(* Checking the result type *)
				let im_result = match imitator_result with
					(* Result for IM, IMK, IMunion *)
					| IM_result im_result -> im_result
					(* Other *)
					| _ -> raise (InternalError("An im_result is expected as an output of the execution of " ^ algo#algorithm_name ^ "."))
				in
				
				(* Abstracting the result *)
				let abstract_im_result = AlgoCartoGeneric.abstract_im_result_of_im_result im_result pi0 in
				
				(* Send the result to the master *)
				DistributedUtilities.send_abstract_im_result abstract_im_result;
				
				(* Print some information *)
				self#print_algo_message_newline Verbose_medium (
						"K computed by " ^ (algo#algorithm_name) (*^ " after "
						^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""*)
						^ " in " ^ (string_of_seconds abstract_im_result.computation_time) ^ ": "
						^ (string_of_int abstract_im_result.abstract_state_space.nb_states) ^ " state" ^ (s_of_int abstract_im_result.abstract_state_space.nb_states)
						^ " with "
						^ (string_of_int abstract_im_result.abstract_state_space.nb_transitions) ^ " transition" ^ (s_of_int abstract_im_result.abstract_state_space.nb_transitions) ^ " explored.");

				self#print_algo_message Verbose_medium ("Sent the result to the master");

				counter_working#stop;
				
			| DistributedUtilities.Stop ->
				self#print_algo_message Verbose_medium ("I was just told to stop working.");
				finished := true
			
			| _ -> raise (InternalError("Unexpected tag received from the master."))
		
		done;
		
		
		self#print_algo_message Verbose_low ("I'm done.");
		
		(* Return dummy result *)
		Distributed_worker_result


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm (useless method) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_bc_result : Result.imitator_result =
		Distributed_worker_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
