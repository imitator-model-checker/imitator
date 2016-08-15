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
 * Last modified     : 2016/08/15
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
	inherit AlgoBCCoverDistributed.algoBCCoverDistributed as super
	
	
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

(* 		let model = Input.get_model() in *)

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

				(* Call IM *)
				let abstract_point_based_result = self#run_im  pi0 None in
				
				(* Send the result to the master *)
				DistributedUtilities.send_abstract_point_based_result abstract_point_based_result;
				
				(* Print some information *)
				self#print_algo_message_newline Verbose_medium (
						"K computed" (*^ " by " ^ (algo#algorithm_name) *)(*^ " after "
						^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""*)
						^ " in " ^ (string_of_seconds abstract_point_based_result.computation_time) ^ ": "
						^ (string_of_int abstract_point_based_result.abstract_state_space.nb_states) ^ " state" ^ (s_of_int abstract_point_based_result.abstract_state_space.nb_states)
						^ " with "
						^ (string_of_int abstract_point_based_result.abstract_state_space.nb_transitions) ^ " transition" ^ (s_of_int abstract_point_based_result.abstract_state_space.nb_transitions) ^ " explored.");

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
