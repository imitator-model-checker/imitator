(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-slave with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/10
 * Last modified     : 2016/03/10
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
	method virtual im_instance : AlgoIMK.algoIMK

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		raise (InternalError ("Not implemented"))
		(*** NOTE: the initial state is computed again and again for each new instance of IM; TO OPTIMIZE? ***)
		

(*		(* Get the model *)
		let model = Input.get_model() in
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Init counters *)
		(*** TODO ***)
		counter_worker_waiting#init;
		counter_worker_working#init;
		
		let size = get_nb_nodes() in
		init_slave rank size;
		
		let finished = ref false in
		
		begin
		match options#distribution_mode with 
			(* First start by compyting a random pi0 (while the master is computing the shuffle list) *)
			| Distributed_ms_shuffle -> 
				(*** TODO !!! TODO !!! TODO !!! ***)
				(*** NOTE: in fact, maybe NOT efficient; even for a big V0 (500,000 points), the shuffle takes only a few seconds (12s) ***)
				(*** TODO: test and compare ***)
				send_work_request ();
			(* Otherwise just ask lazily for some work *)
			| _ ->
				(* Ask for some work *)
				send_work_request ();
		end;
			
		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
		
		(* In the meanwhile: compute the initial state *)
		let init_state = Reachability.get_initial_state_or_abort model in
		
		while not !finished do
			
			counter_worker_waiting#start;
			let work = receive_work () in
			counter_worker_waiting#stop;
			
			match work with
			
		(*** WARNING: pattern-matching not exhaustive; is that safe? ***)

		(* receive a chunk of work *)
			| Work pi0 ->
				counter_worker_working#start;

				print_message Verbose_medium( "[Worker " ^ ( string_of_int rank ) ^ "] received work. Send a result." );

				(* Set the new pi0 *)
				Input.set_pi0 pi0;
				
				(* Print some messages *)
				print_message Verbose_medium ("\n**************************************************");
				print_message Verbose_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
				print_message Verbose_standard ("\n[Worker " ^ (string_of_int rank) ^ "] Launching IM for the following pi:" (*^ (string_of_int !current_iteration)*));
				print_message Verbose_standard (ModelPrinter.string_of_pi0 model pi0);
				
				(* Save verbose mode *)
				let global_verbose_mode = get_verbose_mode() in 
				
				(* Prevent the verbose messages (except in verbose modes high or total) *)
				if not (verbose_mode_greater Verbose_high) then
						set_verbose_mode Verbose_mute;
				
				(* Call IM *)
				let im_result , _ = Reachability.inverse_method_gen model init_state in
						
				(* Get the verbose mode back *)
				set_verbose_mode global_verbose_mode;
				
				print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] finished a computation of IM.");
						
				(* Process the result by IM *)
				(*** TODO (cannot jus call process_im_result) ***)
				
				(* Print message *)
				print_message Verbose_medium (
						"\n[Worker " ^ (string_of_int rank) ^ "] K computed by IM after "
						^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
						^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
						^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
						^ " with "
						^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
				
				(*** TODO: handle a special case if the result is NOT valid (e.g., stopped before the end due to timeout or state limit reached) ***)
				
				send_result  im_result;
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint ");

				counter_worker_working#stop;
				
			(* The end *)
			| Stop ->
				print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work.");
				finished := true

				(*** WARNING: pattern-matching not exhaustive; is that safe? ***)
			
		done;
		
		(* Print some information *)
		let occupancy = counter_worker_working#value /. (counter_worker_working#value +. counter_worker_waiting#value) *. 100. in
		print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
		print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Total waiting time     : " ^ (string_of_float (counter_worker_waiting#value)) ^ " s");
		print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Total working time     : " ^ (string_of_float (counter_worker_working#value)) ^ " s");
		print_message Verbose_standard ("[Worker " ^ (string_of_int rank) ^ "] Occupancy              : " ^ (string_of_float occupancy) ^ " %");		
		
		raise (InternalError("not implemented"))*)


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		raise (InternalError("not implemented"))


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
