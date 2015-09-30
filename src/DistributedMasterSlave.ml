(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre, Camille Coti
 * 
 * Created:       2014/03/24
 * Last modified: 2015/04/12
 *
 ****************************************************************)


(**************************************************)
(* External modules *)
(**************************************************)
open Mpi


(**************************************************)
(* Internal modules *)
(**************************************************)
open Exceptions
open CamlUtilities
open ImitatorUtilities
open Options
open Reachability
open DistributedUtilities


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
(* Counters *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

(*------------------------------------------------------------*)
(* Slaves *)
(*------------------------------------------------------------*)

(*let counter_worker_waiting = ref 0.0
let start_time_next_point = ref 0.0

let start_counter_next_point () =
	start_time_next_point := Unix.gettimeofday()

let stop_counter_next_point () =
	counter_next_point := !counter_next_point +. Unix.gettimeofday() -. !start_time_next_point
*)

let counter_master_waiting 		= new Counter.counter
let counter_master_find_nextpi0	= new Counter.counter
let counter_worker_waiting 		= new Counter.counter
let counter_worker_working 		= new Counter.counter


(****************************************************************)
(**     MASTER      *)
(****************************************************************)

(*let is_limit_reached () =
  cnt := !cnt + 1;
  print_message Verbose_standard( "[Master] " ^ ( string_of_int !cnt ) ^" rounds done" );
  if !cnt > nb_of_rounds then
    true
  else
    false
    *)
let receive_pull_request_and_store_constraint () =
	print_message Verbose_high ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	
	counter_master_waiting#start;
	let pull_request = receive_pull_request () in
	counter_master_waiting#stop;
	
	match pull_request with
	| PullOnly source_rank ->
		print_message Verbose_low ("[Master] Received PullOnly request...");
		source_rank, None

	| OutOfBound source_rank ->
		print_message Verbose_low ("[Master] Received OutOfBound request...");
		(*** TODO: DO SOMETHING TO HANDLE THE CASE OF A POINT THAT WAS NOT SUCCESSFUL ***)
		raise (InternalError("OutOfBound not implemented."))
		(* source_rank, None*)

	| Tile (source_rank , im_result) -> 
		print_message Verbose_low ("[Master] Received Tile request...");
		print_message Verbose_standard ("\n[Master] Received the following constraint from worker " ^ (string_of_int source_rank));
		ignore (Cartography.bc_process_im_result im_result);
		(* Return source rank *)
		source_rank, Some im_result.tile_nature
		
	(*** WARNING: pattern-matching not exhaustive; is that safe? ***)
	
;;


(*------------------------------------------------------------*)
(* Generic function handling the next sequential point *)
(*------------------------------------------------------------*)
let compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option =
	(* Start timer *)
	counter_master_find_nextpi0#start ;

	(* Case first point *)
	if !first_point then(
		print_message Verbose_low ("[Master] This is the first pi0.");
		Cartography.compute_initial_pi0 ();
		first_point := false;
	(* Other case *)
	)else(
		print_message Verbose_low ("[Master] Computing next pi0 sequentially...");
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
	);
	
	(* Stop timer *)
	counter_master_find_nextpi0#stop;
	
	()


(*------------------------------------------------------------*)
(* Generic function handling the next point in shuffle mode *)
(*------------------------------------------------------------*)
let compute_next_pi0_shuffle more_pi0 limit_reached first_point tile_nature_option =
	(* Start timer *)
	counter_master_find_nextpi0#start ;

	print_message Verbose_low ("[Master] Computing next pi0 in shuffle mode...");
	
	let found_pi0 , time_limit_reached = Cartography.find_next_pi0_shuffle tile_nature_option in
	(* Update the time limit *)
	limit_reached := time_limit_reached;
	(* Update the found pi0 flag *)
	more_pi0 := found_pi0;

	(* Stop timer *)
	counter_master_find_nextpi0#stop;
	
	()

(*------------------------------------------------------------*)
(* Global variable for the random distributed mode *)
(*------------------------------------------------------------*)
(*** TODO: put somewhere else ***)
let still_random = ref true


(*------------------------------------------------------------*)
(* Generic function handling all cases *)
(*------------------------------------------------------------*)
let compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	match options#distribution_mode with
	
	(*** WARNING: pattern-matching not exhaustive; is that safe? ***)

	(** Distributed mode: Master slave with sequential pi0 *)
	| Distributed_ms_sequential -> 
		compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
	
	(** Distributed mode: Master slave with shuffle pi0 *)
	| Distributed_ms_shuffle -> 
		compute_next_pi0_shuffle more_pi0 limit_reached first_point tile_nature_option
	
	(** Distributed mode: Master slave with random pi0 and n retries before switching to sequential mode *)
	| Distributed_ms_random nb_tries ->
		(* Test whether the algorithm is still in random mode *)
		if !still_random then(
			(* Try to find a random pi0 *)
			let successful = Cartography.random_pi0 nb_tries in
			if successful then(
				print_message Verbose_low ("[Master] Computed a new random pi0.");
			)else(
				(* Switch mode ! *)
				still_random := false;
				print_message Verbose_standard ("\n**************************************************");
				print_message Verbose_standard ("[Master] Could not find a new random pi0 after " ^ (string_of_int nb_tries) ^ " attemps! Now switching mode from random to sequential.");
			);
		);
		(* Test whether the algorithm is now in sequential mode *)
		(*** NOTE: important to test again (instead of using 'else') in case the value was changed just above ***)
		if not !still_random then(
			print_message Verbose_medium ("[Master] Now in sequential mode: computing a sequential pi0.");
			compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
		);
		print_message Verbose_high ("[Master] Exiting function compute_next_pi0...");
	(*** TODO: missing something there ***)
		
	| Distributed_static -> raise (InternalError(Constants.program_name ^ " cannot be in static distribution mode at this point."))

	| Distributed_unsupervised -> raise (InternalError(Constants.program_name ^ " cannot be unsupervised at this point."))

	| Distributed_unsupervised_multi_threaded -> raise (InternalError(Constants.program_name ^ " cannot be unsupervised-multi-threaded at this point."))

	(** Normal mode *)
	| Non_distributed -> raise (InternalError(Constants.program_name ^ " should be distributed at this point."))


(*------------------------------------------------------------*)
(* Functions handling shuffle mode *)
(*------------------------------------------------------------*)

(*(* Sends one point to each node (to have all nodes occupied for a while) *)
let send_one_point_to_each_node() = ()*)

(* Statically compute the shuffled array of all points *)
let compute_shuffled_array() =
	(* Compute the array of all points *)
	print_message Verbose_low ("[Master] Shuffle mode: about to compute the array of all points");
	Cartography.compute_all_pi0 ();
	(* Shuffle it! *)
	print_message Verbose_low ("[Master] Shuffle mode: about to shuffle the array of points");
	Cartography.shuffle_all_pi0 ();
	()

(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
let master () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Initialize counters *)
	counter_master_find_nextpi0#init;
	counter_master_waiting#init;
	
	print_message Verbose_medium ("[Master] Hello world!");
	
	(* Perform initialization *)
	Cartography.bc_initialize ();
	
	let more_pi0 = ref true in
	let limit_reached = ref false in
	
	(* To differentiate between initialization of pi0 / next_point *)
	let first_point = ref true in
	
	(* For the end of the algorithm *)
	let workers_done = ref 0 in
	
	(* If random: start with random (switch to sequential later) *)
	begin
	match options#distribution_mode with
		| Distributed_ms_random _ -> still_random := true;
		| _ -> ();
	end;
	
	(* IF precompute: Compute the first point pi0 *)
	if options#precomputepi0 then(
		(*** WARNING: this may lead to an unexpected behavior if mode is shuffle ! ***)
		compute_next_pi0 more_pi0 limit_reached first_point None;
(* 		Cartography.compute_initial_pi0 (); *)
	);
	
	(* IF shuffle mode: first sends one point to each node, and then statically compute the shuffled array of all points *)
	if options#distribution_mode = Distributed_ms_shuffle then(
		(* First sends one point to each node (to have all nodes occupied for a while) *)
		(*** NO!!!! just ask the slaves to do this ***)
(*		print_message Verbose_standard ("[Master] Shuffle mode: sending one point to each node");
		send_one_point_to_each_node();*)
		
		(* In the meanwhile: statically compute the shuffled array of all points *)
		print_message Verbose_standard ("[Master] Shuffle mode: starting to compute the shuffled array of points");
		compute_shuffled_array();
		print_message Verbose_standard ("[Master] Shuffle mode: finished to compute the shuffled array of points");
	);
	(* end if shuffle *)
	
	(* Iterate on all the possible pi0 *)
	while !more_pi0 && not !limit_reached do
		print_message Verbose_low ("[Master] Waiting for a pull request");
		
		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
		print_message Verbose_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
		
		(* IF no-precompute: compute pi0 NOW *)
		if not options#precomputepi0 then(
			print_message Verbose_high ("[Master] Computing pi0...");
			compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option;
		);
		
		(* Access the pi0 *)
		let pi0 = Cartography.get_current_pi0 () in
		
		(* If finished: say goodbye *)
		if !limit_reached || not !more_pi0 then(
			send_finished source_rank;
			workers_done := !workers_done + 1;
			print_message Verbose_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
		(* Otherwise send pi0 *)
		)else(
			print_message Verbose_medium ( "[Master] Sending pi0 to [Worker " ^ (string_of_int source_rank ) ^ "]" );
			send_pi0 pi0 source_rank;
		);

		(* IF precompute: Compute the next pi0 for next time, and return flags for more pi0 and co *)
		(*** WARNING: computing the pi0 BEFORE it is asked may be stupid! It may be smarter to compute it on demand (to be compared) ***)
		if options#precomputepi0 && not !limit_reached && !more_pi0 then(
			compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option;
		);
	done;


	(*** NOTE: we could check here (and at every further iteration) whether all integer points are already covered!!! If yes, stop. ***)
	
	print_message Verbose_standard ( "[Master] Done with sending pi0; waiting for last results.");

	let size = get_nb_nodes() in
	while !workers_done < ( size - 1) do
		print_message Verbose_medium ("[Master] " ^ ( string_of_int ( size - 1 - !workers_done )) ^ " workers left" );
		let source_rank , _ = receive_pull_request_and_store_constraint () in
		print_message Verbose_medium ("[Master] Received from Worker " ^ ( string_of_int source_rank ) ^"");
		(* Say good bye *)
		send_finished source_rank;
		workers_done := !workers_done + 1;
		print_message Verbose_medium( "\t[Master] Worker " ^ (string_of_int source_rank ) ^ " is done");
	done;
		
	print_message Verbose_standard ("[Master] All workers done" );

	(* Process the finalization *)
	print_message Verbose_standard ("[Master] Finalizing the cartography..." );
	Cartography.bc_finalize ();
	
	print_message Verbose_standard ("[Master] Total waiting time     : " ^ (string_of_float (counter_master_waiting#value)) ^ " s");
	print_message Verbose_standard ("**************************************************");

	(* Generate the graphics *)
	Cartography.output_graphical_cartography (Some "_cart_patator");
	
	(* The end *)
	()

;;

(****************************************************************)
(**        WORKER         *)
(****************************************************************)

let init_slave rank size =
	print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
;;

let worker() =

	(* Get the model *)
	let model = Input.get_model() in
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	(* Init counters *)
	counter_worker_waiting#init;
	counter_worker_working#init;
	
	let rank = get_rank() in
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
;;

