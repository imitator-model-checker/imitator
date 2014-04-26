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
 * Last modified: 2014/04/26
 *
 ****************************************************************)

 
open Global
open Options
open Mpi
open Reachability
open DistributedUtilities


(****************************************************************)
(**     MASTER      *)
(****************************************************************)

(*let is_limit_reached () =
  cnt := !cnt + 1;
  print_message Debug_standard( "[Master] " ^ ( string_of_int !cnt ) ^" rounds done" );
  if !cnt > nb_of_rounds then
    true
  else
    false
    *)
let receive_pull_request_and_store_constraint () =
	print_message Debug_high ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	match receive_pull_request () with
	| PullOnly source_rank ->
		print_message Debug_low ("[Master] Received PullOnly request...");
		source_rank, None

	| OutOfBound source_rank ->
		print_message Debug_low ("[Master] Received OutOfBound request...");
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)

	| PullAndResult (source_rank , im_result) -> 
		print_message Debug_low ("[Master] Received PullAndResult request...");
		print_message Debug_standard ("\n[Master] Received the following constraint from worker " ^ (string_of_int source_rank));
		Cartography.bc_process_im_result im_result;
		(* Return source rank *)
		source_rank, Some im_result.tile_nature
;;

(*------------------------------------------------------------*)
(* Generic function handling the next sequential point *)
(*------------------------------------------------------------*)
let compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option =
	(* Case first point *)
	if !first_point then(
		print_message Debug_low ("[Master] This is the first pi0.");
		Cartography.compute_initial_pi0 ();
		first_point := false;
	(* Other case *)
	)else(
		print_message Debug_low ("[Master] Computing next pi0 sequentially...");
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;
	)

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
	(** Distributed mode: Master slave with sequential pi0 *)
	| Distributed_sequential -> 
		compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
	
	(** Distributed mode: Master slave with random pi0 and n retries before switching to sequential mode *)
	| Distributed_random nb_tries ->
		(* Test whether the algorithm is still in random mode *)
		if !still_random then(
			(* Try to find a random pi0 *)
			let successful = Cartography.random_pi0 nb_tries in
			if successful then(
				print_message Debug_low ("[Master] Computed a new random pi0.");
			)else(
				(* Switch mode ! *)
				still_random := false;
				print_message Debug_standard ("[Master] Could not find a new random pi0 after " ^ (string_of_int nb_tries) ^ " attemps. Now switching mode from random to sequential.");
			);
		);
		(* Test whether the algorithm is now in sequential mode *)
		(*** NOTE: important to test again (instead of using 'else') in case the value was changed just above ***)
		if not !still_random then(
			print_message Debug_medium ("[Master] Now in sequential mode: computing a sequential pi0.");
			compute_next_pi0_sequentially more_pi0 limit_reached first_point tile_nature_option
		);
		print_message Debug_high ("[Master] Exiting function compute_next_pi0...");
	
	(** Normal mode *)
	| Non_distributed -> raise (InternalError("IMITATOR should be distributed at this point."))


(*------------------------------------------------------------*)
(* The cartography algorithm implemented in the master *)
(*------------------------------------------------------------*)
let master () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	print_message Debug_medium ("[Master] Hello world!");
	
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
	| Distributed_random _ -> still_random := true;
	| _ -> ();
	end;
	
	(* IF precompute: Compute the first point pi0 *)
	if options#precomputepi0 then(
		compute_next_pi0 more_pi0 limit_reached first_point None;
(* 		Cartography.compute_initial_pi0 (); *)
	);
	
	(* Iterate on all the possible pi0 *)
	while !more_pi0 && not !limit_reached do
		print_message Debug_low ("[Master] Waiting for a pull request");
		
		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "");
		
		(* IF no-precompute: compute pi0 NOW *)
		if not options#precomputepi0 then(
			print_message Debug_high ("[Master] Computing pi0...");
			compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option;
		);
		
		(* Access the pi0 *)
		let pi0 = Cartography.get_current_pi0 () in
		
		(* If finished: say goodbye *)
		if !limit_reached || not !more_pi0 then(
			send_finished source_rank;
			workers_done := !workers_done + 1;
			print_message Debug_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
		(* Otherwise send pi0 *)
		)else(
			print_message Debug_medium ( "[Master] Sending pi0 to [Worker " ^ (string_of_int source_rank ) ^ "]" ) ;
			send_pi0 pi0 source_rank;
		);

		(* IF precompute: Compute the next pi0 for next time, and return flags for more pi0 and co *)
		(*** WARNING: computing the pi0 BEFORE it is asked may be stupid! It may be smarter to compute it on demand (to be compared) ***)
		if options#precomputepi0 && not !limit_reached && !more_pi0 then(
			compute_next_pi0 more_pi0 limit_reached first_point tile_nature_option;
		);
	done;


	(*** NOTE: we could check here (and at every further iteration) whether all integer points are already covered!!! If yes, stop. ***)
	
	print_message Debug_medium ( "[Master] Done with sending pi0; waiting for last results." );

	let size = Mpi.comm_size Mpi.comm_world in
		while !workers_done < ( size - 1) do
		print_message Debug_medium ("[Master] " ^ ( string_of_int ( size - 1 - !workers_done )) ^ " workers left" );
		let source_rank , _ = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Received from [Worker " ^ ( string_of_int source_rank ) ^"]");
		(* Say good bye *)
		send_finished source_rank;
		workers_done := !workers_done + 1;
		print_message Debug_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
	done;
		
	print_message Debug_medium ("[Master] All workers done" );

	(* Process the finalization *)
	Cartography.bc_finalize ();
	
	(* Process the result and return *)
	let tiles = Cartography.bc_result () in
	(* Render zones in a graphical form *)
	if options#cart then (
		Graphics.cartography (Input.get_model()) (Input.get_v0()) tiles (options#files_prefix ^ "_cart_patator")
	) else (
		print_message Debug_high "Graphical cartography not asked: graph not generated.";
	);
	
	()
;;

(****************************************************************)
(**        WORKER         *)
(****************************************************************)

let init_slave rank size =
	print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I am worker " ^ (string_of_int rank) ^ "/" ^ (string_of_int (size-1)) ^ ".");
;;

let worker() =

  (* Get the model *)
  let model = Input.get_model() in
  
  let rank = Mpi.comm_rank Mpi.comm_world in
  let size = Mpi.comm_size Mpi.comm_world in
  init_slave rank size;
  
  let finished = ref false in
  
  (* Ask for some work *)
  send_work_request ();
    
  print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
  
  (* In the meanwhile: compute the initial state *)
  let init_state = Reachability.get_initial_state_or_abort model in
 
  while not !finished do
    
    match receive_work () with
    | Work pi0 ->  (* receive a chunk of work *)

       print_message Debug_medium( "[Worker " ^ ( string_of_int rank ) ^ "] received work. Send a result." );

       (* Set the new pi0 *)
       Input.set_pi0 pi0;
       
       (* Print some messages *)
       print_message Debug_medium ("\n**************************************************");
       print_message Debug_medium ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
       print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] working now.");
       print_message Debug_medium ("Considering the following pi" (*^ (string_of_int !current_iteration)*));
       print_message Debug_medium (ModelPrinter.string_of_pi0 model pi0);
       
       (* Save debug mode *)
       let global_debug_mode = get_debug_mode() in 
       
       (* Prevent the debug messages (except in verbose modes high or total) *)
       if not (debug_mode_greater Debug_high) then
	 set_debug_mode Debug_nodebug;
       
       (* Call IM *)
       let im_result , _ = Reachability.inverse_method_gen model init_state in
			
       print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] finished a computation of IM.");
			
       (* Get the debug mode back *)
       set_debug_mode global_debug_mode;
       
       (* Process the result by IM *)
       (*** TODO (cannot jus call process_im_result) ***)
       
       (* Print message *)
       print_message Debug_medium (
		       "\n[Worker " ^ (string_of_int rank) ^ "] K computed by IM after "
		       ^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
		       ^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
		       ^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
		       ^ " with "
		       ^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
       
       (*** TODO: handle a special case if the result is NOT valid (e.g., stopped before the end due to timeout or state limit reached) ***)
       
       send_result  im_result;
       print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint ");

    | Stop ->      (* end *)
       print_message Debug_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work.");
       finished := true
  done;
  print_message Debug_medium ("\t[Worker " ^ (string_of_int rank) ^ "] I'm done.");
;;
		     
