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
		Cartography.bc_process_im_result im_result;
		(* Return source rank *)
		source_rank, Some im_result.tile_nature
;;



let master () =
	(* Retrieve the input options *)
	let options = Input.get_options () in
	
	print_message Debug_medium ("[Master] Hello world!");
	
	(* Perform initialization *)
	Cartography.bc_initialize ();
	
	(* Compute the first point pi0 *)
	Cartography.compute_initial_pi0 ();
	
	(* Iterate on all the possible pi0 *)
	let more_pi0 = ref true in
	let limit_reached = ref false in
	
	while !more_pi0 && not !limit_reached do
		print_message Debug_low ("[Master] Waiting for a pull request");
		
		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Got a pull request from slave " ^ (string_of_int source_rank) ^ "");
		
		(* IF no-precompute: compute pi0 NOW *)
		if not options#precomputepi0 then(
			let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
			(* Update the time limit *)
			limit_reached := time_limit_reached;
			(* Update the found pi0 flag *)
			more_pi0 := found_pi0;
		);
		
		(* Access the pi0 *)
		let pi0 = Cartography.get_current_pi0 () in
		
		(* Send it *)
		print_message Debug_medium ( "[Master] Sent pi0 to [Worker " ^ (string_of_int source_rank ) ^ "]" ) ;
		send_pi0 pi0 source_rank;

		(* IF precompute: Compute the next pi0 for next time, and return flags for more pi0 and co *)
		(*** WARNING: computing the pi0 BEFORE it is asked may be stupid! It may be smarter to compute it on demand (to be compared) ***)
		if options#precomputepi0 then(
			let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
			(* Update the time limit *)
			limit_reached := time_limit_reached;
			(* Update the found pi0 flag *)
			more_pi0 := found_pi0;
		);
	done;
	

	(*** NOTE: we could check here (and at every further iteration) whether all integer points are already covered!!! If yes, stop. ***)
	
	print_message Debug_medium ( "[Master] Done with sending pi0; waiting for last results." );

	let size = Mpi.comm_size Mpi.comm_world in
		let k = ref 0 in
		while !k < ( size - 1) do
		print_message Debug_medium ("[Master] " ^ ( string_of_int ( size - 1 - !k )) ^ " slaves left" );
		let source_rank , _ = receive_pull_request_and_store_constraint () in
		print_message Debug_medium ("[Master] Received from [Worker " ^ ( string_of_int source_rank ) ^"]");
		(* Say good bye *)
		send_finished source_rank;
		k := !k + 1;
		print_message Debug_medium( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done");
	done;
		
	print_message Debug_medium ("[Master] All slaves done" );

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
		     
