open Global
open Mpi
(* open Printf (* a terme : retirer tout ca *) *)
(* open Unix (* temporaire : necessaire pour sleep *) *)
open Reachability
open DistributedUtilities
	
let masterrank = 0 
let cnt = ref 0 (* DEBUG *)
let nb_of_rounds = 20    


(****************************************************************)
(**     MASTER      *)
(****************************************************************)

let is_limit_reached () =
  cnt := !cnt + 1;
  print_string( "[Master] " ^ ( string_of_int !cnt ) ^" rounds done" ); print_newline() ;
  if !cnt > nb_of_rounds then
    true
  else
    false
let receive_pull_request_and_store_constraint () =
	print_string ("[Master] Entered function 'receive_pull_request_and_store_constraint'..."); print_newline() ;
	match receive_pull_request () with
	| PullOnly source_rank ->
		print_string ("[Master] Received PullOnly request..."); print_newline() ;
		source_rank, None

	| OutOfBound source_rank ->
		print_string ("[Master] Received OutOfBound request..."); print_newline() ;
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)

	| PullAndResult (source_rank , im_result) -> 
		print_string ("[Master] Received PullAndResult request..."); print_newline() ;
		Cartography.bc_process_im_result im_result;
		(* Return source rank *)
		source_rank, Some im_result.tile_nature
;;

let master () =
  (* Retrieve the input options *)
  let options = Input.get_options () in
  
  print_message Debug_standard ("[Master] Hello world!");
  
  (* Perform initialization *)
  Cartography.bc_initialize ();
  
  (* Compute the first point pi0 *)
  Cartography.compute_initial_pi0 ();
  
  (* Iterate on all the possible pi0 *)
  let more_pi0 = ref true in
  let limit_reached = ref false in
  
  while !more_pi0 && not !limit_reached do
    print_string ("[Master] Waiting for a pull request"); print_newline() ;
    
    (* Get the pull_request *)
    let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
    print_message Debug_standard ("[Master] Got a pull request from slave " ^ (string_of_int source_rank) ^ "");
    
    (* Retrieve pi0 *)
    let pi0 = Cartography.get_current_pi0 () in
    			
    (* Send it *)
    print_string ( "[Master] : sent pi0 to [Worker " ^ (string_of_int source_rank ) ^ "]" ) ; print_newline(); 
    send_pi0 pi0 source_rank;

    (* Compute the next pi0 for next time, and return flags for more pi0 and co *)
    
    (*** WARNING: computing the pi0 BEFORE it is asked is probably stupid! It would be smarter to compute it on demand (to be compared) ***)
    
    let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
    
    (* Update the time limit *)
    limit_reached := time_limit_reached;
    (* Update the found pi0 flag *)
    more_pi0 := found_pi0;
  done;

    print_string( "[Master] Done." ); print_newline() ;

  let size = Mpi.comm_size Mpi.comm_world in
    let k = ref 0 in
    while !k < ( size - 1) do
      print_string ("[Master] " ^ ( string_of_int ( size - 1 - !k )) ^ " slaves left" ); print_newline() ;
      let source_rank , _ = receive_pull_request_and_store_constraint () in
      print_string ("[Master] Received from [Worker " ^ ( string_of_int source_rank ) ^"]"); print_newline() ;
      (* Say good bye *)
      send_finished source_rank;
      k := !k + 1;
      print_string( "\t[Master] - [Worker " ^ (string_of_int source_rank ) ^ "] is done"); print_newline();
    done;
    
    print_string ("[Master] All slaves done" ); print_newline() ;

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
	print_string ("[Worker " ^ (string_of_int rank) ^ "] I am worker [" ^ (string_of_int rank) ^ "] in " ^ (string_of_int (size-1)) ^ "."); print_newline() ;
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
    
  print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");
  
  (* In the meanwhile: compute the initial state *)
  let init_state = Reachability.get_initial_state_or_abort model in
 
  while not !finished do
    
    match receive_work () with
    | Work pi0 ->  (* receive a chunk of work *)

       print_string( "[Worker " ^ ( string_of_int rank ) ^ "] received work. Send a result." ); print_newline() ;

       (* Set the new pi0 *)
       Input.set_pi0 pi0;
       
       (* Print some messages *)
       print_message Debug_standard ("\n**************************************************");
       print_message Debug_standard ("BEHAVIORAL CARTOGRAPHY ALGORITHM: "(* ^ (string_of_int !current_iteration) ^ ""*));
       print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] working now.");
       print_message Debug_standard ("Considering the following pi" (*^ (string_of_int !current_iteration)*));
       print_message Debug_standard (ModelPrinter.string_of_pi0 model pi0);
       
       (* Save debug mode *)
       let global_debug_mode = get_debug_mode() in 
       
       (* Prevent the debug messages (except in verbose modes high or total) *)
       if not (debug_mode_greater Debug_high) then
	 set_debug_mode Debug_nodebug;
       
       (* Call IM *)
       let im_result , _ = Reachability.inverse_method_gen model init_state in
			
       print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] finished a computation of IM.");
			
       (* Get the debug mode back *)
       set_debug_mode global_debug_mode;
       
       (* Process the result by IM *)
       (*** TODO (cannot jus call process_im_result) ***)
       
       (* Print message *)
       print_message Debug_standard (
		       "\n[Worker " ^ (string_of_int rank) ^ "] K computed by IM after "
		       ^ (string_of_int im_result.nb_iterations) ^ " iteration" ^ (s_of_int im_result.nb_iterations) ^ ""
		       ^ " in " ^ (string_of_seconds im_result.total_time) ^ ": "
		       ^ (string_of_int im_result.nb_states) ^ " state" ^ (s_of_int im_result.nb_states)
		       ^ " with "
		       ^ (string_of_int im_result.nb_transitions) ^ " transition" ^ (s_of_int im_result.nb_transitions) ^ " explored.");
       
       (*** TODO: handle a special case if the result is NOT valid (e.g., stopped before the end due to timeout or state limit reached) ***)
       
       send_result  im_result;
       print_string ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint "); print_newline() ;

    | Stop ->      (* end *)
       print_string ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work."); print_newline() ;
       finished := true
  done;
  print_string ("\t[Worker " ^ (string_of_int rank) ^ "] I'm done."); print_newline() ;
;;
		     
