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
 * Last modified: 2014/04/18
 *
 ****************************************************************)
 
open Global
open Mpi
(* open Printf (* a terme : retirer tout ca *) *)
(* open Unix (* temporaire : necessaire pour sleep *) *)
open Reachability
open DistributedUtilities
	

(* Declaration of the tags we are going to use in 
   communications within the M/W pattern *)

(* tmp *)
let cnt = ref 0

(* ** *** *** ***** *******       UTILS       ******* ***** *** *** ** *)


(* Store the result of a computation.                                  *)

(*let store result source =
	print_string "MASTER - recv result" ; print_string result;
	print_string " from ";
	print_int source ;
	print_newline();
	0
;;*)

(* Returns the next set of input data                                  *)
(* This is where the master's intelligence will come.                  *)
(*                                                                     *)
(* Returns a couple: the first element is the size of the data, the    *)
(* second one is the data itself. The data is serialized and ready to  *)
(* be sent.                                                            *)

(*let get_data () =
	let size = 1 + ( Random.int 25 ) in
	let buff = String.create size in
	let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
	let len = String.length alphanum in
	for i = 0 to pred size do
		buff.[i] <- alphanum.[Random.int len]
	done;
	cnt := !cnt + 1;
	(size, buff)
;;*)


(* Initialize a slave                                                  *)

let init_slave rank size =
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I am worker [" ^ (string_of_int rank) ^ "] in " ^ (string_of_int (size-1)) ^ ".");
;;


(* ** *** *** ***** *******       MASTER      ******* ***** *** *** ** *)

let receive_pull_request_and_store_constraint () =
	print_message Debug_medium ("[Master] Entered function 'receive_pull_request_and_store_constraint'...");
	match receive_pull_request () with
	| PullOnly source_rank ->
		print_message Debug_medium ("[Master] Received PullOnly request...");
		source_rank, None
	
	| OutOfBound source_rank ->
		print_message Debug_medium ("[Master] Received OutOfBound request...");
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		raise (InternalError("OutOfBound not implemented."))(*;
		source_rank, None*)

	| PullAndResult (source_rank , im_result) -> 
		print_message Debug_medium ("[Master] Received PullAndResult request...");
		(* Process the result by IM *)
		Cartography.bc_process_im_result im_result;
		(* Return source rank *)
		source_rank, Some im_result.tile_nature


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
		print_message Debug_standard ("[Master] Waiting for a pull request");

		(* Get the pull_request *)
		let source_rank, tile_nature_option = receive_pull_request_and_store_constraint () in
	
		print_message Debug_standard ("[Master] Got a pull request from slave " ^ (string_of_int source_rank) ^ "");

		(* Retrieve pi0 *)
		let pi0 = Cartography.get_current_pi0 () in
			
		(* Send it *)
		send_pi0 pi0 source_rank;
		
		(* Compute the next pi0 for next time, and return flags for more pi0 and co *)
		
		(*** WARNING: computing the pi0 BEFORE it is asked is probably stupid! It would be smarter to compute it on demand (to be compared) ***)
		
		let found_pi0 , time_limit_reached = Cartography.find_next_pi0 tile_nature_option in
		
		(* Update the time limit *)
		limit_reached := time_limit_reached;
		(* Update the found pi0 flag *)
		more_pi0 := found_pi0;

	done; (* while more pi0 *)

	(* Print info if premature termination *)
	if !limit_reached && !more_pi0 then (
		(*** WARNING : what about other limits?! (iterations, etc.?) ***)
		match options#time_limit with
			| None -> ()
			| Some limit -> if (get_time()) > (float_of_int limit) then print_warning (
				"The time limit (" ^ (string_of_int limit) ^ " second" ^ (s_of_int limit) ^ ") has been reached.
					The behavioral cartography algorithm will now stop to launch instances of IM, although the cartography is not covered yet."
			);
	);
	

    print_message Debug_standard ("[Master] Done!" );

    (* I am done sending all my data. Receive the results of the last
       computations, and wrap up. *)
    
	let size = Mpi.comm_size Mpi.comm_world in

    let k = ref 0 in
    while !k < ( size - 1) do
		print_message Debug_standard ("[Master] Waiting for a pull request");
		(* Get the pull_request *)
		let source_rank , _ = receive_pull_request_and_store_constraint () in
		print_message Debug_standard ("[Master] Received from " ^ ( string_of_int source_rank ));
		(* Say good bye *)
		send_finished source_rank;
		
(*		
		
      let (len, src, tag) = 
	Mpi.receive_status Mpi.any_source any_tag Mpi.comm_world in
      
      if tag = result_tag then 
	begin
	  (* receive the result itself *)
	  let buff = String.create len in
	  let res = ref buff in
	  res := Mpi.receive src result_tag Mpi.comm_world; 
	    
	  (* Received the result of a computation - store it. *)
	  rc := store !res src ;
	  print_string "MASTER - stored result and returned value " ;
	  print_int !rc;
	  print_newline();*)
	  
	  k := ( !k + 1 );
	  
(* 	end *)
    done;

    print_message Debug_standard ("[Master] All slaves done" );
    
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
  
(* *** *** ***** *******      WORKER      ******* ***** *** *** *)

let worker () = 
    
	(* Get the model *)
	let model = Input.get_model() in

	let rank = rank() in
	let size = size() in

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Hello!");
	init_slave rank size;

(*     let n = rank in *)
    let finished = ref false in

    (* Start: ask for some work *)
    send_work_request();

    print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] sent pull request to the master.");

	(* In the meanwhile: compute the initial state *)
    let init_state = Reachability.get_initial_state_or_abort model in

    while not !finished do

		match receive_work () with
		
		| Work pi0 -> 
            (* Do the job here *)

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
			
			(* Prevent the debug messages (except in debug medium, high or total) *)
			if not (debug_mode_greater Debug_medium) then
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

			(* Send the result *)
			send_result im_result;
			print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] Sent a constraint.");
			
		| Stop ->
			print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I was just told to stop work.");
			finished := true
    done;
	print_message Debug_standard ("[Worker " ^ (string_of_int rank) ^ "] I'm done.");
;;
