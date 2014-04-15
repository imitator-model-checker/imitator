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
 * Last modified: 2014/03/24
 *
 ****************************************************************)
 
open Global
open Mpi
open Printf (* a terme : retirer tout ca *)
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

(* Here will come the slave's real work                                *)

(*let compute( input_data ) =
	(* don't do anything for the moment *)
	Unix.sleep 1;
	input_data
;;*)

(* Initialize a slave                                                  *)

let init_slave rank size =
	print_string( "I am slave " );
	print_int( rank );
	print_string( " in " );
	print_int( size-1 );
	print_newline(); 
;;


(* ** *** *** ***** *******       MASTER      ******* ***** *** *** ** *)
let receive_pull_request_and_store_constraint () =
	match receive_pull_request () with
	| PullOnly source_rank -> source_rank
	| OutOfBound source_rank ->
		(* FAIRE QUELQUE CHOSE POUR DIRE QU'UN POINT N'A PAS MARCHÉ *)
		source_rank
	| PullAndResult (source_rank , linear_constraint) -> 
		(* FAIRE QUELQUE CHOSE POUR METTRE LA CONTRAINTE EN MÉMOIRE *)
		source_rank


let master () = 
    let size = Mpi.comm_size Mpi.comm_world in

    print_string( "I am the maaaastah" );
    print_newline();

    let finished = ref false in
    let rc = ref 0 in

    while not !finished do
		print_message Debug_low ("Master: waiting for a pull request");
		(* Get the pull_request *)
		let source_rank = receive_pull_request_and_store_constraint () in
	
		print_message Debug_low ("Master: got a pull request from slave " ^ (string_of_int source_rank) ^ "");
	(* 	TODO: TROUVER LE POINT *)
		let mypi0 = [] in
			(*** CACHE ; si plus de point, alors finished := true *)
		
			send_pi0 mypi0 source_rank;
	

(*(*(*      (* Send some new data: send the size, then the data *)
      let (len, mydata) = get_data() in

      Mpi.send len src data_tag comm_world ;
      Mpi.send mydata src data_tag comm_world ;

      Printf.printf "MASTER - sent %d bytes of data %s to %d with tag %d" len mydata src data_tag ; 
      print_newline();

      (* Do I still have data to send? *)
      if !cnt > 20 then
        finished := true 
      ;*)*)*)
	
    done;

    print_string( "MASTER - done" );
    print_newline();

    (* I am done sending all my data. Receive the results of the last
       computations, and wrap up. *)
    
    let k = ref 0 in
    while !k < ( size - 1) do
		print_message Debug_low ("Master: waiting for a pull request");
		(* Get the pull_request *)
		let source_rank = receive_pull_request_and_store_constraint () in
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
    	
    print_string( "MASTER - slaves done" );
    print_newline();
;;
  
(* *** *** ***** *******      WORKER      ******* ***** *** *** *)

let worker () = 
	let rank = rank() in
	let size = size() in

	init_slave rank size;

(*     let n = rank in *)
    let finished = ref false in

    (* Start: ask for some work *)
    send_work_request();

    print_message Debug_low ("[" ^ (string_of_int rank) ^ "] sent pull request to the master");

	(* Get the model *)
	let model = Input.get_model() in

	(* In the meanwhile: compute the initial state *)
    let init_state = Reachability.get_initial_state_or_abort model in

    while not !finished do

        (* Receive some work: size of the input data *)
(*
        let ( w, _, tag ) =
	  Mpi.receive_status masterrank Mpi.any_tag Mpi.comm_world in

        if tag = data_tag then
	  begin
	    (* Receive the data itself *)
	    let buff = String.create w in
	    let work = ref buff in

	    work := Mpi.receive masterrank data_tag Mpi.comm_world; 
	    Printf.printf "recv %d bytes of work %s with tag %d" w !work tag ;
	    print_newline() ;*)

		match receive_work () with
		
		| Work pi0 -> 
            (* Do the job here *)
			Printf.printf "[%d] working now" rank ; print_newline();

			(* Set the new pi0 *)
			Input.set_pi0 pi0;
			(* Call IM *)
			let im_result , _ = Reachability.inverse_method_gen model init_state in

			
(* 			inverse_method_gen : abstract_model -> state -> returned_constraint * StateSpace.reachability_graph * tile_nature * bool * int * float *)

						(*** TODO: do something ***)


			(*** RESULTAT BIDON ***)
(* 			LinearConstraint.p_true_constraint() *)
(* 				im_result.result *)
(* 			in *)
		
			(* Send the result *)
			send_result im_result;
			print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " sent a constraint.");
			
		| Stop ->
			print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " says: done!");
			finished := true
    done;
	print_message Debug_low ("Worker " ^ (string_of_int rank) ^ " is done.");
;;
