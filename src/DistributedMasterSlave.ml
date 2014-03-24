(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/03/24
 * Last modified: 2014/03/24
 *
 ****************************************************************)
 
open Mpi
open Printf (* a terme : retirer tout ca *)
open Unix (* temporaire : necessaire pour sleep *)
open Marshal
open DistributedUtilities
	

(* Declaration of the tags we are going to use in 
   communications within the M/W pattern *)

let data_tag = 1      (* used when we are sending input data           *)
let finished_tag = 2  (* used to mean that work is done                *)
let result_tag = 3    (* used when we are sending computation results  *)
let work_tag = 4      (* used to ask for some work                     *)

(* tmp *)
let cnt = ref 0

(* ** *** *** ***** *******       UTILS       ******* ***** *** *** ** *)

let serialize( data ) =
  Marshal.to_string( data )
;;

(* Store the result of a computation.                                  *)

let store result source =
	print_string "MASTER - recv result" ; print_string result;
	print_string " from ";
	print_int source ;
	print_newline();
	0
;;

(* Returns the next set of input data                                  *)
(* This is where the master's intelligence will come.                  *)
(*                                                                     *)
(* Returns a couple: the first element is the size of the data, the    *)
(* second one is the data itself. The data is serialized and ready to  *)
(* be sent.                                                            *)

let get_data () =
	let size = 1 + ( Random.int 25 ) in
	let buff = String.create size in
	let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
	let len = String.length alphanum in
	for i = 0 to pred size do
		buff.[i] <- alphanum.[Random.int len]
	done;
	cnt := !cnt + 1;
	(size, buff)
;;

(* Here will come the slave's real work                                *)

let compute( input_data ) =
	(* don't do anything for the moment *)
	Unix.sleep 1;
	input_data
;;

(* Initialize a slave                                                  *)

let init_slave rank size =
	print_string( "I am slave " );
	print_int( rank );
	print_string( " in " );
	print_int( size-1 );
	print_newline(); 
;;


(* ** *** *** ***** *******       MASTER      ******* ***** *** *** ** *)

let master () = 
    let size = Mpi.comm_size Mpi.comm_world in

    print_string( "I am the maaaastah" );
    print_newline();

    let finished = ref false in
    let rc = ref 0 in

    while not !finished do
      
      (* First receive the length of the data we are about to receive *)
      let (len, src, tag) = 
	Mpi.receive_status Mpi.any_source Mpi.any_tag Mpi.comm_world in

      (* Is this a result or a simple pull ? *)     

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
	  print_newline()
	end
      else 
	begin
	  (* This is a simple pull - no result coming *)
	  print_string(  "MASTER - Recv pull request from " ) ; 
	  print_int( src );
	  print_newline()
	end
      ;
      
      (* Send some new data: send the size, then the data *)
      let (len, mydata) = get_data() in

      Mpi.send len src data_tag comm_world ;
      Mpi.send mydata src data_tag comm_world ;

      Printf.printf "MASTER - sent %d bytes of data %s to %d with tag %d" len mydata src data_tag ; 
      print_newline();

      (* Do I still have data to send? *)
      if !cnt > 20 then
        finished := true 
      ;
	
    done;

    print_string( "MASTER - done" );
    print_newline();

    (* I am done sending all my data. Receive the results of the last
       computations, and wrap up. *)
    
    let k = ref 0 in
    while !k < ( size - 1) do
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
	  print_newline();
	  k := ( !k + 1 );
	  Mpi.send k src finished_tag comm_world ;
	end
    done;
    	
    print_string( "MASTER - slaves done" );
    print_newline();
;;
  
(* *** *** ***** *******      WORKER      ******* ***** *** *** *)

let worker () = 
    let size = Mpi.comm_size Mpi.comm_world in
    let rank = Mpi.comm_rank Mpi.comm_world in

    init_slave rank size;

    let n = rank in
    let finished = ref false in

    (* Start: ask for some work *)

    Mpi.send n masterrank work_tag Mpi.comm_world ;

    Printf.printf "%d] sent pull request to the master" rank ;
    print_newline(); 

    while not !finished do

        (* Receive some work: size of the input data *)

        let ( w, _, tag ) =
	  Mpi.receive_status masterrank Mpi.any_tag Mpi.comm_world in

        if tag = data_tag then
	  begin
	    (* Receive the data itself *)
	    let buff = String.create w in
	    let work = ref buff in

	    work := Mpi.receive masterrank data_tag Mpi.comm_world; 
	    Printf.printf "recv %d bytes of work %s with tag %d" w !work tag ;
	    print_newline() ;

            (* Do the job here *)

            Printf.printf "[%d] working now" rank ; print_newline();
	    let result = compute !work in
	    let res_size = String.length result in
	    
	    (* Send the result: 1st send the data size, then the data *)

	    Mpi.send res_size masterrank result_tag Mpi.comm_world;
	    Mpi.send result masterrank result_tag Mpi.comm_world ;
            Printf.printf "[%d] result %s sent" rank result ; 
	    print_newline();

	  end
        else
	  begin
            (* Done - exit now *)
            Printf.printf "[%d] done now - EXITING" rank ; 
	    print_newline();
            finished := true 
	  end
        ;
    done 
;;
