(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/03/15
 * Last modified: 2014/06/19
 *
 ****************************************************************)

open Global

(* ocamlc -I +ocamlmpi -o masterslave mpi.cma masterslave.ml           *)
(* ocamlc -I +ocamlmpi -o masterslave mpi.cma unix.cma masterslave.ml   *)
(* mpiexec -n 4 ./masterslave                                          *)

(* Si j'ai bien compris :                                              *)
(* send variable rank tag communicateur                                *)
(* res = receive source tag communicateur                              *)
(* res, source, tag = receive_status source tag communicateur          *)
(* wildcards : any_source any_tag                                      *)


(* Notes :
   - il y a un probleme avec les affichages quand je ne mets pas un  
     print_newline(); après !
 *)

open Mpi
open DistributedUtilities
open Marshal
open Options

	

(* * *** *** ***** *******    MAIN FUNCTION    ******* ***** *** *** * *)
let run () =
        let options = Input.get_options () in
(*	(* Retrieve the input options *)
	let options = Input.get_options () in
	(* Get the model *)
	let model = Input.get_model() in*)

	let rank = Mpi.comm_rank Mpi.comm_world in

(*	(* Print some information *)
	if rank = 0 then
	  print_message Debug_standard ("Hi guys! This is PaTATOR speaking!");*)
	
	
 	(**************************************************)
	(* Starting here *)
	(**************************************************)
(* 	Random.init 0; (* tmp pour tests *) *)

	let code =
	(* Fork between the algorithms *)
	  match options#distribution_mode with
	    | Distributed_unsupervised ->
		(* Fork between master and slave *)
		if rank = masterrank
		then DistributedUnsupervised.coordinator
		else DistributedUnsupervised.worker
	    | _ ->
		(* Fork between master and slave *)
		if rank = masterrank
		then DistributedMasterSlave.master
		else DistributedMasterSlave.worker
	in
	  code ();
  
	  

	(* At the end: synchronization barrier *)
	barrier Mpi.comm_world;

	
 	(**************************************************)
	(* Bye bye! *)
	(**************************************************)
	terminate_program()

 
