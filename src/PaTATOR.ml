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
 * Last modified: 2015/04/03
 *
 ****************************************************************)

 
open Mpi
open ImitatorUtilities
open Options
open DistributedUtilities

	

(* * *** *** ***** *******    MAIN FUNCTION    ******* ***** *** *** * *)
let run () =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	(* Retrieve MPI rank *)
	let rank = get_rank() in

	(* Print some information *)
	if rank = masterrank then
	  print_message Verbose_standard ("Hi guys! This is PaTATOR speaking!");
	
	
 	(**************************************************)
	(* Starting here *)
	(**************************************************)

	let code =
	(* Fork between the algorithms *)
	  match options#distribution_mode with
	    
	    | Distributed_unsupervised ->
		(* Fork between worker and coordinator *)
		if rank = masterrank
		then DistributedUnsupervised.coordinator
		else DistributedUnsupervised.worker
	    
	    | Distributed_unsupervised_multi_threaded ->
		(* Fork between worker and coordinator *)
		if rank = masterrank
		then DistributedMultiThreadedUnsupervised.coordinator
		else DistributedMultiThreadedUnsupervised.worker
	    
	    | Distributed_ms_subpart ->
		(* Fork between worker and coordinator *)
		if rank = masterrank
		then DistributedMasterSlaveSubparts.master
		else DistributedMasterSlaveSubparts.worker
		
	    | Distributed_static ->
		(* No fork for now *)
		DistributedMasterSlaveSubparts.collaborator
		
	    (* Other master slave scheme *)
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

 
