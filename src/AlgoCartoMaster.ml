

***** THIS FILE IS CURRENTLY NOT USED *****
(this comment should prevent compiling)


(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Virtual class for distributed master-slave algorithms, on the master side
 * 
 * File contributors : Étienne André
 * Created           : 2016/02/05
 * Last modified     : 2016/03/04
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open ImitatorUtilities
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
class algoMaster =
	object (self)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Number of workers; just a shortcut *)
	val nb_workers = get_nb_nodes()
	
	(* Number of workers who finished their duty; important in particular at the end of the algorithm *)
	val mutable workers_done = 0

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	
	method private print_algo_message verbose_level message =
		print_message verbose_level ("  [master] " ^ message)
	
	
	method run_as_master : Result.imitator_result =
		(* Retrieve the input options *)
		let options = Input.get_options () in
		
		(* Initialize counters *)
		(*** TODO ***)
(*		counter_master_find_nextpi0#init;
		counter_master_waiting#init;*)
		
		self#print_algo_message Verbose_medium ("Hello world!");
		
		(* Perform initialization *)
		(*** TODO: probably nothing ***)
(* 		Cartography.bc_initialize (); *)
		
		let more_pi0 = ref true in
		let limit_reached = ref false in
		
		(* To differentiate between initialization of pi0 / next_point *)
		let first_point = ref true in
		
		(* For the end of the algorithm *)
		workers_done <- 0;
		
		raise (InternalError "not implemented")


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
