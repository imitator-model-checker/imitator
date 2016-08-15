(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with random pi0 and n retries before switching to sequential mode. [ACE14]
 * Master algorithm
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/16
 * Last modified     : 2016/03/17
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open Exceptions
open ImitatorUtilities
open AlgoBCCover


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
class algoBCCoverDistributedMSRandomSeqMaster =
	object (self)
	inherit AlgoBCCoverDistributedMSPointBasedMaster.algoBCCoverDistributedMSPointBasedMaster as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Variable to be initialized *)
	val mutable max_tries : int option = None
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the maximum number of tries (must be done right after creating the algorithm object!) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_max_tries m =
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Setting max_tries to " ^ (string_of_int m ) ^ "");
		
		(* Set *)
		max_tries <- Some m
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Getting max_tries *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private get_max_tries : int =
		match max_tries with
			| Some m -> m
			| None -> raise (InternalError ("In algoBCCoverDistributedMSRandomSeqMaster.get_max_tries, the number of maximum tries should have been already initialized."))
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** NOTE: raises an exception due to access to get_max_tries before setting max_tries ***)
(* 	method algorithm_name = "BC (full cov) distr MS random(" ^ (string_of_int self#get_max_tries) ^ ")+seq MASTER" *)
	method algorithm_name = "BC (full cov) distr MS random+seq MASTER"

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Return a new instance of the underlying cartography algorithm (typically BCrandom or BCcover) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method bc_instance =
		(* Print some information *)
		self#print_algo_message Verbose_standard ("Creating BC instance...");
		
		let algo_bcrandom = new AlgoBCRandomSeq.algoBCRandomSeq in
		
		(*** NOTE: very important: must set NOW the maximum number of tries! ***)
		algo_bcrandom#set_max_tries (self#get_max_tries);
		algo_bcrandom#set_tiles_manager_type (self#get_tiles_manager_type);

		let myalgo :> AlgoCartoGeneric.algoCartoGeneric = algo_bcrandom in
		myalgo
		
		

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
