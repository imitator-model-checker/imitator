(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Classical Behavioral Cartography with exhaustive coverage of integer points [AF10]. Distribution mode: master-worker with point-based distribution of points. [ACE14,ACN15]
 * 
 * File contributors : Étienne André
 * Created           : 2016/03/04
 * Last modified     : 2016/03/17
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoGeneric
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
class virtual algoBCCoverDistributedMSPointBased =
	object (self)
	inherit algoGeneric as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*** BADPROG: code shared with AlgoCartoGeneric ***)
	(* The function creating a new instance of the algorithm to call (typically IM or PRP). Initially None, to be updated immediatly after the object creation. *)
	(*** NOTE: this should be a parameter of the class; but cannot due to inheritance from AlgoGeneric ***)
	val mutable algo_instance_function = None
	
		
	
	(************************************************************)
	(* Class methods to simulate class parameters *)
	(************************************************************)
	
	(*** BADPROG: code shared with AlgoBCCoverDistributedMSPointBased ***)
	(* Sets the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method set_algo_instance_function (f : unit -> AlgoIMK.algoIMK) : unit =
		match algo_instance_function with
		| Some _ -> 
			raise (InternalError("algo_instance_function was already set in algoBCCoverDistributedMSPointBased."))
		| None ->
			algo_instance_function <- Some f
	
	(*** BADPROG: code shared with AlgoBCCoverDistributedMSPointBased ***)
	(* Get the function creating a new instance of the algorithm to call (typically IM or PRP) *)
	method get_algo_instance_function =
		match algo_instance_function with
		| Some f -> f
		| None ->
			raise (InternalError("algo_instance_function not yet set in algoBCCoverDistributedMSPointBased."))

			
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize_variables : unit


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generic algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual run : unit -> Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
