(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: root of the class hierarchy of algorithms. Only most basic functions are defined.
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open ImitatorUtilities


(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoGeneric : AbstractModel.abstract_model -> Options.imitator_options ->
	object

		(************************************************************)
		(* Class variables *)
		(************************************************************)
		
		(** Start time for the algorithm *)
		val mutable start_time : float

		
		(*------------------------------------------------------------*)
		(** Name of the algorithm (to be defined in subclasses) *)
		(*------------------------------------------------------------*)
		method virtual algorithm_name : string
		

		(************************************************************)
		(* Class methods *)
		(************************************************************)
		
		(** Write a message preceded by "[algorithm_name]" *)
		method print_algo_message : verbose_mode -> string -> unit
		
		(** Write a message preceded by "\n[algorithm_name]" *)
		method print_algo_message_newline : verbose_mode -> string -> unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Write an error message preceded by "[algorithm_name]" *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method print_algo_error : string -> unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Main method to run the algorithm: virtual method to be defined in subclasses *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method virtual run : Result.imitator_result
		
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
