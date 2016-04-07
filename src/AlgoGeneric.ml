(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: root of the class hierarchy of algorithms. Only most basic functions are defined.
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/19
 * Last modified     : 2016/03/30
 *
 ************************************************************)


(**************************************************************)
(* Modules *)
(**************************************************************)
open ImitatorUtilities


(**************************************************************)
(* Class definition *)
(**************************************************************)
class virtual algoGeneric =
	object (self)

	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Start time for the algorithm *)
	val mutable start_time = 0.

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Virtual method: the algorithm name is not defined for BFS as it is not supposed to be called *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual algorithm_name : string
	

	(************************************************************)
	(* Class methods *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Write a message preceeded by "[algorithm_name]" *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method print_algo_message verbose_mode message =
		print_message verbose_mode ("  [" ^ self#algorithm_name ^ "] " ^ message)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Write an error message preceeded by "[algorithm_name]" *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method print_algo_error message =
		print_error ("  [" ^ self#algorithm_name ^ "] " ^ message)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Write a message preceeded by "\n[algorithm_name]" *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method print_algo_message_newline verbose_mode message =
		print_message verbose_mode ("\n  [" ^ self#algorithm_name ^ "] " ^ message)
	
	
	(* Variable initialization (to be defined in subclasses) *)
	method virtual initialize_variables : unit
	
	(* Main method to run the algorithm: virtual method to be defined in subclasses *)
	method virtual run : unit -> Result.imitator_result
	

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
