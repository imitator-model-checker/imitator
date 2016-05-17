(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Timed counter, that depends on a level of verbosity (the counter will only update when the global verbose_mode is larger or equal to that of the counter). Also maintains a list of all counters, for obtaining statistics easily.
 * 
 * File contributors : Étienne André
 * Created           : 2014/04/27
 * Fork from         : Counter.mli
 * Fork date         : 2016/05/17
 * Last modified     : 2016/05/17
 *
 ************************************************************)

(************************************************************)
(************************************************************)
(* Type *)
(************************************************************)
(************************************************************)

type counterType =
	(** Algorithm functions *)
	| Algorithm_counter
	
	(** The global counter *)
	(*** TODO: prevent more than one such counter to be created ***)
	| Global_counter
	
	(** Everything related to graphics generation *)
	| Graphics_counter
	
	(** Lexing and parsing *)
	| Parsing_counter

	(** All calls to PPL *)
	| PPL_counter



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class timeCounter : string -> counterType -> ImitatorUtilities.verbose_mode ->
	object
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Start the counter *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method start : unit
			
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Stop the counter *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method stop : unit
			
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Reset the counter *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method reset : unit

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Get the counter's value *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method value : float
(************************************************************)
(************************************************************)
end
(************************************************************)
(************************************************************)


(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(** Shortcut: create new counter and register it *)
val create_and_register : string -> counterType -> ImitatorUtilities.verbose_mode -> timeCounter

(** Register a counter *)
val register : timeCounter -> unit

