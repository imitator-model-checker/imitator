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
 * Fork from         : Counter.ml
 * Fork date         : 2016/05/17
 * Last modified     : 2016/05/17
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open ImitatorUtilities


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
class timeCounter (name : string) (counter_type : counterType) (level : ImitatorUtilities.verbose_mode) =
	object(self)

	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Counter attributes *)
	val name = name
	val counter_type = counter_type
	val level = level

	(* Current value *)
	val mutable value = ref 0.0
	(* Latest start time *)
	val mutable start_time = ref 0.0
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Start the counter *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method start =
		if verbose_mode_greater level then(
			start_time := Unix.gettimeofday()
		)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Stop the counter *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method stop =
		if verbose_mode_greater level then(
			value := !value +. Unix.gettimeofday() -. !start_time
		)
			
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Reset the counter *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method reset =
		if verbose_mode_greater level then(
			value := 0.0
		)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(** Get the counter's value *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method value =
		!value

	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)



(************************************************************)
(************************************************************)
(* Class-independent functions and variables *)
(************************************************************)
(************************************************************)

(* Global variable listing all counters (useful to get all statistics at once) *)
let all_counters = ref []


(* Register a counter *)
let register (counter : timeCounter) =
	all_counters := counter :: !all_counters;
	()

(** Shortcut: create new counter and register it *)
let create_and_register (name : string) (counter_type : counterType) (level : ImitatorUtilities.verbose_mode) =
	let my_new_counter = new timeCounter name counter_type level in
	register my_new_counter;
	my_new_counter

