(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structur into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/09
 * Last modified: 2010/03/29
 *
 ****************************************************************)

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open AbstractImitatorFile

(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram
exception InvalidPi0

(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the parsing structure into an abstract program *)
val abstract_program_of_parsing_structure : 
	ParsingStructure.parsing_structure ->  ParsingStructure.pi0 -> ParsingStructure.pi0cube -> 
		acyclic:bool -> 
		sync_auto_detection:bool ->
		inclusion_mode:bool ->
		union_mode:bool -> 
		no_random:bool ->
		with_parametric_log:bool -> 
		imitator_mode -> string -> (abstract_program * pi0 * pi0cube)
