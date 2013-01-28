(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structur into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/09
 * Last modified: 2012/06/15
 *
 ****************************************************************)

 
(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open Options
open AbstractModel


(****************************************************************)
(** Exceptions *)
(****************************************************************)

(* When checking pi0 *)
exception InvalidPi0



(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the parsing structure into an abstract program *)
val abstract_program_of_parsing_structure : ParsingStructure.parsing_structure ->  ParsingStructure.pi0 -> ParsingStructure.v0 ->  imitator_options -> (abstract_program * pi0 * v0)
