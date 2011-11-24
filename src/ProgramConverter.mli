(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structur into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/09
 * Last modified: 2011/11/24
 *
 ****************************************************************)

(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open Options
open AbstractImitatorFile

(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidPi0

(****************************************************************)
(** Functions *)
(****************************************************************)
(* Convert the parsing structure into an abstract program *)
val abstract_program_of_parsing_structure : ParsingStructure.parsing_structure ->  ParsingStructure.pi0 -> ParsingStructure.pi0cube ->  imitator_options -> (abstract_program * pi0 * pi0cube)
