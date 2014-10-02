(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structur into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/09
 * Last modified: 2014/10/02
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
(* Convert the parsing structure into an abstract model *)

(*** WARNING: should better return a structure PI0 OR v0 ***)

val abstract_model_of_parsing_structure : ParsingStructure.parsing_structure ->  ParsingStructure.pi0 -> ParsingStructure.v0 ->  imitator_options -> (abstract_model * pi0 * v0)
