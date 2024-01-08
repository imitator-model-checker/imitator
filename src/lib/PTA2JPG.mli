(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Conversion to dot
 *
 * File contributors : Étienne André et al.
 * Created           : 2012/08/24
 *
 ************************************************************)

open AbstractModel


(**************************************************)
(** model *)
(**************************************************)
(* Convert a pi0 into a string *)
(* val string_of_pi0 : abstract_model -> pi0 -> string *)

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

