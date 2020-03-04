(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert an abstract model to the input syntax of PHAVerLite
 *
 * File contributors : Étienne André
 * Created           : 2020/03/04
 * Last modified     : 2020/03/04
 *
 ************************************************************)


open AbstractModel

(************************************************************)
(** Model and property *)
(************************************************************)

(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string

