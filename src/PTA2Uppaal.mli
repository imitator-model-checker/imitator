(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Translater to Uppaal
 * 
 * File contributors : Étienne André
 * Created           : 2019/03/01
 * Last modified     : 2019/03/01
 *
 ************************************************************)
 

open AbstractModel


(* Convert a model into a string *)
val string_of_model : abstract_model -> string

(** Convert the correctness property to a string *)
val string_of_property : abstract_model -> property_definition -> string
