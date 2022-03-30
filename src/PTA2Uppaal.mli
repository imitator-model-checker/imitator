(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Translater to Uppaal
 * 
 * File contributors : Étienne André
 * Created           : 2019/03/01
 *
 ************************************************************)
 

open AbstractModel


(* Convert a model into a string *)
val string_of_model : abstract_model -> string
