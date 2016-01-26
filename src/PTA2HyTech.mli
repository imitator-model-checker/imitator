(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Convert an input IMITATOR file to a file readable by HyTech
 * 
 * Remark            : extensively copied from ModelPrinter as IMITATOR and HyTech syntax are very similar
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/26
 * Last modified     : 2016/01/26
 *
 ************************************************************)


open AbstractModel



(**************************************************)
(** model *)
(**************************************************)
(* Convert a model into a string *)
val string_of_model : abstract_model -> string
