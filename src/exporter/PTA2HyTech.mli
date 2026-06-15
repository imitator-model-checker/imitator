(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Convert an input IMITATOR file to a file readable by HyTech
 * 
 * Remark            : extensively copied from ModelPrinter as IMITATOR and HyTech syntax are very similar
 * 
 * File contributors : Étienne André
 * Created           : 2016/01/26
 *
 ************************************************************)



(* Convert a model into a string in the HyTech format *)
val string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string
