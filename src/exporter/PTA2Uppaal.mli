(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Translater to Uppaal
 * 
 * File contributors : Étienne André
 * Created           : 2019/03/01
 *
 ************************************************************)
 
(** Convert a model into a string in the Uppaal XML format *)
val string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string
