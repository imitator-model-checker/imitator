(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Global input elements (model, property)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 *
 ************************************************************)


open Options

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

