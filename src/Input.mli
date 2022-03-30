(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Global input elements (model, property)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 *
 ************************************************************)


open Options

val set_model: AbstractModel.abstract_model -> unit
val get_model: unit -> AbstractModel.abstract_model

val set_property: AbstractProperty.abstract_property -> unit
val has_property: unit -> bool
val get_property: unit -> AbstractProperty.abstract_property

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

