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
 * Last modified     : 2019/10/16
 *
 ************************************************************)


open Options

val get_model: unit -> AbstractModel.abstract_model
val set_model: AbstractModel.abstract_model -> unit

val get_property: unit -> AbstractProperty.abstract_property
val set_property: AbstractProperty.abstract_property -> unit

(*val get_pi0: unit -> PVal.pval
val set_pi0: PVal.pval -> unit

val get_v0: unit -> v0
val set_v0: v0 -> unit*)

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

