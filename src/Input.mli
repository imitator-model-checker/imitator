(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Global input elements (model, pi0, v0)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 * Last modified     : 2019/08/22
 *
 ************************************************************)


open AbstractModel
open Options

val get_model: unit -> abstract_model
val set_model: abstract_model -> unit

val get_pi0: unit -> PVal.pval
val set_pi0: PVal.pval -> unit

val get_v0: unit -> v0
val set_v0: v0 -> unit

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

