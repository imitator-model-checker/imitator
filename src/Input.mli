(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Global input elements (model, pi0, v0)
 * 
 * File contributors : Étienne André
 * Created           : 2012/06/15
 * Last modified     : 2016/08/04
 *
 ************************************************************)


open AbstractModel
open Options

val get_model: unit -> abstract_model
val set_model: abstract_model -> unit

val get_pi0: unit -> pi0
val set_pi0: pi0 -> unit

val get_v0: unit -> v0
val set_v0: v0 -> unit

val get_options: unit -> imitator_options
val set_options: imitator_options -> unit

