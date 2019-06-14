(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Parsing functions for input elements
 * 
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2014/03/15
 * Last modified     : 2016/10/10
 *
 ************************************************************)

(** Compile the concrete model and convert it into an abstract model *)
val compile_model: Options.imitator_options -> bool -> AbstractModel.abstract_model

(** Parse the pi0 file and convert it into an abstract representation *)
val compile_pi0: Options.imitator_options -> PVal.pval

(** Parse the v0 file and convert it into an abstract representation *)
val compile_v0: Options.imitator_options -> HyperRectangle.hyper_rectangle
