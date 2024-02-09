(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Convert an IMITATOR model to a dot file
 *
 * File contributors : Étienne André et al.
 * Created           : 2012/08/24
 *
 ************************************************************)


(** Convert a model into a string to be then converted into graphics using dot *)
val string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string

