(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Translater to JaniSpec
 *
 * File contributors : Dylan Marinho
 * Created           : 2021/02/23
 *
 ************************************************************)


(** Convert a model into a string in the Jani format *)
val string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string
