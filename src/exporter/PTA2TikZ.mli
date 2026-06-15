(*****************************************************************
 *
 *                       IMITATOR
 *
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Author:        Étienne André
 *
 * File contributors : Étienne André
 * Created           : 2015/03/24
 *
 ****************************************************************)


(** Convert a model into a string in the LaTeX TikZ format *)
val tikz_string_of_model : Options.imitator_options -> AbstractModel.abstract_model -> string
