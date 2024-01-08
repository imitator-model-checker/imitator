(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Dynamic elimination of clocks not used in the future [Andre13, FSFMA]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/27
 *
 ************************************************************)



 
(*------------------------------------------------------------*)
(** Function for preparing data structures for dynamic clock elimination *)
(*------------------------------------------------------------*)
val prepare_clocks_elimination : AbstractModel.abstract_model -> unit


(*------------------------------------------------------------*)
(** Eliminating useless clocks in a linear constraint *)
(*------------------------------------------------------------*)
val dynamic_clock_elimination : AbstractModel.abstract_model -> DiscreteState.global_location -> LinearConstraint.px_linear_constraint -> unit
