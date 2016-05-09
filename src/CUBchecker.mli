(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Checks whether a PTA is a CUB-PTA
 * 
 * File contributors : Étienne André
 * Created           : 2016/04/13
 * Last modified     : 2016/04/13
 *
 ************************************************************)


(** Takes an abstract model as input, and infers a constraint on the parameters (possibly false) under which this PTA is a CUB-PTA *)
val check_cub : AbstractModel.abstract_model -> LinearConstraint.p_linear_constraint