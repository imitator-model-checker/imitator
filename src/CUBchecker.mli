(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Checks whether a PTA is a CUB-PTA
 * 
 * File contributors : Étienne André
 * Created           : 2016/04/13
 *
 ************************************************************)

(*** NOTE/HACK: duplicate function in ModelConverter and StateSpace ***)
val continuous_part_of_guard : AbstractModel.guard -> LinearConstraint.pxd_linear_constraint

(** Takes an abstract model as input, and infers a constraint on the parameters (possibly false) under which this PTA is a CUB-PTA *)
val check_cub : AbstractModel.abstract_model -> LinearConstraint.p_linear_constraint

(** Takes an abstract model as input, and convert it into an equivalent CUB-PTA *)
val cubpta_of_pta : AbstractModel.abstract_model -> AbstractModel.abstract_model
