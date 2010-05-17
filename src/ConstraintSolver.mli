(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/17
 * Last modified: 2010/02/19
 *
 ****************************************************************)


open AbstractImitatorFile
open Constraint

(****************************************************************)
(** Real constraints functions *)
(****************************************************************)

(* Negate an inequality; for an equality, perform the pi0-compatible negation *)
val negate_inequality : pi0 -> linear_inequality -> linear_inequality

(* 'rename_variables program renaming_function c' renames all variable according to the renaming_function *)
val rename_variables : abstract_program -> (variable_index -> variable_index) -> linear_constraint -> linear_constraint


(* 'add_d program is_selected c' adds a constant 'd' to any variable variable_index s.t. is_selected variable_index = true *)
val add_d : abstract_program -> (variable_index -> bool) -> linear_constraint -> linear_constraint


(* 'sub_d program is_selected c' substracts a constant 'd' to any variable variable_index s.t. is_selected variable_index = true *)
val sub_d : abstract_program -> (variable_index -> bool) -> linear_constraint -> linear_constraint


(* Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
val partition_pi0_compatible : pi0 -> linear_constraint -> (linear_inequality list * linear_inequality list)


(* Check if a linear constraint is pi0-compatible *)
val is_pi0_compatible : pi0 -> linear_constraint -> bool


(* Perform the intersection of a list of constraints *)
val and_constraints : abstract_program -> linear_constraint list -> linear_constraint


(* Hide variables in a constraint *)
val hide_variables : abstract_program -> variable_index list -> linear_constraint -> linear_constraint


(* Hide non-parameters in a constraint *)
val hide_non_parameters : abstract_program -> linear_constraint -> linear_constraint


(* Check if a constraint is satisfiable, i.e., has valuations *)
val is_satisfiable : abstract_program -> linear_constraint -> bool

(* Check if 2 constraints are equal *)
val is_equal : abstract_program -> linear_constraint -> linear_constraint -> bool

(* Check if a constraint is included or equal to another *)
val is_le : abstract_program -> linear_constraint -> linear_constraint -> bool
