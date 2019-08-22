(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Description: linear terms and constraints on the parameters only
 *
 * Author:        Etienne Andre
 * 
 * Created:       2013/08/02
 * Last modified: 2013/08/02
 *
 ****************************************************************) 

(************************************************************)
(* TYPES *)
(************************************************************)


type variable = LinearConstraint.variable
type coef = LinearConstraint.coef

type linear_term = LinearConstraint.linear_term

type linear_inequality = LinearConstraint.linear_inequality

type linear_constraint = LinearConstraint.linear_constraint


(************************************************************)
(** {2 Linear terms} *)
(************************************************************)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

let make_linear_term = LinearConstraint.make_linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear term into a string *)	
let string_of_linear_term = LinearConstraint.string_of_linear_term



(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear constraint into a string *)
let string_of_linear_constraint = LinearConstraint.string_of_linear_constraint


	
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear constraint is pi0-compatible *)
let is_pi0_compatible = LinearConstraint.is_pi0_compatible


(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
let partition_pi0_compatible = LinearConstraint.partition_pi0_compatible

