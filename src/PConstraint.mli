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
(** {2 Variables and coefficients} *)
(************************************************************)

type variable = int
type coef = NumConst.t


(************************************************************)
(** {2 Linear terms} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(* NOTE: no other choice than defining it explicitely *)
type linear_term = LinearConstraint.linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear term using a list of coef and variables, and a constant *)
val make_linear_term : (coef * variable) list -> coef -> linear_term



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear term into a string *)
val string_of_linear_term : (variable -> string) -> linear_term -> string


(************************************************************)
(** {2 Linear inequalities} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(*type op =
	| Op_g
	| Op_ge
	| Op_eq*)

type linear_inequality




(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type linear_constraint
 

 
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Pi0-compatibility} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a linear constraint is pi0-compatible *)
val is_pi0_compatible : (variable -> coef) -> linear_constraint -> bool

(** Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
val partition_pi0_compatible : (variable -> coef) -> linear_constraint -> (linear_inequality list * linear_inequality list)


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear constraint into a string *)
val string_of_linear_constraint : (variable -> string) -> linear_constraint -> string
