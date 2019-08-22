(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Description: linear terms and constraints on the parameters, clocks and discrete
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

type linear_term


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear term into a string *)
val string_of_linear_term : (variable -> string) -> linear_term -> string


(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Type} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
type linear_constraint

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a true constraint *)
val true_constraint : unit -> linear_constraint


(** "linear_constraint_of_clock_and_parameters x ~ d neg" will create a linear_constraint x ~ d, with x a clock, d a PConstraint.linear_term, and "neg" indicates whether x and d should be kept in this direction or reversed (viz., "x < p1 true" generates "x < p1" whereas "x <= p1+p2 false" generates "p1+p2 <= x" *)
val linear_constraint_of_clock_and_parameters : variable -> LinearConstraint.op -> PConstraint.linear_term -> bool -> linear_constraint



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Tests} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Check if a constraint is false *)
(* val is_false : linear_constraint -> bool *)

(** Check if a constraint is true *)
val is_true : linear_constraint -> bool



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear constraint into a string *)
val string_of_linear_constraint : (variable -> string) -> linear_constraint -> string



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to GrML} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** Convert a linear term into a string for GML *)
val grml_of_linear_term : (variable -> string) -> int -> linear_term -> string

(** Convert a linear constraint into a string for GML *)
val grml_of_linear_constraint : (variable -> string) -> int -> linear_constraint -> string


