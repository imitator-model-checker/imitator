(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/11/11
 * Last modified: 2010/03/04
 *
 ****************************************************************)



(****************************************************************)
(** Exception *)
(****************************************************************)
exception Undefined_variable of int


(****************************************************************)
(** Constants *)
(****************************************************************)
val zero : NumConst.t
val one : NumConst.t
val minus_one : NumConst.t

(****************************************************************)
(** Type definition *)
(****************************************************************)

type variable_name = string
type variable = int
type coef = NumConst.t
type constant = NumConst.t

type member = coef * variable

type op =
	| Op_g
	| Op_ge
	| Op_eq

(** Linear term: list of members, and a constant *)
type linear_term = member list * constant

(** Linear inequality: linear term and operator *)
type linear_inequality = linear_term * op

(** Linear constraint: false or maybe *)
type linear_constraint =
	(* False *)
	| LC_false
	(* Maybe *)
	| LC_maybe of linear_inequality list


(****************************************************************)
(** Operation on linear terms *)
(****************************************************************)

(** Perform lt1 + lt2 *)
val add_linear_terms : linear_term -> linear_term -> linear_term

(** Perform lt1 - lt2 *)
val substract_linear_terms : linear_term -> linear_term -> linear_term


(****************************************************************)
(** Functions of string *)
(****************************************************************)

(* Convert a 'linear_term' into a string  (with variable names) *)
val string_of_linear_term : variable_name array -> linear_term -> string

(* Convert an inequality into a string (with variable names) *)
val string_of_inequality : variable_name array -> linear_inequality -> string

(* Convert a constraint into a string *)
val string_of_constraint_debug : linear_constraint -> string

(* Convert a constraint to a string  (with variable names) *)
val string_of_constraint : variable_name array -> linear_constraint -> string

