(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 *
 * Description: linear terms and constraints on the parameters and clocks only
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

(* type linear_term = LinearConstraint.linear_term *)

type linear_constraint = LinearConstraint.linear_constraint



(************************************************************)
(** {2 Linear terms} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** Create a linear term from its list of members and its constant coefficient *)
let make_linear_term = LinearConstraint.make_linear_term

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Add two linear terms *)
(* let add_linear_terms = LinearConstraint.add_linear_terms *)

(** Substract two linear terms *)
let sub_linear_terms_p_px = LinearConstraint.sub_linear_terms
let sub_linear_terms_px_p = LinearConstraint.sub_linear_terms


(************************************************************)
(** {2 Linear inequalities} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(* Functions *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear inequality using a linear term and an operator *)
let make_linear_inequality = LinearConstraint.make_linear_inequality


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Functions} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)



(************************************************************)
(** {2 Linear Constraints} *)
(************************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Creation} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Create a linear constraint from a list of linear inequalities *)
let make = LinearConstraint.make


(** "linear_constraint_of_clock_and_parameters x ~ d neg" will create a linear_constraint x ~ d, with "x" a clock, "~" in {>, >=, =}, "d" a PConstraint.linear_term, and "neg" indicates whether x and d should be kept in this direction or reversed (viz., "x > p1 true" generates "x > p1" whereas "x >= p1+p2 false" generates "p1+p2 >= x" *)
let linear_constraint_of_clock_and_parameters (x : variable) (op : LinearConstraint.op) (d : PConstraint.linear_term) (direction : bool) =
	(* Create a linear term made of x *)
	let lt_x = make_linear_term [NumConst.one, x] NumConst.zero in
	(* Handle order *)
	let lt =
		if direction
			then sub_linear_terms_p_px d lt_x
		else sub_linear_terms_px_p lt_x d
	in
	(* Create the constraint with the operator *)
	make [LinearConstraint.make_linear_inequality lt op]



(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(** {3 Conversion to string} *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(** Convert a linear constraint into a string *)
let string_of_linear_constraint = LinearConstraint.string_of_linear_constraint
