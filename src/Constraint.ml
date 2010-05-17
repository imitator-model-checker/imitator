(***************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/11/24
 * Last modified: 2010/03/04
 *
 **************************************************)



(**************************************************)
(** Modules *)
(**************************************************)
open Global
open AbstractImitatorFile


(**************************************************)
(** Exception *)
(**************************************************)
exception Undefined_variable of int


(**************************************************)
(** Constants *)
(**************************************************)
let zero = NumConst.zero
let one = NumConst.one
let minus_one = NumConst.numconst_of_int (-1)


(**************************************************)
(** Type definition *)
(**************************************************)


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


(**************************************************)
(** Name of variables *)
(**************************************************)
type variable_names =
	(* No variable names *)
	| No_names
	(* Array variable_index -> variable_name *)
	| Names of variable_name array


(**************************************************)
(** Operation on linear terms *)
(**************************************************)

(** Perform lt1 + lt2 *)
let add_linear_terms (members1, const1) (members2, const2) =
	(* TO OPTIMIZE *)
	list_append members1 members2,
	NumConst.add const1 const2


(** Perform lt1 - lt2 *)
let substract_linear_terms (members1, const1) (members2, const2) =
	(* TO OPTIMIZE *)
	let minus_members2 = List.map (fun (coef, var) -> (NumConst.neg  coef), var) members2 in
	let minus_const2 = NumConst.neg const2 in
	add_linear_terms (members1, const1) (minus_members2, minus_const2)



(**************************************************)
(** String functions *)
(**************************************************)

(* Convert various constants into a string *)
let string_of_zero = "0"
let string_of_true = "True"
let string_of_false = "False"


(* Convert a 'coef' into a string *)
let string_of_coef = NumConst.string_of_numconst


(* Convert a 'constant' into a string *)
let string_of_constant = NumConst.string_of_numconst


(* Convert an 'op' into a string *)
let string_of_op = function
	| Op_g -> ">"
	| Op_ge -> ">="
	| Op_eq -> "="


(* Convert a variable into a string *)
let string_of_variable variable_names variable =
	match variable_names with
	| No_names -> "var" ^ (string_of_int variable)
	| Names variable_names ->
		try variable_names.(variable)
		with Invalid_argument message -> (
			print_error ("InvalidArgument: " ^ message);
			raise (Undefined_variable variable)
		)


(* Convert a member into a string *)
let string_of_member variable_names (coef, variable) =
	(* Case coef 1 *)
	if NumConst.equal coef one then (string_of_variable variable_names variable)
	(* Case coef -1 *)
	else if NumConst.neq coef minus_one then ("- " ^ (string_of_variable variable_names variable))
	(* Other case *)
	else ((string_of_coef coef) ^ " * " ^ (string_of_variable variable_names variable))


(* Convert a linear term into a string *)
let string_of_term variable_names (list_of_members, constant) =
	(**** TO DO: remove the members whose coef is equal to 0 (not so sure...) ****)
	match list_of_members with
	(* Case: empty list *)
	| [] -> string_of_constant constant
	(* Case: non-empty list *)
	| first :: rest ->
		(* Convert the first *)
		(string_of_member variable_names first)
		(* Convert the rest *)
		^ (List.fold_left (fun the_string (coef, variable) -> 
			the_string
			(* Add the +/- *)
			 ^ (if NumConst.g coef zero then " + " else " ")
			(* Convert the member *)
			 ^ (string_of_member variable_names (coef, variable))
		) "" rest)
		(* Convert the constant *)
		^ (if NumConst.neq constant zero then(
			(* Add the +/- *)
			 (if NumConst.g constant zero then " + " else " ")
			^ (string_of_constant constant)
			) else ""
		)


(* Convert an inequality into a string *)
let string_of_inequality_unique variable_names (linear_term, op) =
	(* Keep only positive terms on both sides of the equality for esthetic reasons *)
	let list_of_members, constant = linear_term in
	(* Partition the positive and negative members *)
	let positive_members, negative_members =
		List.partition (fun (coef, variable) -> NumConst.g coef zero) list_of_members in
	(* Invert the negatives *)
	let inverted_negative_members = List.map (fun (coef, variable) -> (NumConst.neg coef, variable)) negative_members in
	(* Create the constant *)
	let left_constant, right_constant =
		if NumConst.g constant zero then constant, zero else zero, NumConst.neg constant in
	(* Left *)
	(string_of_term variable_names (positive_members, left_constant))
	(* op *)
	^ " " ^ (string_of_op op) ^ " "
	(* Right *)
	^ (string_of_term variable_names (inverted_negative_members, right_constant))


(* Convert an inequality into a string (with variable names) *)
let string_of_inequality variable_names =
	string_of_inequality_unique (Names variable_names)


(* Convert a constraint into a string *)
let string_of_constraint_unique variable_names = function
	(* Case false *)
	| LC_false -> string_of_false
	(* Other case: *)
	| LC_maybe linear_constraint -> begin match linear_constraint with 
		(* Case empty list: true *)
		| [] -> string_of_true
		(* Case non-empty list: iterate *)
		| first :: rest -> "  " ^ (
			string_of_list_of_string_with_sep "\n& "
			(List.map (fun inequality -> string_of_inequality_unique variable_names inequality) (first :: rest))
			)
		end

(* Convert a 'linear_term' into a string  (with variable names) *)
let string_of_linear_term variable_names = string_of_term (Names variable_names)

(* Convert a constraint into a string *)
let string_of_constraint_debug =
	string_of_constraint_unique No_names

(* Convert a constraint into a string (with variable names) *)
let string_of_constraint variable_names =
	string_of_constraint_unique (Names variable_names)
