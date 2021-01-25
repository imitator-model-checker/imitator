open DiscreteExpressions
open OCamlUtilities
open Constants

(* Non linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression
(*type nonlinear_constraint = nonlinear_inequality list*)
type nonlinear_constraint =
  | True_nonlinear_constraint
  | False_nonlinear_constraint
  | Nonlinear_constraint of nonlinear_inequality list

(************************************************************)
(** Check whether a non linear constraint evaluates to true when valuated with a valuation *)
(************************************************************)

let check_nonlinear_inequality discrete_valuation nonlinear_inequality =
    let l_expr, relop, r_expr = nonlinear_inequality in
    let l_expr_eval, r_expr_eval =
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation l_expr,
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation r_expr
    in
      DiscreteExpressions.eval_discrete_relop relop l_expr_eval r_expr_eval

(* if all true, it's satisfied *)
let check_nonlinear_inequalities discrete_valuation =
  List.for_all (check_nonlinear_inequality discrete_valuation)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint discrete_valuation = function
  | True_nonlinear_constraint -> true
  | False_nonlinear_constraint -> false
  | Nonlinear_constraint nonlinear_inequalities -> check_nonlinear_inequalities discrete_valuation nonlinear_inequalities









(** Convert a non linear inequality into a string with customized string *)
let string_of_nonlinear_inequality customized_string variable_names (l_expr, op, r_expr) =
	let lstr = DiscreteExpressions.customized_string_of_arithmetic_expression customized_string variable_names l_expr in
	let rstr = DiscreteExpressions.customized_string_of_arithmetic_expression customized_string variable_names r_expr in
	let opstr = match op with
		| OP_L          -> customized_string.l_operator
		| OP_LEQ        -> customized_string.le_operator
		| OP_EQ         -> customized_string.eq_operator
		| OP_GEQ        -> customized_string.ge_operator
		| OP_G          -> customized_string.g_operator
		| OP_NEQ        -> customized_string.l_operator ^ customized_string.g_operator
	in
	lstr ^ opstr ^ rstr


(* Get string of non-linear constraint inequalities with customized strings *)
let customized_string_of_nonlinear_constraint customized_string variable_names = function
    | True_nonlinear_constraint -> customized_string.true_string
    | False_nonlinear_constraint -> customized_string.false_string
    | Nonlinear_constraint nonlinear_constraint ->
	    " " ^
	    (string_of_list_of_string_with_sep
		    customized_string.and_operator
		    (List.map (string_of_nonlinear_inequality customized_string variable_names) nonlinear_constraint)
	    )

(* Get string of non-linear constraint inequalities with default strings *)
let string_of_nonlinear_constraint = customized_string_of_nonlinear_constraint default_string