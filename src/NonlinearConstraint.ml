open DiscreteExpressions
open OCamlUtilities
open Constants

type variable = int

(* Non-linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression

type nonlinear_constraint =
  | True_nonlinear_constraint
  | False_nonlinear_constraint
  | Nonlinear_constraint of DiscreteExpressions.discrete_boolean_expression list

(************************************************************)
(** Check whether a non-linear constraint evaluates to true when valuated with a valuation *)
(************************************************************)

(*let check_nonlinear_inequality discrete_valuation nonlinear_inequality =*)
(*    let l_expr, relop, r_expr = nonlinear_inequality in*)
(*    let l_expr_eval, r_expr_eval =*)
(*      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation l_expr,*)
(*      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation r_expr*)
(*    in*)
(*      DiscreteExpressions.eval_discrete_relop relop l_expr_eval r_expr_eval*)

(* if all true, it's satisfied *)
let check_nonlinear_inequalities discrete_valuation =
  List.for_all (DiscreteExpressions.check_discrete_boolean_expression discrete_valuation)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint discrete_valuation = function
  | True_nonlinear_constraint -> true
  | False_nonlinear_constraint -> false
  | Nonlinear_constraint nonlinear_inequalities -> check_nonlinear_inequalities discrete_valuation nonlinear_inequalities


(* Utility *)

(*(*------------------------------------------------------------*)*)
(*(* Check if a variable is present in a nonlinear constraint *)*)
(*(*------------------------------------------------------------*)*)
(*(* TODO see if it's possible to have general way to do that a general tree structure *)*)
(*let rec is_variable_in_nonlinear_constraint variable = function*)
(*    | True_nonlinear_constraint*)
(*    | False_nonlinear_constraint -> false*)
(*    | Nonlinear_constraint nonlinear_inequalities -> List.exists (is_variable_in_nonlinear_inequality variable) nonlinear_inequalities*)

(*and is_variable_in_nonlinear_inequality variable nonlinear_inequality =*)
(*    let l_expr, _ (* relop *), r_expr = nonlinear_inequality in*)
(*        (is_variable_in_discrete_arithmetic_expression variable l_expr) ||*)
(*        (is_variable_in_discrete_arithmetic_expression variable r_expr)*)

(*and is_variable_in_discrete_arithmetic_expression variable = function*)
(*	| DAE_plus (discrete_arithmetic_expression, discrete_term)*)
(*	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->*)
(*	    (is_variable_in_discrete_arithmetic_expression variable discrete_arithmetic_expression) ||*)
(*	    (is_variable_in_discrete_term variable discrete_term)*)
(*	| DAE_term discrete_term ->*)
(*	    is_variable_in_discrete_term variable discrete_term*)

(*and is_variable_in_discrete_term variable = function*)
(*	| DT_mul (discrete_term, discrete_factor)*)
(*	| DT_div (discrete_term, discrete_factor) ->*)
(*	    (is_variable_in_discrete_term variable discrete_term) ||*)
(*	    (is_variable_in_discrete_factor variable discrete_factor)*)
(*	| DT_factor discrete_factor ->*)
(*	    is_variable_in_discrete_factor variable discrete_factor*)

(*and is_variable_in_discrete_factor variable = function*)
(*	| DF_variable variable_index -> variable == variable_index*)
(*	| DF_constant variable_value -> false*)
(*	| DF_expression discrete_arithmetic_expression -> is_variable_in_discrete_arithmetic_expression variable discrete_arithmetic_expression*)
(*	| DF_unary_min discrete_factor -> is_variable_in_discrete_factor variable discrete_factor*)

(*(* Get variables in this nonlinear constraint *)*)
(*let is_constrained nonlinear_constraint variable =*)
(*    	is_variable_in_nonlinear_constraint variable nonlinear_constraint*)


(*let is_all_nonlinear_constraints_true =*)
(*    List.for_all(fun guard -> match guard with | True_nonlinear_constraint -> true | _ -> false)*)

(*let is_any_nonlinear_constraint_false =*)
(*    List.exists(fun guard -> match guard with | False_nonlinear_constraint -> true | _ -> false)*)

(*let intersection = function*)
(*    | nonlinear_constraints when is_all_nonlinear_constraints_true nonlinear_constraints -> True_nonlinear_constraint*)
(*    | nonlinear_constraints when is_any_nonlinear_constraint_false nonlinear_constraints -> False_nonlinear_constraint*)
(*    | nonlinear_constraints ->*)




(*(** Convert a non-linear inequality into a string with customized string *)*)
(*let string_of_nonlinear_inequality customized_string variable_names (l_expr, op, r_expr) =*)
(*	let lstr = DiscreteExpressions.customized_string_of_arithmetic_expression customized_string variable_names l_expr in*)
(*	let rstr = DiscreteExpressions.customized_string_of_arithmetic_expression customized_string variable_names r_expr in*)
(*	let opstr = match op with*)
(*		| OP_L          -> customized_string.l_operator*)
(*		| OP_LEQ        -> customized_string.le_operator*)
(*		| OP_EQ         -> customized_string.eq_operator*)
(*		| OP_GEQ        -> customized_string.ge_operator*)
(*		| OP_G          -> customized_string.g_operator*)
(*		| OP_NEQ        -> customized_string.l_operator ^ customized_string.g_operator*)
(*	in*)
(*	lstr ^ opstr ^ rstr*)


(* Get string of non-linear constraint inequalities with customized strings *)
let customized_string_of_nonlinear_constraint customized_string variable_names = function
    | True_nonlinear_constraint -> customized_string.true_string
    | False_nonlinear_constraint -> customized_string.false_string
    | Nonlinear_constraint nonlinear_constraint ->
	    " " ^
	    (string_of_list_of_string_with_sep
		    customized_string.and_operator
		    (List.rev_map (DiscreteExpressions.customized_string_of_discrete_boolean_expression customized_string variable_names) nonlinear_constraint)
	    )

(* Get string of non-linear constraint inequalities with default strings *)
let string_of_nonlinear_constraint = customized_string_of_nonlinear_constraint default_string

(*JANI*)
(* Get string of non-linear constraint inequalities with customized strings *)
let customized_string_of_nonlinear_constraint_for_jani customized_string variable_names = function
    | True_nonlinear_constraint -> customized_string.true_string
    | False_nonlinear_constraint -> customized_string.false_string
    | Nonlinear_constraint nonlinear_constraint ->
	    " " ^
	    (string_of_list_of_string_with_sep
		    customized_string.and_operator
		    (List.rev_map (DiscreteExpressions.customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names) nonlinear_constraint)
	    )

(* Get string of non-linear constraint inequalities with default strings *)
let string_of_nonlinear_constraint_for_jani = customized_string_of_nonlinear_constraint_for_jani default_string
