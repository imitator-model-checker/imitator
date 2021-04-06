open DiscreteExpressions
open OCamlUtilities
open Constants

type variable = int

(* Non-linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression

type nonlinear_constraint =
  | True_nonlinear_constraint
  | False_nonlinear_constraint
  | Nonlinear_constraint of DiscreteExpressions.typed_discrete_boolean_expression list

(* if all true, it's satisfied *)
let check_nonlinear_inequalities discrete_valuation =
  List.for_all (ExpressionsEvaluator.DiscreteExpressionsEvaluator.check_typed_discrete_boolean_expression discrete_valuation)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint discrete_valuation = function
  | True_nonlinear_constraint -> true
  | False_nonlinear_constraint -> false
  | Nonlinear_constraint nonlinear_inequalities -> check_nonlinear_inequalities discrete_valuation nonlinear_inequalities

(* Get string of non-linear constraint inequalities with customized strings *)
let customized_string_of_nonlinear_constraint customized_string variable_names = function
    | True_nonlinear_constraint -> customized_string.true_string
    | False_nonlinear_constraint -> customized_string.false_string
    | Nonlinear_constraint nonlinear_constraint ->
	    " " ^
	    (string_of_list_of_string_with_sep
		    customized_string.and_operator
		    (List.rev_map (DiscreteExpressions.customized_string_of_typed_discrete_boolean_expression customized_string variable_names) nonlinear_constraint)
	    )

(* Get string of non-linear constraint inequalities with default strings *)
let string_of_nonlinear_constraint = customized_string_of_nonlinear_constraint default_string

(*JANI*)
(* Get list of non-linear constraint inequalities with customized strings *)
let customized_strings_of_nonlinear_constraint_for_jani customized_string variable_names = function
    | True_nonlinear_constraint -> [customized_string.true_string]
    | False_nonlinear_constraint -> [customized_string.false_string]
    | Nonlinear_constraint nonlinear_constraint ->
	(List.rev_map (DiscreteExpressions.customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names) nonlinear_constraint)

(* Get string of non-linear constraint inequalities with default strings *)
let strings_of_nonlinear_constraint_for_jani = customized_strings_of_nonlinear_constraint_for_jani default_string
