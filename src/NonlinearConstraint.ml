open DiscreteExpressions
open LinearConstraint (* TODO remove ref to LinearConstraint *)
open OCamlUtilities

(* TODO move to another module *)
(** Default values *)
let default_string = {
	true_string   = "True";
	false_string  = "False";
	and_operator  = "\n& ";
	or_operator   = " or ";
	l_operator    = " < ";
	le_operator   = " <= ";
	eq_operator   = " = ";
	ge_operator   = " >= ";
	g_operator    = " > ";
}

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
(* TODO benjamin to test *)

let check_nonlinear_inequality discrete_valuation nonlinear_inequality =
    let l_expr, relop, r_expr = nonlinear_inequality in
    let l_expr_eval, r_expr_eval =
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation l_expr,
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation r_expr
    in
      DiscreteExpressions.eval_discrete_relop relop l_expr_eval l_expr_eval

(* if all true, it's satisfied *)
let check_nonlinear_inequalities discrete_valuation nonlinear_inequalities =
  List.for_all (check_nonlinear_inequality discrete_valuation) nonlinear_inequalities

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint discrete_valuation = function
  | True_nonlinear_constraint -> true
  | False_nonlinear_constraint -> false
  | Nonlinear_constraint nonlinear_inequalities -> check_nonlinear_inequalities discrete_valuation nonlinear_inequalities





(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let string_of_arithmetic_expression customized_string variable_names =
	let rec string_of_arithmetic_expression customized_string = function
		(* Shortcut: Remove the "+0" / -"0" cases *)
		| DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
		| DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when NumConst.equal c NumConst.zero ->
			string_of_arithmetic_expression customized_string discrete_arithmetic_expression

		| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
			(string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
			^ " + "
			^ (string_of_term customized_string discrete_term)

		| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
			(string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
			^ " - "
			^ (string_of_term customized_string discrete_term)

		| DAE_term discrete_term -> string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
			string_of_factor customized_string discrete_factor
		(* No parentheses for constant * variable *)
		| DT_mul (DT_factor (DF_constant c), DF_variable v) ->
			(string_of_factor customized_string (DF_constant c))
			^ " * "
			^ (string_of_factor customized_string (DF_variable v))
		(*** TODO: No parentheses on the left for constant or variable * something ***)
		(* Otherwise: parentheses on the left *)
		| DT_mul (discrete_term, discrete_factor) ->
			"(" ^ (string_of_term customized_string discrete_term) ^ ")"
			^ " * "
			^ (string_of_factor customized_string discrete_factor)

		(*** TODO: No parentheses on the left for constant or variable / something ***)
		(*** TODO: No parentheses on the left for something / constant or variable ***)
		(* Otherwise: parentheses on the left *)
		| DT_div (discrete_term, discrete_factor) ->
			"(" ^ (string_of_term customized_string discrete_term) ^ ")"
			^ " / "
			^ (string_of_factor customized_string discrete_factor)

		| DT_factor discrete_factor -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> variable_names discrete_index
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
		| DF_unary_min discrete_factor -> "-" ^ (string_of_factor customized_string discrete_factor)
		| DF_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			"(" ^ (string_of_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ")"
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string




(** Convert a non linear inequality into a string with customized string *)
let string_of_nonlinear_inequality customized_string variable_names (l_expr, op, r_expr) =
	let lstr = string_of_arithmetic_expression customized_string variable_names l_expr in
	let rstr = string_of_arithmetic_expression customized_string variable_names r_expr in
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