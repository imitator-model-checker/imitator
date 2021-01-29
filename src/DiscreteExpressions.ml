(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André
 * Created           : 2019/12/10
 * Last modified     : 2020/01/07
 *
 ************************************************************)



(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G

(****************************************************************)
(** Valuation *)
(****************************************************************)
type discrete_valuation = Automaton.discrete_index -> Automaton.discrete_value


(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
type discrete_arithmetic_expression =
	| DAE_plus of discrete_arithmetic_expression * discrete_term
	| DAE_minus of discrete_arithmetic_expression * discrete_term
	| DAE_term of discrete_term

and discrete_term =
	| DT_mul of discrete_term * discrete_factor
	| DT_div of discrete_term * discrete_factor
	| DT_factor of discrete_factor

and discrete_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of Automaton.variable_value
	| DF_expression of discrete_arithmetic_expression
	| DF_unary_min of discrete_factor


(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

type discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression


(************************************************************)
(** Evaluate arithmetic expressions with a valuation *)
(************************************************************)

let rec eval_discrete_factor discrete_valuation = function
	| DF_variable variable_index ->
		discrete_valuation variable_index
		
	| DF_constant variable_value ->
		variable_value
		
	| DF_expression discrete_arithmetic_expression ->
		eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression
	
	| DF_unary_min discrete_factor ->
		NumConst.neg (eval_discrete_factor discrete_valuation discrete_factor)

and eval_discrete_term discrete_valuation = function
	| DT_mul (discrete_term, discrete_factor) ->
		NumConst.mul
		(eval_discrete_term discrete_valuation discrete_term)
		(eval_discrete_factor discrete_valuation discrete_factor)
		
	| DT_div (discrete_term, discrete_factor) ->
			(*** TODO: division by zero ***)
		NumConst.div
		(eval_discrete_term discrete_valuation discrete_term)
		(eval_discrete_factor discrete_valuation discrete_factor)
		
	| DT_factor discrete_factor ->
		eval_discrete_factor discrete_valuation discrete_factor

and eval_discrete_arithmetic_expression discrete_valuation = function
	| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
		NumConst.add
		(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression)
		(eval_discrete_term discrete_valuation discrete_term)
		
	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
		NumConst.sub
		(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression)
		(eval_discrete_term discrete_valuation discrete_term)
		
	| DAE_term discrete_term ->
		eval_discrete_term discrete_valuation discrete_term

	
	
	
(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

let eval_discrete_relop relop value_1 value_2 : bool =
	match relop with
	| OP_L		-> value_1 <  value_2
	| OP_LEQ	-> value_1 <= value_2
	| OP_EQ		-> value_1 =  value_2
	| OP_NEQ	-> value_1 <> value_2
	| OP_GEQ	-> value_1 >= value_2
	| OP_G		-> value_1 >  value_2

let check_discrete_boolean_expression discrete_valuation = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression_1, relop, discrete_arithmetic_expression_2) ->
		eval_discrete_relop
			relop
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1)
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
			
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
		(* Compute the first one to avoid redundancy *)
		let expr1_evaluated = eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1 in
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
			<=
			expr1_evaluated
			&&
			expr1_evaluated
			<= 
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_3)





(* Check if a discrete term factor of an arithmetic expression should have parenthesis *)
let is_discrete_factor_has_parenthesis = function
    | DF_expression(DAE_plus _)
    | DF_expression(DAE_minus _) -> true
    | _ -> false

(* Check if discrete factor is a multiplication *)
let is_discrete_factor_is_mul = function
    | DF_expression(DAE_term(DT_mul _)) -> true
    | _ -> false

(* Check if a left expression should have parenthesis *)
(* is (x + y) * z *)
(* or (x - y) * z *)
(* or (x + y) / z *)
(* or (x - y) / z *)
let is_left_expr_has_parenthesis = function
    | DT_factor factor -> is_discrete_factor_has_parenthesis factor
    | _ -> false

(* Check if a right expression should have parenthesis *)
(* is x * (y + z) *)
(* or x * (y - z) *)
(* or x / (y + z) *)
(* or x / (y - z) *)
(* or x / (y * z) *)
let is_right_expr_has_parenthesis = function
    (* check x / (y * z) *)
    | DT_div (discrete_term, discrete_factor) when is_discrete_factor_is_mul discrete_factor -> true
    (* check x / (y + z) or x / (y - z) *)
    | DT_div (discrete_term, discrete_factor)
    (* check x * (y + z) or x * (y - z) *)
    | DT_mul (discrete_term, discrete_factor) -> is_discrete_factor_has_parenthesis discrete_factor
    | _ -> false

let add_left_parenthesis expr str =
    if is_left_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

let add_right_parenthesis str expr =
    if is_right_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let customized_string_of_arithmetic_expression customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
        | DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) as expr when NumConst.equal c NumConst.zero ->
            string_of_arithmetic_expression customized_string discrete_arithmetic_expression

		| DAE_plus (discrete_arithmetic_expression, discrete_term) as expr ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_operator_string.plus_string
            ^ (string_of_term customized_string discrete_term)
		| DAE_minus (discrete_arithmetic_expression, discrete_term) as expr ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_operator_string.minus_string
            ^ (string_of_term customized_string discrete_term)
        | DAE_term discrete_term -> string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) as expr when NumConst.equal c NumConst.one ->
			string_of_factor customized_string discrete_factor
		| DT_mul (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
		)
        ^ Constants.default_operator_string.mul_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_div (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
        )
        ^ Constants.default_operator_string.div_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_factor discrete_factor as expr -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> variable_names discrete_index
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
		| DF_unary_min discrete_factor as expr -> Constants.default_operator_string.unary_min_string ^ (string_of_factor customized_string discrete_factor)
		| DF_expression discrete_arithmetic_expression as expr ->
			(*** TODO: simplify a bit? ***)
			(string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string

let string_of_arithmetic_expression = customized_string_of_arithmetic_expression Constants.default_string