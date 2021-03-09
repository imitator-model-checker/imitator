(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2019/12/10
 * Last modified     : 2021/03/05
 *
 ************************************************************)

open Constants

exception NotImplemented (* TODO benjamnin to remove *)

(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G

(****************************************************************)
(** Valuation *)
(****************************************************************)
type discrete_valuation = Automaton.discrete_index -> DiscreteValue.discrete_value


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
	| DF_constant of DiscreteValue.discrete_value
	| DF_expression of discrete_arithmetic_expression
	| DF_unary_min of discrete_factor


(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

(** Boolean expression *)
type boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
	| Not_bool of boolean_expression (** Negation *)
	| And_bool of boolean_expression * boolean_expression (** Conjunction *)
	| Or_bool of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolean_expression of discrete_boolean_expression

and discrete_boolean_expression =
    | Discrete_arithmetic_expression of discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(* Parsed_DB_variable of variable_name *)
	| DB_variable of Automaton.variable_index
	| DB_constant of bool

(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Rational_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression

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
		DiscreteValue.neg (eval_discrete_factor discrete_valuation discrete_factor)

and eval_discrete_term discrete_valuation = function
	| DT_mul (discrete_term, discrete_factor) ->
		DiscreteValue.mul
		(eval_discrete_term discrete_valuation discrete_term)
		(eval_discrete_factor discrete_valuation discrete_factor)
		
	| DT_div (discrete_term, discrete_factor) ->
		let numerator	= (eval_discrete_term discrete_valuation discrete_term) in
		let denominator	= (eval_discrete_factor discrete_valuation discrete_factor) in
		
		(* Check for 0-denominator *)
		if DiscreteValue.equal denominator (DiscreteValue.zero_of denominator) then(
			raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (DiscreteValue.string_of_value numerator) ^ " / " ^ (DiscreteValue.string_of_value denominator) ^ ""))
		);

		(* Divide *)
		DiscreteValue.div
		numerator
		denominator
		
	| DT_factor discrete_factor ->
		eval_discrete_factor discrete_valuation discrete_factor

and eval_discrete_arithmetic_expression discrete_valuation = function
	| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
		DiscreteValue.add
		(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression)
		(eval_discrete_term discrete_valuation discrete_term)
		
	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
		DiscreteValue.sub
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

(** Check if a boolean expression is satisfied *)
let rec is_boolean_expression_satisfied discrete_valuation = function
    | True_bool -> true
    | False_bool -> false
    | Not_bool b -> not (is_boolean_expression_satisfied discrete_valuation b) (* negation *)
    | And_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) && (is_boolean_expression_satisfied discrete_valuation b2) (* conjunction *)
    | Or_bool (b1, b2) -> (is_boolean_expression_satisfied discrete_valuation b1) || (is_boolean_expression_satisfied discrete_valuation b2) (* disjunction *)
    | Discrete_boolean_expression dbe -> check_discrete_boolean_expression discrete_valuation dbe

(** Check if a discrete boolean expression is satisfied *)
and check_discrete_boolean_expression discrete_valuation = function
    (** Discrete arithmetic expression of the form variable *)
    | Discrete_arithmetic_expression expr ->
        let eval_expr = eval_discrete_arithmetic_expression discrete_valuation expr in
        (* No need to check type, because it was been at model conversion (ModelConverter) *)
        DiscreteValue.bool_value eval_expr
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
    | Boolean_expression boolean_expression ->
        is_boolean_expression_satisfied discrete_valuation boolean_expression
    | DB_variable variable_index ->
        (* DB_variable should be a bool value, so we can convert directly to bool with no problem *)
        DiscreteValue.bool_value (discrete_valuation variable_index)
    | DB_constant value ->
        value


(************************************************************)
(** Evaluate global expressions with a valuation            *)
(************************************************************)
let eval_global_expression discrete_valuation = function
    | Rational_expression expr -> eval_discrete_arithmetic_expression discrete_valuation expr
    | Bool_expression expr -> DiscreteValue.Bool_value (is_boolean_expression_satisfied discrete_valuation expr)
    | _ -> raise NotImplemented

(* Check if a discrete term factor of an arithmetic expression should have parenthesis *)
let is_discrete_factor_has_parenthesis = function
    | DF_unary_min _
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

let add_parenthesis_to_unary_minus str = function
    | DF_expression _ -> "(" ^ str ^ ")"
    | _ -> str

(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let customized_string_of_arithmetic_expression customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
        | DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when DiscreteValue.equal c (DiscreteValue.zero_of c) ->
            string_of_arithmetic_expression customized_string discrete_arithmetic_expression

		| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_arithmetic_string.plus_string
            ^ (string_of_term customized_string discrete_term)
		| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_arithmetic_string.minus_string
            ^ (string_of_term customized_string discrete_term)
        | DAE_term discrete_term -> string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when DiscreteValue.equal c (DiscreteValue.one_of c) ->
			string_of_factor customized_string discrete_factor
		| DT_mul (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
		)
        ^ Constants.default_arithmetic_string.mul_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_div (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
        )
        ^ Constants.default_arithmetic_string.div_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_factor discrete_factor -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> variable_names discrete_index
		| DF_constant discrete_value -> DiscreteValue.string_of_value discrete_value
		| DF_unary_min discrete_factor ->
		    Constants.default_arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| DF_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			(string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string

let string_of_arithmetic_expression = customized_string_of_arithmetic_expression Constants.default_string

(* TODO benjamin ref in ModelPrinter *)
let string_of_boolean_operations customized_string = function
	| OP_L		-> customized_string.l_operator
	| OP_LEQ	-> customized_string.le_operator
	| OP_EQ		-> customized_string.eq_operator
	| OP_NEQ	-> customized_string.neq_operator
	| OP_GEQ	-> customized_string.ge_operator
	| OP_G		-> customized_string.g_operator

(* TODO benjamin ref in ModelPrinter *)
(* TODO replace operator by customized_string *)
(** Convert a Boolean expression into a string *)
let rec customized_string_of_boolean_expression customized_string variable_names = function
	| True_bool -> customized_string.true_string
	| False_bool -> customized_string.false_string
	| Not_bool b -> "<> (" ^ (customized_string_of_boolean_expression customized_string variable_names b) ^ ")"
	| And_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ " && "
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Or_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ " || "
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Discrete_boolean_expression discrete_boolean_expression ->
		customized_string_of_discrete_boolean_expression customized_string variable_names discrete_boolean_expression

(* TODO benjamin ref in ModelPrinter *)
(** Convert a discrete_boolean_expression into a string *)
and customized_string_of_discrete_boolean_expression customized_string variable_names = function
    (** Discrete arithmetic expression of the form variable *)
    | Discrete_arithmetic_expression expr ->
        customized_string_of_arithmetic_expression customized_string variable_names expr
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ (string_of_boolean_operations customized_string relop)
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ " in ["
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
		^ " , "
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression3)
		^ "]"
    | Boolean_expression boolean_expression ->
        "(" ^ (customized_string_of_boolean_expression customized_string variable_names boolean_expression) ^ ")"
    | DB_variable variable_index ->
        variable_names variable_index
    | DB_constant value ->
        string_of_bool value

(* TODO benjamin ref in ModelPrinter *)
let string_of_boolean_expression = customized_string_of_boolean_expression Constants.default_string
let string_of_discrete_boolean_expression = customized_string_of_discrete_boolean_expression Constants.default_string

let customized_string_of_global_expression customized_string variable_names = function
    | Rational_expression expr -> customized_string_of_arithmetic_expression customized_string.arithmetic_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression customized_string.boolean_string variable_names expr

let string_of_global_expression = customized_string_of_global_expression Constants.global_default_string

(************** Jani translation **************)
(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let customized_string_of_arithmetic_expression_for_jani customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
        | DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when NumConst.equal c NumConst.zero ->
            string_of_arithmetic_expression customized_string discrete_arithmetic_expression

	| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ Constants.default_operator_string.plus_string ^ "\", "
            ^ "\"left\" : " ^ (string_of_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ", " (*OTDO remove space for +*)
            ^ "\"right\" : " ^ (string_of_term customized_string discrete_term) ^ "}"

	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ Constants.default_operator_string.minus_string ^ "\", "
            ^ "\"left\" : " ^ (string_of_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ", " (*OTDO remove space for +*)
            ^ "\"right\" : " ^ (string_of_term customized_string discrete_term) ^ "}"

        | DAE_term discrete_term -> string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
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

		| DT_factor discrete_factor -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
		| DF_unary_min discrete_factor ->
		    Constants.default_operator_string.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| DF_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			(string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string

let string_of_arithmetic_expression_for_jani = customized_string_of_arithmetic_expression_for_jani Constants.default_string

(** Convert a discrete_boolean_expression into a string *)
let customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		let expr1 =  (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression1) in
		let relop =  (string_of_boolean_operations customized_string relop) in (*TODO check*)
		let expr2 =  (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression2) in
		"{"
		^ "\"op\": \"" ^ relop ^ "\", "
		^ "\"left\": " ^ expr1 ^ ", "
		^ "\"right\": " ^ expr2
		^ "}"
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	(*Done for jani, but without test*)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		let expr1 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression1) in
		let expr2 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression2) in
		let expr3 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression3) in
		  "{\"op\": \"" ^ customized_string.and_operator ^ "\", "
		(* expr2 <= expr1 *)
		^ "\"left\": "
			^ "{"
			^ "\"op\": \"" ^ customized_string.le_operator ^ "\", "
			^ "\"left\": " ^ expr2 ^ ", "
			^ "\"right\": " ^ expr1
			^ "}"
		(* expr1 <= expr3 *)
		^ "\"right\": "
			^ "{"
			^ "\"op\": \"" ^ customized_string.le_operator ^ "\", "
			^ "\"left\": " ^ expr1 ^ ", "
			^ "\"right\": " ^ expr3
			^ "}"
		^ "}"

(* TODO benjamin ref in ModelPrinter *)
let string_of_discrete_boolean_expression_for_jani = customized_string_of_discrete_boolean_expression_for_jani Constants.default_string
