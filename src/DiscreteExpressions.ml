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


(* Expression type *)
type expression_type =
    | Expression_type_discrete_bool of DiscreteValue.var_type_discrete
    | Expression_type_discrete_number of DiscreteValue.var_type_discrete_number

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
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Discrete variable *)
	| DB_variable of Automaton.variable_index
	(** Discrete constant *)
	| DB_constant of DiscreteValue.discrete_value

(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Rational_expression of discrete_arithmetic_expression
    | Int_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression * DiscreteValue.var_type_discrete

(* Get var type of expression *)
let var_type_of_expression = function
    | Rational_expression _ -> DiscreteValue.var_type_rational
    | Int_expression _ -> DiscreteValue.var_type_int
    | Bool_expression _ -> DiscreteValue.var_type_bool

(****************************************************************)
(** Strings *)
(****************************************************************)
let string_of_expression_type = function
    | Expression_type_discrete_number x -> "arithmetic of " ^ (DiscreteValue.string_of_var_type_discrete_number x)
    | Expression_type_discrete_bool x -> "boolean of " ^ (DiscreteValue.string_of_var_type_discrete x)

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
(** Convert a Boolean expression into a string *)
let rec customized_string_of_boolean_expression customized_string variable_names = function
	| True_bool -> customized_string.true_string
	| False_bool -> customized_string.false_string
	| Not_bool b -> customized_string.not_operator ^ " (" ^ (customized_string_of_boolean_expression customized_string variable_names b) ^ ")"
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
    | DB_variable discrete_index -> variable_names discrete_index
    | DB_constant discrete_value -> DiscreteValue.string_of_value discrete_value

(* TODO benjamin ref in ModelPrinter *)
let string_of_boolean_expression = customized_string_of_boolean_expression Constants.default_string
let string_of_discrete_boolean_expression = customized_string_of_discrete_boolean_expression Constants.default_string

let customized_string_of_global_expression customized_string variable_names = function
    | Int_expression expr
    | Rational_expression expr -> customized_string_of_arithmetic_expression customized_string.arithmetic_string variable_names expr
    | Bool_expression (expr, _) -> customized_string_of_boolean_expression customized_string.boolean_string variable_names expr

let string_of_global_expression = customized_string_of_global_expression Constants.global_default_string

(************************************************************)
(** General functions on expression types *)
(************************************************************)

(* Check if an expression is a boolean expression *)
let is_bool_expression_type = function
    | Expression_type_discrete_bool _ -> true
    | _ -> false

(* Check if expression type is a unknown number type *)
let is_unknown_number_expression_type = function
    | Expression_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number -> true
    | _ -> false

(* Check if expression type is a bool of unknown number type *)
let is_bool_of_unknown_number_expression_type = function
    | Expression_type_discrete_bool (DiscreteValue.Var_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number) -> true
    | _ -> false

(* Check if a variable type is compatible with an expression type *)
let is_var_type_discrete_compatible_with_expr_type var_type_discrete expr_type =
    match var_type_discrete, expr_type with
    (* Booleans are compatible with any boolean expression *)
    | DiscreteValue.Var_type_discrete_bool,  Expression_type_discrete_bool _ -> true
    (* All number types are compatible with unknown number typed expression *)
    | DiscreteValue.Var_type_discrete_number _, Expression_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number -> true
    (* Number type is compatible with an arithmetic expression of the same type *)
    | DiscreteValue.Var_type_discrete_number var_type, Expression_type_discrete_number expr_type when var_type = expr_type -> true
    | _ -> false

(* Check if a variable type is compatible with an expression type *)
let is_var_type_compatible_with_expr_type var_type expr_type =
    match var_type, expr_type with
    (*
    (* Clocks are rationals *)
    | Var_type_clock, Expression_type_discrete_number Var_type_discrete_rational
    (* Parameters are rationals *)
    | Var_type_parameter, Expression_type_discrete_number Var_type_discrete_rational
    *)
    (* Booleans are compatible with any boolean expression *)
    | DiscreteValue.Var_type_discrete DiscreteValue.Var_type_discrete_bool,  Expression_type_discrete_bool _ -> true
    (* All number types are compatible with unknown number typed expression *)
    | DiscreteValue.Var_type_discrete (DiscreteValue.Var_type_discrete_number _), Expression_type_discrete_number DiscreteValue.Var_type_discrete_unknown_number -> true
    (* Number type is compatible with an arithmetic expression of the same type *)
    | DiscreteValue.Var_type_discrete (DiscreteValue.Var_type_discrete_number var_type), Expression_type_discrete_number expr_type when var_type = expr_type -> true
    | _ -> false

(* TODO benjamin comment all below *)

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
(** Evaluate a rational expression with a valuation *)
(************************************************************)
(*
let eval_rational_expression discrete_valuation expr =
    let evaluator = ExpressionsEvaluator.MakeEvaluator(NumConst) in
    evaluator.eval_expression discrete_valuation expr
*)

let eval_rational_expression discrete_valuation expr =

    let rec eval_rational_expression_rec = function
        | DAE_plus (expr, term) ->
            NumConst.add
                (eval_rational_expression_rec expr)
                (eval_rational_term term)
        | DAE_minus (expr, term) ->
            NumConst.sub
                (eval_rational_expression_rec expr)
                (eval_rational_term term)
        | DAE_term term ->
            eval_rational_term term

    and eval_rational_term = function
        | DT_mul (term, factor) ->
            NumConst.mul
            (eval_rational_term term)
            (eval_rational_factor factor)
        | DT_div (term, factor) ->
            let numerator	= (eval_rational_term term) in
            let denominator	= (eval_rational_factor factor) in

            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (NumConst.string_of_numconst numerator) ^ " / " ^ (NumConst.string_of_numconst denominator) ^ ""))
            );

            (* Divide *)
            NumConst.div
                numerator
                denominator

        | DT_factor factor ->
            eval_rational_factor factor

    and eval_rational_factor = function
        | DF_variable variable_index ->
            DiscreteValue.numconst_value (discrete_valuation variable_index)
        | DF_constant variable_value ->
            DiscreteValue.numconst_value variable_value
        | DF_expression expr ->
            eval_rational_expression_rec expr
        | DF_unary_min factor ->
            NumConst.neg (eval_rational_factor factor)

    in
    eval_rational_expression_rec expr

(*
(************************************************************)
(** Evaluate a int32 expression with a valuation *)
(************************************************************)
let eval_int_expression discrete_valuation expr =
    let evaluator = ExpressionsEvaluator.MakeEvaluator(Int32) in
    evaluator.evaluate_expression discrete_valuation expr
*)

let eval_int_expression discrete_valuation expr =

    let rec eval_int_expression_rec = function
        | DAE_plus (expr, term) ->
            Int32.add
                (eval_int_expression_rec expr)
                (eval_int_term term)
        | DAE_minus (expr, term) ->
            Int32.sub
                (eval_int_expression_rec expr)
                (eval_int_term term)
        | DAE_term term ->
            eval_int_term term

    and eval_int_term = function
        | DT_mul (term, factor) ->
            Int32.mul
            (eval_int_term term)
            (eval_int_factor factor)
        | DT_div (term, factor) ->
            let numerator	= (eval_int_term term) in
            let denominator	= (eval_int_factor factor) in

            (* Check for 0-denominator *)
            if Int32.equal denominator Int32.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ ""))
            );

            (* Divide *)
            Int32.div
                numerator
                denominator

        | DT_factor factor ->
            eval_int_factor factor

    and eval_int_factor = function
        | DF_variable variable_index ->
            DiscreteValue.int_value (discrete_valuation variable_index)
        | DF_constant variable_value ->
            DiscreteValue.int_value variable_value
        | DF_expression expr ->
            eval_int_expression_rec expr
        | DF_unary_min factor ->
            Int32.neg (eval_int_factor factor)

    in
    eval_int_expression_rec expr


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
    | DB_variable variable_index ->
        DiscreteValue.bool_value (discrete_valuation variable_index)
    | DB_constant var_value ->
        DiscreteValue.bool_value var_value
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


(************************************************************)
(** Evaluate global expressions with a valuation            *)
(************************************************************)
let eval_global_expression discrete_valuation = function
    | Rational_expression expr ->
(*        ImitatorUtilities.print_message Verbose_standard ("Evaluate rational expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
        DiscreteValue.Rational_value (eval_rational_expression discrete_valuation expr)
    | Int_expression expr ->
(*        ImitatorUtilities.print_message Verbose_standard ("Evaluate int expression : " ^ (string_of_arithmetic_expression discrete_valuation expr));*)
        DiscreteValue.Int_value (eval_int_expression discrete_valuation expr)
    | Bool_expression (expr, _) ->
(*        ImitatorUtilities.print_message Verbose_standard ("Evaluate bool expression : ");*)
        DiscreteValue.Bool_value (is_boolean_expression_satisfied discrete_valuation expr)

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
