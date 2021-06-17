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
 * Last modified     : 2021/06/02
 *
 ************************************************************)

open Constants

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
type rational_arithmetic_expression =
	| DAE_plus of rational_arithmetic_expression * rational_term
	| DAE_minus of rational_arithmetic_expression * rational_term
	| DAE_term of rational_term

and rational_term =
	| DT_mul of rational_term * rational_factor
	| DT_div of rational_term * rational_factor
	| DT_factor of rational_factor

and rational_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of NumConst.t
	| DF_expression of rational_arithmetic_expression
	| DF_rational_of_int of int_arithmetic_expression
	| DF_unary_min of rational_factor
    | DF_pow of rational_arithmetic_expression * int_arithmetic_expression


(************************************************************)
(** Int arithmetic expressions for discrete variables *)
(************************************************************)
(************************************************************)
and int_arithmetic_expression =
	| Int_plus of int_arithmetic_expression * int_term
	| Int_minus of int_arithmetic_expression * int_term
	| Int_term of int_term

and int_term =
	| Int_mul of int_term * int_factor
	| Int_div of int_term * int_factor
	| Int_factor of int_factor

and int_factor =
	| Int_variable of Automaton.variable_index
	| Int_constant of Int32.t
	| Int_expression of int_arithmetic_expression
	| Int_unary_min of int_factor
    | Int_pow of int_arithmetic_expression * int_arithmetic_expression

type discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(************************************************************)
(************************************************************)
(************************************************************)
(** Binary word expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Binary word expression *)
type binary_word_expression =
    | Logical_shift_left of binary_word_expression * int_arithmetic_expression
    | Logical_shift_right of binary_word_expression * int_arithmetic_expression
    | Binary_word_constant of BinaryWord.t
    | Binary_word_variable of Automaton.variable_index

(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

(** Boolean expression *)
type boolean_expression =
	| True_bool (** True *)
	| False_bool (** False *)
	| And_bool of boolean_expression * boolean_expression (** Conjunction *)
	| Or_bool of boolean_expression * boolean_expression (** Disjunction *)
	| Discrete_boolean_expression of discrete_boolean_expression

and discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	| Boolean_comparison of discrete_boolean_expression * relop * discrete_boolean_expression
	| Binary_comparison of binary_word_expression * relop * binary_word_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | }*)
	| Not_bool of boolean_expression (** Negation *)
	(** Discrete variable *)
	| DB_variable of Automaton.variable_index
	(** Discrete constant *)
	| DB_constant of DiscreteValue.discrete_value

(****************************************************************)
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Arithmetic_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression
    | Binary_word_expression of binary_word_expression

(****************************************************************)
(** Strings *)
(****************************************************************)

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






(* Check if a discrete term factor of an arithmetic expression should have parenthesis *)
let is_int_factor_has_parenthesis = function
    | Int_unary_min _
    | Int_expression(Int_plus _)
    | Int_expression(Int_minus _) -> true
    | _ -> false

(* Check if discrete factor is a multiplication *)
let is_int_factor_is_mul = function
    | Int_expression(Int_term(Int_mul _)) -> true
    | _ -> false

(* Check if a left expression should have parenthesis *)
(* is (x + y) * z *)
(* or (x - y) * z *)
(* or (x + y) / z *)
(* or (x - y) / z *)
let is_left_int_expr_has_parenthesis = function
    | Int_factor factor -> is_int_factor_has_parenthesis factor
    | _ -> false

(* Check if a right expression should have parenthesis *)
(* is x * (y + z) *)
(* or x * (y - z) *)
(* or x / (y + z) *)
(* or x / (y - z) *)
(* or x / (y * z) *)
let is_right_int_expr_has_parenthesis = function
    (* check x / (y * z) *)
    | Int_div (term, factor) when is_int_factor_is_mul factor -> true
    (* check x / (y + z) or x / (y - z) *)
    | Int_div (term, factor)
    (* check x * (y + z) or x * (y - z) *)
    | Int_mul (term, factor) -> is_int_factor_has_parenthesis factor
    | _ -> false

let add_left_parenthesis_int expr str =
    if is_left_int_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

let add_right_parenthesis_int str expr =
    if is_right_int_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

let add_parenthesis_to_unary_minus_int str = function
    | Int_expression _ -> "(" ^ str ^ ")"
    | _ -> str

(* Constructors strings *)

let string_of_rational_factor_constructor = function
	| DF_variable _ -> "rational variable"
	| DF_constant _ -> "rational constant"
	| DF_expression _ -> "rational expression"
	| DF_unary_min _ -> "rational minus"
	| DF_rational_of_int _ -> "rational_of_int"
	| DF_pow _ -> "pow"

let string_of_int_factor_constructor = function
	| Int_variable _ -> "int variable"
	| Int_constant _ -> "int constant"
	| Int_expression _ -> "int expression"
	| Int_unary_min _ -> "int minus"
	| Int_pow _ -> "pow"

let string_of_binary_word_expression_constructor = function
    | Logical_shift_left _ -> "shift_left"
    | Logical_shift_right _ -> "shift_right"
    | Binary_word_constant _ -> "binary word constant"
    | Binary_word_variable _ -> "binary word variable"

(* Expressions strings *)

let rec customized_string_of_global_expression customized_string variable_names = function
    | Arithmetic_expression expr -> customized_string_of_arithmetic_expression customized_string.boolean_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression customized_string.boolean_string variable_names expr
    | Binary_word_expression expr -> customized_string_of_binary_word_expression customized_string.boolean_string variable_names expr

(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
and customized_string_of_arithmetic_expression customized_string variable_names = function
    | Rational_arithmetic_expression expr -> customized_string_of_rational_arithmetic_expression customized_string variable_names expr
    | Int_arithmetic_expression expr -> customized_string_of_int_arithmetic_expression customized_string variable_names expr

and customized_string_of_rational_arithmetic_expression customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
        | DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when NumConst.equal c NumConst.zero ->
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
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
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
		| DF_constant value -> NumConst.to_string value
		| DF_unary_min discrete_factor ->
		    Constants.default_arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| DF_rational_of_int discrete_arithmetic_expression as factor ->
		    string_of_rational_factor_constructor factor
		    ^ "("
		    ^ customized_string_of_int_arithmetic_expression customized_string variable_names discrete_arithmetic_expression
		    ^ ")"
        | DF_pow (expr, exp) as factor ->
            string_of_rational_factor_constructor factor
            ^ "("
            ^ string_of_arithmetic_expression customized_string expr
            ^ ", "
            ^ customized_string_of_int_arithmetic_expression customized_string variable_names exp
            ^ ")"
		| DF_expression discrete_arithmetic_expression ->
			string_of_arithmetic_expression customized_string discrete_arithmetic_expression
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string
(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
and customized_string_of_int_arithmetic_expression customized_string variable_names =

    let rec string_of_int_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Int_plus (expr, Int_factor (Int_constant c))
        | Int_minus (expr, Int_factor (Int_constant c)) when Int32.equal c Int32.zero ->
            string_of_int_arithmetic_expression customized_string expr

		| Int_plus (expr, term) ->
            (string_of_int_arithmetic_expression customized_string expr)
            ^ Constants.default_arithmetic_string.plus_string
            ^ (string_of_int_term customized_string term)

		| Int_minus (expr, term) ->
            (string_of_int_arithmetic_expression customized_string expr)
            ^ Constants.default_arithmetic_string.minus_string
            ^ (string_of_int_term customized_string term)

        | Int_term term ->
            string_of_int_term customized_string term

	and string_of_int_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| Int_mul (Int_factor (Int_constant c), factor) when Int32.equal c Int32.one ->
			string_of_int_factor customized_string factor

		| Int_mul (term, factor) as expr ->
            add_left_parenthesis_int term (
                (string_of_int_term customized_string term)
            )
            ^ Constants.default_arithmetic_string.mul_string
            ^
            (add_right_parenthesis_int (
                string_of_int_factor customized_string factor
            ) expr)

		| Int_div (term, factor) as expr ->
            add_left_parenthesis_int term (
                (string_of_int_term customized_string term)
            )
            ^ Constants.default_arithmetic_string.div_string
            ^
            (add_right_parenthesis_int (
                string_of_int_factor customized_string factor
            ) expr)

		| Int_factor factor ->
		    string_of_int_factor customized_string factor

	and string_of_int_factor customized_string = function
		| Int_variable i -> variable_names i
		| Int_constant value -> Int32.to_string value
		| Int_unary_min factor ->
		    Constants.default_arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus_int (
		         (string_of_int_factor customized_string factor)
		    ) factor
		| Int_expression expr ->
			string_of_int_arithmetic_expression customized_string expr
        | Int_pow (expr, exp) as factor ->
            string_of_int_factor_constructor factor
            ^ "("
            ^ string_of_int_arithmetic_expression customized_string expr
            ^ ", "
            ^ string_of_int_arithmetic_expression customized_string exp
            ^ ")"
	(* Call top-level *)
	in string_of_int_arithmetic_expression customized_string

(** Convert a Boolean expression into a string *)
and customized_string_of_boolean_expression customized_string variable_names = function
	| True_bool -> customized_string.true_string
	| False_bool -> customized_string.false_string
	| And_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ customized_string.and_operator
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Or_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ customized_string.or_operator
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Discrete_boolean_expression discrete_boolean_expression ->
		customized_string_of_discrete_boolean_expression customized_string variable_names discrete_boolean_expression

(** Convert a discrete_boolean_expression into a string *)
and customized_string_of_discrete_boolean_expression customized_string variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ (customized_string_of_boolean_operations customized_string relop)
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
    | Boolean_comparison (l_expr, relop, r_expr) ->
		(customized_string_of_discrete_boolean_expression customized_string variable_names l_expr)
		^ (customized_string_of_boolean_operations customized_string relop)
		^ (customized_string_of_discrete_boolean_expression customized_string variable_names r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
		(customized_string_of_binary_word_expression customized_string variable_names l_expr)
		^ (customized_string_of_boolean_operations customized_string relop)
		^ (customized_string_of_binary_word_expression customized_string variable_names r_expr)
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ customized_string.in_operator
		^ "["
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
		^ " , "
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression3)
		^ "]"
    | Boolean_expression boolean_expression ->
        "(" ^ (customized_string_of_boolean_expression customized_string variable_names boolean_expression) ^ ")"
	| Not_bool b ->
	    customized_string.not_operator ^ " (" ^ (customized_string_of_boolean_expression customized_string variable_names b) ^ ")"
    | DB_variable discrete_index -> variable_names discrete_index
    | DB_constant discrete_value -> DiscreteValue.string_of_value discrete_value

and customized_string_of_boolean_operations customized_string = function
	| OP_L		-> customized_string.l_operator
	| OP_LEQ	-> customized_string.le_operator
	| OP_EQ		-> customized_string.eq_operator
	| OP_NEQ	-> customized_string.neq_operator
	| OP_GEQ	-> customized_string.ge_operator
	| OP_G		-> customized_string.g_operator

and customized_string_of_binary_word_expression customized_string variable_names = function
    (* TODO benjamin refactor not hard-coded string of constructor *)
    | Logical_shift_left (binary_word, expr) as binary_word_expression ->
        string_of_binary_word_expression_constructor binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ ", "
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names expr
        ^ ")"
    | Logical_shift_right (binary_word, expr) as binary_word_expression ->
        string_of_binary_word_expression_constructor binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ ", "
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names expr
        ^ ")"
    | Binary_word_constant value -> BinaryWord.string_of_binaryword value
    | Binary_word_variable variable_index -> variable_names variable_index


let string_of_global_expression = customized_string_of_global_expression Constants.global_default_string
let string_of_arithmetic_expression = customized_string_of_arithmetic_expression Constants.default_string
let string_of_boolean_expression = customized_string_of_boolean_expression Constants.default_string
let string_of_discrete_boolean_expression = customized_string_of_discrete_boolean_expression Constants.default_string

(* JANI *)

(************** Jani translation **************)
(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let jani_separator = ", "

let rec customized_string_of_global_expression_for_jani customized_string variable_names = function
    | Arithmetic_expression expr -> customized_string_of_arithmetic_expression_for_jani customized_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression_for_jani customized_string variable_names expr
    | Binary_word_expression expr -> customized_string_of_binary_word_expression_for_jani customized_string variable_names expr

and customized_string_of_boolean_expression_for_jani customized_string variable_names = function
	| True_bool -> customized_string.boolean_string.true_string
	| False_bool -> customized_string.boolean_string.false_string
	| And_bool (b1, b2) ->
		"{\"op\": \"" ^ customized_string.boolean_string.and_operator ^ "\"" ^ jani_separator
		^ "\"left\": " ^ (customized_string_of_boolean_expression_for_jani customized_string variable_names b1) ^ jani_separator
		^ "\"right\": " ^ (customized_string_of_boolean_expression_for_jani customized_string variable_names b2) ^ "}"
	| Or_bool (b1, b2) ->
		"{\"op\": \"" ^ customized_string.boolean_string.or_operator ^ "\"" ^ jani_separator
		^ "\"left\": " ^ (customized_string_of_boolean_expression_for_jani customized_string variable_names b1) ^ jani_separator
		^ "\"right\": " ^ (customized_string_of_boolean_expression_for_jani customized_string variable_names b2) ^"}"
	| Discrete_boolean_expression discrete_boolean_expression ->
		customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names discrete_boolean_expression

(** Convert a discrete_boolean_expression into a string *)
and customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		let expr1 =  (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression1) in
		let relop =  (customized_string_of_boolean_operations customized_string.boolean_string relop) in
		let expr2 =  (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression2) in
		"{"
		^ "\"op\": \"" ^ relop ^ "\", "
		^ "\"left\": " ^ expr1 ^ ", "
		^ "\"right\": " ^ expr2
		^ "}"
    | Boolean_comparison (l_expr, relop, r_expr) ->
		let expr1 =  (customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names l_expr) in
		let relop =  (customized_string_of_boolean_operations customized_string.boolean_string relop) in
		let expr2 =  (customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names r_expr) in
		"{"
		^ "\"op\": \"" ^ relop ^ "\", "
		^ "\"left\": " ^ expr1 ^ ", "
		^ "\"right\": " ^ expr2
		^ "}"
    | Binary_comparison (l_expr, relop, r_expr) ->
		let expr1 =  (customized_string_of_binary_word_expression_for_jani customized_string variable_names l_expr) in
		let relop =  (customized_string_of_boolean_operations customized_string.boolean_string relop) in
		let expr2 =  (customized_string_of_binary_word_expression_for_jani customized_string variable_names r_expr) in
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
		  "{\"op\": \"" ^ customized_string.boolean_string.and_operator ^ "\", "
		(* expr2 <= expr1 *)
		^ "\"left\": "
			^ "{"
			^ "\"op\": \"" ^ customized_string.boolean_string.le_operator ^ "\", "
			^ "\"left\": " ^ expr2 ^ ", "
			^ "\"right\": " ^ expr1
			^ "}"
		(* expr1 <= expr3 *)
		^ "\"right\": "
			^ "{"
			^ "\"op\": \"" ^ customized_string.boolean_string.le_operator ^ "\", "
			^ "\"left\": " ^ expr1 ^ ", "
			^ "\"right\": " ^ expr3
			^ "}"
		^ "}"
    | Boolean_expression expr ->
        customized_string_of_boolean_expression_for_jani customized_string variable_names expr
	| Not_bool b ->
	    "{\"op\": \""^ customized_string.boolean_string.not_operator ^"\"" ^ jani_separator ^ "\"exp\": " ^ (customized_string_of_boolean_expression_for_jani customized_string variable_names b) ^ "}"
    | DB_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
    | DB_constant value -> DiscreteValue.string_of_value value

and customized_string_of_arithmetic_expression_for_jani customized_string variable_names = function
    | Rational_arithmetic_expression expr -> customized_string_of_rational_arithmetic_expression_for_jani customized_string variable_names expr
    | Int_arithmetic_expression expr -> customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr

and customized_string_of_rational_arithmetic_expression_for_jani customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | DAE_plus (discrete_arithmetic_expression, DT_factor (DF_constant c))
        | DAE_minus (discrete_arithmetic_expression, DT_factor (DF_constant c)) when NumConst.equal c NumConst.zero ->
            string_of_arithmetic_expression customized_string discrete_arithmetic_expression

	| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ Constants.default_arithmetic_string_without_whitespace.plus_string ^ "\", "
            ^ "\"left\" : " ^ (string_of_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ", " (*OTDO remove space for +*)
            ^ "\"right\" : " ^ (string_of_term customized_string discrete_term) ^ "}"

	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ Constants.default_arithmetic_string_without_whitespace.minus_string ^ "\", "
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
        ^ Constants.default_arithmetic_string_without_whitespace.mul_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_div (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
        )
        ^ Constants.default_arithmetic_string_without_whitespace.div_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| DT_factor discrete_factor -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
		| DF_unary_min discrete_factor ->
		    Constants.default_arithmetic_string_without_whitespace.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| DF_expression expr ->
			(*** TODO: simplify a bit? ***)
			string_of_arithmetic_expression customized_string expr
		| DF_rational_of_int expr ->
		    customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr
        | DF_pow (expr, exp) as factor ->
            "{\"op\": \""
            ^ string_of_rational_factor_constructor factor
            ^ "\", \"left\":"
            ^ string_of_arithmetic_expression customized_string expr
            ^ ", \"right\":"
            ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names exp
            ^ "}"
	(* Call top-level *)
	in string_of_arithmetic_expression customized_string

and customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names =
    let rec string_of_int_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Int_plus (discrete_arithmetic_expression, Int_factor (Int_constant c))
        | Int_minus (discrete_arithmetic_expression, Int_factor (Int_constant c)) when Int32.equal c Int32.zero ->
            string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression

	| Int_plus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ customized_string.arithmetic_string.plus_string ^ "\", "
            ^ "\"left\" : " ^ (string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ", " (*OTDO remove space for +*)
            ^ "\"right\" : " ^ (string_of_int_term customized_string discrete_term) ^ "}"

	| Int_minus (discrete_arithmetic_expression, discrete_term) ->
		"{ \"op\": \"" ^ customized_string.arithmetic_string.minus_string ^ "\", "
            ^ "\"left\" : " ^ (string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression) ^ ", " (*OTDO remove space for +*)
            ^ "\"right\" : " ^ (string_of_int_term customized_string discrete_term) ^ "}"

        | Int_term discrete_term -> string_of_int_term customized_string discrete_term

	and string_of_int_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| Int_mul (Int_factor (Int_constant c), discrete_factor) when Int32.equal c Int32.one ->
			string_of_int_factor customized_string discrete_factor
		| Int_mul (discrete_term, discrete_factor) as expr ->
            add_left_parenthesis_int discrete_term (
                (string_of_int_term customized_string discrete_term)
            )
            ^ customized_string.arithmetic_string.mul_string
            ^
            (add_right_parenthesis_int (
                string_of_int_factor customized_string discrete_factor
            ) expr)

		| Int_div (discrete_term, discrete_factor) as expr ->
            add_left_parenthesis_int discrete_term (
                (string_of_int_term customized_string discrete_term)
            )
            ^ customized_string.arithmetic_string.div_string
            ^
            (add_right_parenthesis_int (
                string_of_int_factor customized_string discrete_factor
            ) expr)

		| Int_factor discrete_factor -> string_of_int_factor customized_string discrete_factor

	and string_of_int_factor customized_string = function
		| Int_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
		| Int_constant value -> Int32.to_string value
		| Int_unary_min discrete_factor ->
		    customized_string.arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus_int (
		         (string_of_int_factor customized_string discrete_factor)
		    ) discrete_factor
		| Int_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression
        | Int_pow (expr, exp) as factor ->
            "{\"op\": \""
            ^ string_of_int_factor_constructor factor
            ^ "\", \"left\":"
            ^ string_of_int_arithmetic_expression customized_string expr
            ^ ", \"right\":"
            ^ string_of_int_arithmetic_expression customized_string exp
            ^ "}"
	(* Call top-level *)
	in string_of_int_arithmetic_expression customized_string

and customized_string_of_binary_word_expression_for_jani customized_string variable_names = function
    | Logical_shift_left (binary_word, expr) as binary_word_expression ->
        "{\"op\": \""
        ^ string_of_binary_word_expression_constructor binary_word_expression
        ^ "\", \"left\":"
        ^ customized_string_of_binary_word_expression_for_jani customized_string variable_names binary_word
        ^ ", \"right\":"
        ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr
        ^ "}"
    | Logical_shift_right (binary_word, expr) as binary_word_expression ->
        "{\"op\": \""
        ^ string_of_binary_word_expression_constructor binary_word_expression
        ^ "\", \"left\":"
        ^ customized_string_of_binary_word_expression_for_jani customized_string variable_names binary_word
        ^ ", \"right\":"
        ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr
        ^ "}"
    | Binary_word_constant value -> BinaryWord.string_of_binaryword value
    | Binary_word_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""

let string_of_arithmetic_expression_for_jani = customized_string_of_arithmetic_expression_for_jani Constants.global_default_string
let string_of_discrete_boolean_expression_for_jani = customized_string_of_discrete_boolean_expression_for_jani Constants.global_default_string
