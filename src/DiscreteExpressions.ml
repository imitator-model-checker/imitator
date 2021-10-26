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
(** Global expression *)
(****************************************************************)
type global_expression =
    (* A typed expression *)
    | Arithmetic_expression of discrete_arithmetic_expression
    | Bool_expression of boolean_expression
    | Binary_word_expression of binary_word_expression
    | Array_expression of array_expression

and discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(****************************************************************)
(** Rational arithmetic expressions for discrete variables *)
(****************************************************************)
and rational_arithmetic_expression =
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
    | Rational_array_access of array_expression * int_arithmetic_expression
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
    | Int_array_access of array_expression * int_arithmetic_expression

(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

(** Boolean expression *)
and boolean_expression =
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
	| Array_comparison of array_expression * relop * array_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Parsed boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Parsed boolean expression of the form not(Expr ~ Expr), with ~ = { &, | }*)
	| Not_bool of boolean_expression (** Negation *)
	(** Discrete variable *)
	| DB_variable of Automaton.variable_index
	(** Discrete constant *)
	| DB_constant of bool
	(** access to a boolean array **)
    | Bool_array_access of array_expression * int_arithmetic_expression


(************************************************************)
(************************************************************)
(************************************************************)
(** Binary word expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Binary word expression *)
and binary_word_expression =
    | Logical_shift_left of binary_word_expression * int_arithmetic_expression * int
    | Logical_shift_right of binary_word_expression * int_arithmetic_expression * int
    | Logical_fill_left of binary_word_expression * int_arithmetic_expression * int
    | Logical_fill_right of binary_word_expression * int_arithmetic_expression * int
    | Logical_and of binary_word_expression * binary_word_expression * int
    | Logical_or of binary_word_expression * binary_word_expression * int
    | Logical_xor of binary_word_expression * binary_word_expression * int
    | Logical_not of binary_word_expression * int
    | Binary_word_constant of BinaryWord.t
    | Binary_word_variable of Automaton.variable_index * int
    | Binary_word_array_access of array_expression * int_arithmetic_expression * int

(************************************************************)
(************************************************************)
(************************************************************)
(** Array expressions for discrete variables *)
(************************************************************)
(************************************************************)

(** Array expression *)
and array_expression =
    | Literal_array of global_expression array
    | Array_constant of DiscreteValue.discrete_value array
    | Array_variable of Automaton.variable_index
    | Array_array_access of array_expression * int_arithmetic_expression
    (* Add here some function on array *)
    | Array_concat of array_expression * array_expression

type discrete_variable_access =
    | Discrete_variable_index of Automaton.discrete_index
    | Discrete_variable_access of discrete_variable_access * int_arithmetic_expression


(** Check linearity of a discrete expression **)

let rec is_variable_rational_arithmetic_expression = function
    | DAE_plus _
    | DAE_minus _ -> false
    | DAE_term term ->
        is_variable_rational_term term

and is_variable_rational_term = function
    | DT_mul _
    | DT_div _ -> false
    | DT_factor factor -> is_variable_rational_factor factor

and is_variable_rational_factor = function
    | DF_variable _ -> true
    | DF_unary_min factor -> is_variable_rational_factor factor
    | DF_expression expr -> is_variable_rational_arithmetic_expression expr
    | _ -> false

let rec is_linear_global_expression = function
    | Arithmetic_expression expr -> is_linear_arithmetic_expression expr
    | Bool_expression expr -> is_linear_boolean_expression expr
    | _ -> false

and is_linear_boolean_expression = function
	| True_bool
	| False_bool
	| And_bool _ -> true
	| Or_bool _ -> false
	| Discrete_boolean_expression expr ->
		is_linear_discrete_boolean_expression expr

and is_linear_discrete_boolean_expression = function
	| Expression (l_expr, _, r_expr) ->
	    is_linear_arithmetic_expression l_expr &&
	    is_linear_arithmetic_expression r_expr
	| Expression_in (expr_1, expr_2, expr_3) ->
	    is_linear_arithmetic_expression expr_1 && (
	        is_linear_arithmetic_expression expr_2 &&
	        is_linear_arithmetic_expression expr_3
	    )
    | DB_constant _ -> true
    | _ -> false

and is_linear_arithmetic_expression = function
    | Rational_arithmetic_expression expr -> is_linear_rational_arithmetic_expression expr
    | Int_arithmetic_expression expr -> false

and is_linear_rational_arithmetic_expression = function
    | DAE_plus (expr, term) ->
        is_linear_rational_arithmetic_expression expr &&
        is_linear_rational_term term
    | DAE_minus (expr, term) ->
        is_linear_rational_arithmetic_expression expr &&
        is_linear_rational_term term
    | DAE_term term ->
        is_linear_rational_term term

and is_linear_rational_term = function
    | DT_mul (term, factor) ->
        (* Two variable ? false, otherwise true *)
        not (is_variable_rational_term term && is_variable_rational_factor factor)
    | DT_div _ -> false
    | DT_factor factor -> is_linear_rational_factor factor

and is_linear_rational_factor = function
    | DF_variable _
    | DF_constant _ -> true
    | DF_unary_min factor -> is_linear_rational_factor factor
    | DF_expression expr -> is_linear_rational_arithmetic_expression expr
    | Rational_array_access _
    | DF_rational_of_int _
    | DF_pow _ -> false

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

let label_of_rational_factor = function
	| DF_variable _ -> "rational variable"
	| DF_constant _ -> "rational constant"
	| DF_expression _ -> "rational expression"
	| DF_unary_min _ -> "rational minus"
	| DF_rational_of_int _ -> "rational_of_int"
	| DF_pow _ -> "pow"
	| Rational_array_access _ -> "rational array access"

let label_of_int_factor = function
	| Int_variable _ -> "int variable"
	| Int_constant _ -> "int constant"
	| Int_expression _ -> "int expression"
	| Int_unary_min _ -> "int minus"
	| Int_pow _ -> "pow"
	| Int_array_access _ -> "int array access"

let label_of_binary_word_expression = function
    | Logical_shift_left _ -> "shift_left"
    | Logical_shift_right _ -> "shift_right"
    | Logical_fill_left _ -> "fill_left"
    | Logical_fill_right _ -> "fill_right"
    | Logical_and _ -> "logand"
    | Logical_or _ -> "logor"
    | Logical_xor _ -> "logxor"
    | Logical_not _ -> "lognot"
    | Binary_word_constant _ -> "binary word constant"
    | Binary_word_variable _ -> "binary word variable"
    | Binary_word_array_access _ -> "binary word array access"

let label_of_array_expression = function
    | Literal_array _ -> "literal array"
    | Array_constant _ -> "array constant"
    | Array_variable _ -> "array variable"
    | Array_array_access _ -> "array_access"
    | Array_concat _ -> "array_concat"

(* Expressions strings *)

let rec customized_string_of_global_expression customized_string variable_names = function
    | Arithmetic_expression expr -> customized_string_of_arithmetic_expression customized_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression customized_string variable_names expr
    | Binary_word_expression expr -> customized_string_of_binary_word_expression customized_string variable_names expr
    | Array_expression expr -> customized_string_of_array_expression customized_string variable_names expr

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
		| Rational_array_access (array_expr, index_expr) ->
		    customized_string_of_array_expression customized_string variable_names array_expr
		    ^ "["
		    ^ customized_string_of_int_arithmetic_expression customized_string variable_names index_expr
		    ^ "]"
		| DF_unary_min discrete_factor ->
		    Constants.default_arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| DF_rational_of_int discrete_arithmetic_expression as factor ->
		    label_of_rational_factor factor
		    ^ "("
		    ^ customized_string_of_int_arithmetic_expression customized_string variable_names discrete_arithmetic_expression
		    ^ ")"
        | DF_pow (expr, exp) as factor ->
            label_of_rational_factor factor
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
            label_of_int_factor factor
            ^ "("
            ^ string_of_int_arithmetic_expression customized_string expr
            ^ ", "
            ^ string_of_int_arithmetic_expression customized_string exp
            ^ ")"
        | Int_array_access (array_expr, index_expr) ->
            customized_string_of_array_expression customized_string variable_names array_expr
            ^ "["
            ^ string_of_int_arithmetic_expression customized_string index_expr
            ^ "]"
	(* Call top-level *)
	in string_of_int_arithmetic_expression customized_string

(** Convert a Boolean expression into a string *)
and customized_string_of_boolean_expression customized_string variable_names = function
	| True_bool -> customized_string.boolean_string.true_string
	| False_bool -> customized_string.boolean_string.false_string
	| And_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ customized_string.boolean_string.and_operator
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Or_bool (b1, b2) ->
		(customized_string_of_boolean_expression customized_string variable_names b1)
		^ customized_string.boolean_string.or_operator
		^ (customized_string_of_boolean_expression customized_string variable_names b2)
	| Discrete_boolean_expression discrete_boolean_expression ->
		customized_string_of_discrete_boolean_expression customized_string variable_names discrete_boolean_expression

(** Convert a discrete_boolean_expression into a string *)
and customized_string_of_discrete_boolean_expression customized_string variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression1, relop, discrete_arithmetic_expression2) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ (customized_string_of_boolean_operations customized_string.boolean_string relop)
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
    | Boolean_comparison (l_expr, relop, r_expr) ->
		(customized_string_of_discrete_boolean_expression customized_string variable_names l_expr)
		^ (customized_string_of_boolean_operations customized_string.boolean_string relop)
		^ (customized_string_of_discrete_boolean_expression customized_string variable_names r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
		(customized_string_of_binary_word_expression customized_string variable_names l_expr)
		^ (customized_string_of_boolean_operations customized_string.boolean_string relop)
		^ (customized_string_of_binary_word_expression customized_string variable_names r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        customized_string_of_array_expression customized_string variable_names l_expr
        ^ customized_string_of_boolean_operations customized_string.boolean_string relop
        ^ customized_string_of_array_expression customized_string variable_names r_expr
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		(customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression1)
		^ customized_string.boolean_string.in_operator
		^ "["
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression2)
		^ " , "
		^ (customized_string_of_arithmetic_expression customized_string variable_names discrete_arithmetic_expression3)
		^ "]"
    | Boolean_expression boolean_expression ->
        "(" ^ (customized_string_of_boolean_expression customized_string variable_names boolean_expression) ^ ")"
	| Not_bool b ->
	    customized_string.boolean_string.not_operator ^ " (" ^ (customized_string_of_boolean_expression customized_string variable_names b) ^ ")"
    | DB_variable discrete_index -> variable_names discrete_index
    | DB_constant value -> customized_string_of_bool_value customized_string.boolean_string value
    | Bool_array_access (array_expr, index_expr) ->
        customized_string_of_array_expression customized_string variable_names array_expr
        ^ "["
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names index_expr
        ^ "]"

and customized_string_of_boolean_operations customized_string = function
	| OP_L		-> customized_string.l_operator
	| OP_LEQ	-> customized_string.le_operator
	| OP_EQ		-> customized_string.eq_operator
	| OP_NEQ	-> customized_string.neq_operator
	| OP_GEQ	-> customized_string.ge_operator
	| OP_G		-> customized_string.g_operator

and customized_string_of_bool_value customized_string = function
    | true -> customized_string.true_string
    | false -> customized_string.false_string

and customized_string_of_binary_word_expression customized_string variable_names = function
    | Logical_shift_left (binary_word, expr, length) as binary_word_expression ->
        let length_arg =
            match customized_string.binary_word_representation with
            | Binary_word_representation_standard -> ""
            | Binary_word_representation_int -> ", " ^ string_of_int length
        in
        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ ", "
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names expr
        ^ length_arg
        ^ ")"

    | Logical_shift_right (binary_word, expr, _)
    | Logical_fill_left (binary_word, expr, _)
    | Logical_fill_right (binary_word, expr, _) as binary_word_expression ->
        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ ", "
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names expr
        ^ ")"

    | Logical_and (l_binary_word, r_binary_word, _)
    | Logical_or (l_binary_word, r_binary_word, _)
    | Logical_xor (l_binary_word, r_binary_word, _) as binary_word_expression ->
        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names l_binary_word
        ^ ", "
        ^ customized_string_of_binary_word_expression customized_string variable_names r_binary_word
        ^ ")"
    | Logical_not (binary_word, length) as binary_word_expression ->
        let length_arg =
            match customized_string.binary_word_representation with
            | Binary_word_representation_standard -> ""
            | Binary_word_representation_int -> ", " ^ string_of_int length
        in
        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ length_arg
        ^ ")"
    | Binary_word_constant value ->
        (match customized_string.binary_word_representation with
        | Binary_word_representation_standard -> BinaryWord.string_of_binaryword value
        | Binary_word_representation_int -> string_of_int (BinaryWord.to_int value)
        )

    | Binary_word_variable (variable_index, _) -> variable_names variable_index
    | Binary_word_array_access (array_expr, index_expr, _) ->
        customized_string_of_array_expression customized_string variable_names array_expr
        ^ "["
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names index_expr
        ^ "]"

and customized_string_of_array_expression customized_string variable_names = function
    | Literal_array expr_array ->
        let str_expr = Array.map (customized_string_of_global_expression customized_string variable_names) expr_array in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        l_delimiter ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_expr ^ r_delimiter
    | Array_constant values ->
        let str_values = Array.map DiscreteValue.string_of_value values in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        l_delimiter ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_values ^ r_delimiter
    | Array_variable variable_index -> variable_names variable_index
    | Array_array_access (array_expr, index_expr) ->
        customized_string_of_array_expression customized_string variable_names array_expr
        ^ "["
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names index_expr
        ^ "]"
    | Array_concat (array_expr_0, array_expr_1) as func ->
        label_of_array_expression func
        ^ "("
        ^ customized_string_of_array_expression customized_string variable_names array_expr_0
        ^ ", "
        ^ customized_string_of_array_expression customized_string variable_names array_expr_1
        ^ ")"



let string_of_global_expression = customized_string_of_global_expression Constants.global_default_string
let string_of_arithmetic_expression = customized_string_of_arithmetic_expression Constants.global_default_string
let string_of_int_arithmetic_expression = customized_string_of_int_arithmetic_expression Constants.global_default_string
let string_of_boolean_expression = customized_string_of_boolean_expression Constants.global_default_string
let string_of_discrete_boolean_expression = customized_string_of_discrete_boolean_expression Constants.global_default_string
let string_of_array_expression = customized_string_of_array_expression Constants.global_default_string

let rec string_of_discrete_variable_access variable_names = function
    | Discrete_variable_index discrete_index ->
        variable_names discrete_index
    | Discrete_variable_access (variable_access, index_expr) ->
        string_of_discrete_variable_access variable_names variable_access
        ^ "["
        ^ string_of_int_arithmetic_expression variable_names index_expr
        ^ "]"


