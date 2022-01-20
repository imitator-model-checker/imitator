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
    | List_expression of list_expression

and discrete_arithmetic_expression =
    | Rational_arithmetic_expression of rational_arithmetic_expression
    | Int_arithmetic_expression of int_arithmetic_expression

(****************************************************************)
(** Rational arithmetic expressions for discrete variables *)
(****************************************************************)
and rational_arithmetic_expression =
	| Rational_plus of rational_arithmetic_expression * rational_term
	| Rational_minus of rational_arithmetic_expression * rational_term
	| Rational_term of rational_term

and rational_term =
	| Rational_mul of rational_term * rational_factor
	| Rational_div of rational_term * rational_factor
	| Rational_factor of rational_factor

and rational_factor =
	| Rational_variable of Automaton.variable_index
	| Rational_constant of NumConst.t
    | Rational_access of expression_access_type * int_arithmetic_expression
	| Rational_expression of rational_arithmetic_expression

	| Rational_unary_min of rational_factor

    | Rational_of_int of int_arithmetic_expression
    | Rational_pow of rational_arithmetic_expression * int_arithmetic_expression
    | Rational_list_hd of list_expression
(*    | Rational_function_call of string * global_expression list*)


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
    | Int_access of expression_access_type * int_arithmetic_expression
    | Int_pow of int_arithmetic_expression * int_arithmetic_expression
    | Int_list_hd of list_expression
    | Array_length of array_expression
    | List_length of list_expression
(*    | Int_function_call of string * global_expression list*)

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
	| List_comparison of list_expression * relop * list_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression
	(** Boolean expression of the form Expr ~ Expr, with ~ = { &, | } or not (Expr) *)
	| Boolean_expression of boolean_expression
	(** Boolean expression of the form not(Expr ~ Expr), with ~ = { &, | }*)
	| Not_bool of boolean_expression (** Negation *)
	(** Discrete boolean variable *)
	| Bool_variable of Automaton.variable_index
	(** Discrete boolean constant *)
	| Bool_constant of bool
	(** Access to a boolean array **)
    | Bool_access of expression_access_type * int_arithmetic_expression
    | Bool_list_hd of list_expression
    (* Function list_mem *)
    | List_mem of global_expression * list_expression
    (* Function array_mem *)
    | Array_mem of global_expression * array_expression

(*    | Bool_function_call of string * global_expression list*)


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
    | Binary_word_access of expression_access_type * int_arithmetic_expression * int
    (* Add here some functions *)
    | Binary_word_list_hd of list_expression
(*    | Binary_word_function_call of string * global_expression list*)

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
    | Array_access of expression_access_type * int_arithmetic_expression
    (* Add here some function on array *)
    | Array_concat of array_expression * array_expression
    | Array_list_hd of list_expression
(*    | Array_function_call of string * global_expression list*)

(** List expression **)
and list_expression =
    | Literal_list of global_expression list
    | List_constant of DiscreteValue.discrete_value list
    | List_variable of Automaton.variable_index
    | List_access of expression_access_type * int_arithmetic_expression
    | List_cons of global_expression * list_expression
    | List_list_hd of list_expression
	| List_list_tl of list_expression
	| List_rev of list_expression
(*    | List_function_call of string * global_expression list*)

and expression_access_type =
    | Expression_array_access of array_expression
    | Expression_list_access of list_expression

type discrete_variable_access =
    | Discrete_variable_index of Automaton.discrete_index
    | Discrete_variable_access of discrete_variable_access * int_arithmetic_expression


(** Check linearity of a discrete expression **)

let rec is_variable_rational_arithmetic_expression = function
    | Rational_plus _
    | Rational_minus _ -> false
    | Rational_term term ->
        is_variable_rational_term term

and is_variable_rational_term = function
    | Rational_mul _
    | Rational_div _ -> false
    | Rational_factor factor -> is_variable_rational_factor factor

and is_variable_rational_factor = function
    | Rational_variable _ -> true
    | Rational_unary_min factor -> is_variable_rational_factor factor
    | Rational_expression expr -> is_variable_rational_arithmetic_expression expr
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
    | Bool_constant _ -> true
    | _ -> false

and is_linear_arithmetic_expression = function
    | Rational_arithmetic_expression expr -> is_linear_rational_arithmetic_expression expr
    | Int_arithmetic_expression expr -> false

and is_linear_rational_arithmetic_expression = function
    | Rational_plus (expr, term) ->
        is_linear_rational_arithmetic_expression expr &&
        is_linear_rational_term term
    | Rational_minus (expr, term) ->
        is_linear_rational_arithmetic_expression expr &&
        is_linear_rational_term term
    | Rational_term term ->
        is_linear_rational_term term

and is_linear_rational_term = function
    | Rational_mul (term, factor) ->
        (* Two variable ? false, otherwise true *)
        not (is_variable_rational_term term && is_variable_rational_factor factor)
    | Rational_div _ -> false
    | Rational_factor factor -> is_linear_rational_factor factor

and is_linear_rational_factor = function
    | Rational_variable _
    | Rational_constant _ -> true
    | Rational_unary_min factor -> is_linear_rational_factor factor
    | Rational_expression expr -> is_linear_rational_arithmetic_expression expr
    | _ -> false

(****************************************************************)
(** Strings *)
(****************************************************************)

(* Check if a discrete term factor of an arithmetic expression should have parenthesis *)
let is_discrete_factor_has_parenthesis = function
    | Rational_unary_min _
    | Rational_expression(Rational_plus _)
    | Rational_expression(Rational_minus _) -> true
    | _ -> false

(* Check if discrete factor is a multiplication *)
let is_discrete_factor_is_mul = function
    | Rational_expression(Rational_term(Rational_mul _)) -> true
    | _ -> false

(* Check if a left expression should have parenthesis *)
(* is (x + y) * z *)
(* or (x - y) * z *)
(* or (x + y) / z *)
(* or (x - y) / z *)
let is_left_expr_has_parenthesis = function
    | Rational_factor factor -> is_discrete_factor_has_parenthesis factor
    | _ -> false

(* Check if a right expression should have parenthesis *)
(* is x * (y + z) *)
(* or x * (y - z) *)
(* or x / (y + z) *)
(* or x / (y - z) *)
(* or x / (y * z) *)
let is_right_expr_has_parenthesis = function
    (* check x / (y * z) *)
    | Rational_div (discrete_term, discrete_factor) when is_discrete_factor_is_mul discrete_factor -> true
    (* check x / (y + z) or x / (y - z) *)
    | Rational_div (discrete_term, discrete_factor)
    (* check x * (y + z) or x * (y - z) *)
    | Rational_mul (discrete_term, discrete_factor) -> is_discrete_factor_has_parenthesis discrete_factor
    | _ -> false

let add_left_parenthesis expr str =
    if is_left_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

let add_right_parenthesis str expr =
    if is_right_expr_has_parenthesis expr then "(" ^ str ^ ")" else str

let add_parenthesis_to_unary_minus str = function
    | Rational_expression _ -> "(" ^ str ^ ")"
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

let label_of_bool_factor = function
	| Expression _
    | Boolean_comparison _
    | Binary_comparison _
    | Array_comparison _
    | List_comparison _ -> "bool comparison"
	| Expression_in _ -> "in expression"
	| Boolean_expression _ -> "bool expression"
	| Not_bool _ -> "bool negation expression"
	| Bool_variable _ -> "bool variable"
	| Bool_constant _ -> "bool constant"
    | Bool_access _ -> "bool access"
    | Bool_list_hd _ -> "list_hd"
    | List_mem _ -> "list_mem"
    | Array_mem _ -> "array_mem"

let label_of_rational_factor = function
	| Rational_variable _ -> "rational variable"
	| Rational_constant _ -> "rational constant"
	| Rational_expression _ -> "rational expression"
	| Rational_unary_min _ -> "rational minus"
	| Rational_of_int _ -> "rational_of_int"
	| Rational_pow _ -> "pow"
	| Rational_access _ -> "rational access"
	| Rational_list_hd _ -> "list_hd"

let label_of_int_factor = function
	| Int_variable _ -> "int variable"
	| Int_constant _ -> "int constant"
	| Int_expression _ -> "int expression"
	| Int_unary_min _ -> "int minus"
	| Int_pow _ -> "pow"
	| Int_access _ -> "int access"
	| Int_list_hd _ -> "list_hd"
	| Array_length _ -> "array_length"
	| List_length _ -> "list_length"

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
    | Binary_word_access _ -> "binary word access"
    | Binary_word_list_hd _ -> "list_hd"

let label_of_array_expression = function
    | Literal_array _ -> "literal array"
    | Array_constant _ -> "array"
    | Array_variable _ -> "array"
    | Array_access _ -> "array access"
    | Array_concat _ -> "array_append"
    | Array_list_hd _ -> "list_hd"

let label_of_list_expression = function
    | Literal_list _ -> "literal list"
    | List_constant _ -> "list"
    | List_variable _ -> "list"
    | List_access _ -> "list access"
    | List_cons _ -> "list_cons"
    | List_list_hd _ -> "list_hd"
    | List_list_tl _ -> "list_tl"
    | List_rev _ -> "list_rev"

(* Check if a binary word encoded on an integer have length greater than 31 bits *)
(* If it's the case, print a warning *)
let print_binary_word_overflow_warning_if_needed expr length = function
    | Binary_word_representation_standard -> ()
    | Binary_word_representation_int ->
        if length > 31 then
            ImitatorUtilities.print_warning ("Encoding a `" ^ label_of_binary_word_expression expr ^ "` of length `" ^ string_of_int length ^ "` on an integer can leads to an overflow.")

(* Expressions strings *)

let print_function function_name str_arguments = function_name ^ "(" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_arguments ^ ")"

let rec customized_string_of_global_expression customized_string variable_names = function
    | Arithmetic_expression expr -> customized_string_of_arithmetic_expression customized_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression customized_string variable_names expr
    | Binary_word_expression expr -> customized_string_of_binary_word_expression customized_string variable_names expr
    | Array_expression expr -> customized_string_of_array_expression customized_string variable_names expr
    | List_expression expr -> customized_string_of_list_expression customized_string variable_names expr

(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
and customized_string_of_arithmetic_expression customized_string variable_names = function
    | Rational_arithmetic_expression expr -> customized_string_of_rational_arithmetic_expression customized_string variable_names expr
    | Int_arithmetic_expression expr -> customized_string_of_int_arithmetic_expression customized_string variable_names expr

and customized_string_of_rational_arithmetic_expression customized_string variable_names =
    let rec string_of_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Rational_plus (discrete_arithmetic_expression, Rational_factor (Rational_constant c))
        | Rational_minus (discrete_arithmetic_expression, Rational_factor (Rational_constant c)) when NumConst.equal c NumConst.zero ->
            string_of_arithmetic_expression customized_string discrete_arithmetic_expression

		| Rational_plus (discrete_arithmetic_expression, discrete_term) ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_arithmetic_string.plus_string
            ^ (string_of_term customized_string discrete_term)
		| Rational_minus (discrete_arithmetic_expression, discrete_term) ->
            (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
            ^ Constants.default_arithmetic_string.minus_string
            ^ (string_of_term customized_string discrete_term)
        | Rational_term discrete_term -> string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| Rational_mul (Rational_factor (Rational_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
			string_of_factor customized_string discrete_factor
		| Rational_mul (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
		)
        ^ Constants.default_arithmetic_string.mul_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| Rational_div (discrete_term, discrete_factor) as expr ->
		add_left_parenthesis discrete_term (
			(string_of_term customized_string discrete_term)
        )
        ^ Constants.default_arithmetic_string.div_string
        ^
        (add_right_parenthesis (
            string_of_factor customized_string discrete_factor
        ) expr)

		| Rational_factor discrete_factor -> string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| Rational_variable discrete_index -> variable_names discrete_index
		| Rational_constant value -> NumConst.to_string value
		| Rational_access (access_type, index_expr) ->
		    string_of_expression_access customized_string variable_names access_type index_expr

		| Rational_unary_min discrete_factor ->
		    Constants.default_arithmetic_string.unary_min_string ^
		    add_parenthesis_to_unary_minus (
		         (string_of_factor customized_string discrete_factor)
		    ) discrete_factor
		| Rational_of_int discrete_arithmetic_expression as factor ->
            print_function
                (label_of_rational_factor factor)
                [customized_string_of_int_arithmetic_expression customized_string variable_names discrete_arithmetic_expression]
        | Rational_pow (expr, exp) as factor ->
            print_function
                (label_of_rational_factor factor)
                [
                    string_of_arithmetic_expression customized_string expr;
                    customized_string_of_int_arithmetic_expression customized_string variable_names exp
                ]
        | Rational_list_hd list_expr as factor ->
            print_function
                (label_of_rational_factor factor)
                [customized_string_of_list_expression customized_string variable_names list_expr]
		| Rational_expression discrete_arithmetic_expression ->
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
        | Int_access (access_type, index_expr) ->
            string_of_expression_access customized_string variable_names access_type index_expr
        | Int_pow (expr, exp) as func ->
            print_function
                (label_of_int_factor func)
                [
                    string_of_int_arithmetic_expression customized_string expr;
                    string_of_int_arithmetic_expression customized_string exp
                ]
        | Int_list_hd list_expr as func ->
            print_function
                (label_of_int_factor func)
                [customized_string_of_list_expression customized_string variable_names list_expr]
        | Array_length array_expr as func ->
            print_function
                (label_of_int_factor func)
                [customized_string_of_array_expression customized_string variable_names array_expr]
        | List_length list_expr as func ->
            print_function
                (label_of_int_factor func)
                [customized_string_of_list_expression customized_string variable_names list_expr]
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
    | List_comparison (l_expr, relop, r_expr) ->
        customized_string_of_list_expression customized_string variable_names l_expr
        ^ customized_string_of_boolean_operations customized_string.boolean_string relop
        ^ customized_string_of_list_expression customized_string variable_names r_expr
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
        "(" ^ customized_string_of_boolean_expression customized_string variable_names boolean_expression ^ ")"
	| Not_bool b ->
	    customized_string.boolean_string.not_operator ^ " (" ^ (customized_string_of_boolean_expression customized_string variable_names b) ^ ")"
    | Bool_variable discrete_index -> variable_names discrete_index
    | Bool_constant value -> customized_string_of_bool_value customized_string.boolean_string value
    | Bool_access (access_type, index_expr) ->
        string_of_expression_access customized_string variable_names access_type index_expr
    | Bool_list_hd list_expr as func ->
        print_function
            (label_of_bool_factor func)
            [customized_string_of_list_expression customized_string variable_names list_expr]
    | List_mem (expr, list_expr) as func ->
        print_function
            (label_of_bool_factor func)
            [
                customized_string_of_global_expression customized_string variable_names expr;
                customized_string_of_list_expression customized_string variable_names list_expr
            ]
    | Array_mem (expr, array_expr) as func ->
        print_function
            (label_of_bool_factor func)
            [
                customized_string_of_global_expression customized_string variable_names expr;
                customized_string_of_array_expression customized_string variable_names array_expr
            ]

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
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;

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

    | Logical_shift_right (binary_word, expr, length)
    | Logical_fill_left (binary_word, expr, length)
    | Logical_fill_right (binary_word, expr, length) as binary_word_expression ->
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;

        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names binary_word
        ^ ", "
        ^ customized_string_of_int_arithmetic_expression customized_string variable_names expr
        ^ ")"

    | Logical_and (l_binary_word, r_binary_word, length)
    | Logical_or (l_binary_word, r_binary_word, length)
    | Logical_xor (l_binary_word, r_binary_word, length) as binary_word_expression ->
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;

        label_of_binary_word_expression binary_word_expression
        ^ "("
        ^ customized_string_of_binary_word_expression customized_string variable_names l_binary_word
        ^ ", "
        ^ customized_string_of_binary_word_expression customized_string variable_names r_binary_word
        ^ ")"
    | Logical_not (binary_word, length) as binary_word_expression ->
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;

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
    | Binary_word_constant value as binary_word_expression ->

        let length = BinaryWord.length value in
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;

        (match customized_string.binary_word_representation with
        | Binary_word_representation_standard -> BinaryWord.string_of_binaryword value
        | Binary_word_representation_int -> string_of_int (BinaryWord.to_int value)
        )

    | Binary_word_variable (variable_index, length) as binary_word_expression ->
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;
        variable_names variable_index

    | Binary_word_access (access_type, index_expr, length) as binary_word_expression ->
        print_binary_word_overflow_warning_if_needed binary_word_expression length customized_string.binary_word_representation;
        string_of_expression_access customized_string variable_names access_type index_expr
    | Binary_word_list_hd list_expr as func ->
        print_function
            (label_of_binary_word_expression func)
            [customized_string_of_list_expression customized_string variable_names list_expr]

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
    | Array_access (access_type, index_expr) ->
        string_of_expression_access customized_string variable_names access_type index_expr

    | Array_concat (array_expr_0, array_expr_1) as func ->
        label_of_array_expression func
        ^ "("
        ^ customized_string_of_array_expression customized_string variable_names array_expr_0
        ^ ", "
        ^ customized_string_of_array_expression customized_string variable_names array_expr_1
        ^ ")"
    | Array_list_hd list_expr as func ->
        print_function
            (label_of_array_expression func)
            [customized_string_of_list_expression customized_string variable_names list_expr]

and customized_string_of_list_expression customized_string variable_names = function
    | Literal_list expr_list as list_expr ->
        let str_expr = List.map (customized_string_of_global_expression customized_string variable_names) expr_list in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        label_of_list_expression list_expr
        ^ "(" ^ l_delimiter ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_expr ^ r_delimiter ^ ")"
    | List_constant values as list_expr ->
        let str_values = List.map DiscreteValue.string_of_value values in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        label_of_list_expression list_expr
        ^ "(" ^ l_delimiter ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_values ^ r_delimiter ^ ")"
    | List_variable variable_index -> variable_names variable_index
    | List_access (access_type, index_expr) ->
        string_of_expression_access customized_string variable_names access_type index_expr
    | List_cons (global_expression, list_expr) as func ->
        label_of_list_expression func
        ^ "("
        ^ customized_string_of_global_expression customized_string variable_names global_expression
        ^ ", "
        ^ customized_string_of_list_expression customized_string variable_names list_expr
        ^ ")"
    | List_list_hd list_expr
    | List_list_tl list_expr
    | List_rev list_expr as func ->
        label_of_list_expression func
        ^ "("
        ^ customized_string_of_list_expression customized_string variable_names list_expr
        ^ ")"

and string_of_expression_of_access customized_string variable_names = function
    | Expression_array_access array_expr ->
        customized_string_of_array_expression customized_string variable_names array_expr
    | Expression_list_access list_expr ->
        customized_string_of_list_expression customized_string variable_names list_expr

and string_of_expression_access customized_string variable_names access_type index_expr =
    string_of_expression_of_access customized_string variable_names access_type ^ "[" ^ customized_string_of_int_arithmetic_expression customized_string variable_names index_expr ^ "]"

let string_of_global_expression = customized_string_of_global_expression Constants.global_default_string
let string_of_arithmetic_expression = customized_string_of_arithmetic_expression Constants.global_default_string
let string_of_int_arithmetic_expression = customized_string_of_int_arithmetic_expression Constants.global_default_string
let string_of_boolean_expression = customized_string_of_boolean_expression Constants.global_default_string
let string_of_discrete_boolean_expression = customized_string_of_discrete_boolean_expression Constants.global_default_string
let string_of_array_expression = customized_string_of_array_expression Constants.global_default_string
let string_of_list_expression = customized_string_of_list_expression Constants.global_default_string

let rec string_of_discrete_variable_access variable_names = function
    | Discrete_variable_index discrete_index ->
        variable_names discrete_index
    | Discrete_variable_access (variable_access, index_expr) ->
        string_of_discrete_variable_access variable_names variable_access
        ^ "["
        ^ string_of_int_arithmetic_expression variable_names index_expr
        ^ "]"