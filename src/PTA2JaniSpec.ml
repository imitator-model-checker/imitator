(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Translater to JaniSpec
 *
 * File contributors : Dylan Marinho
 * Created           : 2021/02/23
 * Last modified     : 2021/12/03
 *
 ************************************************************)

open Exceptions
open Constants
open OCamlUtilities
open ImitatorUtilities
open LinearConstraint
open DiscreteExpressions
open NonlinearConstraint
open AbstractModel
open DiscreteValue
open DiscreteType
open Result

(************************************************************)
(** Customized values for constraint conversion *)
(************************************************************)

(* Some Jani operators *)
let jani_separator = ","
let jani_assignment = "="

(* Some Jani metadata *)
let jani_version = "1"
let jani_type = "sha"

(* Jani boolean strings *)
let jani_boolean_strings : customized_boolean_string = {
	true_string   = "true";
	false_string  = "false";
	and_operator  = "∧";
	or_operator   = "∨"; (* useless *)
	l_operator    = "<";
	le_operator   = "≤";
	eq_operator   = "=";
	neq_operator   = "≠";
	ge_operator   = ">";
	g_operator    = "≥";
	not_operator  = "¬";
	in_operator   = ""; (* useless *)
}

(* All Jani custom strings *)
let jani_strings = {
    arithmetic_string = Constants.default_arithmetic_string_without_whitespace;
    boolean_string = jani_boolean_strings;
    array_string = Constants.default_array_string;
    binary_word_representation = Binary_word_representation_int;
}

(** Message and warning functions **)

(* Print and get a warning for undeclared functions in Jani *)
let undeclared_function_warning expression_label =
    (* Prepare and print warning message *)
    let message = "`" ^ expression_label ^ "` function is not implemented in Jani." in
    ImitatorUtilities.print_warning message;
    message

(** Formats **)

(* Json formats *)

let json_quoted str =
    "\"" ^ str ^ "\""

let json_property str_key str_value =
    json_quoted str_key ^ ":" ^ str_value

let json_struct str_properties =
    "{" ^ OCamlUtilities.string_of_array_of_string_with_sep jani_separator str_properties ^ "}"

let json_array str_values =
    "[" ^ OCamlUtilities.string_of_array_of_string_with_sep jani_separator str_values ^ "]"

let json_empty_array = "[]"

(* Jani formats *)

let jani_operator_dv =
    json_property "op" (json_quoted "dv")

let jani_operator_av =
    json_property "op" (json_quoted "av")

(* Format a Jani binary operator *)
let jani_binary_operator str_operator str_left str_right =
    json_struct [|
        json_property "op" (json_quoted str_operator);
        json_property "left" str_left;
        json_property "right" str_right
    |]

(* Format a Jani unary operator *)
let jani_unary_operator str_operator str_expr =
    json_struct [|
        json_property "op" (json_quoted str_operator);
        json_property "exp" str_expr
    |]

(* Format a Jani expression *)
let jani_expression str_expression =
    json_struct [|
        json_property "exp" str_expression
    |]

(* Format a Jani array value *)
let jani_array_value str_values =
    let str_array = OCamlUtilities.string_of_array_of_string_with_sep "," str_values in

    json_struct [|
        jani_operator_av;
        json_property "elements" (json_array [|
            str_array
        |])
    |]

(* Format a Jani array access *)
let jani_array_access str_array_expr str_index_expr =
    json_struct [|
        json_property "op" (json_quoted "aa");
        json_property "exp" str_array_expr;
        json_property "index" str_index_expr
    |]

(* Format a Jani custom datatype value *)
let jani_binary_word_datavalue binary_word =
    let array_value = BinaryWord.to_array (DiscreteValue.binary_word_value binary_word) in
    let str_values = Array.map (fun x -> if x then "true" else "false") array_value in

    json_struct [|
        jani_operator_dv;
        json_property "type" (json_quoted "binary_word");
        json_property "values" (json_array [|
            json_struct [|
                json_property "member" (json_quoted "elements");
                json_property "value" (jani_array_value str_values)
            |]
        |])
    |]

(* Format a Jani function call *)
let jani_function_call ?(str_comment="") str_function_name str_args =

    json_struct [|
        json_property "op" (json_quoted "call");
        json_property "function" (json_quoted str_function_name);
        json_property "args" (json_array str_args);
        json_property "comment" (json_quoted str_comment)
    |]

(* Format a Jani function declaration *)
let jani_function_declaration str_function_name str_type parameters str_body =
    json_struct [|
        json_property "name" (json_quoted str_function_name);
        json_property "type" (json_quoted str_type);
        json_property "parameters" (json_array parameters);
        json_property "body" str_body
    |]

(* Format a Jani function parameter *)
let jani_function_parameter str_parameter_name str_type =
    json_struct [|
        json_property "name" (json_quoted str_parameter_name);
        json_property "type" (json_quoted str_type)
    |]

(* Format array of the declared Jani features *)
let jani_features =
    json_array [|
        json_quoted "derived-operators";
        json_quoted "arrays";
        json_quoted "datatypes";
        json_quoted "functions";
    |]

(* Get string representation of a discrete value in Jani *)
let rec string_of_value = function

    | Number_value value
    | Rational_value value ->
        NumConst.jani_string_of_numconst value

    | Int_value value ->
        Int32.to_string value

    | Bool_value value ->
        if value then
            jani_strings.boolean_string.true_string
        else
            jani_strings.boolean_string.false_string

    | Array_value value ->
        let str_values = Array.map string_of_value value in
        jani_array_value str_values

    | List_value value ->
        let str_values = List.map string_of_value value in
        jani_array_value (Array.of_list str_values)

    | Stack_value value ->
        let str_values = Stack.fold (fun acc x -> acc @ [string_of_value x]) [] value in
        jani_array_value (Array.of_list str_values)

    | Queue_value value ->
        let str_values = Queue.fold (fun acc x -> acc @ [string_of_value x]) [] value in
        jani_array_value (Array.of_list str_values)

    | Binary_word_value value ->
        let bool_array = BinaryWord.to_array value in
        let str_values = Array.map (fun x -> if x then "true" else "false") bool_array in
        jani_array_value str_values



(************** Jani translation **************)
(* Convert an expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)

let string_of_comparison variable_names l_expr relop r_expr string_fun =
    jani_binary_operator
        (DiscreteExpressions.customized_string_of_boolean_operations jani_strings.boolean_string relop)
        (string_fun variable_names l_expr)
        (string_fun variable_names r_expr)

let rec string_of_global_expression variable_names = function
    | Arithmetic_expression expr -> string_of_arithmetic_expression variable_names expr
    | Bool_expression expr -> string_of_boolean_expression variable_names expr
    | Binary_word_expression expr -> string_of_binary_word_expression variable_names expr
    | Array_expression expr -> string_of_array_expression variable_names expr
    | List_expression expr -> string_of_list_expression variable_names expr
    | Stack_expression expr -> string_of_stack_expression variable_names expr
    | Queue_expression expr -> string_of_queue_expression variable_names expr

and string_of_boolean_expression variable_names = function
	| True_bool -> jani_strings.boolean_string.true_string
	| False_bool -> jani_strings.boolean_string.false_string
	| And_bool (b1, b2) ->
	    jani_binary_operator
            jani_strings.boolean_string.and_operator
            (string_of_boolean_expression variable_names b1)
            (string_of_boolean_expression variable_names b2)

	| Or_bool (b1, b2) ->
	    jani_binary_operator
		    jani_strings.boolean_string.or_operator
            (string_of_boolean_expression variable_names b1)
		    (string_of_boolean_expression variable_names b2)

	| Discrete_boolean_expression discrete_boolean_expression ->
		string_of_discrete_boolean_expression variable_names discrete_boolean_expression

(** Convert a discrete_boolean_expression into a string *)
and string_of_discrete_boolean_expression variable_names = function
	| Expression (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_arithmetic_expression

    | Boolean_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_discrete_boolean_expression

    | Binary_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_binary_word_expression

    | Array_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_array_expression

    | List_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_list_expression

    | Stack_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_stack_expression

    | Queue_comparison (l_expr, relop, r_expr) ->
        string_of_comparison variable_names l_expr relop r_expr string_of_queue_expression


	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	(*Done for jani, but without test*)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		let expr1 = (string_of_arithmetic_expression variable_names discrete_arithmetic_expression1) in
		let expr2 = (string_of_arithmetic_expression variable_names discrete_arithmetic_expression2) in
		let expr3 = (string_of_arithmetic_expression variable_names discrete_arithmetic_expression3) in
		jani_binary_operator
		    jani_strings.boolean_string.and_operator
		    (jani_binary_operator jani_strings.boolean_string.le_operator expr2 expr1 )
		    (jani_binary_operator jani_strings.boolean_string.le_operator expr1 expr3 )

    | Boolean_expression expr ->
        string_of_boolean_expression variable_names expr
	| Not_bool b ->
	    jani_unary_operator
	        jani_strings.boolean_string.not_operator
	        (string_of_boolean_expression variable_names b)

    | Bool_variable discrete_index -> json_quoted (variable_names discrete_index)
    | Bool_constant value -> DiscreteExpressions.customized_string_of_bool_value jani_strings.boolean_string value
    | Bool_access (access_type, index_expr) ->
        string_of_expression_access_for_jani variable_names access_type index_expr
    | Bool_list_hd list_expr as func ->
        let label = label_of_bool_factor func in
        jani_function_call
            label
            [|string_of_list_expression variable_names list_expr|]
            ~str_comment:(undeclared_function_warning label)
    | List_mem (expr, list_expr) as func ->
        let label = label_of_bool_factor func in
        jani_function_call
            label
            [|
                string_of_global_expression variable_names expr;
                string_of_list_expression variable_names list_expr
            |]
            ~str_comment:(undeclared_function_warning label)
    | Array_mem (expr, array_expr) as func ->
        let label = label_of_bool_factor func in
        jani_function_call
            label
            [|
                string_of_global_expression variable_names expr;
                string_of_array_expression variable_names array_expr
            |]
            ~str_comment:(undeclared_function_warning label)

    | Stack_is_empty stack_expr as func ->
        let label = label_of_bool_factor func in
        jani_function_call
            label
            [|string_of_stack_expression variable_names stack_expr|]
            ~str_comment:(undeclared_function_warning label)

    | Queue_is_empty queue_expr as func ->
        let label = label_of_bool_factor func in
        jani_function_call
            label
            [|string_of_queue_expression variable_names queue_expr|]
            ~str_comment:(undeclared_function_warning label)

and string_of_arithmetic_expression variable_names = function
    | Rational_arithmetic_expression expr -> string_of_rational_arithmetic_expression variable_names expr
    | Int_arithmetic_expression expr -> string_of_int_arithmetic_expression variable_names expr

and string_of_rational_arithmetic_expression variable_names =
    let rec string_of_arithmetic_expression = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Rational_plus (discrete_arithmetic_expression, Rational_factor (Rational_constant c))
        | Rational_minus (discrete_arithmetic_expression, Rational_factor (Rational_constant c)) when NumConst.equal c NumConst.zero ->
            string_of_arithmetic_expression discrete_arithmetic_expression

        | Rational_plus (discrete_arithmetic_expression, discrete_term) ->
            jani_binary_operator
                jani_strings.arithmetic_string.plus_string
                (string_of_arithmetic_expression discrete_arithmetic_expression)
                (string_of_term discrete_term)


        | Rational_minus (discrete_arithmetic_expression, discrete_term) ->
            jani_binary_operator
                jani_strings.arithmetic_string.minus_string
                (string_of_arithmetic_expression discrete_arithmetic_expression)
                (string_of_term discrete_term)


        | Rational_term discrete_term ->
            string_of_term discrete_term

	and string_of_term = function
		(* Eliminate the '1' coefficient *)
		| Rational_mul (Rational_factor (Rational_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
			string_of_factor discrete_factor
		| Rational_mul (discrete_term, discrete_factor) ->
            jani_binary_operator
                jani_strings.arithmetic_string.mul_string
                (string_of_term discrete_term)
                (string_of_factor discrete_factor)


		| Rational_div (discrete_term, discrete_factor) ->
		    jani_binary_operator
		        jani_strings.arithmetic_string.div_string
		        (string_of_term discrete_term)
                (string_of_factor discrete_factor)


		| Rational_factor discrete_factor ->
		    string_of_factor discrete_factor

	and string_of_factor = function
		| Rational_variable discrete_index -> json_quoted (variable_names discrete_index)
		| Rational_constant value -> NumConst.jani_string_of_numconst value
        | Rational_access (access_type, index_expr) ->
            string_of_expression_access_for_jani variable_names access_type index_expr

		| Rational_unary_min discrete_factor ->
		    jani_binary_operator
		        jani_strings.arithmetic_string.unary_min_string
		        "0"
                (string_of_factor discrete_factor)

		| Rational_expression expr ->
			string_of_arithmetic_expression expr
		| Rational_of_int expr ->
		    string_of_int_arithmetic_expression variable_names expr
        | Rational_pow (expr, exp) as factor ->
            jani_binary_operator
                (label_of_rational_factor factor)
                (string_of_arithmetic_expression expr)
                (string_of_int_arithmetic_expression variable_names exp)
        | Rational_list_hd list_expr as func ->
            let label = label_of_rational_factor func in
            jani_function_call
                label
                [|string_of_list_expression variable_names list_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Rational_stack_pop stack_expr as func ->
            let label = label_of_rational_factor func in
            jani_function_call
                label
                [|string_of_stack_expression variable_names stack_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Rational_stack_top stack_expr as func ->
            let label = label_of_rational_factor func in
            jani_function_call
                label
                [|string_of_stack_expression variable_names stack_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Rational_queue_pop queue_expr as func ->
            let label = label_of_rational_factor func in
            jani_function_call
                label
                [|string_of_queue_expression variable_names queue_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Rational_queue_top queue_expr as func ->
            let label = label_of_rational_factor func in
            jani_function_call
                label
                [|string_of_queue_expression variable_names queue_expr|]
                ~str_comment:(undeclared_function_warning label)

	(* Call top-level *)
	in string_of_arithmetic_expression

and string_of_int_arithmetic_expression variable_names =
    let rec string_of_int_arithmetic_expression = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Int_plus (discrete_arithmetic_expression, Int_factor (Int_constant c))
        | Int_minus (discrete_arithmetic_expression, Int_factor (Int_constant c)) when Int32.equal c Int32.zero ->
            string_of_int_arithmetic_expression discrete_arithmetic_expression

	| Int_plus (discrete_arithmetic_expression, discrete_term) ->
	    jani_binary_operator
		    jani_strings.arithmetic_string.plus_string
            (string_of_int_arithmetic_expression discrete_arithmetic_expression)
            (string_of_int_term discrete_term)


	| Int_minus (discrete_arithmetic_expression, discrete_term) ->
	    jani_binary_operator
		    jani_strings.arithmetic_string.minus_string
            (string_of_int_arithmetic_expression discrete_arithmetic_expression)
            (string_of_int_term discrete_term)


        | Int_term discrete_term -> string_of_int_term discrete_term

	and string_of_int_term = function
		(* Eliminate the '1' coefficient *)
		| Int_mul (Int_factor (Int_constant c), discrete_factor) when Int32.equal c Int32.one ->
			string_of_int_factor discrete_factor
		| Int_mul (discrete_term, discrete_factor) ->
		    jani_binary_operator
                jani_strings.arithmetic_string.mul_string
                (string_of_int_term discrete_term)
                (string_of_int_factor discrete_factor)


		| Int_div (discrete_term, discrete_factor) ->
		    jani_binary_operator
                jani_strings.arithmetic_string.div_string
                (string_of_int_term discrete_term)
                (string_of_int_factor discrete_factor)


		| Int_factor discrete_factor -> string_of_int_factor discrete_factor

	and string_of_int_factor = function
		| Int_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
		| Int_constant value -> Int32.to_string value
        | Int_access (access_type, index_expr) ->
            string_of_expression_access_for_jani variable_names access_type index_expr

		| Int_unary_min discrete_factor ->
		    jani_unary_operator
                jani_strings.arithmetic_string.unary_min_string
                (string_of_int_factor discrete_factor)

		| Int_expression discrete_arithmetic_expression ->
			string_of_int_arithmetic_expression discrete_arithmetic_expression
        | Int_pow (expr, exp) as func ->
            jani_function_call
                (label_of_int_factor func)
                [|
                    string_of_int_arithmetic_expression expr;
                    string_of_int_arithmetic_expression exp
                |]
        | Int_list_hd list_expr as func ->
            let label = label_of_int_factor func in
            jani_function_call
                label
                [|string_of_list_expression variable_names list_expr|]
                ~str_comment:(undeclared_function_warning label)
        | Array_length array_expr as func ->
            let label = label_of_int_factor func in
            jani_function_call
                label
                [|string_of_array_expression variable_names array_expr|]
                ~str_comment:(undeclared_function_warning label)
        | List_length list_expr as func ->
            let label = label_of_int_factor func in
            jani_function_call
                label
                [|string_of_list_expression variable_names list_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Stack_length stack_expr as func ->
            let label = label_of_int_factor func in
            jani_function_call
                label
                [|string_of_stack_expression variable_names stack_expr|]
                ~str_comment:(undeclared_function_warning label)

        | Queue_length queue_expr as func ->
            let label = label_of_int_factor func in
            jani_function_call
                label
                [|string_of_queue_expression variable_names queue_expr|]
                ~str_comment:(undeclared_function_warning label)

	(* Call top-level *)
	in string_of_int_arithmetic_expression

and string_of_binary_word_expression variable_names binary_word_expr =

    (* Get label of expression *)
    let label = label_of_binary_word_expression binary_word_expr in
    (* Prepare undeclared_function_warning function with given label *)
    let undeclared_function_warning = lazy(undeclared_function_warning label) in

    (* Convert a binary word expression into a string *)
    let string_of_binary_word_expression = function
        | Logical_fill_left (binary_word, expr, _)
        | Logical_fill_right (binary_word, expr, _)
        | Logical_shift_left (binary_word, expr, _)
        | Logical_shift_right (binary_word, expr, _) ->
            jani_function_call
                label
                [|
                    string_of_binary_word_expression variable_names binary_word;
                    string_of_int_arithmetic_expression variable_names expr
                |]
                ~str_comment:(Lazy.force undeclared_function_warning)

        | Logical_and (l_binary_word, r_binary_word, _)
        | Logical_or (l_binary_word, r_binary_word, _)
        | Logical_xor (l_binary_word, r_binary_word, _) ->
            jani_function_call
                label
                [|
                    string_of_binary_word_expression variable_names l_binary_word;
                    string_of_binary_word_expression variable_names r_binary_word
                |]
                ~str_comment:(Lazy.force undeclared_function_warning)

        | Logical_not (binary_word, length) ->
            jani_function_call
                label
                [|
                    string_of_binary_word_expression variable_names binary_word;
                    (string_of_int length)
                |]
                ~str_comment:(Lazy.force undeclared_function_warning)

        | Binary_word_constant value -> string_of_value (Binary_word_value value)
        | Binary_word_variable (variable_index, _) -> "\"" ^ variable_names variable_index ^ "\""
        | Binary_word_access (access_type, index_expr, _) ->
            string_of_expression_access_for_jani variable_names access_type index_expr
        | Binary_word_list_hd list_expr ->
            jani_function_call
                label
                [|string_of_list_expression variable_names list_expr|]
                ~str_comment:(Lazy.force undeclared_function_warning)
    in
    string_of_binary_word_expression binary_word_expr

and string_of_array_expression variable_names = function
    | Literal_array expr_array ->
        let str_values = Array.map (string_of_global_expression variable_names) expr_array in
        jani_array_value str_values

    | Array_constant values ->
        let str_values = Array.map string_of_value values in
        jani_array_value str_values

    | Array_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | Array_access (access_type, index_expr) ->
        string_of_expression_access_for_jani variable_names access_type index_expr

    | Array_concat (array_expr_0, array_expr_1) as func ->
        (* Get label of expression *)
        let label = label_of_array_expression func in
        jani_function_call label
            [|
                string_of_array_expression variable_names array_expr_0;
                string_of_array_expression variable_names array_expr_1
            |]
            ~str_comment:(undeclared_function_warning label)

    | Array_list_hd list_expr as func ->
        (* Get label of expression *)
        let label = label_of_array_expression func in
        jani_function_call label
            [|string_of_list_expression variable_names list_expr|]
            ~str_comment:(undeclared_function_warning label)

and string_of_list_expression variable_names = function
    | Literal_list expr_list ->
        let str_values = List.map (string_of_global_expression variable_names) expr_list in
        jani_array_value (Array.of_list str_values)

    | List_constant values ->
        let str_values = List.map string_of_value values in
        jani_array_value (Array.of_list str_values)

    | List_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | List_access (access_type, index_expr) ->
        string_of_expression_access_for_jani variable_names access_type index_expr
    | List_cons (expr, list_expr) as func ->
        (* Get label of expression *)
        let label = label_of_list_expression func in

        jani_function_call
            label
            [|
                string_of_global_expression variable_names expr;
                string_of_list_expression variable_names list_expr
            |]
            ~str_comment:(undeclared_function_warning label)
    | List_list_hd list_expr
    | List_list_tl list_expr
    | List_rev list_expr as func ->
        (* Get label of expression *)
        let label = label_of_list_expression func in
        jani_function_call
            label
            [| string_of_list_expression variable_names list_expr |]
            ~str_comment:(undeclared_function_warning label)

and string_of_stack_expression variable_names = function
    | Stack_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | Stack_push (expr, stack_expr) as func ->
        (* Get label of expression *)
        let label = label_of_stack_expression func in
        jani_function_call
            label
            [|
                string_of_global_expression variable_names expr;
                string_of_stack_expression variable_names stack_expr
            |]
            ~str_comment:(undeclared_function_warning label)
    | Stack_clear stack_expr as func ->
        (* Get label of expression *)
        let label = label_of_stack_expression func in
        jani_function_call
            label
            [|string_of_stack_expression variable_names stack_expr|]
            ~str_comment:(undeclared_function_warning label)

and string_of_queue_expression variable_names = function
    | Queue_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | Queue_push (expr, queue_expr) as func ->
        (* Get label of expression *)
        let label = label_of_queue_expression func in
        jani_function_call
            label
            [|
                string_of_global_expression variable_names expr;
                string_of_queue_expression variable_names queue_expr
            |]
            ~str_comment:(undeclared_function_warning label)
    | Queue_clear queue_expr as func ->
        (* Get label of expression *)
        let label = label_of_queue_expression func in
        jani_function_call
            label
            [|string_of_queue_expression variable_names queue_expr|]
            ~str_comment:(undeclared_function_warning label)


and string_of_expression_of_access_for_jani variable_names = function
    | Expression_array_access array_expr ->
        string_of_array_expression variable_names array_expr
    | Expression_list_access list_expr ->
        string_of_list_expression variable_names list_expr

and string_of_expression_access_for_jani variable_names access_type index_expr =
    let str_expr = string_of_expression_of_access_for_jani variable_names access_type in
    let str_index_expr = string_of_int_arithmetic_expression variable_names index_expr in
    jani_array_access str_expr str_index_expr


(* Get list of non-linear constraint inequalities with customized strings *)
let strings_of_nonlinear_constraint variable_names (* nonlinear_constraint *) =
    List.rev_map (string_of_discrete_boolean_expression variable_names ) (* nonlinear_constraint *)



(************************************************************)
(** OCaml Utilities overwrite. TODO: fusion if OK *)
(************************************************************)
let string_of_list_of_string_with_sep sep list =
	let deal_string sep list =
		OCamlUtilities.string_of_list_of_string_with_sep sep list
	in
		deal_string sep (List.filter (fun string -> string<>"") list)

(************************************************************)
(** Header *)
(************************************************************)

(* Add a header to the model *)
let string_of_header model =
    let options = Input.get_options () in
    json_property "jani-version" jani_version ^ jani_separator
    ^ json_property "name" (json_quoted options#model_file_name) ^ jani_separator
    ^ json_property "type" (json_quoted jani_type) ^ jani_separator
    ^ json_property "features" jani_features

(************************************************************
 Declarations
************************************************************)

(* Declarations of custom datatype *)
let string_of_custom_datatypes =

    json_property "datatypes" (json_array [|
        json_struct [|
            json_property "name" (json_quoted "binary_word");
            json_property "members" (json_array [|
                json_struct [|
                    json_property "name" (json_quoted "elements");
                    json_property "type" (json_struct [|
                        json_property "kind" (json_quoted "array");
                        json_property "base" (json_quoted "bool")
                    |])
                |]
            |])
        |]
    |])

(* Declarations of custom functions *)
let string_of_custom_functions = 
    json_property "functions" (json_array [|

    |])

(* Declaration of actions *)
let string_of_actions model =
  "\"actions\": ["
  ^ (string_of_list_of_string_with_sep jani_separator
		(List.filter (fun string -> string<>"")
		   (List.map (fun action_index ->
			  (* Do not declare silent actions *)
			  match model.action_types action_index with
			  | Action_type_nosync -> ""
			  | Action_type_sync ->
			  (* Get name *)
			  let action_name = model.action_names action_index in

			  (* Get number of automata *)
			  let nb_automata = List.length (model.automata_per_action action_index) in

			  (* Case action unused: drop *)
			  if nb_automata = 0 then ""

			  else "{\"name\":\"" ^ action_name ^ "\"}"
			) model.actions
		   )
		)
    )
  ^ "]"

(* Convert the initial clocks declarations into a string *)
let string_of_clocks model =
  let multirate_bol = model.has_non_1rate_clocks in
  let string_of_variables list_of_variables =
    string_of_list_of_string_with_sep jani_separator (List.map
      (fun var ->
		let clocks_type = if multirate_bol then "continuous" else "clock" in
		json_struct [|
		    json_property "name" (json_quoted (model.variable_names var));
		    json_property "type" (json_quoted clocks_type);
		    json_property "initial-value" "0"
		|]
      )
      list_of_variables
    )
  in
  if model.nb_clocks > 0 then
    (string_of_variables model.clocks_without_special_reset_clock)
    else ""

(* String of number var type *)
let string_of_var_type_discrete_number_for_jani = function
    | Var_type_discrete_rational -> json_quoted "real"
    | Var_type_discrete_int -> json_quoted "int"
    | Var_type_discrete_unknown_number -> json_quoted "number"

(* String of discrete var type *)
let rec string_of_var_type_discrete_for_jani = function
    | Var_type_discrete_number x -> string_of_var_type_discrete_number_for_jani x
    | Var_type_discrete_bool -> json_quoted "bool"
    | Var_type_discrete_binary_word _ ->
        json_struct [|
            json_property "kind" (json_quoted "array");
            json_property "base" (json_quoted "bool");
        |]

    | Var_type_discrete_array (inner_type, _)
    | Var_type_discrete_list inner_type
    | Var_type_discrete_stack inner_type
    | Var_type_discrete_queue inner_type ->
        json_struct [|
            json_property "kind" (json_quoted "array");
            json_property "base" (string_of_var_type_discrete_for_jani inner_type)
        |]
    | Var_type_weak ->
        raise (InternalError "An expression should have a determined type. Maybe something has failed before.")


(* Convert the initial discrete var declarations into a string *)
let string_of_discrete model =

	if model.nb_discrete > 0 then(
		(string_of_list_of_string_with_sep jani_separator
			(List.map (fun discrete_index ->
				(* Get the name *)
				let discrete_name = model.variable_names discrete_index in
                (* Get the type *)
                let discrete_type = DiscreteType.discrete_type_of_var_type (model.type_of_variables discrete_index) in
                (* Get str for the type*)
                let str_discrete_type = string_of_var_type_discrete_for_jani discrete_type in
				(* Get the initial value *)
				let inital_global_location  = model.initial_location in

				let initial_value = Location.get_discrete_value inital_global_location discrete_index in

				let str_initial_value = string_of_value initial_value in

				(* Assign *)
                json_struct [|
                    json_property "name" (json_quoted discrete_name);
                    json_property "type" str_discrete_type;
                    json_property "initial-value" str_initial_value
                |]

            ) model.discrete
			)
		)
	) else ""

(* Convert the parameter declarations into a string *)
let string_of_parameters model =
	if model.nb_parameters > 0 then (
		(string_of_list_of_string_with_sep (jani_separator)
			(List.map (fun parameter_index ->
				(* Get the name *)
				let parameter_name = model.variable_names parameter_index in

				(* Assign *)
                json_struct [|
                    json_property "name" (json_quoted parameter_name);
                    json_property "type" (json_quoted "real")
                |]

            ) model.parameters
			)
		)
	) else ""

(* Declaration of variables *)
let string_of_variables model =
	let clocks = string_of_clocks model in
	let discrete = string_of_discrete model in
	let parameters = string_of_parameters model in
	let sep_after_clocks = (clocks <> "" && (discrete <> "" || parameters <> "")) in
	let sep_after_discrete = (discrete <> "" && parameters <> "") in

	if clocks = "" && discrete = "" && parameters = "" then "" else (
        "\"variables\": ["
        ^ clocks
        ^ (if sep_after_clocks then jani_separator else "")
        ^ discrete
        ^ (if sep_after_discrete then jani_separator else "")
        ^ parameters
        ^ "]"
    )

(* Properties *)
let string_of_properties =
    json_property "properties" (json_array [||])

(************************************************************)
(** Automata *)
(************************************************************)

let rec string_of_strings_with_sep_and string_list =
	match string_list with
	| [] -> ""
	| (elem::[]) -> elem
	| (elem::q) ->
	    jani_binary_operator
	        jani_boolean_strings.and_operator
	        elem
	        (string_of_strings_with_sep_and q)

(** Convert a guard or an invariant into a string *)
let rec string_of_guard_or_invariant actions_and_nb_automata variable_names = function
	(* True guard = no guard *)
	| True_guard -> ""

	(* False *)
	| False_guard ->
	    jani_boolean_strings.false_string

	| Discrete_guard discrete_guard ->

        let list_discrete_guard = (strings_of_nonlinear_constraint variable_names discrete_guard) in
        let list_discrete_guard_without_true = if list_discrete_guard = [jani_boolean_strings.true_string] then [""] else list_discrete_guard in
        let content = string_of_strings_with_sep_and list_discrete_guard_without_true in
        if content = "" then "" else content

	| Continuous_guard continuous_guard ->
		(* Remove true guard *)
		if LinearConstraint.pxd_is_true continuous_guard then "" else
			let list_of_inequalities = LinearConstraint.pxd_get_inequalities continuous_guard in
			(string_of_strings_with_sep_and
				(List.map (fun (inequality) ->
					let op = match (LinearConstraint.op_of_pxd_linear_inequality inequality) with
						| Op_l		-> jani_boolean_strings.l_operator
						| Op_le		-> jani_boolean_strings.le_operator
						| Op_eq		-> jani_boolean_strings.eq_operator
						| Op_ge 	-> jani_boolean_strings.ge_operator
						| Op_g		-> jani_boolean_strings.g_operator
					in
					let left = LinearConstraint.string_of_left_term_of_pxd_linear_inequality variable_names inequality in
					let right = LinearConstraint.string_of_right_term_of_pxd_linear_inequality variable_names inequality in
                    (jani_binary_operator op left right)

				) list_of_inequalities)

			)

	| Discrete_continuous_guard discrete_continuous_guard ->
		let non_linear_constraint_list = strings_of_nonlinear_constraint variable_names discrete_continuous_guard.discrete_guard in
		let linear_constraint_string = (string_of_guard_or_invariant actions_and_nb_automata variable_names (Continuous_guard discrete_continuous_guard.continuous_guard)) in
		let list = List.append non_linear_constraint_list [linear_constraint_string] in
	    let content = string_of_strings_with_sep_and list in
	    if content = "" then "" else content


(* Convert the invariant of a location into a string *)
let string_of_invariant model actions_and_nb_automata automaton_index location_index =
    let invariant = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (model.invariants automaton_index location_index) in
    (* Invariant *)
    invariant

(* Convert the guard of an edge into a string *)
let string_of_guard model actions_and_nb_automata model_variable_names transition_guard =
    let guard = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (transition_guard) in
    (* Guard *)
    guard

let string_of_clock_rate model actions_and_nb_automata automaton_index location_index =
	let rec clock_is_1rate clock_index flow_list =
		match flow_list with
		| [] -> true
		| ((var, _)::q) -> if clock_index = var then false else (clock_is_1rate clock_index q)
	in
	if not model.has_non_1rate_clocks then ""
	else (
	"" ^ string_of_strings_with_sep_and (List.append
		(*Step 1: explicit rates*)
		(
			List.map (
				fun (variable_index, flow_value) ->
					let variable_name = (model.variable_names variable_index) in
					let value = (NumConst.jani_string_of_numconst flow_value) in
					  "{\"op\": \"=\"" ^ jani_separator
					^ " \"left\": {\"op\": \"der\",\"var\": \"" ^ variable_name ^ "\"}" ^ jani_separator
					^ " \"right\": " ^ value ^ "}"
			) (model.flow automaton_index location_index)
		)

		(*Step 2: set rate 1 to unspecified clocks*)
		(
			List.map (
				fun variable_index ->
					let variable_name = (model.variable_names variable_index) in
					  "{\"op\": \"=\"" ^ jani_separator
					^ " \"left\": {\"op\": \"der\",\"var\": \"" ^ variable_name ^ "\"}" ^ jani_separator
					^ " \"right\": 1}"
			)
			(List.filter (fun clock_index -> clock_is_1rate clock_index (model.flow automaton_index location_index)) model.clocks)
		)
	)
	)

(* Convert a location of an automaton into a string *)
let string_of_location model actions_and_nb_automata automaton_index location_index =
	let invariant = string_of_invariant model actions_and_nb_automata automaton_index location_index in
	let der_clock = string_of_clock_rate model actions_and_nb_automata automaton_index location_index in
	let not_display_timeprogress = (invariant = "" && der_clock = "") in
	let twoparts = (invariant <> "" && der_clock <> "") in
	(* Header *)
	"{"

	(* Name *)
	^ "\"name\": \"" ^ (model.location_names automaton_index location_index) ^ "\""

	(* Invariant and stopwatches *)
	^ (if not_display_timeprogress then "" else (
	    jani_separator ^
	    json_property "time-progress" (
	        jani_expression (
                if twoparts then
                    jani_binary_operator
                        jani_boolean_strings.and_operator
                        invariant
                        der_clock
                else
                    invariant ^ jani_separator ^ der_clock
            )
        )
	))

	(* Footer *)
	^ "}"

(* Convert the locations of an automaton into a string *)
let string_of_locations model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep (jani_separator) (List.map (fun location_index ->
		string_of_location model actions_and_nb_automata automaton_index location_index
	) (model.locations_per_automaton automaton_index))

(* Convert the initial location of an automaton *)
let string_of_initial_location model automaton_index =
	let inital_global_location  = model.initial_location in
	let initial_location = Location.get_location inital_global_location automaton_index in
	"\"" ^ (model.location_names automaton_index initial_location) ^ "\""

let string_of_clock_updates model = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep (jani_separator) (List.map (fun variable_index ->
			"{\"ref\": \""
			^ (model.variable_names variable_index)
			^ "\"" ^ jani_separator ^ " \"value\" : 0}"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep jani_separator (List.map (fun (variable_index, linear_term) ->
			"{\"ref\": \""
			^ (model.variable_names variable_index)
			^ "\"" ^ jani_separator ^ " \"value\" : "
			^ (LinearConstraint.string_of_pxd_linear_term_for_jani model.variable_names linear_term)
			^ "}"
		) list_of_clocks_lt)

(* Convert a list of updates into a string *)
let string_of_discrete_updates model updates =
	string_of_list_of_string_with_sep jani_separator (List.map (fun (variable_access, global_expression) ->
		"{\"ref\": \""
		(* Convert variable access to string *)
		^ ModelPrinter.string_of_variable_access model variable_access
		^ "\"" ^ jani_separator ^ " \"value\" : "
		^ (string_of_global_expression model.variable_names global_expression)
		^ "}"
	) updates)

(** Return if there is no clock updates *)
let no_clock_updates clock_updates =
	clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []

let string_of_conditional_clock_updates model boolean_expr order = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep (jani_separator) (List.map (fun variable_index ->
			let variable_name = "\"" ^ (model.variable_names variable_index) ^ "\"" in
			"{\"ref\": " ^ variable_name ^ jani_separator
			^ " \"value\" : "
			^ "{\"op\": \"ite\"" ^ jani_separator
			^ "\"if\":" ^ string_of_boolean_expression model.variable_names boolean_expr ^ jani_separator
			^ "\"then\":" ^ (if order="if" then "0" else variable_name) ^ jani_separator
			^ "\"else\":" ^ (if order="if" then variable_name else "0")
			^ "}}"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep (jani_separator) (List.map (fun (variable_index, linear_term) ->
			let variable_name = "\"" ^ (model.variable_names variable_index) ^ "\"" in
			let expression = (LinearConstraint.string_of_pxd_linear_term_for_jani model.variable_names linear_term) in
			"{\"ref\": " ^ variable_name ^ jani_separator
			^ " \"value\" : "
			^ " {\"op\": \"ite\"" ^ jani_separator
			^ "\"if\":" ^ string_of_boolean_expression model.variable_names boolean_expr ^ jani_separator
			^ "\"then\":" ^ (if order="if" then expression else variable_name) ^ jani_separator
			^ "\"else\":" ^ (if order="if" then variable_name else expression)
			^ "}}"
		) list_of_clocks_lt)

(* Convert a list of discrete updates into a string *)
let string_of_conditional_discrete_updates model boolean_expr order updates =
	string_of_list_of_string_with_sep (jani_separator) (List.rev_map (fun (variable_access, global_expression) ->

		let expression = (string_of_global_expression model.variable_names global_expression) in
		let variable_name = "\"" ^ ModelPrinter.string_of_variable_access model variable_access ^ "\"" in
		"{\"ref\": " ^ variable_name ^ jani_separator
		^ " \"value\" : "
		^ " {\"op\": \"ite\"" ^ jani_separator
		^ "\"if\":" ^ string_of_boolean_expression model.variable_names boolean_expr ^ jani_separator
		^ "\"then\":" ^ (if order="if" then expression else variable_name) ^ jani_separator
		^ "\"else\":" ^ (if order="if" then variable_name else expression)
		^ "}}"
	) updates)

(** Convert a list of conditional updates into a string *)
let string_of_conditional_updates model conditional_updates =
  string_of_list_of_string_with_sep (jani_separator) (List.map (fun (boolean_expr, if_updates, else_updates) ->
    let empty_else = no_clock_updates else_updates.clock && else_updates.discrete = [] && else_updates.conditional = [] in

    (*Then*)
    let clocks_string = (string_of_conditional_clock_updates model boolean_expr "if" if_updates.clock) in
    let discrete_string = (string_of_conditional_discrete_updates model boolean_expr "if" if_updates.discrete) in
    let if_separator = clocks_string<>"" && discrete_string<>"" in
        clocks_string
    ^ (if if_separator then jani_separator else "")
    ^ discrete_string
    ^ (
        if empty_else then "" else
        let clocks_string = (string_of_conditional_clock_updates model boolean_expr "else" else_updates.clock) in
        let discrete_string = (string_of_conditional_discrete_updates model boolean_expr "else" else_updates.discrete) in
        let else_separator = clocks_string<>"" && discrete_string<>"" in
                jani_separator
            ^	clocks_string
            ^ (if else_separator then jani_separator else "")
            ^ discrete_string
    )
  ) conditional_updates)

let string_of_updates model automaton_index action_index clock_updates discrete_updates transition =
  (*TODO DYLAN: use transition for cloclk/discrete ?*)
	(* Check for emptiness of some updates *)
	let no_clock_updates =
		clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []
	in
	let no_discrete_updates = discrete_updates = [] in
  let no_conditional_updates = transition.updates.conditional = [] in
	(* If no update at all: empty string *)
	if no_clock_updates && no_discrete_updates && no_conditional_updates then ""

	else(
    let first_separator = (not no_clock_updates) && (not no_discrete_updates) in
    let second_separator = ( (not no_clock_updates) && (no_discrete_updates) && (not no_conditional_updates) ) || ( (not no_discrete_updates) && (not no_conditional_updates) ) in
    let conditional_updates = transition.updates.conditional in
    ""
		^ (string_of_clock_updates model clock_updates)
		^ (if first_separator then jani_separator else "")
		^ (string_of_discrete_updates model discrete_updates)
    ^ (if second_separator then jani_separator else "")
    ^ (string_of_conditional_updates model conditional_updates)
		^ ""
	)

(* Convert a transition of a location into a string *)
let string_of_transition model actions_and_nb_automata automaton_index source_location transition =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let guard = string_of_guard model actions_and_nb_automata model.variable_names transition.guard in
	let assignments = (string_of_updates model automaton_index transition.action clock_updates discrete_updates transition) in
	(* Header *)
	"{"

	(* Source *)
	^ "\"location\": \"" ^ (model.location_names automaton_index source_location) ^ "\"" ^ jani_separator

	(* Guard *)
	^ (if guard = "" then "" else
		((
			"\"guard\": " ^ jani_expression guard ^ ""
		) ^ jani_separator))

	(* Target *)
	^ "\"destinations\": [{"
	^ "\"location\": \"" ^ (model.location_names automaton_index transition.target) ^ "\""
	^  (if assignments = "" then "" else (jani_separator ^ "\"assignments\": [" ^ assignments ^ "]"))
	^ "}]"

	(* Footer *)
	^ "}"

(* Convert the transitions of an automaton into a string *)
let string_of_transitions model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep (jani_separator)
			(
			(* For each location *)
			List.map (fun location_index ->
				string_of_list_of_string_with_sep (jani_separator) (
				(* For each action *)
				List.map (fun action_index ->
					(* Get the list of transitions *)
					let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
					(* Convert to string *)
					string_of_list_of_string_with_sep (jani_separator) (
						(* For each transition *)
						List.map (string_of_transition model actions_and_nb_automata automaton_index location_index) transitions
						)
					) (model.actions_per_location automaton_index location_index)
				)
			) (model.locations_per_automaton automaton_index))

(* Convert an automaton into a string *)
let string_of_automaton model actions_and_nb_automata automaton_index =
  	"{"
    ^ "\"name\": \"" ^ (model.automata_names automaton_index) ^ "\"" ^ jani_separator
  	^ "\"locations\": [" ^ (string_of_locations model actions_and_nb_automata automaton_index) ^ "]" ^ jani_separator
  	^ "\"initial-locations\": [" ^ (string_of_initial_location model automaton_index) ^ "]" ^ jani_separator
  	^ "\"edges\": [" ^ (string_of_transitions model actions_and_nb_automata automaton_index) ^ "]"
    ^ "}"

(* Convert the automata into a string *)
let string_of_automata model actions_and_nb_automata =
	(*** WARNING: Do not print the observer TODO ? ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Print all (other) PTA *)
    "\"automata\": ["
  ^ string_of_list_of_string_with_sep (jani_separator) (
  		List.map (fun automaton_index ->
        string_of_automaton model actions_and_nb_automata automaton_index
  	) pta_without_obs)
  ^ "]"


(************************************************************)
(** System *)
(************************************************************)
(* Create the system definition *)
let string_of_system model =
	let is_in_automaton action_index automaton_index = (List.mem action_index (model.actions_per_automaton automaton_index)) in
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Open *)
	"\"system\": {"
	^ "\"elements\": ["
		(* System definition *)
		^ "" ^ (string_of_list_of_string_with_sep (jani_separator) (
					List.map (fun automaton_index ->
						let automaton = model.automata_names automaton_index in
						"{\"automaton\": \"" ^ automaton ^ "\"}"
					 )
					 (pta_without_obs)
				  ))
		^ ""
		(* Close *)
		^ "]" ^ jani_separator ^ ""
	^ "\"syncs\": ["
		(* Actions *)
		^ "" ^ (string_of_list_of_string_with_sep (jani_separator) (
					List.map (fun action_index ->
						let action_name = model.action_names action_index in
						  "{\"synchronise\": ["
						^ (
							string_of_list_of_string_with_sep (jani_separator^" ") (
							List.map (fun automaton_index ->
								if (is_in_automaton action_index automaton_index)
								then ("\"" ^ action_name ^ "\"")
								else "null"
							) (pta_without_obs))
						  )
						^ "]"
						^ jani_separator ^ " "
						^ "\"result\": \"" ^ action_name ^ "\" }"
					 )
					 (List.filter (fun action_index -> model.action_types action_index = Action_type_sync) model.actions)
				  ))
		^ ""
		(* Close *)
		^ "]"
	^ "}"

(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* Add a warning due to semantic difference *)
	print_warning "Translation to JANI is mostly correct, but may feature some subtle differences in semantics. Notably, JANI assumes that a guard x=1 followed by an invariant x>=2 *can* be fired, which is not the case in IMITATOR semantics.";
	
    let actions_and_nb_automata = List.map (fun action_index ->
        (* Get number of automata *)
        let nb_automata = List.length (model.automata_per_action action_index) in
        (* Make it a pair *)
        action_index , nb_automata
    ) model.actions
    in

    let variables = string_of_variables model in
    let ugly_json_model =
        json_struct [|
            string_of_header model;
            string_of_custom_datatypes;
            string_of_custom_functions;
            string_of_actions model;
            if variables = "" then "" else variables;
            string_of_properties;
            string_of_automata model actions_and_nb_automata;
            string_of_system model
        |]
    in
    OCamlUtilities.prettify_json ugly_json_model