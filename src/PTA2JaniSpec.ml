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
 * Last modified     : 2021/03/25
 *
 ************************************************************)

 (*
 TODO:
 - if..then..else
 - check if all the "string_of" were translated
 *)

open Constants
open OCamlUtilities
open ImitatorUtilities
open LinearConstraint
open DiscreteExpressions
open NonlinearConstraint
open AbstractModel
open Result


(************************************************************)
(** Customized values for constraint conversion *)
(************************************************************)

let jani_boolean_strings : customized_boolean_string = {
	true_string   = "true";
	false_string  = "false";
	and_operator  = "∧";
	or_operator   = "∨"; (* useless *)
	l_operator    = "<";
	le_operator   = "≤";
	eq_operator   = "=";
	neq_operator   = "<>";
	ge_operator   = ">";
	g_operator    = "≥";
	not_operator  = "¬";
	in_operator   = ""; (* useless *)
}

let jani_strings = {
    arithmetic_string = Constants.default_arithmetic_string_without_whitespace;
    boolean_string = jani_boolean_strings;
    array_string = Constants.default_array_string;
}

let jani_separator = ","

let jani_assignment = "="

let jani_version = "1"
let jani_type = "sha"
let jani_features = "[\"derived-operators\",\"arrays\",\"datatypes\"]"

(* JANI *)

(************** Jani translation **************)
(* Convert an arithmetic expression into a string *)
(*** NOTE: we consider more cases than the strict minimum in order to improve readability a bit ***)
let jani_separator = ","

let string_of_jani_binary_expression str_operator str_left str_right =
    "{"
    ^ "\"op\": \"" ^ str_operator ^ "\"" ^ jani_separator
    ^ "\"left\": " ^ str_left ^ jani_separator
    ^ "\"right\": " ^ str_right
    ^ "}"

let string_of_jani_unary_expression str_operator str_expr =
    "{"
    ^ "\"op\": \"" ^ str_operator ^"\"" ^ jani_separator
    ^ "\"exp\": " ^ str_expr
    ^ "}"

let jani_quoted str =
    "\"" ^ str ^ "\""

(* TODO benjamin REFACTOR duplicate from DiscreteExpression *)
let customized_string_of_boolean_operations customized_string = function
	| OP_L		-> customized_string.l_operator
	| OP_LEQ	-> customized_string.le_operator
	| OP_EQ		-> customized_string.eq_operator
	| OP_NEQ	-> customized_string.neq_operator
	| OP_GEQ	-> customized_string.ge_operator
	| OP_G		-> customized_string.g_operator

(* TODO benjamin REFACTOR duplicate from DiscreteExpression *)
let customized_string_of_bool_value customized_string = function
    | true -> customized_string.true_string
    | false -> customized_string.false_string

let rec customized_string_of_global_expression_for_jani customized_string variable_names = function
    | Arithmetic_expression expr -> customized_string_of_arithmetic_expression_for_jani customized_string variable_names expr
    | Bool_expression expr -> customized_string_of_boolean_expression_for_jani customized_string variable_names expr
    | Binary_word_expression expr -> customized_string_of_binary_word_expression_for_jani customized_string variable_names expr
    | Array_expression expr -> customized_string_of_array_expression_for_jani customized_string variable_names expr

and customized_string_of_boolean_expression_for_jani customized_string variable_names = function
	| True_bool -> customized_string.boolean_string.true_string
	| False_bool -> customized_string.boolean_string.false_string
	| And_bool (b1, b2) ->
	    string_of_jani_binary_expression
            customized_string.boolean_string.and_operator
            (customized_string_of_boolean_expression_for_jani customized_string variable_names b1)
            (customized_string_of_boolean_expression_for_jani customized_string variable_names b2)

	| Or_bool (b1, b2) ->
	    string_of_jani_binary_expression
		    customized_string.boolean_string.or_operator
            (customized_string_of_boolean_expression_for_jani customized_string variable_names b1)
		    (customized_string_of_boolean_expression_for_jani customized_string variable_names b2)

	| Discrete_boolean_expression discrete_boolean_expression ->
		customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names discrete_boolean_expression

(** Convert a discrete_boolean_expression into a string *)
and customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (l_expr, relop, r_expr) ->
	    string_of_jani_binary_expression
	        (customized_string_of_boolean_operations customized_string.boolean_string relop)
		    (customized_string_of_arithmetic_expression_for_jani customized_string variable_names l_expr)
		    (customized_string_of_arithmetic_expression_for_jani customized_string variable_names r_expr)

    | Boolean_comparison (l_expr, relop, r_expr) ->
        string_of_jani_binary_expression
            (customized_string_of_boolean_operations customized_string.boolean_string relop)
		    (customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names l_expr)
		    (customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names r_expr)

    | Binary_comparison (l_expr, relop, r_expr) ->
        string_of_jani_binary_expression
            (customized_string_of_boolean_operations customized_string.boolean_string relop)
		    (customized_string_of_binary_word_expression_for_jani customized_string variable_names l_expr)
		    (customized_string_of_binary_word_expression_for_jani customized_string variable_names r_expr)

    | Array_comparison (l_expr, relop, r_expr) ->
        string_of_jani_binary_expression
            (customized_string_of_boolean_operations customized_string.boolean_string relop)
            (customized_string_of_array_expression_for_jani customized_string variable_names l_expr)
            (customized_string_of_array_expression_for_jani customized_string variable_names r_expr)

	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	(*Done for jani, but without test*)
	| Expression_in (discrete_arithmetic_expression1, discrete_arithmetic_expression2, discrete_arithmetic_expression3) ->
		let expr1 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression1) in
		let expr2 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression2) in
		let expr3 = (customized_string_of_arithmetic_expression_for_jani customized_string variable_names discrete_arithmetic_expression3) in
		string_of_jani_binary_expression
		    customized_string.boolean_string.and_operator
		    (string_of_jani_binary_expression customized_string.boolean_string.le_operator expr2 expr1 )
		    (string_of_jani_binary_expression customized_string.boolean_string.le_operator expr1 expr3 )

    | Boolean_expression expr ->
        customized_string_of_boolean_expression_for_jani customized_string variable_names expr
	| Not_bool b ->
	    string_of_jani_unary_expression
	        customized_string.boolean_string.not_operator
	        (customized_string_of_boolean_expression_for_jani customized_string variable_names b)

    | DB_variable discrete_index -> jani_quoted (variable_names discrete_index)
    | DB_constant value -> customized_string_of_bool_value customized_string.boolean_string value
    | Bool_array_access (array_expr, index_expr) ->
        "{"
        ^ "\"op\": \"aa\","
        ^ "\"exp\": " ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr ^ ", "
        ^ "\"index\": " ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names index_expr
        ^ "}"

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
            string_of_jani_binary_expression
                Constants.default_arithmetic_string_without_whitespace.plus_string
                (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
                (string_of_term customized_string discrete_term)


        | DAE_minus (discrete_arithmetic_expression, discrete_term) ->
            string_of_jani_binary_expression
                Constants.default_arithmetic_string_without_whitespace.minus_string
                (string_of_arithmetic_expression customized_string discrete_arithmetic_expression)
                (string_of_term customized_string discrete_term)


        | DAE_term discrete_term ->
            string_of_term customized_string discrete_term

	and string_of_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| DT_mul (DT_factor (DF_constant c), discrete_factor) when NumConst.equal c NumConst.one ->
			string_of_factor customized_string discrete_factor
		| DT_mul (discrete_term, discrete_factor) ->
            string_of_jani_binary_expression
                Constants.default_arithmetic_string_without_whitespace.mul_string
                (string_of_term customized_string discrete_term)
                (string_of_factor customized_string discrete_factor)


		| DT_div (discrete_term, discrete_factor) ->
		    string_of_jani_binary_expression
		        Constants.default_arithmetic_string_without_whitespace.div_string
		        (string_of_term customized_string discrete_term)
                (string_of_factor customized_string discrete_factor)


		| DT_factor discrete_factor ->
		    string_of_factor customized_string discrete_factor

	and string_of_factor customized_string = function
		| DF_variable discrete_index -> jani_quoted (variable_names discrete_index)
		| DF_constant discrete_value -> NumConst.string_of_numconst discrete_value
        | Rational_array_access (array_expr, index_expr) ->
            "{"
            ^ "\"op\": \"aa\","
            ^ "\"exp\": " ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr ^ ", "
            ^ "\"index\": " ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names index_expr
            ^ "}"
		| DF_unary_min discrete_factor ->
		    string_of_jani_unary_expression
		        Constants.default_arithmetic_string_without_whitespace.unary_min_string
                (string_of_factor customized_string discrete_factor)

		| DF_expression expr ->
			(*** TODO: simplify a bit? ***)
			string_of_arithmetic_expression customized_string expr
		| DF_rational_of_int expr ->
		    customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr
        | DF_pow (expr, exp) as factor ->
            string_of_jani_binary_expression
                (label_of_rational_factor factor)
                (string_of_arithmetic_expression customized_string expr)
                (customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names exp)

	(* Call top-level *)
	in string_of_arithmetic_expression customized_string

and customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names =
    let rec string_of_int_arithmetic_expression customized_string = function
        (* Shortcut: Remove the "+0" / -"0" cases *)
        | Int_plus (discrete_arithmetic_expression, Int_factor (Int_constant c))
        | Int_minus (discrete_arithmetic_expression, Int_factor (Int_constant c)) when Int32.equal c Int32.zero ->
            string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression

	| Int_plus (discrete_arithmetic_expression, discrete_term) ->
	    string_of_jani_binary_expression
		    customized_string.arithmetic_string.plus_string
            (string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression)
            (string_of_int_term customized_string discrete_term)


	| Int_minus (discrete_arithmetic_expression, discrete_term) ->
	    string_of_jani_binary_expression
		    customized_string.arithmetic_string.minus_string
            (string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression)
            (string_of_int_term customized_string discrete_term)


        | Int_term discrete_term -> string_of_int_term customized_string discrete_term

	and string_of_int_term customized_string = function
		(* Eliminate the '1' coefficient *)
		| Int_mul (Int_factor (Int_constant c), discrete_factor) when Int32.equal c Int32.one ->
			string_of_int_factor customized_string discrete_factor
		| Int_mul (discrete_term, discrete_factor) ->
		    string_of_jani_binary_expression
                Constants.default_arithmetic_string_without_whitespace.mul_string
                (string_of_int_term customized_string discrete_term)
                (string_of_int_factor customized_string discrete_factor)


		| Int_div (discrete_term, discrete_factor) ->
		    string_of_jani_binary_expression
                Constants.default_arithmetic_string_without_whitespace.div_string
                (string_of_int_term customized_string discrete_term)
                (string_of_int_factor customized_string discrete_factor)


		| Int_factor discrete_factor -> string_of_int_factor customized_string discrete_factor

	and string_of_int_factor customized_string = function
		| Int_variable discrete_index -> "\"" ^ variable_names discrete_index ^ "\""
		| Int_constant value -> Int32.to_string value
        | Int_array_access (array_expr, index_expr) ->
            "{"
            ^ "\"op\": \"aa\","
            ^ "\"exp\": " ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr ^ ", "
            ^ "\"index\": " ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names index_expr
            ^ "}"
		| Int_unary_min discrete_factor ->
		    string_of_jani_unary_expression
                Constants.default_arithmetic_string_without_whitespace.unary_min_string
                (string_of_int_factor customized_string discrete_factor)

		| Int_expression discrete_arithmetic_expression ->
			(*** TODO: simplify a bit? ***)
			string_of_int_arithmetic_expression customized_string discrete_arithmetic_expression
        | Int_pow (expr, exp) as factor ->
            string_of_jani_binary_expression
                (label_of_int_factor factor)
                (string_of_int_arithmetic_expression customized_string expr)
                (string_of_int_arithmetic_expression customized_string exp)

	(* Call top-level *)
	in string_of_int_arithmetic_expression customized_string

and customized_string_of_binary_word_expression_for_jani customized_string variable_names = function
    | Logical_fill_left (binary_word, expr)
    | Logical_shift_left (binary_word, expr) as binary_word_expression ->
        string_of_jani_binary_expression
            (label_of_binary_word_expression binary_word_expression)
            (customized_string_of_binary_word_expression_for_jani customized_string variable_names binary_word)
            (customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr)


    | Logical_fill_right (binary_word, expr)
    | Logical_shift_right (binary_word, expr) as binary_word_expression ->
        string_of_jani_binary_expression
            (label_of_binary_word_expression binary_word_expression)
            (customized_string_of_binary_word_expression_for_jani customized_string variable_names binary_word)
            (customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names expr)


    | Logical_and (l_binary_word, r_binary_word)
    | Logical_or (l_binary_word, r_binary_word)
    | Logical_xor (l_binary_word, r_binary_word) as binary_word_expression ->
        string_of_jani_binary_expression
            (label_of_binary_word_expression binary_word_expression)
            (customized_string_of_binary_word_expression_for_jani customized_string variable_names l_binary_word)
            (customized_string_of_binary_word_expression_for_jani customized_string variable_names r_binary_word)


    | Logical_not binary_word as binary_word_expression ->
        string_of_jani_unary_expression
	        (label_of_binary_word_expression binary_word_expression)
	        (customized_string_of_binary_word_expression_for_jani customized_string variable_names binary_word)


    | Binary_word_constant value -> BinaryWord.string_of_binaryword value
    | Binary_word_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | Binary_word_array_access (array_expr, index_expr) ->
        "{"
        ^ "\"op\": \"aa\","
        ^ "\"exp\": " ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr ^ ", "
        ^ "\"index\": " ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names index_expr
        ^ "}"


and customized_string_of_array_expression_for_jani customized_string variable_names = function
    | Literal_array expr_array ->
        let str_expr = Array.map (customized_string_of_global_expression_for_jani customized_string variable_names ) expr_array in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep "," str_expr in
        "{\"op\":\"av\",\"elements\":[" ^ str_values ^ "]}"

    | Array_constant values ->
        let str_values = Array.map DiscreteValue.string_of_value values in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep "," str_values in
        "{\"op\":\"av\",\"elements\":[" ^ str_values ^ "]}"

    | Array_variable variable_index -> "\"" ^ variable_names variable_index ^ "\""
    | Array_array_access (array_expr, index_expr) ->
        "{"
        ^ "\"op\": \"aa\","
        ^ "\"exp\": " ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr ^ ","
        ^ "\"index\": " ^ customized_string_of_int_arithmetic_expression_for_jani customized_string variable_names index_expr
        ^ "}"
    | Array_concat (array_expr_0, array_expr_1) as func ->
        "{"
        ^ "\"op\":\"call\","
        ^ "\"function\":" ^ label_of_array_expression func ^ ","
        ^ "\"args\":["
        ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr_0 ^ ","
        ^ customized_string_of_array_expression_for_jani customized_string variable_names array_expr_1
        ^ "]"
        ^ "}"

(*JANI*)
(* Get list of non-linear constraint inequalities with customized strings *)
let customized_strings_of_nonlinear_constraint_for_jani customized_string variable_names = function
    | True_nonlinear_constraint -> [customized_string.boolean_string.true_string]
    | False_nonlinear_constraint -> [customized_string.boolean_string.false_string]
    | Nonlinear_constraint nonlinear_constraint ->
	(List.rev_map (customized_string_of_discrete_boolean_expression_for_jani customized_string variable_names ) nonlinear_constraint)



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
    "\"jani-version\": " ^ jani_version ^ jani_separator ^ ""
    ^  "\"name\": " ^ "\"" ^ options#model_file_name ^ "\"" ^ jani_separator ^ ""
    ^  "\"type\": " ^ "\"" ^ jani_type ^ "\"" ^ jani_separator ^ ""
    ^  "\"features\": " ^ jani_features ^ jani_separator ^ ""

(************************************************************
 Declarations
************************************************************)

let string_of_binary_word_datatype =
    "{"
    ^ "\"name\":\"binary_word\","
    ^ "\"members\":[{\"name\":\"elements\","
    ^ "\"type\":{\"kind\":\"array\",\"base\":\"bool\"}}]"
    ^ "}"

let string_of_custom_datatype =
    "\"datatypes\":[" ^ string_of_binary_word_datatype ^ "]" ^ jani_separator


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
  ^ "]" ^ jani_separator

(* Convert the initial clocks declarations into a string *)
let string_of_clocks model =
  let multirate_bol = model.has_non_1rate_clocks in
  let string_of_variables list_of_variables =
    string_of_list_of_string_with_sep (jani_separator^"") (List.map
      (fun var ->
		let clocks_type = if multirate_bol then "continuous" else "clock" in
          "{"
        ^ "\"name\": \"" ^ model.variable_names var ^ "\"" ^ jani_separator
        ^ "\"type\": \""^ clocks_type ^"\"" ^ jani_separator
        ^ "\"initial-value\": 0"
        ^ "}"
      )
      list_of_variables
    )
  in
  if model.nb_clocks > 0 then
    (string_of_variables model.clocks_without_special_reset_clock)
    else ""

(* String of number var type *)
let string_of_var_type_discrete_number_for_jani = function
    | DiscreteValue.Var_type_discrete_rational -> jani_quoted "real"
    | DiscreteValue.Var_type_discrete_int -> jani_quoted "int"
    | DiscreteValue.Var_type_discrete_unknown_number -> jani_quoted "number"

(* String of discrete var type *)
let rec string_of_var_type_discrete_for_jani = function
    | DiscreteValue.Var_type_discrete_number x -> string_of_var_type_discrete_number_for_jani x
    | DiscreteValue.Var_type_discrete_bool -> jani_quoted "bool"
    | DiscreteValue.Var_type_discrete_binary_word _ -> "{\"kind\":\"datatype\",\"ref\":" ^ jani_quoted "binary_word" ^ "}"
    | DiscreteValue.Var_type_discrete_array (discrete_type, _) ->
        "{\"kind\":\"array\",\"base\":" ^ string_of_var_type_discrete_for_jani discrete_type ^ "}"

(* Convert the initial discrete var declarations into a string *)
let string_of_discrete model =

    let string_of_initial_value discrete_name initial_value = function
        | DiscreteValue.Var_type_discrete_array (inner_type, length) ->
            let array_value = DiscreteValue.array_value initial_value in
            let str_values = Array.map DiscreteValue.string_of_value array_value in
            let str_array = OCamlUtilities.string_of_array_of_string_with_sep "," str_values in
            "{\"op\":\"av\",\"elements\":[" ^ str_array ^ "]}"
        | DiscreteValue.Var_type_discrete_binary_word _ ->
            let array_value = BinaryWord.to_array (DiscreteValue.binary_word_value initial_value) in
            let str_values = Array.map (fun x -> if x then "true" else "false") array_value in
            let str_array = OCamlUtilities.string_of_array_of_string_with_sep "," str_values in
            "{\"op\":\"dv\",\"type\":\"binary_word\",\"values\":[{\"member\":\"elements\",\"value\":"
            ^ "{\"op\":\"av\",\"elements\":[" ^ str_array ^ "]}"
            ^ "}]}"
        | _ ->
            DiscreteValue.string_of_value initial_value
    in

	if model.nb_discrete > 0 then(
		(string_of_list_of_string_with_sep jani_separator
			(List.map (fun discrete_index ->
				(* Get the name *)
				let discrete_name = model.variable_names discrete_index in
                (* Get the type *)
                let discrete_type = DiscreteValue.discrete_type_of_var_type (model.type_of_variables discrete_index) in
                (* Get str for the type*)
                let str_discrete_type = string_of_var_type_discrete_for_jani discrete_type in
				(* Get the initial value *)
				let inital_global_location  = model.initial_location in

				let initial_value = Location.get_discrete_value inital_global_location discrete_index in
                let initial_value_type = DiscreteValue.discrete_type_of_value initial_value in

				let str_initial_value = string_of_initial_value discrete_name initial_value initial_value_type in

				(* Assign *)
                "{"
                ^ "\"name\": \"" ^ discrete_name ^ "\"" ^ jani_separator
                ^ "\"type\":" ^ str_discrete_type ^ jani_separator
                ^ "\"initial-value\": " ^ str_initial_value
                ^ "}"
            ) model.discrete
			)
		)
	) else ""

(* Convert the parameter declarations into a string *)
let string_of_parameters model =
	if model.nb_parameters > 0 then (
		(string_of_list_of_string_with_sep (jani_separator^"")
			(List.map (fun parameter_index ->
				(* Get the name *)
				let parameter_name = model.variable_names parameter_index in

				(* Assign *)
                "{"
                ^ "\"name\": \"" ^ parameter_name ^ "\"" ^ jani_separator
                ^ "\"type\": \"real\""
                ^ "}"
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
    "\"properties\": []" ^ jani_separator

(************************************************************)
(** Automata *)
(************************************************************)

let rec string_of_strings_with_sep_and string_list =
	match string_list with
	| [] -> ""
	| (elem::[]) -> elem
	| (elem::q) ->
        "{\"op\": \"" ^ jani_boolean_strings.and_operator ^ "\"" ^ jani_separator
        ^ "\"left\": " ^ elem ^ jani_separator
        ^ "\"right\": " ^ string_of_strings_with_sep_and q ^ "}"



(** Convert a guard or an invariant into a string *)
let rec string_of_guard_or_invariant actions_and_nb_automata variable_names = function
	(* True guard = no guard *)
	| True_guard -> ""

	(* False *)
	| False_guard -> "{\"exp\":" ^ jani_boolean_strings.false_string ^ "}"

	| Discrete_guard discrete_guard ->

        let list_discrete_guard = (customized_strings_of_nonlinear_constraint_for_jani jani_strings variable_names discrete_guard) in
        let list_discrete_guard_without_true = if list_discrete_guard = [jani_boolean_strings.true_string] then [""] else list_discrete_guard in
        let content = string_of_strings_with_sep_and list_discrete_guard_without_true in
        if content = "" then "" else "{\"exp\":" ^ content ^ "}"

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
					"{\"exp\":"
                    ^ "{\"op\": \"" ^ op ^ "\"" ^ jani_separator
					^ "\"left\": " ^ left ^ jani_separator
					^ "\"right\": " ^ right ^ "}"
					^ "}"
				) list_of_inequalities)

			)

	| Discrete_continuous_guard discrete_continuous_guard ->
		let non_linear_constraint_list = customized_strings_of_nonlinear_constraint_for_jani jani_strings variable_names discrete_continuous_guard.discrete_guard in
		let linear_constraint_string = (string_of_guard_or_invariant actions_and_nb_automata variable_names (Continuous_guard discrete_continuous_guard.continuous_guard)) in
		let list = List.append non_linear_constraint_list [linear_constraint_string] in
	    let content = string_of_strings_with_sep_and list in
	    if content = "" then "" else "{\"exp\":" ^ content ^ "}"


(* Convert the invariant of a location into a string *)
let string_of_invariant model actions_and_nb_automata automaton_index location_index =
  let invariant = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (model.invariants automaton_index location_index) in
	(* Invariant *)
	"" ^ invariant

(* Convert the guard of an edge into a string *)
let string_of_guard model actions_and_nb_automata model_variable_names transition_guard =
  let guard = string_of_guard_or_invariant actions_and_nb_automata model.variable_names (transition_guard) in
  (* Guard *)
  "" ^ guard

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
		  jani_separator ^ "\"time-progress\": {\"exp\": "
		^ (if twoparts then ("{\"op\": \"" ^ jani_boolean_strings.and_operator ^ "\"" ^ jani_separator) else "")
		^ (if twoparts then "\"left\": " else "") ^ invariant ^ (if twoparts then "" ^ jani_separator else "")
		^ (if twoparts then "\"right\": " else "") ^ der_clock ^ (if twoparts then "}" else "")
		^ "}"
	))

	(* Footer *)
	^ "}"

(* Convert the locations of an automaton into a string *)
let string_of_locations model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep (jani_separator^"") (List.map (fun location_index ->
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
		string_of_list_of_string_with_sep (jani_separator^"") (List.map (fun variable_index ->
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
		^ (customized_string_of_global_expression_for_jani jani_strings model.variable_names global_expression)
		^ "}"
	) updates)
(*
  (** Generic template to convert conditional updates into a string *)
  let string_of_conditional_updates_template model conditional_updates string_of_clock_updates string_of_discrete_updates wrap_if wrap_else wrap_end sep =
  	string_of_list_of_string_with_sep sep (List.map (fun (boolean_expr, if_updates, else_updates) ->
  		let if_separator, _ = separator_comma if_updates in
  		let empty_else = no_clock_updates else_updates.clock && else_updates.discrete = [] && else_updates.conditional = [] in
  		(** Convert the Boolean expression *)
  		(wrap_if boolean_expr)
  		(** Convert the if updates *)
  		^ (string_of_clock_updates model if_updates.clock)
  		^ (if if_separator then sep else "")
  		^ (string_of_discrete_updates model if_updates.discrete)
  		(** Convert the else updates *)
  		^ (if empty_else then "" else
  			let else_separator, _ = separator_comma else_updates in
  			wrap_else
  			^ (string_of_clock_updates model else_updates.clock)
  			^ (if else_separator then sep else "")
  			^ (string_of_discrete_updates model else_updates.discrete))
  		^ wrap_end
  	) conditional_updates)
*)
(** Return if there is no clock updates *)
let no_clock_updates clock_updates =
	clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []

(** Convert a discrete_boolean_expression into a string *)
let string_of_discrete_boolean_expression variable_names =
	customized_string_of_discrete_boolean_expression_for_jani jani_strings variable_names

(* TODO benjamin CLEAN look if really necessary ? duplicate ? *)
(** Convert a Boolean expression into a string *)
let rec string_of_boolean variable_names = function
	| True_bool -> string_of_true
	| False_bool -> string_of_false
	| And_bool (b1, b2) ->
	    string_of_jani_binary_expression
		    jani_boolean_strings.and_operator
		    (string_of_boolean variable_names b1)
		    (string_of_boolean variable_names b2)

	| Or_bool (b1, b2) ->
	    string_of_jani_binary_expression
		    jani_boolean_strings.or_operator
		    (string_of_boolean variable_names b1)
		    (string_of_boolean variable_names b2)

	| Discrete_boolean_expression discrete_boolean_expression ->
		string_of_discrete_boolean_expression variable_names discrete_boolean_expression

let string_of_conditional_clock_updates model boolean_expr order = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep (jani_separator^"") (List.map (fun variable_index ->
			let variable_name = "\"" ^ (model.variable_names variable_index) ^ "\"" in
			"{\"ref\": " ^ variable_name ^ jani_separator
			^ " \"value\" : "
			^ "{\"op\": \"ite\"" ^ jani_separator
			^ "\"if\":" ^ string_of_boolean model.variable_names boolean_expr ^ jani_separator
			^ "\"then\":" ^ (if order="if" then "0" else variable_name) ^ jani_separator
			^ "\"else\":" ^ (if order="if" then variable_name else "0")
			^ "}}"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep (jani_separator^"") (List.map (fun (variable_index, linear_term) ->
			let variable_name = "\"" ^ (model.variable_names variable_index) ^ "\"" in
			let expression = (LinearConstraint.string_of_pxd_linear_term_for_jani model.variable_names linear_term) in
			"{\"ref\": " ^ variable_name ^ jani_separator
			^ " \"value\" : "
			^ " {\"op\": \"ite\"" ^ jani_separator
			^ "\"if\":" ^ string_of_boolean model.variable_names boolean_expr ^ jani_separator
			^ "\"then\":" ^ (if order="if" then expression else variable_name) ^ jani_separator
			^ "\"else\":" ^ (if order="if" then variable_name else expression)
			^ "}}"
		) list_of_clocks_lt)

(* Convert a list of discrete updates into a string *)
let string_of_conditional_discrete_updates model boolean_expr order updates =
	string_of_list_of_string_with_sep (jani_separator^"") (List.rev_map (fun (variable_access, global_expression) ->

		let expression = (customized_string_of_global_expression_for_jani jani_strings model.variable_names global_expression) in
		let variable_name = "\"" ^ ModelPrinter.string_of_variable_access model variable_access ^ "\"" in
		"{\"ref\": " ^ variable_name ^ jani_separator
		^ " \"value\" : "
		^ " {\"op\": \"ite\"" ^ jani_separator
		^ "\"if\":" ^ string_of_boolean model.variable_names boolean_expr ^ jani_separator
		^ "\"then\":" ^ (if order="if" then expression else variable_name) ^ jani_separator
		^ "\"else\":" ^ (if order="if" then variable_name else expression)
		^ "}}"
	) updates)

(** Convert a list of conditional updates into a string *)
let string_of_conditional_updates model conditional_updates =
  string_of_list_of_string_with_sep (jani_separator^"") (List.map (fun (boolean_expr, if_updates, else_updates) ->
    let empty_else = no_clock_updates else_updates.clock && else_updates.discrete = [] && else_updates.conditional = [] in
	(*
	(** Convert the if updates *)
    ^ (string_of_conditional_clock_updates model if_updates.clock)
    ^ (if if_separator then jani_separator else "")
    ^ (string_of_discrete_updates model if_updates.discrete)
    (** Convert the else updates *)
    ^ (if empty_else then "" else
      let else_separator, _ = separator_comma else_updates in
      wrap_else
      ^ (string_of_clock_updates model else_updates.clock)
      ^ (if else_separator then sep else "")
      ^ (string_of_discrete_updates model else_updates.discrete))
    ^ wrap_end
*)
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
					jani_separator^""
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
		^ (if first_separator then jani_separator^"" else "")
		^ (string_of_discrete_updates model discrete_updates)
    ^ (if second_separator then jani_separator^"" else "")
    ^ (string_of_conditional_updates model conditional_updates)
		^ ""
	)

(* Convert a transition of a location into a string *)
let string_of_transition model actions_and_nb_automata automaton_index source_location transition =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	let guard = (string_of_guard model actions_and_nb_automata model.variable_names transition.guard) in
	let assignments = (string_of_updates model automaton_index transition.action clock_updates discrete_updates transition) in
	(* Header *)
	"{"

	(* Source *)
	^ "\"location\": \"" ^ (model.location_names automaton_index source_location) ^ "\"" ^ jani_separator

	(* Guard *)
	^ (if guard = "" then "" else
		((
			"\"guard\": " ^ guard ^ ""
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
	string_of_list_of_string_with_sep (jani_separator^"")
			(
			(* For each location *)
			List.map (fun location_index ->
				string_of_list_of_string_with_sep (jani_separator^"") (
				(* For each action *)
				List.map (fun action_index ->
					(* Get the list of transitions *)
					let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
					(* Convert to string *)
					string_of_list_of_string_with_sep (jani_separator^"") (
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
  ^ string_of_list_of_string_with_sep (jani_separator^"") (
  		List.map (fun automaton_index ->
        string_of_automaton model actions_and_nb_automata automaton_index
  	) pta_without_obs)
  ^ "]" ^ jani_separator ^ ""


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
		^ "" ^ (string_of_list_of_string_with_sep (jani_separator^"") (
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
		^ "" ^ (string_of_list_of_string_with_sep (jani_separator^"") (
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
    let actions_and_nb_automata = List.map (fun action_index ->
        (* Get number of automata *)
        let nb_automata = List.length (model.automata_per_action action_index) in
        (* Make it a pair *)
        action_index , nb_automata
    ) model.actions
    in

    let variables = string_of_variables model in
    let ugly_json_model =
    "{"
    (*Header*)
    ^ string_of_header model
    ^ string_of_custom_datatype
    ^ string_of_actions model
    ^ (if variables = "" then "" else variables ^ jani_separator ^ "")
    ^ string_of_properties
    ^ string_of_automata model actions_and_nb_automata
    ^ string_of_system model
    ^ "}"
    in
    OCamlUtilities.prettify_json ugly_json_model

