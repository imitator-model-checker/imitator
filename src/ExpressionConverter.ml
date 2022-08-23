open ParsingStructure
open VariableInfo
open ParsingStructureUtilities
open ImitatorUtilities
open OCamlUtilities
open Exceptions
open FunctionSig
open TypeConstraintResolver
open CustomModules
open DiscreteType


(* Type checking module *)
(* This module aim to type check expressions and create a typed tree from a parsed tree *)
module rec TypeChecker : sig


type inner_type = var_type_discrete

type typed_variable_scope =
    | Global
    | Local

type typed_sequence_type =
    | Typed_array
    | Typed_list
    | Typed_stack
    | Typed_queue

type typed_conj_dis =
    | Typed_and
    | Typed_or

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
    | Typed_conj_dis of typed_boolean_expression * typed_boolean_expression * typed_conj_dis (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
    | Typed_sum_diff of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number * typed_sum_diff
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_sum_diff =
    | Typed_plus
    | Typed_minus

and typed_discrete_term =
    | Typed_product_quotient of typed_discrete_term * typed_discrete_factor * var_type_discrete_number * typed_product_quotient
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_product_quotient =
    | Typed_mul
    | Typed_div

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete * typed_variable_scope
	| Typed_constant of ParsedValue.parsed_value * var_type_discrete
	| Typed_sequence of typed_boolean_expression list * inner_type * typed_sequence_type
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_scalar_or_index_update_type =
    | Typed_scalar_update of variable_name
    | Typed_indexed_update of typed_scalar_or_index_update_type * typed_discrete_arithmetic_expression * var_type_discrete

type typed_update_type =
    | Typed_variable_update of typed_scalar_or_index_update_type
    | Typed_void_update

type typed_normal_update = typed_update_type * typed_global_expression

type typed_loc_predicate =
	| Typed_loc_predicate_EQ of automaton_name * location_name
	| Typed_loc_predicate_NEQ of automaton_name * location_name

type typed_simple_predicate =
	| Typed_discrete_boolean_expression of typed_discrete_boolean_expression * var_type_discrete
	| Typed_loc_predicate of typed_loc_predicate
	| Typed_state_predicate_true
	| Typed_state_predicate_false
	| Typed_state_predicate_accepting

type typed_state_predicate_factor =
	| Typed_state_predicate_factor_NOT of typed_state_predicate_factor
	| Typed_simple_predicate of typed_simple_predicate * var_type_discrete
	| Typed_state_predicate of typed_state_predicate * var_type_discrete

and typed_state_predicate_term =
	| Typed_state_predicate_term_AND of typed_state_predicate_term * typed_state_predicate_term
	| Typed_state_predicate_factor of typed_state_predicate_factor * var_type_discrete

and typed_state_predicate =
	| Typed_state_predicate_OR of typed_state_predicate * typed_state_predicate
	| Typed_state_predicate_term of typed_state_predicate_term * var_type_discrete

type typed_guard = typed_discrete_boolean_expression list

type typed_fun_body =
    | Typed_fun_local_decl of variable_name * var_type_discrete * typed_global_expression * typed_fun_body
    | Typed_fun_instruction of typed_normal_update * typed_fun_body
    | Typed_fun_expr of typed_global_expression


type typed_fun_definition = {
    name : variable_name; (* function name *)
    parameters : variable_name list; (* parameter names *)
    signature : var_type_discrete list; (* signature *)
    body : typed_fun_body; (* body *)
    side_effect : bool;
}

val string_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> string
val string_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> string
val string_of_typed_discrete_arithmetic_expression : variable_infos -> var_type_discrete -> typed_discrete_arithmetic_expression -> string
val string_of_typed_discrete_term : variable_infos -> var_type_discrete -> typed_discrete_term -> string
val string_of_typed_discrete_factor : variable_infos -> var_type_discrete -> typed_discrete_factor -> string

(* Check that a discrete init is well typed *)
val check_discrete_init : variable_infos -> variable_name -> parsed_global_expression -> typed_global_expression
(* Check that a constant declarations is well typed *)
val check_constant_expression : variable_infos -> variable_name * parsed_global_expression * DiscreteType.var_type -> typed_global_expression
(* Check that a guard is well typed *)
val check_guard : variable_infos -> guard -> typed_guard
(* Check that an update is well typed *)
val check_update : variable_infos -> updates_type -> parsed_update_type -> ParsingStructure.parsed_global_expression -> typed_update_type * typed_global_expression
(* Check that a condition is well typed *)
val check_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> typed_boolean_expression
(* Check that a predicate is well typed *)
val check_state_predicate : variable_infos -> parsed_state_predicate -> typed_state_predicate
(* Check that a function definition is well typed *)
val check_fun_definition : variable_infos -> parsed_fun_definition -> typed_fun_definition

end = struct



type inner_type = var_type_discrete

type typed_variable_scope =
    | Global
    | Local

type typed_sequence_type =
    | Typed_array
    | Typed_list
    | Typed_stack
    | Typed_queue

type typed_conj_dis =
    | Typed_and
    | Typed_or

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
    | Typed_conj_dis of typed_boolean_expression * typed_boolean_expression * typed_conj_dis (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
    | Typed_sum_diff of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number * typed_sum_diff
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_sum_diff =
    | Typed_plus
    | Typed_minus

and typed_discrete_term =
    | Typed_product_quotient of typed_discrete_term * typed_discrete_factor * var_type_discrete_number * typed_product_quotient
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_product_quotient =
    | Typed_mul
    | Typed_div

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete * typed_variable_scope
	| Typed_constant of ParsedValue.parsed_value * var_type_discrete
    | Typed_sequence of typed_boolean_expression list * inner_type * typed_sequence_type
    | Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_scalar_or_index_update_type =
    | Typed_scalar_update of variable_name
    | Typed_indexed_update of typed_scalar_or_index_update_type * typed_discrete_arithmetic_expression * var_type_discrete

type typed_update_type =
    | Typed_variable_update of typed_scalar_or_index_update_type
    | Typed_void_update

type typed_normal_update = typed_update_type * typed_global_expression

type typed_loc_predicate =
	| Typed_loc_predicate_EQ of automaton_name * location_name
	| Typed_loc_predicate_NEQ of automaton_name * location_name

type typed_simple_predicate =
	| Typed_discrete_boolean_expression of typed_discrete_boolean_expression * var_type_discrete
	| Typed_loc_predicate of typed_loc_predicate
	| Typed_state_predicate_true
	| Typed_state_predicate_false
	| Typed_state_predicate_accepting

type typed_state_predicate_factor =
	| Typed_state_predicate_factor_NOT of typed_state_predicate_factor
	| Typed_simple_predicate of typed_simple_predicate * var_type_discrete
	| Typed_state_predicate of typed_state_predicate * var_type_discrete

and typed_state_predicate_term =
	| Typed_state_predicate_term_AND of typed_state_predicate_term * typed_state_predicate_term
	| Typed_state_predicate_factor of typed_state_predicate_factor * var_type_discrete

and typed_state_predicate =
	| Typed_state_predicate_OR of typed_state_predicate * typed_state_predicate
	| Typed_state_predicate_term of typed_state_predicate_term * var_type_discrete

type typed_guard = typed_discrete_boolean_expression list

type typed_fun_body =
    | Typed_fun_local_decl of variable_name * var_type_discrete * typed_global_expression * typed_fun_body
    | Typed_fun_instruction of typed_normal_update * typed_fun_body
    | Typed_fun_expr of typed_global_expression

type typed_fun_definition = {
    name : variable_name; (* function name *)
    parameters : variable_name list; (* parameter names *)
    signature : var_type_discrete list; (* signature *)
    body : typed_fun_body; (* body *)
    side_effect : bool;
}

(** Strings **)

let string_format_typed_node str_node discrete_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete discrete_type ^ "}"

let string_format_number_typed_node str_node discrete_number_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete_number discrete_number_type ^ "}"

let string_of_typed_sum_diff = function
    | Typed_plus -> Constants.default_arithmetic_string.plus_string
    | Typed_minus -> Constants.default_arithmetic_string.minus_string

let string_of_typed_product_quotient = function
    | Typed_mul -> Constants.default_arithmetic_string.mul_string
    | Typed_div -> Constants.default_arithmetic_string.div_string

let string_of_typed_conj_dis = function
    | Typed_and -> Constants.default_string.and_operator
    | Typed_or -> Constants.default_string.or_operator

let rec string_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, _) ->
        string_of_typed_boolean_expression variable_infos expr

and string_of_typed_boolean_expression variable_infos = function
    | Typed_conj_dis (l_expr, r_expr, typed_conj_dis) ->
        string_of_typed_boolean_expression variable_infos l_expr
        ^ string_of_typed_conj_dis typed_conj_dis
        ^ string_of_typed_boolean_expression variable_infos r_expr

	| Typed_discrete_bool_expr (expr, _) ->
        string_of_typed_discrete_boolean_expression variable_infos expr

and string_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        string_of_typed_discrete_arithmetic_expression variable_infos discrete_type expr

	| Typed_comparison (l_expr, relop, r_expr, discrete_type) ->
	    let str_l_expr = string_of_typed_discrete_boolean_expression variable_infos l_expr in
	    let str_r_expr = string_of_typed_discrete_boolean_expression variable_infos r_expr in
	    let str_node = ParsingStructureUtilities.string_of_parsed_relop relop str_l_expr str_r_expr in
        let x = str_node ^ " " ^ DiscreteType.string_of_var_type_discrete discrete_type ^ " comparison " in
        string_format_typed_node x Var_type_discrete_bool

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_number_type) ->
	    let discrete_type = Var_type_discrete_number discrete_number_type in
	    let str_node =
            string_of_typed_discrete_arithmetic_expression variable_infos discrete_type in_expr
            ^ Constants.default_string.in_operator
            ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type lw_expr
            ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type up_expr
        in
        string_format_typed_node str_node Var_type_discrete_bool

	| Typed_bool_expr expr ->
        string_of_typed_boolean_expression variable_infos expr

	| Typed_not_expr expr ->
	    Constants.default_string.not_operator
	    ^ "("
        ^ string_of_typed_boolean_expression variable_infos expr
        ^ ")"

and string_of_typed_discrete_arithmetic_expression variable_infos discrete_type = function
	| Typed_sum_diff (l_expr, r_expr, _, typed_sum_diff) ->
	        string_of_typed_discrete_arithmetic_expression variable_infos discrete_type l_expr
	        ^ string_of_typed_sum_diff typed_sum_diff
	        ^ string_of_typed_discrete_term variable_infos discrete_type r_expr

	| Typed_term (term, _) ->
	        string_of_typed_discrete_term variable_infos discrete_type term

and string_of_typed_discrete_term variable_infos discrete_type = function
	| Typed_product_quotient (l_expr, r_expr, _, typed_product_quotient) ->
        string_of_typed_discrete_term variable_infos discrete_type l_expr
        ^ string_of_typed_product_quotient typed_product_quotient
        ^ string_of_typed_discrete_factor variable_infos discrete_type r_expr

	| Typed_factor (factor, _) ->
            string_of_typed_discrete_factor variable_infos discrete_type factor

and string_of_typed_discrete_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _, _) ->
	    string_format_typed_node variable_name discrete_type
	| Typed_constant (value, _) ->
        string_format_typed_node (ParsedValue.string_of_value value) discrete_type

    | Typed_sequence (list_expr, _, seq_type) ->
	    let l_del, r_del = Constants.default_array_string.array_literal_delimiter in
	    let l_par_del, r_par_del = Constants.default_paren_delimiter in

	    let str_elements = List.map (string_of_typed_boolean_expression variable_infos) list_expr in
	    let str_array = l_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_elements ^ r_del in

	    let str_node =
	        match seq_type with
	        | Typed_array -> str_array
	        | Typed_list -> Constants.list_string ^ l_par_del ^ str_array ^ r_par_del
	        | Typed_stack -> Constants.stack_string ^ l_par_del ^ str_array ^ r_par_del
	        | Typed_queue -> Constants.queue_string ^ l_par_del ^ str_array ^ r_par_del
        in

	    string_format_typed_node str_node discrete_type

	| Typed_expr (expr, _) ->
	    let str_node = "(" ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type expr ^ ")" in
        string_format_typed_node str_node discrete_type


    | Typed_access (factor, index_expr, discrete_type, inner_type) ->
        let str_node =
            string_of_typed_discrete_factor variable_infos discrete_type factor
            ^ "["
            ^ string_of_typed_discrete_arithmetic_expression variable_infos (Var_type_discrete_number Var_type_discrete_int) index_expr
            ^ "]"
        in
        string_format_typed_node str_node inner_type

	| Typed_function_call (function_name, argument_expressions, _) ->
	    let str_arguments_list = List.map (string_of_typed_boolean_expression variable_infos) argument_expressions in
	    let str_arguments = OCamlUtilities.string_of_list_of_string_with_sep ", " str_arguments_list in
	    let str_node =
            function_name
            ^ "("
            ^ str_arguments
            ^ ")"
	    in
	    string_format_typed_node str_node discrete_type

	| Typed_unary_min (factor, _) ->
	    Constants.default_arithmetic_string.unary_min_string
        ^ string_of_typed_discrete_factor variable_infos discrete_type factor

let rec string_of_typed_scalar_or_index_update_type variable_infos = function
    | Typed_scalar_update variable_name -> variable_name
    | Typed_indexed_update (typed_scalar_or_index_update_type, index_expr, discrete_type) ->
        string_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type
        ^ "[" ^ string_of_typed_discrete_arithmetic_expression variable_infos (Var_type_discrete_number Var_type_discrete_int) index_expr ^ "]"

let string_of_typed_update_type variable_infos = function
    | Typed_variable_update typed_scalar_or_index_update_type ->
        string_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type
    | Typed_void_update -> ""

let rec string_of_fun_body variable_infos = function
    | Typed_fun_local_decl (variable_name, discrete_type, expr, next_expr) ->
        ParsingStructureUtilities.string_of_let_in
            variable_name
            (DiscreteType.string_of_var_type_discrete discrete_type)
            (string_of_typed_global_expression variable_infos expr)
        ^ "\n"
        ^ string_of_fun_body variable_infos next_expr

    | Typed_fun_instruction ((typed_update_type, update_expr), next_expr) ->
        let str_left_member = string_of_typed_update_type variable_infos typed_update_type in
        let str_right_member = string_of_typed_global_expression variable_infos update_expr in
        ParsingStructureUtilities.string_of_assignment str_left_member str_right_member
        ^ ";\n"
        ^ string_of_fun_body variable_infos next_expr

    | Typed_fun_expr expr ->
        string_of_typed_global_expression variable_infos expr



let string_of_typed_loc_predicate variable_infos = function
	| Typed_loc_predicate_EQ (automaton_name, location_name) ->
	    automaton_name ^ " = " ^ location_name
	| Typed_loc_predicate_NEQ (automaton_name, location_name) ->
	    automaton_name ^ " <> " ^ location_name

let string_of_typed_simple_predicate variable_infos = function
	| Typed_discrete_boolean_expression (expr, _) ->
	    string_of_typed_discrete_boolean_expression variable_infos expr
	| Typed_loc_predicate (loc_predicate) ->
        string_of_typed_loc_predicate variable_infos loc_predicate
	| Typed_state_predicate_true ->
	    Constants.default_string.true_string
	| Typed_state_predicate_false  ->
	    Constants.default_string.false_string
	| Typed_state_predicate_accepting -> "accepting"

let rec string_of_typed_state_predicate_factor variable_infos = function
	| Typed_state_predicate_factor_NOT (predicate_factor) ->
	    Constants.default_string.not_operator
	    ^ "("
	    ^ string_of_typed_state_predicate_factor variable_infos predicate_factor
	    ^ ")"
	| Typed_simple_predicate (predicate, _) ->
	    string_of_typed_simple_predicate variable_infos predicate
	| Typed_state_predicate (state_predicate, _) ->
        string_of_typed_state_predicate variable_infos state_predicate

and string_of_typed_state_predicate_term variable_infos = function
	| Typed_state_predicate_term_AND (l_term, r_term) ->
	    string_of_typed_state_predicate_term variable_infos l_term
	    ^ Constants.default_string.and_operator
	    ^ string_of_typed_state_predicate_term variable_infos r_term
	| Typed_state_predicate_factor (predicate_factor, _) ->
	    string_of_typed_state_predicate_factor variable_infos predicate_factor

and string_of_typed_state_predicate variable_infos = function
	| Typed_state_predicate_OR (l_predicate, r_predicate) ->
	    string_of_typed_state_predicate variable_infos l_predicate
	    ^ Constants.default_string.or_operator
	    ^ string_of_typed_state_predicate variable_infos r_predicate

	| Typed_state_predicate_term (predicate_term, _) ->
        string_of_typed_state_predicate_term variable_infos predicate_term

(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)

(* Message when many members of an expression are not compatibles *)
let ill_typed_message_of_expressions str_expressions discrete_types str_outer_expr =
    let x = List.combine str_expressions discrete_types in
    let str_expressions_with_type_list = List.map (fun (str_expr, discrete_type) -> "`" ^ str_expr ^ ":" ^ DiscreteType.string_of_var_type_discrete discrete_type ^ "`") x in
    let str_expressions_with_type = OCamlUtilities.string_of_list_of_string_with_sep "," str_expressions_with_type_list in

    "Some of the expressions "
    ^ str_expressions_with_type
    ^ " in `"
    ^ str_outer_expr
    ^ "` are ill-typed or incompatibles."

(* Message when two members of an expression are not compatibles *)
let ill_typed_message str_left_expr str_right_expr str_outer_expr l_type r_type =
    "Some of the expressions `"
    ^ str_left_expr
    ^ ":"
    ^ DiscreteType.string_of_var_type_discrete l_type
    ^ "`, `"
    ^ str_right_expr
    ^ ":"
    ^ DiscreteType.string_of_var_type_discrete r_type
    ^ "` in `"
    ^ str_outer_expr
    ^ "` are ill-typed or incompatibles."

(* Message when variable type not compatible with assigned expression type *)
let ill_typed_variable_message variable_name str_discrete_type str_expr expr_discrete_type =
    "Variable `"
    ^ variable_name
    ^ "` of type "
    ^ str_discrete_type
    ^ " is not compatible with expression `"
    ^ str_expr
    ^ "` of type "
    ^ DiscreteType.string_of_var_type_discrete expr_discrete_type
    ^ "."

(* Get inner type of a collection analysing types of it's elements *)
(* If the collection is empty, it's type will be inferred by the context *)
let type_check_collection discrete_types infer_type_opt =

    (* If list isn't empty, we can deduce easily it's inner type *)
    if List.length discrete_types > 0 then
        List.nth discrete_types 0
    (* If list is empty, either it was inferred by a given type, or not deducible (weak) *)
    else
        (* Some infer type ? *)
        match infer_type_opt with
        (* Yes, we infer to this type *)
        | Some inner_type -> inner_type
        (* No, the type remain unknown for now: weak *)
        | None -> Var_type_weak

let rec type_check_global_expression local_variables_opt variable_infos infer_type_opt = function
    | Parsed_global_expression expr ->
        let typed_expr, discrete_type, has_side_effects = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt expr in
        Typed_global_expr (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt = function
	| Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) as outer_expr ->

        let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt r_expr in

        let typed_conj_dis =
            match parsed_conj_dis with
            | Parsed_and -> Typed_and
            | Parsed_or -> Typed_or
        in

        (* Check that left and right members are Boolean *)
        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_conj_dis (l_typed_expr, r_typed_expr, typed_conj_dis), Var_type_discrete_bool, l_has_side_effects || r_has_side_effects
        | _ ->
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_boolean_expression variable_infos l_expr;
                        string_of_parsed_boolean_expression variable_infos r_expr
                    ]
                    [l_type; r_type]
                    (string_of_parsed_boolean_expression variable_infos outer_expr)
            ))
        )

	| Parsed_Discrete_boolean_expression expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_discrete_bool_expr (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt = function
    | Parsed_arithmetic_expression expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_arithmetic_expr (typed_expr, discrete_type), discrete_type, has_side_effects

	| Parsed_comparison (l_expr, relop, r_expr) as outer_expr ->

	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt r_expr in

        (* Check that left and right members are type compatibles *)
        if is_discrete_type_compatibles l_type r_type then (
            let discrete_type = stronger_discrete_type_of l_type r_type in
            Typed_comparison (l_typed_expr, relop, r_typed_expr, discrete_type), Var_type_discrete_bool, l_has_side_effects || r_has_side_effects
        )
        else
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_discrete_boolean_expression variable_infos l_expr;
                        string_of_parsed_discrete_boolean_expression variable_infos r_expr
                    ]
                    [l_type; r_type]
                    (string_of_parsed_discrete_boolean_expression variable_infos outer_expr)
            ))

	| Parsed_comparison_in (in_expr, lw_expr, up_expr) as outer_expr ->
	    let in_typed_expr, in_type, in_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt in_expr in
	    let lw_typed_expr, lw_type, lw_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt lw_expr in
	    let up_typed_expr, up_type, up_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt up_expr in

        (* Check that expression are numbers *)
        let in_number_type, lw_number_type, up_number_type =
            match in_type, lw_type, up_type with
            | Var_type_discrete_number in_number_type, Var_type_discrete_number lw_number_type, Var_type_discrete_number up_number_type -> in_number_type, lw_number_type, up_number_type
            | _ -> raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_arithmetic_expression variable_infos in_expr;
                        string_of_parsed_arithmetic_expression variable_infos lw_expr;
                        string_of_parsed_arithmetic_expression variable_infos up_expr
                    ]
                    [in_type; lw_type; up_type]
                    (string_of_parsed_discrete_boolean_expression variable_infos outer_expr)
            ))
        in


        (* Check members type compatibility *)
        let in_lw_compatible = is_discrete_type_compatibles in_type lw_type in
        let in_up_compatible = is_discrete_type_compatibles in_type up_type in
        let lw_up_compatible = is_discrete_type_compatibles lw_type up_type in

        if in_lw_compatible && in_up_compatible && lw_up_compatible then (
            let inner_number_type = stronger_discrete_number_type_of (stronger_discrete_number_type_of in_number_type lw_number_type) up_number_type in
            Typed_comparison_in (in_typed_expr, lw_typed_expr, up_typed_expr, inner_number_type), Var_type_discrete_bool, in_has_side_effects || lw_has_side_effects || up_has_side_effects
        )
        else
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_arithmetic_expression variable_infos in_expr;
                        string_of_parsed_arithmetic_expression variable_infos lw_expr;
                        string_of_parsed_arithmetic_expression variable_infos up_expr
                    ]
                    [in_type; lw_type; up_type]
                    (string_of_parsed_discrete_boolean_expression variable_infos outer_expr)
            ))

	| Parsed_boolean_expression expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_bool_expr typed_expr, discrete_type, has_side_effects

	| Parsed_Not expr as outer_expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt expr in

        (* Check that expression type is Boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_not_expr typed_expr, Var_type_discrete_bool, has_side_effects
	    | _ ->
            raise (TypeError (
                "Expression `"
                ^ string_of_parsed_boolean_expression variable_infos expr
                ^ ":"
                ^ DiscreteType.string_of_var_type_discrete discrete_type
                ^ "` in `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos outer_expr
                ^ "` is not a Boolean expression."
            ));
        )

and type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt = function
	| Parsed_sum_diff (expr, term, sum_diff) as outer_expr ->
	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in

        let typed_sum_diff =
            match sum_diff with
            | Parsed_plus -> Typed_plus
            | Parsed_minus -> Typed_minus
        in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_sum_diff (l_typed_expr, r_typed_expr, discrete_number_type, typed_sum_diff), Var_type_discrete_number discrete_number_type, l_has_side_effects || r_has_side_effects
        | _ ->
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_arithmetic_expression variable_infos expr;
                        string_of_parsed_term variable_infos term
                    ]
                    [l_type; r_type]
                    (string_of_parsed_arithmetic_expression variable_infos outer_expr)
            ))
        )

	| Parsed_DAE_term term ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    Typed_term (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt = function
    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)

    | Parsed_product_quotient ((Parsed_DT_factor (Parsed_DF_constant lv) as term), (Parsed_DF_constant rv as factor), Parsed_div) as outer_expr ->

	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->

            (* Doing division *)
            let l_numconst = ParsedValue.to_numconst_value lv in
            let r_numconst = ParsedValue.to_numconst_value rv in
            let numconst_value = NumConst.div l_numconst r_numconst in
            (* Check if result is representable by an int *)
            let can_be_int = NumConst.is_int numconst_value in

            (* If it's representable by an int, it can be a rational or an int *)
            let discrete_number_type =
                if can_be_int then
                    Var_type_discrete_weak_number
                (* If it's not representable by an int, it's a rational *)
                else
                    Var_type_discrete_rat
            in

            Typed_product_quotient (l_typed_expr, r_typed_expr, discrete_number_type, Typed_div), Var_type_discrete_number discrete_number_type, l_has_side_effects || r_has_side_effects

        | _ ->

            raise (TypeError (
                ill_typed_message
                    (string_of_parsed_term variable_infos term)
                    (string_of_parsed_factor variable_infos factor)
                    (string_of_parsed_term variable_infos outer_expr)
                    l_type
                    r_type
            ))
        )

    | Parsed_product_quotient (term, factor, parsed_product_quotient) as outer_expr ->
	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

        let typed_product_quotient =
            match parsed_product_quotient with
            | Parsed_mul -> Typed_mul
            | Parsed_div -> Typed_div
        in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_product_quotient (l_typed_expr, r_typed_expr, discrete_number_type, typed_product_quotient), Var_type_discrete_number discrete_number_type, l_has_side_effects || r_has_side_effects
        | _ -> raise (TypeError (
            ill_typed_message
                (string_of_parsed_term variable_infos term)
                (string_of_parsed_factor variable_infos factor)
                (string_of_parsed_term variable_infos outer_expr)
                l_type
                r_type
        ))
        )

	| Parsed_DT_factor factor ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in
	    Typed_factor (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt = function
	| Parsed_DF_variable variable_name ->

        (* If it's local variable, take it's type *)
        (* local variables are more priority and shadow global variables  *)
	    let discrete_type, scope =
            match local_variables_opt with
            | Some local_variables when Hashtbl.mem local_variables variable_name  ->
                let discrete_type = Hashtbl.find local_variables variable_name in
                discrete_type, Local

            | _ ->
                VariableInfo.discrete_type_of_variable_or_constant variable_infos variable_name, Global
        in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some ((Var_type_discrete_number _) as infer_type), Var_type_discrete_number Var_type_discrete_weak_number -> infer_type
            | _ -> discrete_type
        in

        Typed_variable (variable_name, infer_discrete_type, scope), infer_discrete_type, false

	| Parsed_DF_constant value ->
        let discrete_type = ParsedValue.discrete_type_of_value value in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some ((Var_type_discrete_number _) as infer_type), Var_type_discrete_number Var_type_discrete_weak_number -> infer_type
            | _ -> discrete_type
        in

        Typed_constant (value, infer_discrete_type), infer_discrete_type, false

	| Parsed_sequence (list_expr, seq_type) as outer_expr ->
        let type_checks = List.map (type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type, _) -> discrete_type) type_checks in
        let has_side_effects = List.exists (fun (_, _, side_effects) -> side_effects) type_checks in

        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        (* Check that all elements types are compatible *)
        if all_compatibles then (
            (* Get inner type of a collection analysing types of it's elements *)
            (* If the collection is empty, it's type will be inferred by the context *)
            let inner_type = type_check_collection discrete_types infer_type_opt in
            (* Determine discrete type and typed sequence type *)
            let discrete_type, seq_type =
                match seq_type with
                    | Parsed_array ->
                        Var_type_discrete_array (inner_type, List.length list_expr), Typed_array
                    | Parsed_list ->
                        Var_type_discrete_list inner_type, Typed_list
                    | Parsed_stack ->
                        Var_type_discrete_stack inner_type, Typed_stack
                    | Parsed_queue ->
                        Var_type_discrete_queue inner_type, Typed_queue
            in
            Typed_sequence (typed_expressions, inner_type, seq_type), discrete_type, has_side_effects
        )
        else (
            let str_expressions = List.map (string_of_parsed_boolean_expression variable_infos) list_expr in

            raise (TypeError (
                ill_typed_message_of_expressions
                    str_expressions
                    discrete_types
                    (string_of_parsed_factor variable_infos outer_expr)
            ))
        )

    | Parsed_DF_access (factor, index_expr) ->
        let typed_factor, factor_type, is_factor_has_side_effects = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in
        let typed_index, index_type, is_index_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos None (* None: mean no inference for index *) index_expr in

        if not (is_discrete_type_number_type index_type) || is_discrete_type_rational_type index_type then
            raise (TypeError "Index cannot be another type than int.");

        (* Check that accessed element is a collection *)
        (match factor_type with
        | Var_type_discrete_array (inner_type, _)
        | Var_type_discrete_list inner_type -> Typed_access (typed_factor, typed_index, factor_type, inner_type), inner_type, is_factor_has_side_effects || is_index_has_side_effects
        | _ -> raise (TypeError "Cannot make an access to another type than array or list.")
        )

	| Parsed_DF_expression expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_expr (typed_expr, discrete_type), discrete_type, has_side_effects

	| Parsed_DF_unary_min factor as outer_expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

        (* Check that expression is a number *)
        (match discrete_type with
        | Var_type_discrete_number discrete_number_type -> Typed_unary_min (typed_expr, discrete_number_type), discrete_type, has_side_effects
        | _ ->
            raise (TypeError (
                ill_typed_message_of_expressions
                    [string_of_parsed_factor variable_infos factor]
                    [discrete_type]
                    (string_of_parsed_factor variable_infos outer_expr)
            ))
        )

	| Parsed_function_call (name_factor, argument_expressions) as func ->
        (* Get function name *)
        let function_name = ParsingStructureUtilities.function_name_of_parsed_factor name_factor in
        (* Get function metadata *)
        let function_metadata = Functions.function_metadata_by_name variable_infos function_name in
        (* Get function arity *)
        let arity = Functions.arity_of_function variable_infos function_name in
        let arguments_number = List.length argument_expressions in

        (* Check call number of arguments is consistent with function arity *)
        if arguments_number <> arity then
            raise (TypeError (
                "Arguments number of `"
                ^ string_of_parsed_factor variable_infos func
                ^ "`/"
                ^ string_of_int (arguments_number)
                ^ " call does not match with arity of the function: `"
                ^ function_name
                ^ "`/"
                ^ string_of_int arity
                ^ "."
            ));

        (* Type check arguments *)
        (* We doesn't infer arguments types because arguments types are not dependent of the context *)
        let type_checks = List.map (type_check_parsed_boolean_expression local_variables_opt variable_infos None (* None: mean no inference for arguments *)) argument_expressions in
        (* Get arguments discrete types  *)
        let call_signature = List.map (fun (_, discrete_type, _) -> discrete_type) type_checks in
        (* Convert to typed arguments expressions *)
        let typed_expressions = List.map (fun (typed_expr, _, _) -> typed_expr) type_checks in

        (* Check possible side-effects in parameters *)
        let has_side_effects = List.exists (fun (_, _, has_side_effects) -> has_side_effects) type_checks in

        (* Get function signature *)
        let function_signature_constraint = function_metadata.signature_constraint in
        (* Get parameters signature *)
        let function_parameter_signature_constraint, _ = FunctionSig.split_signature function_signature_constraint in

        (* Check signature and signature constraint compatibility *)
        let is_compatibles = FunctionSig.is_signature_compatible_with_signature_constraint call_signature function_parameter_signature_constraint in

        (* If signature and signature constraint are not compatibles raise a type error *)
        if not is_compatibles then
        (
            (* Call signature with return type *)
            let complete_call_signature = call_signature @ [Var_type_weak] in

            raise (TypeError (
                "Function call `"
                ^ string_of_parsed_factor variable_infos func
                ^ " has signature `"
                ^ function_name
                ^ FunctionSig.string_of_signature complete_call_signature
                ^ "` which isn't compatible with definition `"
                ^ label_of_parsed_factor_constructor func
                ^ FunctionSig.string_of_signature_constraint function_signature_constraint
                ^ "`."
            ))
        );

        (* Combine each typed expression argument with their parameter constraint *)
        let signature_constraint_with_expressions = List.combine function_parameter_signature_constraint typed_expressions in

        (* Try to resolve dependent types (a dependent type is a type that depend on a value, see wikipedia) *)
        (* For example fill_left(b(l), i) for a binary word of length l, return a binary word of length b(l + i) *)
        (* You can see that the return type depend on the value i *)
        (* As typing system is static, only constant expressions can be considered for a value that produce in dependent type *)
        let dependent_type_constraints = OCamlUtilities.rev_filter_map (fun (type_constraint, expr) ->
            match type_constraint with
            | Defined_type_constraint (Number_constraint (Defined_type_number_constraint Int_constraint (Int_name_constraint constraint_name))) ->

                let converted_expr = Convert.global_expression_of_typed_boolean_expression variable_infos expr (Var_type_discrete_number Var_type_discrete_int) in

                if not (DiscreteExpressionEvaluator.is_global_expression_constant None converted_expr) then (
                    raise (TypeError (
                        "Function `"
                        ^ string_of_parsed_factor variable_infos func
                        ^ " : "
                        ^ FunctionSig.string_of_signature_constraint function_signature_constraint
                        ^ "` return type is a dependent type that depend of the value of the constraint `"
                        ^ constraint_name
                        ^ "`."
                        ^ " This value must be a constant expression."
                    ));
                )
                else (
                    let value = DiscreteExpressionEvaluator.try_eval_constant_global_expression None (* function table *) converted_expr in
                    Some (constraint_name, Resolved_length_constraint (Int32.to_int (AbstractValue.to_int_value value)))
                )
            | _ -> None

        ) signature_constraint_with_expressions
        in

        (* Resolve constraint according to arguments types *)
        let resolved_constraints, malformed_constraints = TypeConstraintResolver.resolve_constraints variable_infos function_parameter_signature_constraint call_signature in

        let resolved_constraints = resolved_constraints @ dependent_type_constraints in

        (* Eventually display the list of malformed constraints, if any, and raise an exception *)
        (* Malformed constraint can be a conflict *)
        if List.length malformed_constraints > 0 then (
            raise (TypeError (
                "Constraints consistency fail, "
                ^ "call of `"
                ^ string_of_parsed_factor variable_infos func
                ^ "` resolve constraints as {"
                ^ TypeConstraintResolver.string_of_resolved_constraints malformed_constraints
                ^ "} which is not compatible with function parameters signature of `"
                ^ function_name
                ^ "` "
                ^ FunctionSig.string_of_signature_constraint function_parameter_signature_constraint
            ))
        );

        (* If an infer type is given, infer `number` constraints to infer type *)
        let infer_resolved_constraints =
            List.map (fun (constraint_name, resolved_constraint) ->

                let infer_resolved_constraint =
                    match infer_type_opt, resolved_constraint with
                    | Some ((Var_type_discrete_number _) as infer_type), Resolved_type_constraint (Var_type_discrete_number Var_type_discrete_weak_number) -> Resolved_type_constraint infer_type
                    | _ -> resolved_constraint
                in
                constraint_name, infer_resolved_constraint

            ) resolved_constraints
        in

        (* Resolve signature from resolved constraints *)
        let resolved_constraints_table = OCamlUtilities.hashtbl_of_tuples infer_resolved_constraints in

        let resolved_signature = TypeConstraintResolver.signature_of_signature_constraint resolved_constraints_table function_signature_constraint in

        (* Print messages *)
        print_message Verbose_high ("\tInfer signature constraint of `" ^ function_name ^ "`: " ^ FunctionSig.string_of_signature_constraint function_signature_constraint);

        (* Print resolved constraints *)
        let str_resolved_constraints = TypeConstraintResolver.string_of_resolved_constraints infer_resolved_constraints in
        if str_resolved_constraints <> "" then print_message Verbose_high ("\tInfer resolved constraints: {" ^ str_resolved_constraints ^ "} for call `" ^ string_of_parsed_factor variable_infos func ^ "`.");

        print_message Verbose_high ("\tInfer signature of `" ^ string_of_parsed_factor variable_infos func ^ "` resolved as: " ^ FunctionSig.string_of_signature resolved_signature);

        let resolved_signature_without_return_type, return_type = FunctionSig.split_signature resolved_signature in

        (* Now we had resolved discrete type of arguments, we can infer arguments to these types *)
        let combine = List.combine argument_expressions resolved_signature_without_return_type in

        let type_checks =
            List.map (fun (argument_exp, discrete_type) ->
                let variable_number_type_opt = Some (DiscreteType.extract_inner_type discrete_type) in
                type_check_parsed_boolean_expression local_variables_opt variable_infos variable_number_type_opt (* inference to type deduced from signature *) argument_exp
            ) combine
        in

        (* Get typed arguments expressions *)
        let typed_expressions = List.map (fun (typed_expr, _, _) -> typed_expr) type_checks in

        let is_subject_to_side_effect = function_metadata.side_effect in
        Typed_function_call (function_name, typed_expressions, return_type), return_type, is_subject_to_side_effect || has_side_effects



let rec type_check_parsed_scalar_or_index_update_type local_variables_opt variable_infos = function
    | Parsed_scalar_update variable_name ->
        (* Get assigned variable type *)
        let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos variable_name in
        let discrete_type = discrete_type_of_var_type var_type in
        Typed_scalar_update variable_name, discrete_type, false (* no side effect *)

    | Parsed_indexed_update (parsed_scalar_or_index_update_type, index_expr) as indexed_update ->

        let typed_update_type, discrete_type, is_parsed_scalar_or_index_update_type_has_side_effects = type_check_parsed_scalar_or_index_update_type local_variables_opt variable_infos parsed_scalar_or_index_update_type in
        let typed_index_expr_type, index_discrete_type, is_index_expr_has_side_effects = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos (Some (Var_type_discrete_number Var_type_discrete_int)) index_expr in

        (* Check that index expression is an int expression *)
        if index_discrete_type <> Var_type_discrete_number Var_type_discrete_int then
            raise (TypeError ("Index of expression `" ^ ParsingStructureUtilities.string_of_parsed_scalar_or_index_update_type variable_infos indexed_update ^ "` is not an integer."));

        (* Check is an array *)
        let discrete_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (TypeError "Trying to make a write access to a non-array variable.")
        in
        Typed_indexed_update (typed_update_type, typed_index_expr_type, discrete_type), discrete_type, is_parsed_scalar_or_index_update_type_has_side_effects || is_index_expr_has_side_effects

let type_check_parsed_update_type local_variables_opt variable_infos = function
    | Parsed_variable_update parsed_scalar_or_index_update_type ->
        let typed_parsed_scalar_or_index_update_type, discrete_type, has_side_effects = type_check_parsed_scalar_or_index_update_type local_variables_opt variable_infos parsed_scalar_or_index_update_type in
        Typed_variable_update typed_parsed_scalar_or_index_update_type, discrete_type, has_side_effects

    | Parsed_void_update -> Typed_void_update, Var_type_weak, false


let rec type_check_fun_body local_variables variable_infos infer_type_opt = function
    | Parsed_fun_local_decl (variable_name, discrete_type, expr, next_expr, _) ->
        (* Add local variable to hashtable *)
        Hashtbl.add local_variables variable_name discrete_type;

        (* Get inner type of discrete type to infer *)
        (* For example : `let s : int stack = stack()` extract `int` type of `int stack` declaration *)
        (* And then, with the type `int` we can infer the right expression having the type `weak stack` to a `int stack` *)
        (* Same examples with queue, array, and any composed type... *)
        let variable_number_type_opt = Some (DiscreteType.extract_inner_type discrete_type) in

        (* Type check and infer init expression of the local variable declaration *)
        let typed_init_expr, init_discrete_type, is_init_expr_has_side_effects = type_check_global_expression (Some local_variables) variable_infos variable_number_type_opt expr in
        (* Type check and infer the next expression of the function body *)
        let typed_next_expr, next_expr_discrete_type, is_next_expr_has_side_effects = type_check_fun_body local_variables variable_infos infer_type_opt next_expr in

        (* Check compatibility between local variable declared type and it's init expression *)
        if not (is_discrete_type_compatibles discrete_type init_discrete_type) then
            raise (TypeError (
                ill_typed_variable_message variable_name (DiscreteType.string_of_var_type_discrete discrete_type) (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr) init_discrete_type
            ));

        (* All is ok, convert to a typed function local declaration *)
        Typed_fun_local_decl (
            variable_name,
            discrete_type,
            typed_init_expr,
            typed_next_expr
        ), next_expr_discrete_type, is_init_expr_has_side_effects || is_next_expr_has_side_effects

    | Parsed_fun_instruction ((parsed_update_type, expr), next_expr) ->
        (* Resolve typed expression *)
        let typed_expr, expr_type, has_side_effects (* side effects *) = type_check_global_expression (Some local_variables) variable_infos infer_type_opt expr in
        (* Resolve typed update type *)
        let typed_update_type, l_value_type, is_parsed_update_type_has_side_effects (* side effects *) = type_check_parsed_update_type (Some local_variables) variable_infos parsed_update_type in
        (* Resolve typed next expr *)
        let typed_next_expr, next_expr_discrete_type, next_expr_has_side_effects (* side effects *) = type_check_fun_body local_variables variable_infos infer_type_opt next_expr in

        Typed_fun_instruction ((typed_update_type, typed_expr), typed_next_expr), next_expr_discrete_type, true

    | Parsed_fun_expr expr ->
        let typed_expr, discrete_type, has_side_effects = type_check_global_expression (Some local_variables) variable_infos infer_type_opt expr in
        Typed_fun_expr typed_expr, discrete_type, has_side_effects

let type_check_parsed_fun_definition variable_infos (fun_definition : ParsingStructure.parsed_fun_definition) =
    (* Get parameter types and return type of the function *)
    let parameter_names, parameter_discrete_types = List.split fun_definition.parameters in
    let return_type = fun_definition.return_type in
    (* Construct signature *)
    let signature = parameter_discrete_types @ [return_type] in

    (* Add parameters as local variables of the function *)
    let nb_parameter = List.length fun_definition.parameters in
    let local_variables = Hashtbl.create nb_parameter in

    List.iter (fun (parameter_name, parameter_type) ->
        Hashtbl.add local_variables parameter_name parameter_type
    ) fun_definition.parameters;

    (* Eventually infer the body expression type of function to the return type underlying type of the function *)
    let infer_type_opt = Some (DiscreteType.extract_inner_type return_type) in
    let typed_body, body_discrete_type, is_body_has_side_effects = type_check_fun_body local_variables variable_infos infer_type_opt fun_definition.body in
    (* Check type compatibility between function body and return type *)
    let is_body_type_compatible = is_discrete_type_compatibles body_discrete_type return_type in

    if not is_body_type_compatible then
        raise (TypeError (
            "Function signature `"
            ^ fun_definition.name
            ^ FunctionSig.string_of_signature signature
            ^ "` does not match with implementation `"
            ^ string_of_fun_body variable_infos typed_body
            ^ "` of type "
            ^ DiscreteType.string_of_var_type_discrete body_discrete_type
            ^ "."
        ));

    let typed_fun_definition = {
        name = fun_definition.name;
        parameters = parameter_names;
        signature = signature;
        body = typed_body;
        side_effect = is_body_has_side_effects;
    }
    in

    typed_fun_definition, return_type, is_body_has_side_effects


let type_check_parsed_loc_predicate variable_infos infer_type_opt = function
	| Parsed_loc_predicate_EQ (automaton_name, loc_name) -> Typed_loc_predicate_EQ (automaton_name, loc_name), Var_type_discrete_bool, false
	| Parsed_loc_predicate_NEQ (automaton_name, loc_name) -> Typed_loc_predicate_NEQ (automaton_name, loc_name), Var_type_discrete_bool, false

let rec type_check_parsed_simple_predicate variable_infos infer_type_opt = function
	| Parsed_discrete_boolean_expression expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_discrete_boolean_expression None variable_infos infer_type_opt expr in

        if not (DiscreteType.is_discrete_type_bool_type discrete_type) then (
            raise (TypeError (
                "Expression `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos expr
                ^ "` in property, is not a Boolean expression: "
                ^ DiscreteType.string_of_var_type_discrete discrete_type
                ^ "."
            ))
        )
        else
	        Typed_discrete_boolean_expression (typed_expr, discrete_type), discrete_type, has_side_effects

	| Parsed_loc_predicate predicate ->
	    let typed_predicate, discrete_type, has_side_effects = type_check_parsed_loc_predicate variable_infos infer_type_opt predicate in
	    Typed_loc_predicate typed_predicate, discrete_type, has_side_effects

	| Parsed_state_predicate_true -> Typed_state_predicate_true, Var_type_discrete_bool, false
	| Parsed_state_predicate_false -> Typed_state_predicate_false, Var_type_discrete_bool, false
	| Parsed_state_predicate_accepting -> Typed_state_predicate_accepting, Var_type_discrete_bool, false

and type_check_parsed_state_predicate variable_infos infer_type_opt = function
	| Parsed_state_predicate_OR (l_expr, r_expr) as outer_expr ->
	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_state_predicate variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_state_predicate variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_OR (l_typed_expr, r_typed_expr), Var_type_discrete_bool, l_has_side_effects || r_has_side_effects
        | _ ->
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_state_predicate variable_infos l_expr;
                        string_of_parsed_state_predicate variable_infos r_expr
                    ]
                    [l_type; r_type]
                    (string_of_parsed_state_predicate variable_infos outer_expr)
            ))
	    )

	| Parsed_state_predicate_term term ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_state_predicate_term variable_infos infer_type_opt term in
	    Typed_state_predicate_term (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_state_predicate_term variable_infos infer_type_opt = function
	| Parsed_state_predicate_term_AND (l_expr, r_expr) as outer_expr ->
	    let l_typed_expr, l_type, l_has_side_effects = type_check_parsed_state_predicate_term variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type, r_has_side_effects = type_check_parsed_state_predicate_term variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_term_AND (l_typed_expr, r_typed_expr), Var_type_discrete_bool, l_has_side_effects || r_has_side_effects
        | _ ->
            raise (TypeError (
                ill_typed_message_of_expressions
                    [
                        string_of_parsed_state_predicate_term variable_infos l_expr;
                        string_of_parsed_state_predicate_term variable_infos r_expr
                    ]
                    [l_type; r_type]
                    (string_of_parsed_state_predicate_term variable_infos outer_expr)
            ))
	    )

	| Parsed_state_predicate_factor factor ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in
	    Typed_state_predicate_factor (typed_expr, discrete_type), discrete_type, has_side_effects

and type_check_parsed_state_predicate_factor variable_infos infer_type_opt = function
	| Parsed_state_predicate_factor_NOT factor as outer_expr ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in

        (* Check that expression type is boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_state_predicate_factor_NOT typed_expr, discrete_type, has_side_effects
	    | _ ->
	        raise (TypeError (
                ill_typed_message_of_expressions
                    [string_of_parsed_state_predicate_factor variable_infos factor]
                    [discrete_type]
                    (string_of_parsed_state_predicate_factor variable_infos outer_expr)
	        ))
        )

	| Parsed_simple_predicate predicate ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_simple_predicate variable_infos infer_type_opt predicate in
	    Typed_simple_predicate (typed_expr, discrete_type), discrete_type, has_side_effects

	| Parsed_state_predicate predicate ->
	    let typed_expr, discrete_type, has_side_effects = type_check_parsed_state_predicate variable_infos infer_type_opt predicate in
	    Typed_state_predicate (typed_expr, discrete_type), discrete_type, has_side_effects



(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment variable_infos variable_name variable_type expr =

    (* Eventually get a number type to infer *)
(*    let variable_number_type_opt = DiscreteType.extract_number_of_discrete_type variable_type in*)
    let variable_number_type_opt = Some (DiscreteType.extract_inner_type variable_type) in
    (* Resolve typed expression *)
    let typed_expr, expr_var_type_discrete, has_side_effects = type_check_global_expression None variable_infos variable_number_type_opt expr in

    (* Check if initialisation (init / constant) has side effects *)
    if has_side_effects then
        raise (TypeError (
            "Init or constant expression `"
            ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr
            ^ "` has side effects."
        ));

    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteType.is_discrete_type_compatibles variable_type expr_var_type_discrete in

    (* Not consistent ? raise a type error with appropriate message *)
    if not (is_consistent) then (
        raise (TypeError (ill_typed_variable_message variable_name (DiscreteType.string_of_var_type_discrete variable_type) (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr) expr_var_type_discrete))
    )
    else (
        typed_expr
    )

(* Check that a discrete variable initialization is well typed *)
let check_discrete_init variable_infos variable_name expr =

    (* Get the variable index *)
    let discrete_index = index_of_variable_name variable_infos variable_name in
    (* Get variable type *)
    let var_type = VariableInfo.var_type_of_variable_index variable_infos discrete_index in

    (* Check whether variable is clock or parameter *)
    let is_clock_or_parameter = var_type == DiscreteType.Var_type_clock || var_type == DiscreteType.Var_type_parameter in

    (* Check if variable is clock or parameter, it's forbidden to init clock or parameter in discrete section *)
    if (is_clock_or_parameter) then (
        raise (TypeError ("Initialisation of a " ^ (DiscreteType.string_of_var_type var_type) ^ " in discrete init state section is forbidden"))
    );

    (* Get variable type *)
    let variable_type = VariableInfo.discrete_type_of_variable_or_constant variable_infos variable_name in
    (* Check expression / variable type consistency *)
    let typed_expr = check_type_assignment variable_infos variable_name variable_type expr in
    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - inits - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_global_expression variable_infos typed_expr
    );
    (*
    ResultProcessor.add_custom_details (
        "annot - inits - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_global_expression variable_infos typed_expr
    );
    *)

    typed_expr


let check_constant_expression variable_infos (name, expr, var_type) =

    (* Get variable type *)
    let discrete_type = DiscreteType.discrete_type_of_var_type var_type in
    (* Check expression / variable type consistency *)
    let typed_expr = check_type_assignment variable_infos name discrete_type expr in
    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - constants - "
        ^ name
        ^ " := "
        ^ string_of_typed_global_expression variable_infos typed_expr
    );
    typed_expr


(* Type non-linear constraint *)
(* return a tuple containing the non-linear constraint uniformly typed and the resolved type of the expression *)
let check_nonlinear_constraint variable_infos nonlinear_constraint =

    let typed_nonlinear_constraint, discrete_type, has_side_effects = type_check_parsed_discrete_boolean_expression None variable_infos None nonlinear_constraint in

    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - guards - "
        ^ string_of_typed_discrete_boolean_expression variable_infos typed_nonlinear_constraint
    );

    (* Check if guard / invariant has side effects *)
    if has_side_effects then
        raise (TypeError (
            "Guard or invariant expression `"
            ^ ParsingStructureUtilities.string_of_parsed_nonlinear_constraint variable_infos nonlinear_constraint
            ^ "` has side effects."
        ));

    (* Check that non-linear constraint is a Boolean expression *)
    match discrete_type with
    | DiscreteType.Var_type_discrete_bool -> typed_nonlinear_constraint
    | _ ->
        raise (TypeError (
            "Guard or invariant expression `"
            ^ string_of_parsed_nonlinear_constraint variable_infos nonlinear_constraint
            ^ "` is not a Boolean expression"
        ))

(* Type check guard / invariant *)
(* return a tuple containing the expression uniformly typed and the resolved type of the expression *)
let check_guard variable_infos =
    List.map (check_nonlinear_constraint variable_infos)





(* Type check an update *)
let check_update variable_infos update_types parsed_update_type expr =

    (* Get assigned variable name *)
    let variable_name_opt = ParsingStructureUtilities.variable_name_of_parsed_update_type_opt parsed_update_type in

    (* Get assigned variable type *)
    let variable_name, var_type =
        match variable_name_opt with
        | Some variable_name -> variable_name, VariableInfo.var_type_of_variable_or_constant variable_infos variable_name
        | None -> "", Var_type_discrete (Var_type_discrete_number Var_type_discrete_weak_number) (* By default, infer numbers to unknown numbers *)
    in

    (* Eventually get a number type to infer *)
    let variable_number_type_opt =
        match var_type with
        | Var_type_clock
        | Var_type_parameter -> None
        | Var_type_discrete discrete_type -> Some (DiscreteType.extract_inner_type discrete_type)
    in

    (* Resolve typed expression *)
    let typed_expr, expr_type, has_side_effects (* side effects *) = type_check_global_expression None variable_infos variable_number_type_opt expr in

    let typed_update_type, l_value_type, is_parsed_update_type_has_side_effects (* side effects *) = type_check_parsed_update_type None variable_infos parsed_update_type in

    (* Check that continuous / discrete not sequential updates doesn't contain side effects *)
    if update_types = Parsed_std_updates && (has_side_effects || is_parsed_update_type_has_side_effects) then
        raise (TypeError (
            "`then` update bloc contain one or more expression with side effects `"
            ^ ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr
            ^ "`. Expression with side effects are only allowed in `seq` bloc."
        ));

    (* Check var_type_discrete is compatible with expression type, if yes, convert expression *)
     if not (DiscreteType.is_discrete_type_compatibles l_value_type expr_type) then (
        raise (TypeError (
            ill_typed_variable_message variable_name (DiscreteType.string_of_var_type var_type) (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr) expr_type
            )
        )
    );

    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - updates - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_global_expression variable_infos typed_expr
    );

    typed_update_type,
    typed_expr

(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
let check_conditional variable_infos expr =

    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer conditional expression: " ^ string_of_parsed_boolean_expression variable_infos expr);

    let typed_expr, expr_type, _ (* side effects *) = type_check_parsed_boolean_expression None variable_infos None expr in

    (* Check that non-linear constraint is a Boolean expression *)
    match expr_type with
    | Var_type_discrete_bool ->
        typed_expr
    | _ ->
        raise (TypeError (
            "Expression `"
            ^ (string_of_parsed_boolean_expression variable_infos expr)
            ^ "` in conditional statement, is not a Boolean expression"
            )
        )

(* Check that a predicate is well typed *)
let check_state_predicate variable_infos predicate =
    (* Type check *)
    let typed_predicate, discrete_type, has_side_effects = type_check_parsed_state_predicate variable_infos None predicate in

    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - property - "
        ^ string_of_typed_state_predicate variable_infos typed_predicate
    );

    (* Check if property has side effects *)
    if has_side_effects then
        raise (TypeError (
            "Property expression `"
            ^ ParsingStructureUtilities.string_of_parsed_state_predicate variable_infos predicate
            ^ "` has side effects."
        ));

    match discrete_type with
    | Var_type_discrete_bool -> typed_predicate
    | _ -> raise (TypeError (
            "Property `"
            ^ string_of_typed_state_predicate variable_infos typed_predicate
            ^ "` is not a Boolean expression."
        ))

let check_fun_definition variable_infos (parsed_fun_definition : parsed_fun_definition) =
    let typed_fun_definition, _, _ = type_check_parsed_fun_definition variable_infos parsed_fun_definition in
    typed_fun_definition

end

(* ----------------------------------------------------------- *)
(* ----------------------------------------------------------- *)
(* ----------------------------------------------------------- *)
(* ----------------------------------------------------------- *)

(* Convert module *)
(* This module aim to create an abstract tree from a typed tree *)
(* It use TypeChecker module *)
and Convert : sig


(** Linear part **)

val linear_term_of_linear_expression : variable_infos -> ParsingStructure.linear_expression -> LinearConstraint.pxd_linear_term
val linear_constraint_of_convex_predicate : variable_infos -> ParsingStructure.linear_constraint list -> LinearConstraint.pxd_linear_constraint

val linear_term_of_typed_global_expression : variable_infos -> TypeChecker.typed_global_expression -> LinearConstraint.pxd_linear_term
val global_expression_of_typed_global_expression : variable_infos -> TypeChecker.typed_global_expression -> DiscreteExpressions.global_expression
val global_expression_of_typed_boolean_expression : variable_infos -> TypeChecker.typed_boolean_expression -> var_type_discrete -> DiscreteExpressions.global_expression
val bool_expression_of_typed_boolean_expression : variable_infos -> TypeChecker.typed_boolean_expression -> DiscreteExpressions.boolean_expression
val bool_expression_of_typed_discrete_boolean_expression : variable_infos -> TypeChecker.typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val nonlinear_constraint_of_typed_nonlinear_constraint : variable_infos -> TypeChecker.typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val update_type_of_typed_update_type : variable_infos -> TypeChecker.typed_update_type -> DiscreteExpressions.update_type

val fun_definition_of_typed_fun_definition : variable_infos -> TypeChecker.typed_fun_definition -> AbstractModel.fun_definition

end = struct

open Constants
open Exceptions
open ParsingStructure
open ParsingStructureUtilities
open AbstractModel
open DiscreteExpressions
open DiscreteType
open TypeChecker


type discrete_index = int

let label_of_typed_sequence_type = function
	| Typed_array -> "array"
	| Typed_list -> "list"
	| Typed_stack -> Constants.stack_string
	| Typed_queue -> "queue"

let label_of_typed_factor_constructor = function
	| Typed_variable _ -> "variable"
	| Typed_constant _ -> "constant"
	| Typed_sequence (_, _, seq_type) -> label_of_typed_sequence_type seq_type
	| Typed_access _ -> "access"
	| Typed_expr _ -> "expression"
	| Typed_unary_min _ -> "minus"
    | Typed_function_call (function_name, _, _) -> function_name

let user_function_meta variable_infos function_name =
    let fun_meta_opt = Hashtbl.find_opt variable_infos.functions function_name in
    match fun_meta_opt with
    | Some fun_meta -> fun_meta
    | None -> raise (UndefinedFunction function_name)

(* Messages *)
let expression_must_have_type_message = "An expression should have a determined type. Maybe something has failed before."
let expr_type_doesnt_match_to_structure_message str_expr_type str_expr = "The deduced expression type indicate that it should be converted to a " ^ str_expr_type ^ " expression, but it's incompatible with this expression structure: `" ^ str_expr ^ "`. Maybe something failed in type checking or conversion."

(** Convert a Boolean operator to its abstract model *)
let convert_parsed_relop = function
	| PARSED_OP_L -> OP_L
	| PARSED_OP_LEQ	-> OP_LEQ
	| PARSED_OP_EQ	-> OP_EQ
	| PARSED_OP_NEQ	-> OP_NEQ
	| PARSED_OP_GEQ	-> OP_GEQ
	| PARSED_OP_G -> OP_G




(* Convert discrete expressions *)

let sum_diff_of_typed_sum_diff = function
    | Typed_plus -> Plus
    | Typed_minus -> Minus

let product_quotient_of_typed_product_quotient = function
    | Typed_mul -> Mul
    | Typed_div -> Div

let conj_dis_of_typed_conj_dis = function
    | Typed_and -> And
    | Typed_or -> Or


let rec global_expression_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, discrete_type) ->
        global_expression_of_typed_boolean_expression variable_infos expr discrete_type

(* TODO benjamin shouldn't have this function, it's because of global expression that isn't useful *)
(* I should remove global expression in parsing structure for avoid it *)
and global_expression_of_typed_boolean_expression_without_type variable_infos = function
	| Typed_conj_dis _ as expr ->
	    global_expression_of_typed_boolean_expression variable_infos expr Var_type_discrete_bool
	| Typed_discrete_bool_expr (_, discrete_type) as expr ->
	    global_expression_of_typed_boolean_expression variable_infos expr discrete_type

and global_expression_of_typed_boolean_expression variable_infos expr discrete_type =
    match discrete_type with
    | Var_type_discrete_number discrete_number_type ->
        Arithmetic_expression (
            discrete_arithmetic_expression_of_typed_boolean_expression variable_infos discrete_number_type expr
        )
    | Var_type_discrete_bool ->
        Bool_expression (
            bool_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_binary_word length ->
        Binary_word_expression (
            binary_expression_of_typed_boolean_expression variable_infos length expr
        )
    | Var_type_discrete_array (inner_type, _) ->
        Array_expression (
            array_expression_of_typed_boolean_expression variable_infos inner_type expr
        )
    | Var_type_discrete_list inner_type ->
        List_expression (
            list_expression_of_typed_boolean_expression variable_infos inner_type expr
        )
    | Var_type_discrete_stack inner_type ->
        Stack_expression (
            stack_expression_of_typed_boolean_expression variable_infos inner_type expr
        )
    | Var_type_discrete_queue inner_type ->
        Queue_expression (
            queue_expression_of_typed_boolean_expression variable_infos inner_type expr
        )
    | Var_type_weak ->
        raise (InternalError expression_must_have_type_message)


and discrete_arithmetic_expression_of_typed_boolean_expression variable_infos discrete_number_type = function
	| Typed_discrete_bool_expr (expr, _) ->
	    (match discrete_number_type with
	    | Var_type_discrete_weak_number
	    | Var_type_discrete_rat ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    )
	| _ -> raise (InternalError "Trying to convert Boolean expression to arithmetic one. Maybe something failed in type checking or conversion.")

and discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type = function
	| Typed_arithmetic_expr (expr, discrete_type) ->
	    (match discrete_number_type with
	    | Var_type_discrete_weak_number
	    | Var_type_discrete_rat ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    )

	| _ -> raise (InternalError "Trying to convert Boolean expression to arithmetic one. Maybe something failed in type checking or conversion.")

and discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos discrete_number_type = function
	| Typed_sum_diff (expr, term, _, typed_sum_diff) ->

	    let sum_diff = sum_diff_of_typed_sum_diff typed_sum_diff in

        (match discrete_number_type with
        | Var_type_discrete_weak_number
        | Var_type_discrete_rat ->
            Rational_arithmetic_expression (
                Rational_sum_diff (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term,
                    sum_diff
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_sum_diff (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term,
                    sum_diff
                )
            )
        )

	| Typed_term (term, _) ->
        (match discrete_number_type with
        | Var_type_discrete_weak_number
        | Var_type_discrete_rat ->
            Rational_arithmetic_expression (Rational_term (rational_arithmetic_expression_of_typed_term variable_infos term))
        | Var_type_discrete_int ->
            Int_arithmetic_expression (Int_term (int_arithmetic_expression_of_typed_term variable_infos term))
        )

(* --------------------*)
(* Bool conversion *)
(* --------------------*)

and bool_expression_of_typed_boolean_expression variable_infos = function
    | Typed_conj_dis (l_expr, r_expr, typed_conj_dis) ->
        Conj_dis (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr,
            conj_dis_of_typed_conj_dis typed_conj_dis
        )

    | Typed_discrete_bool_expr (expr, _) ->
        Discrete_boolean_expression (
            bool_expression_of_typed_discrete_boolean_expression variable_infos expr
        )

and bool_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, _) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

	| Typed_comparison (l_expr, relop, r_expr, discrete_type) ->
	    bool_expression_of_typed_comparison variable_infos l_expr relop r_expr discrete_type

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_number_type) ->
	    Expression_in (
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos discrete_number_type in_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos discrete_number_type lw_expr,
	        discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos discrete_number_type up_expr
	    )

	| Typed_bool_expr expr ->
	    Boolean_expression (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

	| Typed_not_expr expr ->
	    Not_bool (
	        bool_expression_of_typed_boolean_expression variable_infos expr
	    )

and bool_expression_of_typed_comparison variable_infos l_expr relop r_expr = function
    | Var_type_discrete_number discrete_number_type ->
        Arithmetic_comparison (
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type l_expr,
            convert_parsed_relop relop,
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type r_expr
        )
    | Var_type_discrete_bool ->
        Boolean_comparison (
            bool_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            convert_parsed_relop relop,
            bool_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_binary_word length ->
        Binary_comparison (
            binary_expression_of_typed_discrete_boolean_expression variable_infos length l_expr,
            convert_parsed_relop relop,
            binary_expression_of_typed_discrete_boolean_expression variable_infos length r_expr
        )
    | Var_type_discrete_array (inner_type, _) ->
        Array_comparison (
            array_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            array_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_list inner_type ->
        List_comparison (
            list_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            list_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_stack inner_type ->
        Stack_comparison (
            stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_queue inner_type ->
        Queue_comparison (
            queue_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            queue_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_weak ->
        raise (InternalError expression_must_have_type_message)


and bool_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	    bool_expression_of_typed_term variable_infos term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "Boolean" str_expr in
	    raise (InternalError fail_message)


and bool_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	    bool_expression_of_typed_factor variable_infos factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "Boolean" str_expr in
        raise (InternalError fail_message)

and bool_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Bool_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Bool_constant (AbstractValue.bool_value value)
            | Variable_kind discrete_index -> Bool_variable discrete_index
            )
        )
	| Typed_constant (value, _) ->
	    Bool_constant (ParsedValue.bool_value value)

    | Typed_expr (expr, _) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Bool_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    bool_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "Boolean" str_expr in
        raise (InternalError fail_message)

and bool_expression_of_typed_function_call variable_infos argument_expressions function_name =
    let fun_meta = user_function_meta variable_infos function_name in

    Bool_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

(* --------------------*)
(* Rational conversion *)
(* --------------------*)

and rational_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "rational" str_expr in
	    raise (InternalError fail_message)


and rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "rational" str_expr in
	    raise (InternalError fail_message)

and rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_sum_diff (expr, term, _, typed_sum_diff) ->
	    let sum_diff = sum_diff_of_typed_sum_diff typed_sum_diff in

	    Rational_sum_diff (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        sum_diff
	    )

	| Typed_term (term, _) ->
	    Rational_term (
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

and rational_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_product_quotient (term, factor, _, typed_product_quotient) ->

	    let product_quotient = product_quotient_of_typed_product_quotient typed_product_quotient in

	    Rational_product_quotient (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor,
	        product_quotient
	    )

	| Typed_factor (factor, _) ->
	    Rational_factor (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and rational_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min (factor, _) ->
	    Rational_unary_min (
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Rational_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Rational_constant (AbstractValue.numconst_value value)
            | Variable_kind discrete_index -> Rational_variable discrete_index
            )
        )

	| Typed_constant (value, _) ->
	    Rational_constant (ParsedValue.to_numconst_value value)

	| Typed_expr (expr, _) ->
	    Rational_nested_expression (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Rational_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    rational_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "rational" str_expr in
	    raise (InternalError fail_message)

and rational_expression_of_typed_function_call variable_infos argument_expressions function_name =
    let fun_meta = user_function_meta variable_infos function_name in

    Rational_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

(* --------------------*)
(* Int conversion *)
(* --------------------*)

and int_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "int" str_expr in
	    raise (InternalError fail_message)

and int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "int" str_expr in
	    raise (InternalError fail_message)

and int_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_sum_diff (expr, term, _, typed_sum_diff) ->
	    let sum_diff = sum_diff_of_typed_sum_diff typed_sum_diff in

	    Int_sum_diff (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        sum_diff
	    )

	| Typed_term (term, _) ->
	    Int_term (
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

and int_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_product_quotient (term, factor, _, typed_product_quotient) ->

	    let product_quotient = product_quotient_of_typed_product_quotient typed_product_quotient in

	    Int_product_quotient (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor,
	        product_quotient
	    )

	| Typed_factor (factor, _) ->
	    Int_factor (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

and int_arithmetic_expression_of_typed_factor variable_infos = function
	| Typed_unary_min (factor, _) ->
	    Int_unary_min (
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Int_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Int_constant (AbstractValue.to_int_value value)
            | Variable_kind discrete_index -> Int_variable discrete_index
            )
        )

	| Typed_constant (value, _) ->
	    Int_constant (ParsedValue.to_int_value value)

	| Typed_expr (expr, _) ->
	    Int_nested_expression (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Int_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    int_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "int" str_expr in
	    raise (InternalError fail_message)

and int_expression_of_typed_function_call variable_infos argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    Int_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )


(* --------------------*)
(* Binary word conversion *)
(* --------------------*)

and binary_expression_of_typed_boolean_expression variable_infos length = function
    | Typed_discrete_bool_expr (expr, _) ->
        binary_expression_of_typed_discrete_boolean_expression variable_infos length expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "binary word" str_expr in
	    raise (InternalError fail_message)

and binary_expression_of_typed_discrete_boolean_expression variable_infos length = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        binary_expression_of_typed_arithmetic_expression variable_infos length expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "binary word" str_expr in
	    raise (InternalError fail_message)

and binary_expression_of_typed_arithmetic_expression variable_infos length = function
	| Typed_term (term, _) ->
	        binary_expression_of_typed_term variable_infos length term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "binary word" str_expr in
	    raise (InternalError fail_message)

and binary_expression_of_typed_term variable_infos length = function
	| Typed_factor (factor, _) ->
	        binary_expression_of_typed_factor variable_infos length factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "binary word" str_expr in
        raise (InternalError fail_message)

and binary_expression_of_typed_factor variable_infos length = function
	| Typed_variable (variable_name, _, scope) ->
        (match scope with
        | Local ->
            Binary_word_local_variable variable_name
        | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Binary_word_constant (AbstractValue.binary_word_value value)
            | Variable_kind discrete_index -> Binary_word_variable (discrete_index, length)
            )
        )

	| Typed_constant (value, _) ->
	    Binary_word_constant (ParsedValue.binary_word_value value)

	| Typed_expr (expr, _) ->
        binary_expression_of_typed_arithmetic_expression variable_infos length expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Binary_word_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    binary_expression_of_typed_function_call variable_infos length argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "binary word" str_expr in
	    raise (InternalError fail_message)

and binary_expression_of_typed_function_call variable_infos length argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    Binary_word_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

(* --------------------*)
(* Array conversion *)
(* --------------------*)

and array_expression_of_typed_boolean_expression_with_type variable_infos = function
    | Typed_discrete_bool_expr (expr, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | inner_type -> raise (InternalError ("The expression type indicate that it should be converted to an array expression, but a " ^ (DiscreteType.string_of_var_type_discrete inner_type) ^ " expression is found. Maybe something failed in type checking or conversion."))
        in
        array_expression_of_typed_discrete_boolean_expression variable_infos inner_type expr

    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
	    raise (InternalError fail_message)


and array_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
	    raise (InternalError fail_message)

and array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
	    raise (InternalError fail_message)

and array_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        array_expression_of_typed_term variable_infos discrete_type term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
	    raise (InternalError fail_message)

and array_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        array_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
        raise (InternalError fail_message)

and array_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Array_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Array_constant (AbstractValue.array_value value)
            | Variable_kind discrete_index -> Array_variable discrete_index
            )
        )

	| Typed_constant (value, _) ->
	    Array_constant (Array.map AbstractValue.of_parsed_value (ParsedValue.array_value value))

    | Typed_sequence (expr_list, _, Typed_array) ->
        (* Should take inner_type unbox type *)
        let expressions = List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr discrete_type) expr_list in
        Literal_array (Array.of_list expressions)

	| Typed_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Array_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    array_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "array" str_expr in
	    raise (InternalError fail_message)

and array_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    Array_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

(* --------------------*)
(* List conversion *)
(* --------------------*)

and list_expression_of_typed_boolean_expression_with_type variable_infos = function
    | Typed_discrete_bool_expr (expr, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_list inner_type -> inner_type
            | inner_type -> raise (InternalError ("The expression type indicate that it should be converted to a list expression, but a " ^ (DiscreteType.string_of_var_type_discrete inner_type) ^ " expression is found. Maybe something failed in type checking or conversion."))
        in

        list_expression_of_typed_discrete_boolean_expression variable_infos inner_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
	    raise (InternalError fail_message)


and list_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
	    raise (InternalError fail_message)

and list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
	    raise (InternalError fail_message)

and list_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        list_expression_of_typed_term variable_infos discrete_type term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
	    raise (InternalError fail_message)

and list_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        list_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
        raise (InternalError fail_message)

and list_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        List_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> List_constant (AbstractValue.list_value value)
            | Variable_kind discrete_index -> List_variable discrete_index
            )
        )

	| Typed_constant (value, _) ->
	    List_constant (List.map AbstractValue.of_parsed_value (ParsedValue.list_value value))

    | Typed_sequence (expr_list, _, Typed_list) ->
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr discrete_type) expr_list)

	| Typed_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        List_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    list_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "list" str_expr in
	    raise (InternalError fail_message)

and list_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    List_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

and stack_expression_of_typed_boolean_expression_with_type variable_infos = function
    | Typed_discrete_bool_expr (expr, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_stack inner_type -> inner_type
            | inner_type -> raise (InternalError ("The expression type indicate that it should be converted to a stack expression, but a " ^ (DiscreteType.string_of_var_type_discrete inner_type) ^ " expression is found. Maybe something failed in type checking or conversion."))
        in

        stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
	    raise (InternalError fail_message)


and stack_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        stack_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
	    raise (InternalError fail_message)

and stack_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        stack_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
	    raise (InternalError fail_message)

and stack_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        stack_expression_of_typed_term variable_infos discrete_type term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
	    raise (InternalError fail_message)

and stack_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        stack_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
        raise (InternalError fail_message)

and stack_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Stack_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Literal_stack
            | Variable_kind discrete_index -> Stack_variable discrete_index
            )
        )
	| Typed_expr (expr, _) ->
        stack_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Stack_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

    | Typed_sequence (_, _, Typed_stack) -> Literal_stack

	| Typed_function_call (function_name, argument_expressions, _) ->
	    stack_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message Constants.stack_string str_expr in
	    raise (InternalError fail_message)

and stack_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    Stack_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )


and queue_expression_of_typed_boolean_expression_with_type variable_infos = function
    | Typed_discrete_bool_expr (expr, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_queue inner_type -> inner_type
            | inner_type -> raise (InternalError ("The expression type indicate that it should be converted to a queue expression, but a " ^ (DiscreteType.string_of_var_type_discrete inner_type) ^ " expression is found. Maybe something failed in type checking or conversion."))
        in

        queue_expression_of_typed_discrete_boolean_expression variable_infos inner_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
	    raise (InternalError fail_message)



and queue_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        queue_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
	    raise (InternalError fail_message)

and queue_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        queue_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_boolean_expression variable_infos expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
	    raise (InternalError fail_message)

and queue_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        queue_expression_of_typed_term variable_infos discrete_type term
	| _ as expr ->
	    let str_expr = string_of_typed_discrete_arithmetic_expression variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
	    raise (InternalError fail_message)

and queue_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        queue_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
	    let str_expr = string_of_typed_discrete_term variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
        raise (InternalError fail_message)

and queue_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _, scope) ->
	    (match scope with
	    | Local ->
	        Queue_local_variable variable_name
	    | Global ->
            let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
            (match variable_kind with
            | Constant_kind value -> Literal_queue
            | Variable_kind discrete_index -> Queue_variable discrete_index
            )
        )

	| Typed_expr (expr, _) ->
        queue_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Queue_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

    | Typed_sequence (_, _, Typed_queue) -> Literal_queue

	| Typed_function_call (function_name, argument_expressions, _) ->
	    queue_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ as expr ->
	    let str_expr = string_of_typed_discrete_factor variable_infos Var_type_weak expr in
	    let fail_message = expr_type_doesnt_match_to_structure_message "queue" str_expr in
	    raise (InternalError fail_message)

and queue_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name =

    let fun_meta = user_function_meta variable_infos function_name in

    Queue_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression_without_type variable_infos) argument_expressions
    )

(* --------------------*)
(* Access conversion *)
(* --------------------*)

and expression_access_type_of_typed_factor variable_infos factor = function
    | Var_type_discrete_array (inner_type, _) ->
        Expression_array_access (
            array_expression_of_typed_factor variable_infos inner_type factor
        )
    | Var_type_discrete_list inner_type ->
        Expression_list_access (
            list_expression_of_typed_factor variable_infos inner_type factor
        )
    | _ ->
        raise (InternalError (
            "An access on other element than an array or a list was found, "
            ^ " although it was been type checked before."
        ))

let nonlinear_constraint_of_typed_nonlinear_constraint = bool_expression_of_typed_discrete_boolean_expression

let rec scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos = function
    | Typed_scalar_update variable_name ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> raise (InternalError "Unable to set a constant expression. This should be checked before.")
        | Variable_kind discrete_index -> Scalar_update discrete_index
        )

    | Typed_indexed_update (typed_scalar_or_index_update_type, index_expr, _) ->
        Indexed_update (
            scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

let update_type_of_typed_update_type variable_infos = function
    | Typed_variable_update typed_scalar_or_index_update_type ->
        Variable_update (
            scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type
        )
    | Typed_void_update -> Void_update


(*------------------------------------------------------------*)
(* Convert an array of variable coef into a linear term *)
(*------------------------------------------------------------*)
let linear_term_of_array array_of_coef constant =
  (* Create an empty list of members *)
  let members = ref [] in
  (* Iterate on the coef *)
  Array.iteri (fun variable_index coef ->
      if NumConst.neq coef NumConst.zero then (
        (* Add the member *)
        members := (coef, variable_index) :: !members;
      );
    ) array_of_coef;
  (* Create the linear term *)
  LinearConstraint.make_pxd_linear_term !members constant


(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*------------------------------------------------------------*)
let array_of_coef_of_linear_expression index_of_variables constants linear_expression =
  (* Create an array of coef *)
  let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
  (* Create a zero constant *)
  let constant = ref NumConst.zero in

  (* Internal function to update the array for a linear term *)
  let update_array_linear_term mul_coef = function
    (* Case constant -> update the constant with the coef *)
    | Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
      (* Case variables -> update the array with the coef  *)
    | Variable (coef, variable_name) ->
      (* Try to find the variable_index *)
      if Hashtbl.mem index_of_variables variable_name then (
        let variable_index = Hashtbl.find index_of_variables variable_name in
        (* Update the variable with its coef *)
        array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
        (* Try to find a constant *)
      ) else (
        if Hashtbl.mem constants variable_name then (
          (* Retrieve the value of the global constant *)
          let value = Hashtbl.find constants variable_name in
          let numconst_value = AbstractValue.numconst_value value in
          (* Update the NumConst *)
          constant := NumConst.add !constant (NumConst.mul (NumConst.mul numconst_value coef) mul_coef);
        ) else (
          raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))
        )
      );
  in

  (* Internal function to update the array for a linear expression *)
  let rec update_array_linear_expression = function
    | Linear_term lt -> update_array_linear_term NumConst.one lt
    | Linear_plus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.one lt;
    | Linear_minus_expression (le, lt) ->
      (* Fill the array with le *)
      update_array_linear_expression le;
      (* Fill the array with lt *)
      update_array_linear_term NumConst.minus_one lt;
  in
  (* Call the recursive function *)
  update_array_linear_expression linear_expression;
  (* Return the array of coef and the constant *)
  array_of_coef, !constant

(*------------------------------------------------------------*)
(* Convert a ParsingStructure.linear_constraint into a Constraint.linear_inequality *)
(*------------------------------------------------------------*)
let linear_inequality_of_linear_constraint index_of_variables constants (linexpr1, relop, linexpr2) =
    (* Get the array of variables and constant associated to the linear terms *)
    let array1, constant1 = array_of_coef_of_linear_expression index_of_variables constants linexpr1 in
    let array2, constant2 = array_of_coef_of_linear_expression index_of_variables constants linexpr2 in
    (* Consider the operator *)
    match relop with
    (* a < b <=> b - a > 0 *)
    | PARSED_OP_L ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
        (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_g *)

    (* a <= b <=> b - a >= 0 *)
    | PARSED_OP_LEQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
        (* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_ge *)

    (* a = b <=> b - a = 0 *)
    | PARSED_OP_EQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array2 array1 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant2 constant1 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_eq

        (* 	(Constraint.substract_linear_terms lt1 lt2), Constraint.Op_eq *)

    (* a >= b <=> a - b >= 0 *)
    | PARSED_OP_GEQ ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array1 array2 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant1 constant2 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_ge
        (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_ge *)

    (* a > b <=> a - b > 0 *)
    | PARSED_OP_G ->
        (* Create the array *)
        let array12 = OCamlUtilities.sub_array array1 array2 in
        (* Create the constant *)
        let constant12 = NumConst.sub constant1 constant2 in
        (* Create the linear_term *)
        let linear_term = linear_term_of_array array12 constant12 in
        (* Return the linear_inequality *)
        LinearConstraint.make_pxd_linear_inequality linear_term LinearConstraint.Op_g
        (* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_g *)

    | PARSED_OP_NEQ ->
        raise (InternalError("Inequality <> not yet supported"))

(*------------------------------------------------------------*)
(* Convert a ParsingStructure.convex_predicate into a Constraint.linear_constraint *)
(*------------------------------------------------------------*)
let linear_constraint_of_convex_predicate variable_infos convex_predicate : LinearConstraint.pxd_linear_constraint =
  try (
    (* Compute a list of inequalities *)
    let linear_inequalities = List.fold_left
        (fun linear_inequalities linear_inequality ->
           match linear_inequality with
           | Parsed_true_constraint -> linear_inequalities
           | Parsed_false_constraint -> raise False_exception
           | Parsed_linear_constraint (linexpr1, relop, linexpr2) -> (linear_inequality_of_linear_constraint variable_infos.index_of_variables variable_infos.constants (linexpr1, relop, linexpr2)) :: linear_inequalities
        ) [] convex_predicate
    in LinearConstraint.make_pxd_constraint linear_inequalities
    (* Stop if any false constraint is found *)
  ) with False_exception -> LinearConstraint.pxd_false_constraint ()

(*------------------------------------------------------------*)
(* Direct conversion of a ParsingStructure.linear_expression into a Parsed_linear_constraint.linear_term *)
(*------------------------------------------------------------*)
let linear_term_of_linear_expression variable_infos linear_expression =
    let index_of_variables = variable_infos.index_of_variables in
    let constants = variable_infos.constants in

    let array_of_coef, constant = array_of_coef_of_linear_expression index_of_variables constants linear_expression in
    linear_term_of_array array_of_coef constant

(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
(* TODO benjamin REFACTOR rename to linear_term_of_typed_arithmetic_expression *)
let linear_term_of_typed_update_arithmetic_expression variable_infos pdae =

    let index_of_variables = variable_infos.index_of_variables in
    let constants = variable_infos.constants in

	(* Create an array of coef *)
	let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in

	let rec update_coef_array_in_typed_update_arithmetic_expression mult_factor = function
		| Typed_sum_diff (parsed_update_arithmetic_expression, parsed_update_term, _, Typed_plus) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term *)
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;
		| Typed_sum_diff (parsed_update_arithmetic_expression, parsed_update_term, _, Typed_minus) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term: multiply by -1 for negation *)
		update_coef_array_in_parsed_update_term (NumConst.neg mult_factor) parsed_update_term;
		| Typed_term (parsed_update_term, _) ->
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;

	and update_coef_array_in_parsed_update_term mult_factor = function
		(* Multiplication is only allowed with a constant multiplier *)
		| Typed_product_quotient (parsed_update_term, parsed_update_factor, _, Typed_mul) as top_term ->

            (* Convert to abstract tree *)
            let converted_term = rational_arithmetic_expression_of_typed_term variable_infos parsed_update_term in
            let converted_factor = rational_arithmetic_expression_of_typed_factor variable_infos parsed_update_factor in

            (* Try to evaluate the term and the factor *)
            let numconst_valued_term_opt = DiscreteExpressionEvaluator.eval_constant_rational_term_opt None (* function table *) converted_term in
            let numconst_valued_factor_opt = DiscreteExpressionEvaluator.eval_constant_rational_factor_opt None (* function table *) converted_factor in

            (* Update coefficients *)
            (match numconst_valued_term_opt, numconst_valued_factor_opt with
            (* k * x with x a variable and k a constant *)
            | Some numconst_valued_term, None ->
                update_coef_array_in_parsed_update_factor (NumConst.mul numconst_valued_term mult_factor) parsed_update_factor
            (* x * k with x a variable and k a constant *)
            | None, Some numconst_valued_factor ->
                update_coef_array_in_parsed_update_term (NumConst.mul numconst_valued_factor mult_factor) parsed_update_term
            (* k1 * k2 with k1 and k2 constants *)
            | Some numconst_valued_term, Some _ ->
                update_coef_array_in_parsed_update_factor (NumConst.mul numconst_valued_term mult_factor) parsed_update_factor
            (* v1 * v2 with v1 and v2 variables *)
            | None, None -> raise (
                InternalError (
                    "`update_coef_array_in_parsed_update_term` fail, because expression `"
                    ^ string_of_typed_discrete_term variable_infos (Var_type_discrete_number Var_type_discrete_rat) top_term
                    ^ "` isn't linear. Linearity should be checked before."
                ))
            )



		| Typed_product_quotient (parsed_update_term, parsed_update_factor, _, Typed_div) as top_term ->

            (* Convert to abstract tree *)
            let converted_factor = rational_arithmetic_expression_of_typed_factor variable_infos parsed_update_factor in
            (* Try to evaluate the factor *)
            let numconst_valued_factor_opt = DiscreteExpressionEvaluator.eval_constant_rational_factor_opt None (* function table *) converted_factor in

            (* Update coefficients *)
            (match numconst_valued_factor_opt with
            | Some numconst_valued_factor ->
                update_coef_array_in_parsed_update_term (NumConst.div mult_factor numconst_valued_factor) parsed_update_term
            | None -> raise (
                InternalError (
                    "`update_coef_array_in_parsed_update_term` fail, because expression `"
                    ^ string_of_typed_discrete_term variable_infos (Var_type_discrete_number Var_type_discrete_rat) top_term
                    ^ "` isn't linear. Linearity should be checked before."
                ))
            )

		| Typed_factor (parsed_update_factor, _) ->
		    update_coef_array_in_parsed_update_factor mult_factor parsed_update_factor

	and update_coef_array_in_parsed_update_factor mult_factor = function
		| Typed_variable (variable_name, _, _) ->
			(* Try to find the variable_index *)
			if Hashtbl.mem index_of_variables variable_name then (
				let variable_index = Hashtbl.find index_of_variables variable_name in
				(* Update the array *)
				array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (mult_factor);
				(* Try to find a constant *)
			) else (
				if Hashtbl.mem constants variable_name then (
                    (* Retrieve the value of the global constant *)
                    let value = Hashtbl.find constants variable_name in
                    let numconst_value = AbstractValue.numconst_value value in
                    (* Update the constant *)
                    constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
				) else (
				    raise (InvalidExpression ("Impossible to find the index of variable `" ^ variable_name ^ "` in function 'update_coef_array_in_parsed_update_factor' although this should have been checked before."))
				)
			)
		| Typed_constant (var_value, _) ->
            (* Update the constant *)
            let numconst_value = ParsedValue.to_numconst_value var_value in
            constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
		| Typed_unary_min (parsed_discrete_factor, _) ->
			update_coef_array_in_parsed_update_factor mult_factor parsed_discrete_factor
		| Typed_expr (parsed_update_arithmetic_expression, _) ->
            update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression
		| factor ->
            raise (InternalError ("`update_coef_array_in_parsed_update_factor` fail because expression using `" ^ label_of_typed_factor_constructor factor ^ "` isn't linear. Linearity should be checked before."))
	in

	(* Call the recursive function updating the coefficients *)
	update_coef_array_in_typed_update_arithmetic_expression NumConst.one pdae;

	(* Create the linear term *)
	linear_term_of_array array_of_coef !constant

let linear_term_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, _) ->
        linear_term_of_typed_update_arithmetic_expression variable_infos expr
    | expr ->
        raise (
            InvalidExpression (
                "Impossible to convert Boolean expression \""
                ^ "\" to a linear expression, but it should was already type checked, maybe type check has failed."
            )
        )

let linear_term_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        linear_term_of_typed_discrete_boolean_expression variable_infos expr
    | _ ->
        raise (
            InvalidExpression (
                "Impossible to convert boolean expression \""
                ^ "\" to a linear expression, but it should was already type checked, maybe type check has failed."
            )
        )


let linear_term_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, _) ->
        linear_term_of_typed_boolean_expression variable_infos expr

let rec fun_body_of_typed_fun_body variable_infos = function
    | Typed_fun_local_decl (variable_name, discrete_type, typed_init_expr, typed_next_expr) ->
        Fun_local_decl (
            variable_name,
            discrete_type,
            global_expression_of_typed_global_expression variable_infos typed_init_expr,
            fun_body_of_typed_fun_body variable_infos typed_next_expr
        )
    | Typed_fun_instruction ((typed_update_type, typed_expr), typed_next_expr) ->
        Fun_instruction (
            (update_type_of_typed_update_type variable_infos typed_update_type,
            global_expression_of_typed_global_expression variable_infos typed_expr),
            fun_body_of_typed_fun_body variable_infos typed_next_expr
        )
    | Typed_fun_expr typed_expr ->
        Fun_expr (
            global_expression_of_typed_global_expression variable_infos typed_expr
        )

let fun_definition_of_typed_fun_definition variable_infos (typed_fun_definition : typed_fun_definition) : fun_definition =
    {
        name = typed_fun_definition.name;
        parameter_names = typed_fun_definition.parameters;
        signature_constraint = FunctionSig.signature_constraint_of_signature typed_fun_definition.signature;
        body = Fun_user (fun_body_of_typed_fun_body variable_infos typed_fun_definition.body);
        side_effect = typed_fun_definition.side_effect
    }

end