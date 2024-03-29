(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Mirror of parsing structure with type information
 *
 * File contributors : Benjamin L.
 * Created           : 2021/12/07
 *
 ************************************************************)

(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open ParsingStructure
open DiscreteType

type inner_type = var_type_discrete

type typed_assignment_scope =
    | Ass_discrete_global
    | Ass_discrete_local
    | Ass_clock

type typed_sequence_type =
    | Typed_array
    | Typed_list
    | Typed_stack
    | Typed_queue

type typed_conj_dis =
    | Typed_and
    | Typed_or

type typed_boolean_expression =
    | Typed_conj_dis of typed_boolean_expression * typed_boolean_expression * typed_conj_dis (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_nested_bool_expr of typed_boolean_expression (* implicitly bool type *)
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
	| Typed_variable of variable_ref * var_type_discrete
	| Typed_constant of ParsedValue.parsed_value * var_type_discrete
	| Typed_sequence of typed_boolean_expression list * inner_type * typed_sequence_type
	| Typed_nested_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_scalar_or_index_update_type =
    | Typed_scalar_update of variable_ref
    | Typed_indexed_update of typed_scalar_or_index_update_type * typed_discrete_arithmetic_expression * var_type_discrete

type typed_normal_update = typed_scalar_or_index_update_type * typed_boolean_expression

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

type typed_loop_dir =
    | Typed_for_loop_up
    | Typed_for_loop_down

type typed_instruction =
    | Typed_local_decl of variable_ref * var_type_discrete * typed_boolean_expression
    | Typed_assignment of typed_normal_update * typed_assignment_scope
    | Typed_instruction of typed_boolean_expression
    | Typed_for_loop of variable_ref * typed_discrete_arithmetic_expression (* from *) * typed_discrete_arithmetic_expression (* to *) * typed_loop_dir (* up or down *) * typed_seq_code_bloc (* inner bloc *)
    | Typed_while_loop of typed_boolean_expression (* condition *) * typed_seq_code_bloc (* inner bloc *)
    | Typed_if of typed_boolean_expression (* condition *) * typed_seq_code_bloc (* then bloc *) * typed_seq_code_bloc option (* else bloc *)

and typed_seq_code_bloc = typed_instruction list

type typed_fun_definition = {
    name : variable_name; (* function name *)
    parameter_refs : variable_ref list; (* parameter names and ids *)
    signature : var_type_discrete list; (* signature *)
    body : typed_seq_code_bloc * typed_boolean_expression option; (* body *)
}

(** Strings **)

let string_format_typed_node str_node discrete_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete discrete_type ^ "}"

(*let string_format_number_typed_node str_node discrete_number_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete_number discrete_number_type ^ "}"*)

let string_of_typed_sum_diff = function
    | Typed_plus -> Constants.default_arithmetic_string.plus_string
    | Typed_minus -> Constants.default_arithmetic_string.minus_string

let string_of_typed_product_quotient = function
    | Typed_mul -> Constants.default_arithmetic_string.mul_string
    | Typed_div -> Constants.default_arithmetic_string.div_string

let string_of_typed_conj_dis = function
    | Typed_and -> Constants.default_string.and_operator
    | Typed_or -> Constants.default_string.or_operator

let rec string_of_typed_boolean_expression variable_infos = function
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
        string_format_typed_node x Dt_bool

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_number_type) ->
	    let discrete_type = Dt_number discrete_number_type in
	    let str_node =
            string_of_typed_discrete_arithmetic_expression variable_infos discrete_type in_expr
            ^ Constants.default_string.in_operator
            ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type lw_expr
            ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type up_expr
        in
        string_format_typed_node str_node Dt_bool

	| Typed_nested_bool_expr expr ->
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
	| Typed_variable ((variable_name, _ (* id *)), _) ->
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

	| Typed_nested_expr (expr, _) ->
	    let str_node = "(" ^ string_of_typed_discrete_arithmetic_expression variable_infos discrete_type expr ^ ")" in
        string_format_typed_node str_node discrete_type


    | Typed_access (factor, index_expr, discrete_type, inner_type) ->
        let str_node =
            string_of_typed_discrete_factor variable_infos discrete_type factor
            ^ "["
            ^ string_of_typed_discrete_arithmetic_expression variable_infos (Dt_number Dt_int) index_expr
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
    | Typed_scalar_update (variable_name, _ (* id *)) -> variable_name
    | Typed_indexed_update (typed_scalar_or_index_update_type, index_expr, _) ->
        string_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type
        ^ "[" ^ string_of_typed_discrete_arithmetic_expression variable_infos (Dt_number Dt_int) index_expr ^ "]"

let string_of_typed_seq_code_bloc variable_infos (* parsed_seq_code_bloc *) =

    let rec string_of_typed_seq_code_bloc_rec parsed_seq_code_bloc =
        let str_instructions = List.map string_of_typed_instruction parsed_seq_code_bloc in
        OCamlUtilities.string_of_list_of_string_with_sep "\n" str_instructions

    and string_of_typed_instruction = function
        | Typed_local_decl ((variable_name, _), discrete_type, expr) ->
            ParsingStructureUtilities.string_of_let_in
                variable_name
                (DiscreteType.string_of_var_type_discrete discrete_type)
                (string_of_typed_boolean_expression variable_infos expr)

        | Typed_for_loop ((variable_name, _ (* id *)), from_expr, to_expr, loop_dir, inner_bloc) ->
            "for " ^ variable_name ^ " = "
            ^ string_of_typed_discrete_arithmetic_expression variable_infos (Dt_number Dt_int) from_expr
            ^ (match loop_dir with Typed_for_loop_up -> " to " | Typed_for_loop_down -> " downto ")
            ^ string_of_typed_discrete_arithmetic_expression variable_infos (Dt_number Dt_int) to_expr
            ^ " do\n"
            ^ string_of_typed_seq_code_bloc_rec inner_bloc
            ^ "\ndone"

        | Typed_while_loop (condition_expr, inner_bloc) ->
            "while "
            ^ string_of_typed_boolean_expression variable_infos condition_expr
            ^ " do\n"
            ^ string_of_typed_seq_code_bloc_rec inner_bloc
            ^ "\ndone"

        | Typed_if (condition_expr, then_bloc, else_bloc_opt) ->
            (* string representation of else bloc if defined *)
            let str_else =
                match else_bloc_opt with
                | Some else_bloc ->
                    " else " ^ string_of_typed_seq_code_bloc_rec else_bloc
                | None -> ""
            in

            "if "
            ^ string_of_typed_boolean_expression variable_infos condition_expr
            ^ " then "
            ^ string_of_typed_seq_code_bloc_rec then_bloc
            ^ str_else
            ^ " end"

        | Typed_assignment ((typed_scalar_or_index_update_type, update_expr), _) ->
            let str_left_member = string_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type in
            let str_right_member = string_of_typed_boolean_expression variable_infos update_expr in
            ParsingStructureUtilities.string_of_assignment str_left_member str_right_member ^ ";"

        | Typed_instruction expr ->
            string_of_typed_boolean_expression variable_infos expr ^ ";"

    in
    string_of_typed_seq_code_bloc_rec (* parsed_seq_code_bloc *)

let string_of_typed_loc_predicate _ = function
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