open ParsingStructure
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

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
	| Typed_And of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_Or of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
	| Typed_plus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_minus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_discrete_term =
	| Typed_mul of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_div of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete
	| Typed_constant of DiscreteValue.discrete_value * var_type_discrete
	| Typed_array of typed_boolean_expression array * inner_type
	| Typed_list of typed_boolean_expression list * inner_type
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_variable_access =
    | Typed_variable_name of variable_name
    | Typed_variable_access of typed_variable_access * typed_discrete_arithmetic_expression * var_type_discrete
    | Typed_wildcard

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

val get_type_of_variable_by_name : variable_infos -> variable_name -> var_type
val get_type_of_variable_by_name_opt : variable_infos -> variable_name -> var_type option
val get_discrete_type_of_variable_by_name : variable_infos -> variable_name -> var_type_discrete
val get_discrete_type_of_variable_by_name_opt : variable_infos -> variable_name -> var_type_discrete option

val string_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> string
val string_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> string

(* Check that a discrete init is well typed *)
val check_discrete_init : variable_infos -> variable_name -> parsed_global_expression -> typed_global_expression
(* Check that a constant declarations is well typed *)
val check_constant_expression : variable_infos -> variable_name * parsed_global_expression * DiscreteType.var_type -> typed_global_expression
(* Check that a guard is well typed *)
val check_guard : variable_infos -> guard -> typed_guard
(* Check that an update is well typed *)
val check_update : variable_infos -> variable_access -> ParsingStructure.parsed_global_expression -> typed_variable_access * typed_global_expression
(* Check that a condition is well typed *)
val check_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> typed_boolean_expression
(* Check that a predicate is well typed *)
val check_state_predicate : variable_infos -> parsed_state_predicate -> typed_state_predicate

end = struct



type inner_type = var_type_discrete

type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
	| Typed_And of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_Or of typed_boolean_expression * typed_boolean_expression (* implicitly bool type *)
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete (* implicitly bool type *)
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete_number (* implicitly bool type *)
	| Typed_bool_expr of typed_boolean_expression (* implicitly bool type *)
	| Typed_not_expr of typed_boolean_expression (* implicitly bool type *)

and typed_discrete_arithmetic_expression =
	| Typed_plus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_minus of typed_discrete_arithmetic_expression * typed_discrete_term * var_type_discrete_number
	| Typed_term of typed_discrete_term * var_type_discrete

and typed_discrete_term =
	| Typed_mul of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_div of typed_discrete_term * typed_discrete_factor * var_type_discrete_number
	| Typed_factor of typed_discrete_factor * var_type_discrete

and typed_discrete_factor =
	| Typed_variable of variable_name * var_type_discrete
	| Typed_constant of DiscreteValue.discrete_value * var_type_discrete
	| Typed_array of typed_boolean_expression array * inner_type
	| Typed_list of typed_boolean_expression list * inner_type
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor * var_type_discrete_number
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete * inner_type
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

type typed_variable_access =
    | Typed_variable_name of variable_name
    | Typed_variable_access of typed_variable_access * typed_discrete_arithmetic_expression * var_type_discrete
    | Typed_wildcard

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


(** Strings **)

let string_format_typed_node str_node discrete_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete discrete_type ^ "}"

let string_format_number_typed_node str_node discrete_number_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete_number discrete_number_type ^ "}"

let rec string_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, _) ->
        string_of_typed_boolean_expression variable_infos expr

and string_of_typed_boolean_expression variable_infos = function
	| Typed_And (l_expr, r_expr) ->
        string_of_typed_boolean_expression variable_infos l_expr
        ^ Constants.default_string.and_operator
        ^ string_of_typed_boolean_expression variable_infos r_expr

	| Typed_Or (l_expr, r_expr) ->
        string_of_typed_boolean_expression variable_infos l_expr
        ^ Constants.default_string.or_operator
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
	| Typed_plus (l_expr, r_expr, _) ->
	        string_of_typed_discrete_arithmetic_expression variable_infos discrete_type l_expr
	        ^ Constants.default_arithmetic_string.plus_string
	        ^ string_of_typed_discrete_term variable_infos discrete_type r_expr

	| Typed_minus (l_expr, r_expr, _) ->
	        string_of_typed_discrete_arithmetic_expression variable_infos discrete_type l_expr
	        ^ Constants.default_arithmetic_string.minus_string
	        ^ string_of_typed_discrete_term variable_infos discrete_type r_expr

	| Typed_term (term, _) ->
	        string_of_typed_discrete_term variable_infos discrete_type term

and string_of_typed_discrete_term variable_infos discrete_type = function
	| Typed_mul (l_expr, r_expr, _) ->
	        string_of_typed_discrete_term variable_infos discrete_type l_expr
	        ^ Constants.default_arithmetic_string.mul_string
            ^ string_of_typed_discrete_factor variable_infos discrete_type r_expr

	| Typed_div (l_expr, r_expr, _) ->
	        string_of_typed_discrete_term variable_infos discrete_type l_expr
	        ^ Constants.default_arithmetic_string.div_string
            ^ string_of_typed_discrete_factor variable_infos discrete_type r_expr

	| Typed_factor (factor, _) ->
            string_of_typed_discrete_factor variable_infos discrete_type factor

and string_of_typed_discrete_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _) ->
	    string_format_typed_node variable_name discrete_type
	| Typed_constant (value, _) ->
        string_format_typed_node (DiscreteValue.string_of_value value) discrete_type
	| Typed_array (array_expr, _) ->
	    let l_del, r_del = Constants.default_array_string.array_literal_delimiter in
	    let str_array = Array.map (string_of_typed_boolean_expression variable_infos) array_expr in
	    let str_node = l_del ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_array ^ r_del in
	    string_format_typed_node str_node discrete_type

	| Typed_list (list_expr, _) ->
	    let l_del, r_del = Constants.default_array_string.array_literal_delimiter in
	    let str_list = List.map (string_of_typed_boolean_expression variable_infos) list_expr in
	    let str_node = "list(" ^ l_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_list ^ r_del ^ ")" in
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


(** Get variables types **)

(* Get var type of a variable given it's index *)
let get_type_of_variable variable_infos variable_index =
    variable_infos.type_of_variables variable_index

(* Get discrete type of a variable given it's index *)
let get_discrete_type_of_variable variable_infos variable_index =
    let var_type = get_type_of_variable variable_infos variable_index in
    DiscreteType.discrete_type_of_var_type var_type

let get_type_of_variable_by_name_opt variable_infos variable_name =
    if Hashtbl.mem variable_infos.index_of_variables variable_name then (
        (* Get type of variable *)
        let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
        let variable_type = get_type_of_variable variable_infos variable_index in
        Some variable_type
    ) else if Hashtbl.mem variable_infos.constants variable_name then (
        (* Retrieve the value of the global constant *)
        let value = Hashtbl.find variable_infos.constants variable_name in
        (* Get type of constant *)
        Some (DiscreteValue.var_type_of_value value)
    ) else
        None

(* Get var type of a variable given it's name *)
let get_type_of_variable_by_name variable_infos variable_name =
    let discrete_type_opt = get_type_of_variable_by_name_opt variable_infos variable_name in
    match discrete_type_opt with
    | Some discrete_type -> discrete_type
    | None ->
        raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))


(* Get discrete type of a variable given it's name *)
(* Raise an exception if the variable cannot be found *)
let get_discrete_type_of_variable_by_name variable_infos variable_name =
    let var_type = get_type_of_variable_by_name variable_infos variable_name in
    DiscreteType.discrete_type_of_var_type var_type

(* Get discrete type of a variable given it's name *)
(* Get Some discrete type if found, otherwise None *)
let get_discrete_type_of_variable_by_name_opt variable_infos variable_name =
    let var_type_opt = get_type_of_variable_by_name_opt variable_infos variable_name in
    match var_type_opt with
    | Some var_type -> Some (DiscreteType.discrete_type_of_var_type var_type)
    | None -> None


(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)

let ill_typed_message_of_expressions str_expressions discrete_types str_outer_expr =
    let x = List.combine str_expressions discrete_types in
    let str_expressions_with_type_list = List.map (fun (str_expr, discrete_type) -> "`" ^ str_expr ^ ":" ^ DiscreteType.string_of_var_type_discrete discrete_type ^ "`") x in
    let str_expressions_with_type = OCamlUtilities.string_of_list_of_string_with_sep "," str_expressions_with_type_list in

    "Some of the expressions "
    ^ str_expressions_with_type
    ^ " in `"
    ^ str_outer_expr
    ^ "` are ill-typed or of incompatible types."

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
    ^ "` are ill-typed or of incompatible types."

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


let rec type_check_global_expression variable_infos infer_type_opt = function
    | Parsed_global_expression expr ->
        let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in
        Typed_global_expr (typed_expr, discrete_type), discrete_type

and type_check_parsed_boolean_expression variable_infos infer_type_opt = function
	| Parsed_And (l_expr, r_expr) ->
        let l_typed_expr, l_type = type_check_parsed_boolean_expression variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type = type_check_parsed_boolean_expression variable_infos infer_type_opt r_expr in

        (* Check that left and right members are Boolean *)
        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_And (l_typed_expr, r_typed_expr), Var_type_discrete_bool
        | _ -> raise (TypeError (
            "Expression `"
            ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos l_expr
            ^ "` is not compatible with `"
            ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos r_expr
            ^ "`: "
            ^ DiscreteType.string_of_var_type_discrete l_type
            ^ ", "
            ^ DiscreteType.string_of_var_type_discrete r_type
            ^ "."
        ))
        )

	| Parsed_Or (l_expr, r_expr) as outer_expr ->

        let l_typed_expr, l_type = type_check_parsed_boolean_expression variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type = type_check_parsed_boolean_expression variable_infos infer_type_opt r_expr in

        (* Check that left and right members are Boolean *)
        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_Or (l_typed_expr, r_typed_expr), Var_type_discrete_bool
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
	    let typed_expr, discrete_type = type_check_parsed_discrete_boolean_expression variable_infos infer_type_opt expr in
	    Typed_discrete_bool_expr (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_boolean_expression variable_infos infer_type_opt = function
    | Parsed_arithmetic_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    Typed_arithmetic_expr (typed_expr, discrete_type), discrete_type

	| Parsed_expression (l_expr, relop, r_expr) as outer_expr ->

	    let l_typed_expr, l_type = type_check_parsed_discrete_boolean_expression variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_boolean_expression variable_infos infer_type_opt r_expr in

        (* Check that left and right members are type compatibles *)
        if is_discrete_type_compatibles l_type r_type then (
            let discrete_type = stronger_discrete_type_of l_type r_type in
            Typed_comparison (l_typed_expr, relop, r_typed_expr, discrete_type), Var_type_discrete_bool
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

	| Parsed_expression_in (in_expr, lw_expr, up_expr) as outer_expr ->
	    let in_typed_expr, in_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt in_expr in
	    let lw_typed_expr, lw_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt lw_expr in
	    let up_typed_expr, up_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt up_expr in

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
            Typed_comparison_in (in_typed_expr, lw_typed_expr, up_typed_expr, inner_number_type), Var_type_discrete_bool
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
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in
	    Typed_bool_expr typed_expr, discrete_type

	| Parsed_Not expr as outer_expr ->
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in

        (* Check that expression type is Boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_not_expr typed_expr, Var_type_discrete_bool
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

and type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt = function
	| Parsed_DAE_plus (expr, term) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_plus (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
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

	| Parsed_DAE_minus (expr, term) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_minus (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | _ ->
            raise (TypeError (
                ill_typed_message
                    (string_of_parsed_arithmetic_expression variable_infos expr)
                    (string_of_parsed_term variable_infos term)
                    (string_of_parsed_arithmetic_expression variable_infos outer_expr)
                    l_type
                    r_type
            ))
        )

	| Parsed_DAE_term term ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    Typed_term (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_term variable_infos infer_type_opt = function
    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)

    | Parsed_DT_div ((Parsed_DT_factor (Parsed_DF_constant lv) as term), (Parsed_DF_constant rv as factor)) as outer_expr ->

	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->

            (* Doing division *)
            let l_numconst = DiscreteValue.to_numconst_value lv in
            let r_numconst = DiscreteValue.to_numconst_value rv in
            let numconst_value = NumConst.div l_numconst r_numconst in
            (* Check if result is representable by an int *)
            let can_be_int = NumConst.is_int numconst_value in

            (* If it's representable by an int, it can be a rational or an int *)
            let discrete_number_type =
                if can_be_int then
                    Var_type_discrete_unknown_number
                (* If it's not representable by an int, it's a rational *)
                else
                    Var_type_discrete_rational
            in

            Typed_div (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type

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

    | Parsed_DT_mul (term, factor) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_mul (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | _ -> raise (TypeError (
            ill_typed_message
                (string_of_parsed_term variable_infos term)
                (string_of_parsed_factor variable_infos factor)
                (string_of_parsed_term variable_infos outer_expr)
                l_type
                r_type
        ))
        )

	| Parsed_DT_div (term, factor) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in

            (* If left unknown and not right convert left *)

            Typed_div (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
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
	    let typed_expr, discrete_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in
	    Typed_factor (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_factor variable_infos infer_type_opt = function
	| Parsed_DF_variable variable_name ->
        let discrete_type = get_discrete_type_of_variable_by_name variable_infos variable_name in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some ((Var_type_discrete_number _) as infer_type), Var_type_discrete_number Var_type_discrete_unknown_number -> infer_type
            | _ -> discrete_type
        in

        Typed_variable (variable_name, infer_discrete_type), infer_discrete_type

	| Parsed_DF_constant value ->
        let discrete_type = DiscreteValue.discrete_type_of_value value in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some ((Var_type_discrete_number _) as infer_type), Var_type_discrete_number Var_type_discrete_unknown_number -> infer_type
            | _ -> discrete_type
        in

        Typed_constant (value, infer_discrete_type), infer_discrete_type

	| Parsed_DF_array array_expr as outer_expr ->

	    let list_expr = Array.to_list array_expr in
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        (* Check that all elements types are compatible *)
        if all_compatibles then (
            (* Get inner type of a collection analysing types of it's elements *)
            (* If the collection is empty, it's type will be inferred by the context *)
            let inner_type = type_check_collection discrete_types infer_type_opt in
            let discrete_type = Var_type_discrete_array (inner_type, List.length list_expr) in
            Typed_array (Array.of_list typed_expressions, inner_type), discrete_type
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

	| Parsed_DF_list list_expr as outer_expr ->
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        (* Check that all elements types are compatible *)
        if all_compatibles then (
            (* Get inner type of a collection analysing types of it's elements *)
            (* If the collection is empty, it's type will be inferred by the context *)
            let inner_type = type_check_collection discrete_types infer_type_opt in
            Typed_list (typed_expressions, inner_type), Var_type_discrete_list inner_type
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
        let typed_factor, factor_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in
        let typed_index, index_type = type_check_parsed_discrete_arithmetic_expression variable_infos None (* None: mean no inference for index *) index_expr in

        if not (is_discrete_type_number_type index_type) || is_discrete_type_rational_type index_type then
            raise (TypeError "Index cannot be another type than int.");

        (* Check that accessed element is a collection *)
        (match factor_type with
        | Var_type_discrete_array (inner_type, _)
        | Var_type_discrete_list inner_type -> Typed_access (typed_factor, typed_index, factor_type, inner_type), inner_type
        | _ -> raise (TypeError "Cannot make an access to another type than array or list.")
        )

	| Parsed_DF_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    Typed_expr (typed_expr, discrete_type), discrete_type

	| Parsed_DF_unary_min factor as outer_expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that expression is a number *)
        (match discrete_type with
        | Var_type_discrete_number discrete_number_type -> Typed_unary_min (typed_expr, discrete_number_type), discrete_type
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

        (* Get function arity *)
        let arity = Functions.arity_of_function function_name in
        let arguments_number = List.length argument_expressions in

        (* Check call number of arguments is consistent with function arity *)
        if arguments_number <> arity then
            raise (TypeError (
                "Arguments number of `"
                ^ string_of_parsed_factor variable_infos func
                ^ "`/"
                ^ string_of_int (arguments_number)
                ^ " call doesn't match with arity of the function : `"
                ^ function_name
                ^ "`/"
                ^ string_of_int arity
                ^ "."
            ));

        (* Type check arguments *)
        (* We doesn't infer arguments types because arguments types are not dependent of the context *)
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos None (* None: mean no inference for arguments *)) argument_expressions in
        (* Get arguments discrete types  *)
        let call_signature = List.map (fun (_, discrete_type) -> discrete_type) type_checks in
        (* Get typed arguments expressions *)
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in

        (* Get function signature *)
        let function_signature_constraint = Functions.signature_constraint_of_function function_name in
        (* Get parameters signature *)
        let function_parameter_signature_constraint, _ = FunctionSig.split_signature function_signature_constraint in

        (* Check signature and signature constraint compatibility *)
        let is_compatibles = FunctionSig.is_signature_compatible_with_signature_constraint call_signature function_parameter_signature_constraint in

        (* If signature and signature constraint are not compatibles raise a type error *)
        if not is_compatibles then
            raise (TypeError (
                "`"
                ^ string_of_parsed_factor variable_infos func
                ^ " : "
                ^ FunctionSig.string_of_signature call_signature
                ^ " -> _` is not compatible with `"
                ^ label_of_parsed_factor_constructor func
                ^ " : "
                ^ FunctionSig.string_of_signature_constraint function_signature_constraint
                ^ "`."
            ));

        (* --- *)
        let signature_constraint_with_expressions = List.combine function_parameter_signature_constraint typed_expressions in

        let dependent_type_constraints = OCamlUtilities.rev_filter_map (fun (type_constraint, expr) ->
            match type_constraint with
            | Defined_type_constraint (Number_constraint (Defined_type_number_constraint Int_constraint (Int_name_constraint constraint_name))) ->

                let converted_expr = Convert.global_expression_of_typed_boolean_expression variable_infos expr (Var_type_discrete_number Var_type_discrete_int) in

                if not (DiscreteExpressionEvaluator.is_global_expression_constant converted_expr) then (
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
                    let value = DiscreteExpressionEvaluator.try_reduce_global_expression converted_expr in
                    Some (constraint_name, Resolved_length_constraint (Int32.to_int (DiscreteValue.to_int_value value)))
                )
            | _ -> None

        ) signature_constraint_with_expressions
        in

        (* ---- *)
        (* ---- *)
        (* ---- *)
        (* ---- *)

        (* Resolve constraint according to arguments types *)
        let resolved_constraints, malformed_constraints = TypeConstraintResolver.resolve_constraints variable_infos function_parameter_signature_constraint call_signature argument_expressions in

        let resolved_constraints = resolved_constraints @ dependent_type_constraints in

        (* Eventually display the list of malformed constraints, if any, and raise an exception *)
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

        (* Resolve signature from resolved constraints *)
        let resolved_constraints_table = OCamlUtilities.hashtbl_of_tuples resolved_constraints in

        let resolved_signature = TypeConstraintResolver.signature_of_signature_constraint resolved_constraints_table function_signature_constraint in

        (* Print messages *)
        print_message Verbose_high ("\tInfer signature constraint of `" ^ function_name ^ "`: " ^ FunctionSig.string_of_signature_constraint function_signature_constraint);

        (* Print resolved constraints *)
        let str_resolved_constraints = TypeConstraintResolver.string_of_resolved_constraints resolved_constraints in
        if str_resolved_constraints <> "" then print_message Verbose_high ("\tInfer resolved constraints: {" ^ str_resolved_constraints ^ "} for call `" ^ string_of_parsed_factor variable_infos func ^ "`.");

        print_message Verbose_high ("\tInfer signature of `" ^ string_of_parsed_factor variable_infos func ^ "` resolved as: " ^ FunctionSig.string_of_signature resolved_signature);

        let resolved_signature_without_return_type, return_type = FunctionSig.split_signature resolved_signature in

        (* Now we had resolved discrete type of arguments, we can infer arguments to these types *)
        let combine = List.combine argument_expressions resolved_signature_without_return_type in
        let type_checks = List.map (fun (argument_exp, discrete_type) -> type_check_parsed_boolean_expression variable_infos (Some discrete_type) (* inference to type deduced from signature *) argument_exp) combine in
        (* Get typed arguments expressions *)
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in

        Typed_function_call (function_name, typed_expressions, return_type), return_type



let rec type_check_variable_access variable_infos infer_type_opt = function
    | Variable_name variable_name ->
        (* Get assigned variable type *)
        let var_type = get_type_of_variable_by_name variable_infos variable_name in
        let discrete_type = discrete_type_of_var_type var_type in
        Typed_variable_name variable_name, discrete_type

    | Variable_access (variable_access, index_expr) ->

        let typed_variable_access, discrete_type = type_check_variable_access variable_infos infer_type_opt variable_access in
        let typed_index_expr_type, _ = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt index_expr in

        (* Check is an array *)
        let discrete_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (TypeError "Trying to make a write access to a non-array variable.")
        in
        Typed_variable_access (typed_variable_access, typed_index_expr_type, discrete_type), discrete_type

    | Wildcard -> Typed_wildcard, Var_type_weak

let type_check_parsed_loc_predicate variable_infos infer_type_opt = function
	| Parsed_loc_predicate_EQ (automaton_name, loc_name) -> Typed_loc_predicate_EQ (automaton_name, loc_name), Var_type_discrete_bool
	| Parsed_loc_predicate_NEQ (automaton_name, loc_name) -> Typed_loc_predicate_NEQ (automaton_name, loc_name), Var_type_discrete_bool

let rec type_check_parsed_simple_predicate variable_infos infer_type_opt = function
	| Parsed_discrete_boolean_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_boolean_expression variable_infos infer_type_opt expr in

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
	        Typed_discrete_boolean_expression (typed_expr, discrete_type), discrete_type

	| Parsed_loc_predicate predicate ->
	    let typed_predicate, discrete_type = type_check_parsed_loc_predicate variable_infos infer_type_opt predicate in
	    Typed_loc_predicate typed_predicate, discrete_type

	| Parsed_state_predicate_true -> Typed_state_predicate_true, Var_type_discrete_bool
	| Parsed_state_predicate_false -> Typed_state_predicate_false, Var_type_discrete_bool
	| Parsed_state_predicate_accepting -> Typed_state_predicate_accepting, Var_type_discrete_bool

and type_check_parsed_state_predicate variable_infos infer_type_opt = function
	| Parsed_state_predicate_OR (l_expr, r_expr) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_state_predicate variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_state_predicate variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_OR (l_typed_expr, r_typed_expr), Var_type_discrete_bool
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
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt term in
	    Typed_state_predicate_term (typed_expr, discrete_type), discrete_type

and type_check_parsed_state_predicate_term variable_infos infer_type_opt = function
	| Parsed_state_predicate_term_AND (l_expr, r_expr) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_term_AND (l_typed_expr, r_typed_expr), Var_type_discrete_bool
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
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in
	    Typed_state_predicate_factor (typed_expr, discrete_type), discrete_type

and type_check_parsed_state_predicate_factor variable_infos infer_type_opt = function
	| Parsed_state_predicate_factor_NOT factor as outer_expr ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in

        (* Check that expression type is boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_state_predicate_factor_NOT typed_expr, discrete_type
	    | _ ->
	        raise (TypeError (
                ill_typed_message_of_expressions
                    [string_of_parsed_state_predicate_factor variable_infos factor]
                    [discrete_type]
                    (string_of_parsed_state_predicate_factor variable_infos outer_expr)
	        ))
        )

	| Parsed_simple_predicate predicate ->
	    let typed_expr, discrete_type = type_check_parsed_simple_predicate variable_infos infer_type_opt predicate in
	    Typed_simple_predicate (typed_expr, discrete_type), discrete_type

	| Parsed_state_predicate predicate ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate variable_infos infer_type_opt predicate in
	    Typed_state_predicate (typed_expr, discrete_type), discrete_type



(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment variable_infos variable_name variable_type expr =

    (* Function that construct type error message *)
    let get_error_message variable_name variable_type expr_type expr =
        "Variable "
        ^ variable_name
        ^ " of type "
        ^ (DiscreteType.string_of_var_type_discrete variable_type)
        ^ " is not compatible with expression : `"
        ^ (string_of_parsed_global_expression variable_infos expr)
        ^ "`"
        ^ " of type "
        ^ (DiscreteType.string_of_var_type_discrete expr_type)
    in

    (* Eventually get a number type to infer *)
(*    let variable_number_type_opt = DiscreteType.extract_number_of_discrete_type variable_type in*)
    let variable_number_type_opt = Some (DiscreteType.extract_inner_type variable_type) in
    (* Resolve typed expression *)
    let typed_expr, expr_var_type_discrete = type_check_global_expression variable_infos variable_number_type_opt expr in

    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteType.is_discrete_type_compatibles variable_type expr_var_type_discrete in

    (* Not consistent ? raise a type error with appropriate message *)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expr_var_type_discrete expr))
    )
    else (
        typed_expr
    )

(* Check that a discrete variable initialization is well typed *)
let check_discrete_init variable_infos variable_name expr =

    (* Get the variable index *)
    let discrete_index = Hashtbl.find variable_infos.index_of_variables variable_name in
    (* Get variable type *)
    let var_type = get_type_of_variable variable_infos discrete_index in

    (* Check whether variable is clock or parameter *)
    let is_clock_or_parameter = var_type == DiscreteType.Var_type_clock || var_type == DiscreteType.Var_type_parameter in

    (* Check if variable is clock or parameter, it's forbidden to init clock or parameter in discrete section *)
    if (is_clock_or_parameter) then (
        raise (TypeError ("Initialisation of a " ^ (DiscreteType.string_of_var_type var_type) ^ " in discrete init state section is forbidden"))
    );

    (* Get variable type *)
    let variable_type = get_discrete_type_of_variable_by_name variable_infos variable_name in
    (* Check expression / variable type consistency *)
    let typed_expr = check_type_assignment variable_infos variable_name variable_type expr in
    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - inits - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_global_expression variable_infos typed_expr
    );
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

    let typed_nonlinear_constraint, discrete_type = type_check_parsed_discrete_boolean_expression variable_infos None nonlinear_constraint in

    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - guards - "
        ^ string_of_typed_discrete_boolean_expression variable_infos typed_nonlinear_constraint
    );

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
let check_update variable_infos variable_access expr =

    (* Get assigned variable name *)
    let variable_name_opt = ParsingStructureUtilities.variable_name_of_variable_access variable_access in

    (* Get assigned variable type *)
    let variable_name, var_type =
        match variable_name_opt with
        | Some variable_name -> variable_name, get_type_of_variable_by_name variable_infos variable_name
        | None -> "", Var_type_discrete Var_type_weak
    in

    (* Eventually get a number type to infer *)
    let variable_number_type_opt =
        match var_type with
        | Var_type_clock
        | Var_type_parameter -> None
        | Var_type_discrete discrete_type -> Some (DiscreteType.extract_inner_type discrete_type)
    in
    (* Resolve typed expression *)
    let typed_expr, expr_type = type_check_global_expression variable_infos variable_number_type_opt expr in

    let typed_variable_access, l_value_type = type_check_variable_access variable_infos variable_number_type_opt variable_access in

    (* Check var_type_discrete is compatible with expression type, if yes, convert expression *)
     if not (DiscreteType.is_discrete_type_compatibles l_value_type expr_type) then (
        raise (TypeError (
            "Variable `"
            ^ variable_name
            ^ "` of type "
            ^ (DiscreteType.string_of_var_type var_type)
            ^ " is not compatible with expression `"
            ^ (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr)
            ^ "` of type "
            ^ (DiscreteType.string_of_var_type_discrete expr_type)
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

    typed_variable_access,
    typed_expr

(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
let check_conditional variable_infos expr =

    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer conditional expression: " ^ string_of_parsed_boolean_expression variable_infos expr);

    let typed_expr, expr_type = type_check_parsed_boolean_expression variable_infos None expr in

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
    let typed_predicate, discrete_type = type_check_parsed_state_predicate variable_infos None predicate in

    ImitatorUtilities.print_message Verbose_high "-------------------------";
    ImitatorUtilities.print_message Verbose_high "Type tree of property: ";
    ImitatorUtilities.print_message Verbose_high "-------------------------";
    ImitatorUtilities.print_message Verbose_high (string_of_typed_state_predicate variable_infos typed_predicate);
    ImitatorUtilities.print_message Verbose_high "-------------------------";

    match discrete_type with
    | Var_type_discrete_bool -> typed_predicate
    | _ -> raise (TypeError (
            "Property `"
            ^ string_of_typed_state_predicate variable_infos typed_predicate
            ^ "` is not a Boolean expression."
        ))

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

val variable_access_of_typed_variable_access : variable_infos -> TypeChecker.typed_variable_access -> DiscreteExpressions.discrete_variable_access


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

let label_of_typed_factor_constructor = function
	| Typed_variable _ -> "variable"
	| Typed_constant _ -> "constant"
	| Typed_array _ -> "array"
	| Typed_list _ -> "list"
	| Typed_access _ -> "access"
	| Typed_expr _ -> "expression"
	| Typed_unary_min _ -> "minus"
    | Typed_function_call (function_name, _, _) -> function_name

(** Convert a Boolean operator to its abstract model *)
let convert_parsed_relop = function
	| PARSED_OP_L -> OP_L
	| PARSED_OP_LEQ	-> OP_LEQ
	| PARSED_OP_EQ	-> OP_EQ
	| PARSED_OP_NEQ	-> OP_NEQ
	| PARSED_OP_GEQ	-> OP_GEQ
	| PARSED_OP_G -> OP_G




(* Convert discrete expressions *)

let rec global_expression_of_typed_global_expression variable_infos = function
    | Typed_global_expr (expr, discrete_type) ->
        global_expression_of_typed_boolean_expression variable_infos expr discrete_type

(* TODO benjamin shouldn't have this function, it's because of global expression that isn't useful *)
(* I should remove global expression in parsing structure for avoid it *)
and global_expression_of_typed_boolean_expression_without_type variable_infos = function
	| Typed_And _
	| Typed_Or _ as expr ->
	    global_expression_of_typed_boolean_expression variable_infos expr Var_type_discrete_bool
	| Typed_discrete_bool_expr (_, discrete_type) as expr ->
	    global_expression_of_typed_boolean_expression variable_infos expr discrete_type

and global_expression_of_typed_boolean_expression variable_infos expr discrete_type =
    (*
    ImitatorUtilities.print_message Verbose_standard (
        "Convert: "
        ^ string_of_typed_boolean_expression variable_infos expr
        ^ " to "
        ^ DiscreteType.string_of_var_type_discrete discrete_type
        ^ " expression."
    );
    *)
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
    | Var_type_weak ->
        raise (InternalError "An expression should have a determined type. Maybe something has failed before.")
    | Var_type_discrete_stack inner_type ->
        Stack_expression (
            stack_expression_of_typed_boolean_expression variable_infos inner_type expr
        )

and discrete_arithmetic_expression_of_typed_boolean_expression variable_infos discrete_number_type = function
	| Typed_discrete_bool_expr (expr, _) ->
	    (match discrete_number_type with
	    | Var_type_discrete_unknown_number
	    | Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    | Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr)
	    )
	| _ -> raise (InternalError "Trying to convert Boolean expression to arithmetic one. Maybe something failed in type checking or conversion.")

and discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type = function
	| Typed_arithmetic_expr (expr, discrete_type) ->
	    (match discrete_number_type with
	    | Var_type_discrete_unknown_number
	    | Var_type_discrete_rational ->
	        Rational_arithmetic_expression (rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    | Var_type_discrete_int ->
	        Int_arithmetic_expression (int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr)
	    )

	| _ -> raise (InternalError "Trying to convert Boolean expression to arithmetic one. Maybe something failed in type checking or conversion.")

and discrete_arithmetic_expression_of_typed_discrete_arithmetic_expression variable_infos discrete_number_type = function
	| Typed_plus (expr, term, _) ->
        (match discrete_number_type with
        | Var_type_discrete_unknown_number
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                Rational_plus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_plus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        )

	| Typed_minus (expr, term, _) ->
        (match discrete_number_type with
        | Var_type_discrete_unknown_number
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (
                Rational_minus (
                    rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    rational_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        | Var_type_discrete_int ->
            Int_arithmetic_expression (
                Int_minus (
                    int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
                    int_arithmetic_expression_of_typed_term variable_infos term
                )
            )
        )

	| Typed_term (term, _) ->
        (match discrete_number_type with
        | Var_type_discrete_unknown_number
        | Var_type_discrete_rational ->
            Rational_arithmetic_expression (Rational_term (rational_arithmetic_expression_of_typed_term variable_infos term))
        | Var_type_discrete_int ->
            Int_arithmetic_expression (Int_term (int_arithmetic_expression_of_typed_term variable_infos term))
        )

(* --------------------*)
(* Bool conversion *)
(* --------------------*)

and bool_expression_of_typed_boolean_expression variable_infos = function
	| Typed_And (l_expr, r_expr) ->
	    And_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
	    )

	| Typed_Or (l_expr, r_expr) ->
	    Or_bool (
	        bool_expression_of_typed_boolean_expression variable_infos l_expr,
	        bool_expression_of_typed_boolean_expression variable_infos r_expr
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
        Expression (
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
    | Var_type_weak ->
        raise (InternalError "An expression should have a determined type. Maybe something has failed before.")
    | Var_type_discrete_stack inner_type ->
        Stack_comparison (
            stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            convert_parsed_relop relop,
            stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )

and bool_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_term (term, _) ->
	    bool_expression_of_typed_term variable_infos term
	| _ ->
	    raise (InternalError "The expression type indicate that it should be converted to a Boolean expression, but an arithmetic operator is found. Maybe something failed in type checking or conversion.")


and bool_expression_of_typed_term variable_infos = function
	| Typed_factor (factor, _) ->
	    bool_expression_of_typed_factor variable_infos factor
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a Boolean expression, but an arithmetic operator is found. Maybe something failed in type checking or conversion.")

and bool_expression_of_typed_factor variable_infos = function
	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Bool_constant (DiscreteValue.bool_value value)
        | Variable_kind discrete_index -> Bool_variable discrete_index
        )

	| Typed_constant (value, _) ->
	    Bool_constant (DiscreteValue.bool_value value)

    | Typed_expr (expr, _) ->
        bool_expression_of_typed_arithmetic_expression variable_infos expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Bool_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    bool_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to a Boolean expression, but a non-Boolean expression is found. Maybe something failed in type checking or conversion.")

and bool_expression_of_typed_function_call variable_infos argument_expressions = function
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Bool_list_hd (
            list_expression_of_typed_boolean_expression variable_infos Var_type_discrete_bool arg_0
        )
    | "list_mem" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_mem (
            global_expression_of_typed_boolean_expression_without_type variable_infos arg_0,
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_1
        )
    | "array_mem" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_mem (
            global_expression_of_typed_boolean_expression_without_type variable_infos arg_0,
            array_expression_of_typed_boolean_expression_with_type variable_infos arg_1
        )

    | "stack_is_empty" ->
        let arg_0 = List.nth argument_expressions 0 in
        Stack_is_empty (
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )

    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Rational conversion *)
(* --------------------*)

and rational_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an arithmetic expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")


and rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an arithmetic expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    Rational_plus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    Rational_minus (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    Rational_term (
	        rational_arithmetic_expression_of_typed_term variable_infos term
	    )

and rational_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    Rational_mul (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    Rational_div (
	        rational_arithmetic_expression_of_typed_term variable_infos term,
	        rational_arithmetic_expression_of_typed_factor variable_infos factor
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

	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Rational_constant (DiscreteValue.to_numconst_value value)
        | Variable_kind discrete_index -> Rational_variable discrete_index
        )

	| Typed_constant (value, _) ->
	    Rational_constant (DiscreteValue.to_numconst_value value)

	| Typed_expr (expr, _) ->
	    Rational_expression (
	        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Rational_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    rational_expression_of_typed_function_call variable_infos argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to a rational arithmetic expression, but a non rational arithmetic expression is found. Maybe something failed in type checking or conversion.")

and rational_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Rational_pow (
            rational_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "rational_of_int" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_of_int (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0
        )

    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_list_hd (
            list_expression_of_typed_boolean_expression variable_infos (Var_type_discrete_number Var_type_discrete_rational) arg_0
        )
    | "stack_pop" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_stack_pop (
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    | "stack_top" ->
        let arg_0 = List.nth argument_expressions 0 in
        Rational_stack_top (
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )

    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Int conversion *)
(* --------------------*)

and int_arithmetic_expression_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (expr, _) ->
        int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an arithmetic expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an arithmetic expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and int_arithmetic_expression_of_typed_arithmetic_expression variable_infos = function
	| Typed_plus (expr, term, _) ->
	    Int_plus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_minus (expr, term, _) ->
	    Int_minus (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr,
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

	| Typed_term (term, _) ->
	    Int_term (
	        int_arithmetic_expression_of_typed_term variable_infos term
	    )

and int_arithmetic_expression_of_typed_term variable_infos = function
	| Typed_mul (term, factor, _) ->
	    Int_mul (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
	    )

	| Typed_div (term, factor, _) ->
	    Int_div (
	        int_arithmetic_expression_of_typed_term variable_infos term,
	        int_arithmetic_expression_of_typed_factor variable_infos factor
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

	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Int_constant (DiscreteValue.to_int_value value)
        | Variable_kind discrete_index -> Int_variable discrete_index
        )

	| Typed_constant (value, _) ->
	    Int_constant (DiscreteValue.to_int_value value)

	| Typed_expr (expr, _) ->
	    Int_expression (
	        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
        )

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Int_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    int_expression_of_typed_function_call variable_infos argument_expressions function_name
	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to an int arithmetic expression, but a non int arithmetic expression is found. Maybe something failed in type checking or conversion.")

and int_expression_of_typed_function_call variable_infos argument_expressions = function
    | "pow" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Int_pow (
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Int_list_hd (
            list_expression_of_typed_boolean_expression variable_infos (Var_type_discrete_number Var_type_discrete_int) arg_0
        )
    | "array_length" ->
        let arg_0 = List.nth argument_expressions 0 in
        Array_length (
            array_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )

    | "list_length" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_length (
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )

    | "stack_length" ->
        let arg_0 = List.nth argument_expressions 0 in
        Stack_length (
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    (* TODO benjamin, in the future replace raise by custom function call as comment below *)
    | function_name -> raise (UndefinedFunction function_name)

(* --------------------*)
(* Binary word conversion *)
(* --------------------*)

and binary_expression_of_typed_boolean_expression variable_infos length = function
    | Typed_discrete_bool_expr (expr, _) ->
        binary_expression_of_typed_discrete_boolean_expression variable_infos length expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a binary expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and binary_expression_of_typed_discrete_boolean_expression variable_infos length = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        binary_expression_of_typed_arithmetic_expression variable_infos length expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a binary expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and binary_expression_of_typed_arithmetic_expression variable_infos length = function
	| Typed_term (term, _) ->
	        binary_expression_of_typed_term variable_infos length term
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a binary expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and binary_expression_of_typed_term variable_infos length = function
	| Typed_factor (factor, _) ->
	        binary_expression_of_typed_factor variable_infos length factor
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a binary expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and binary_expression_of_typed_factor variable_infos length = function
	| Typed_variable (variable_name, _) ->

        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Binary_word_constant (DiscreteValue.binary_word_value value)
        | Variable_kind discrete_index -> Binary_word_variable (discrete_index, length)
        )

	| Typed_constant (value, _) ->
	    Binary_word_constant (DiscreteValue.binary_word_value value)

	| Typed_expr (expr, _) ->
        binary_expression_of_typed_arithmetic_expression variable_infos length expr

    | Typed_access (factor, index_expr, discrete_type, _) ->

        Binary_word_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr,
            length
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    binary_expression_of_typed_function_call variable_infos length argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to a binary expression, but a non binary expression is found. Maybe something failed in type checking or conversion.")

and binary_expression_of_typed_function_call variable_infos length argument_expressions = function
    | "shift_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_left (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            length
        )
    | "shift_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_shift_right (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            length
        )
    | "fill_left" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_left (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            length
        )
    | "fill_right" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_fill_right (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            int_arithmetic_expression_of_typed_boolean_expression variable_infos arg_1,
            length
        )
    | "logand" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_and (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            binary_expression_of_typed_boolean_expression variable_infos length arg_1,
            length
        )
    | "logor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_or (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            binary_expression_of_typed_boolean_expression variable_infos length arg_1,
            length
        )
    | "logxor" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in

        Logical_xor (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            binary_expression_of_typed_boolean_expression variable_infos length arg_1,
            length
        )

    | "lognot" ->
        let arg_0 = List.nth argument_expressions 0 in

        Logical_not (
            binary_expression_of_typed_boolean_expression variable_infos length arg_0,
            length
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Binary_word_list_hd (
            list_expression_of_typed_boolean_expression variable_infos (Var_type_discrete_binary_word length) arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

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
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")


and array_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and array_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        array_expression_of_typed_term variable_infos discrete_type term
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and array_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        array_expression_of_typed_factor variable_infos discrete_type factor
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and array_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> Array_constant (DiscreteValue.array_value value)
        | Variable_kind discrete_index -> Array_variable discrete_index
        )

	| Typed_constant (value, _) ->
	    Array_constant (DiscreteValue.array_value value)

    | Typed_array (expr_array, _) ->
        (* Should take inner_type unbox type *)

        Literal_array (Array.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr discrete_type) expr_array)

	| Typed_expr (expr, _) ->
        array_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    array_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to an array expression, but a non array expression is found. Maybe something failed in type checking or conversion.")

and array_expression_of_typed_function_call variable_infos discrete_type argument_expressions = function
    | "array_append" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Array_concat (
            array_expression_of_typed_boolean_expression_with_type variable_infos arg_0,
            array_expression_of_typed_boolean_expression_with_type variable_infos arg_1
        )

    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        Array_list_hd (
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)
    (*
    Array_function_call (
        name,
        List.map (global_expression_of_parsed_boolean_expression variable_infos) argument_expressions
    )
    *)

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
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")


and list_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and list_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        list_expression_of_typed_term variable_infos discrete_type term
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and list_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        list_expression_of_typed_factor variable_infos discrete_type factor
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and list_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> List_constant (DiscreteValue.list_value value)
        | Variable_kind discrete_index -> List_variable discrete_index
        )

	| Typed_constant (value, _) ->
	    List_constant (DiscreteValue.list_value value)

    | Typed_list (expr_list, _) ->
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr discrete_type) expr_list)

	| Typed_expr (expr, _) ->
        list_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

    | Typed_access (factor, index_expr, discrete_type, _) ->
        List_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
	    list_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a non list expression is found. Maybe something failed in type checking or conversion.")

and list_expression_of_typed_function_call variable_infos discrete_type argument_expressions = function
    | "list_cons" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        List_cons (
            global_expression_of_typed_boolean_expression variable_infos arg_0 discrete_type,
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_1
        )
    | "list_hd" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_list_hd (
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    | "list_tl" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_list_tl (
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    | "list_rev" ->
        let arg_0 = List.nth argument_expressions 0 in
        List_rev (
            list_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )
    | function_name -> raise (UndefinedFunction function_name)

and stack_expression_of_typed_boolean_expression_with_type variable_infos = function
    | Typed_discrete_bool_expr (expr, discrete_type) ->
        let inner_type =
            match discrete_type with
            | Var_type_discrete_stack inner_type -> inner_type
            | inner_type -> raise (InternalError ("The expression type indicate that it should be converted to a stack expression, but a " ^ (DiscreteType.string_of_var_type_discrete inner_type) ^ " expression is found. Maybe something failed in type checking or conversion."))
        in

        stack_expression_of_typed_discrete_boolean_expression variable_infos inner_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")


and stack_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        stack_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a stack expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and stack_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (expr, _) ->
        stack_expression_of_typed_arithmetic_expression variable_infos discrete_type expr
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a stack expression, but a Boolean expression is found. Maybe something failed in type checking or conversion.")

and stack_expression_of_typed_arithmetic_expression variable_infos discrete_type = function
	| Typed_term (term, _) ->
        stack_expression_of_typed_term variable_infos discrete_type term
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a stack expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and stack_expression_of_typed_term variable_infos discrete_type = function
	| Typed_factor (factor, _) ->
        stack_expression_of_typed_factor variable_infos discrete_type factor
    | _ ->
        raise (InternalError "The expression type indicate that it should be converted to a stack expression, but an arithmetic expression is found. Maybe something failed in type checking or conversion.")

and stack_expression_of_typed_factor variable_infos discrete_type = function
	| Typed_variable (variable_name, _) ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value ->
            raise (InternalError "")
        | Variable_kind discrete_index -> Stack_variable discrete_index
        )
    (*
    | Typed_list (expr_list, _) ->
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression variable_infos expr discrete_type) expr_list)
    *)
	| Typed_expr (expr, _) ->
        stack_expression_of_typed_arithmetic_expression variable_infos discrete_type expr

	| Typed_function_call (function_name, argument_expressions, _) ->
	    stack_expression_of_typed_function_call variable_infos discrete_type argument_expressions function_name

	| _ ->
        raise (InternalError "The expression type indicate that it should be converted to a list expression, but a non list expression is found. Maybe something failed in type checking or conversion.")

and stack_expression_of_typed_function_call variable_infos discrete_type argument_expressions = function
    | "stack_push" ->
        let arg_0 = List.nth argument_expressions 0 in
        let arg_1 = List.nth argument_expressions 1 in
        Stack_push (
            global_expression_of_typed_boolean_expression variable_infos arg_0 discrete_type,
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_1
        )

    | "stack_clear" ->
        let arg_0 = List.nth argument_expressions 0 in
        Stack_clear (
            stack_expression_of_typed_boolean_expression_with_type variable_infos arg_0
        )

    | function_name -> raise (UndefinedFunction function_name)

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

let rec variable_access_of_typed_variable_access variable_infos = function
    | Typed_variable_name variable_name ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in
        (match variable_kind with
        | Constant_kind value -> raise (InternalError "Unable to set a constant expression")
        | Variable_kind discrete_index -> Discrete_variable_index discrete_index
        )

    | Typed_variable_access (variable_access, index_expr, _) ->
        Discrete_variable_access (
            variable_access_of_typed_variable_access variable_infos variable_access,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

    | Typed_wildcard -> Discrete_wildcard


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
          let numconst_value = DiscreteValue.to_numconst_value value in
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
let linear_term_of_typed_update_arithmetic_expression variable_infos pdae =

    let index_of_variables = variable_infos.index_of_variables in
    let constants = variable_infos.constants in

	(* Create an array of coef *)
	let array_of_coef = Array.make (Hashtbl.length index_of_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in

	let rec update_coef_array_in_typed_update_arithmetic_expression mult_factor = function
		| Typed_plus (parsed_update_arithmetic_expression, parsed_update_term, _) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term *)
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;
		| Typed_minus (parsed_update_arithmetic_expression, parsed_update_term, _) ->
		(* Update coefficients in the arithmetic expression *)
		update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression;
		(* Update coefficients in the term: multiply by -1 for negation *)
		update_coef_array_in_parsed_update_term (NumConst.neg mult_factor) parsed_update_term;
		| Typed_term (parsed_update_term, _) ->
		update_coef_array_in_parsed_update_term mult_factor parsed_update_term;

	and update_coef_array_in_parsed_update_term mult_factor = function
		(* Multiplication is only allowed with a constant multiplier *)
		| Typed_mul (parsed_update_term, parsed_update_factor, _) ->

		(* Convert to abstract tree *)
		let converted_term = rational_arithmetic_expression_of_typed_term variable_infos parsed_update_term in
		(* Try to evaluate the term *)
		let numconst_valued_term = DiscreteExpressionEvaluator.try_reduce_rational_term converted_term in

		(* Update coefficients *)
		update_coef_array_in_parsed_update_factor (NumConst.mul numconst_valued_term mult_factor) parsed_update_factor

		| Typed_div (parsed_update_term, parsed_update_factor, _) ->

		(* Convert to abstract tree *)
		let converted_factor = rational_arithmetic_expression_of_typed_factor variable_infos parsed_update_factor in
		(* Try to evaluate the factor *)
		let numconst_valued_factor = DiscreteExpressionEvaluator.try_reduce_rational_factor converted_factor in

		(* Update coefficients *)
		update_coef_array_in_parsed_update_term (NumConst.div mult_factor numconst_valued_factor) parsed_update_term

		| Typed_factor (parsed_update_factor, _) ->
		update_coef_array_in_parsed_update_factor mult_factor parsed_update_factor

	and update_coef_array_in_parsed_update_factor mult_factor = function
		| Typed_variable (variable_name, _) ->
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
				let numconst_value = DiscreteValue.to_numconst_value value in
				(* Update the constant *)
				constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
				) else (
				raise (InvalidExpression ("Impossible to find the index of variable `" ^ variable_name ^ "` in function 'update_coef_array_in_parsed_update_factor' although this should have been checked before."))
				)
			)
		| Typed_constant (var_value, _) ->
            (* Update the constant *)
            let numconst_value = DiscreteValue.to_numconst_value var_value in
            constant := NumConst.add !constant (NumConst.mul mult_factor numconst_value)
		| Typed_unary_min (parsed_discrete_factor, _) ->
			update_coef_array_in_parsed_update_factor mult_factor parsed_discrete_factor
		| Typed_expr (parsed_update_arithmetic_expression, _) ->
            update_coef_array_in_typed_update_arithmetic_expression mult_factor parsed_update_arithmetic_expression
		| factor ->
            raise (InvalidExpression ("Use of `" ^ label_of_typed_factor_constructor factor ^ "` is forbidden in linear term, something failed before."))
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

end