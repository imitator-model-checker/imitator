open ParsingStructure
open ParsingStructureUtilities
open ImitatorUtilities
open OCamlUtilities
open Exceptions
open FunctionSig
open TypeConstraintResolver
open CustomModules
open DiscreteType

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
	    let str_node = l_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_list ^ r_del in
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

	| Typed_function_call (function_name, argument_expressions, _) as factor ->
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

(* Get var type of a variable given it's name *)
let get_type_of_variable_by_name variable_infos variable_name =
    if Hashtbl.mem variable_infos.index_of_variables variable_name then (
        (* Get type of variable *)
        let variable_index = Hashtbl.find variable_infos.index_of_variables variable_name in
        let variable_type = get_type_of_variable variable_infos variable_index in
        variable_type
    ) else if Hashtbl.mem variable_infos.constants variable_name then (
        (* Retrieve the value of the global constant *)
        let value = Hashtbl.find variable_infos.constants variable_name in
        (* Get type of constant *)
        DiscreteValue.var_type_of_value value
    ) else
        raise (InternalError ("Impossible to find the index of variable `" ^ variable_name ^ "` although this should have been checked before."))


(* Get discrete type of a variable given it's name *)
let get_discrete_type_of_variable_by_name variable_infos variable_name =
    let var_type = get_type_of_variable_by_name variable_infos variable_name in
    DiscreteType.discrete_type_of_var_type var_type



(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)

let rec type_check_global_expression variable_infos infer_type_opt = function
    | Parsed_global_expression expr ->
        let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in
        Typed_global_expr (typed_expr, discrete_type), discrete_type

and type_check_parsed_boolean_expression variable_infos infer_type_opt = function
	| Parsed_And (l_expr, r_expr) ->
        let l_typed_expr, l_type = type_check_parsed_boolean_expression variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type = type_check_parsed_boolean_expression variable_infos infer_type_opt r_expr in

        (* Check that left and right members are boolean *)
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

	| Parsed_Or (l_expr, r_expr) ->

        let l_typed_expr, l_type = type_check_parsed_boolean_expression variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type = type_check_parsed_boolean_expression variable_infos infer_type_opt r_expr in

        (* Check that left and right members are boolean *)
        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_Or (l_typed_expr, r_typed_expr), Var_type_discrete_bool
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
                "Expression `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos l_expr
                ^ ":"
                ^ DiscreteType.string_of_var_type_discrete l_type
                ^ "` is not compatible with expression `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos r_expr
                ^ ":"
                ^ DiscreteType.string_of_var_type_discrete r_type
                ^ "` in `"
                ^ string_of_parsed_discrete_boolean_expression variable_infos outer_expr
                ^ "`"
            ));

	| Parsed_expression_in (in_expr, lw_expr, up_expr) ->
	    let in_typed_expr, in_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt in_expr in
	    let lw_typed_expr, lw_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt lw_expr in
	    let up_typed_expr, up_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt up_expr in

        (* Check that expression are numbers *)
        let in_number_type, lw_number_type, up_number_type =
            match in_type, lw_type, up_type with
            | Var_type_discrete_number in_number_type, Var_type_discrete_number lw_number_type, Var_type_discrete_number up_number_type -> in_number_type, lw_number_type, up_number_type
            | _ -> raise (TypeError "")
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
            raise (TypeError "c")

	| Parsed_boolean_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in
	    Typed_bool_expr typed_expr, discrete_type

	| Parsed_Not expr ->
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression variable_infos infer_type_opt expr in

        (* Check that expression type is boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_not_expr typed_expr, Var_type_discrete_bool
	    | _ -> raise (TypeError "d")
        )

and type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt = function
	| Parsed_DAE_plus (expr, term) ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in

        (* Check that members are numbers and compatibles *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_plus (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | _ -> raise (TypeError "not compatible plus")
        )

	| Parsed_DAE_minus (expr, term) ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_arithmetic_expression variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in

        (* Check that members are numbers and compatibles *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_minus (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | l_type, r_type -> raise (TypeError (
            "not compatible minus: "
            ^ DiscreteType.string_of_var_type_discrete l_type
            ^ ", "
            ^ DiscreteType.string_of_var_type_discrete r_type
        ))
        )

	| Parsed_DAE_term term ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    Typed_term (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_term variable_infos infer_type_opt = function
    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)

    | Parsed_DT_div ((Parsed_DT_factor (Parsed_DF_constant lv) as term), (Parsed_DF_constant rv as factor)) ->

	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatibles *)
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
            let discrete_type = Var_type_discrete_number discrete_number_type in
            Typed_div (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type

        | _ -> raise (TypeError "")
        )

    | Parsed_DT_mul (term, factor) ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatibles *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_mul (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | _ -> raise (TypeError "not compatible mul")
        )

	| Parsed_DT_div (term, factor) ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_term variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that members are numbers and compatibles *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in

            (* If left unknown and not right convert left *)

            Typed_div (l_typed_expr, r_typed_expr, discrete_number_type), Var_type_discrete_number discrete_number_type
        | _ -> raise (TypeError "not compatible div")
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
            | Some infer_type, Var_type_discrete_number Var_type_discrete_unknown_number -> Var_type_discrete_number infer_type
            | _ -> discrete_type
        in

        Typed_variable (variable_name, infer_discrete_type), infer_discrete_type

	| Parsed_DF_constant value ->
        let discrete_type = DiscreteValue.discrete_type_of_value value in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some infer_type, Var_type_discrete_number Var_type_discrete_unknown_number -> Var_type_discrete_number infer_type
            | _ -> discrete_type
        in

        Typed_constant (value, infer_discrete_type), infer_discrete_type

	| Parsed_DF_array array_expr ->

	    let list_expr = Array.to_list array_expr in
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        (* Check that all elements types are compatibles *)
        if all_compatibles then (
            let inner_type = List.nth discrete_types 0 in
            let discrete_type = Var_type_discrete_array (inner_type, List.length list_expr) in
            Typed_array (Array.of_list typed_expressions, inner_type), discrete_type
        )
        else
            raise (TypeError "e")

	| Parsed_DF_list list_expr ->
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        (* Check that all elements types are compatibles *)
        if all_compatibles then (
            let inner_type = List.nth discrete_types 0 in
            let discrete_type = Var_type_discrete_list inner_type in
            Typed_list (typed_expressions, inner_type), discrete_type
        )
        else
            raise (TypeError "f")

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

	| Parsed_DF_unary_min factor ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_factor variable_infos infer_type_opt factor in

        (* Check that expression is a number *)
        (match discrete_type with
        | Var_type_discrete_number discrete_number_type -> Typed_unary_min (typed_expr, discrete_number_type), discrete_type
        | _ -> raise (TypeError "g")
        )

	| Parsed_function_call (name_factor, argument_expressions) as func ->
        (* Get function name *)
        let function_name = ParsingStructureUtilities.function_name_of_parsed_factor name_factor in

        (* Get arity of function *)
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

        (* ------- *)
        (* We doesn't infer arguments types because arguments types are not dependent of the context *)
        let type_checks = List.map (type_check_parsed_boolean_expression variable_infos None (* None: mean no inference for arguments *)) argument_expressions in
        let call_signature = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

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
        let signature_constraint_with_expressions = List.combine function_parameter_signature_constraint argument_expressions in

        let dependent_type_constraints = OCamlUtilities.rev_filter_map (fun (type_constraint, expr) ->
            match type_constraint with
            | Defined_type_constraint (Number_constraint (Defined_type_number_constraint Int_constraint (Int_name_constraint constraint_name))) ->
                if not (ParsingStructureUtilities.is_parsed_boolean_expression_constant variable_infos expr) then (
                    raise (TypeError (""));
                )
                else (
                    let value = ParsingStructureUtilities.try_reduce_parsed_boolean_expression variable_infos.constants expr in
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
            | Var_type_discrete_array (inner_type, _)
            | Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (TypeError "Trying to make an access to a non-array or a non-list variable")
        in
        Typed_variable_access (typed_variable_access, typed_index_expr_type, discrete_type), discrete_type

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
                ^ "` in property, is not a boolean expression: "
                ^ DiscreteType.string_of_var_type_discrete discrete_type
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
	| Parsed_state_predicate_OR (l_expr, r_expr) ->
	    let l_typed_expr, l_type = type_check_parsed_state_predicate variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_state_predicate variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_OR (l_typed_expr, r_typed_expr), Var_type_discrete_bool
        | _ -> raise (TypeError "")
	    )

	| Parsed_state_predicate_term term ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt term in
	    Typed_state_predicate_term (typed_expr, discrete_type), discrete_type

and type_check_parsed_state_predicate_term variable_infos infer_type_opt = function
	| Parsed_state_predicate_term_AND (l_expr, r_expr) ->
	    let l_typed_expr, l_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_state_predicate_term variable_infos infer_type_opt r_expr in

	    (match l_type, r_type with
	    | Var_type_discrete_bool, Var_type_discrete_bool ->
	        Typed_state_predicate_term_AND (l_typed_expr, r_typed_expr), Var_type_discrete_bool
        | _ -> raise (TypeError "")
	    )

	| Parsed_state_predicate_factor factor ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in
	    Typed_state_predicate_factor (typed_expr, discrete_type), discrete_type

and type_check_parsed_state_predicate_factor variable_infos infer_type_opt = function
	| Parsed_state_predicate_factor_NOT factor ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate_factor variable_infos infer_type_opt factor in

        (* Check that expression type is boolean *)
	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_state_predicate_factor_NOT typed_expr, discrete_type
	    | _ ->
	        raise (TypeError (
	            ""
	        ))
        )

	| Parsed_simple_predicate predicate ->
	    let typed_expr, discrete_type = type_check_parsed_simple_predicate variable_infos infer_type_opt predicate in
	    Typed_simple_predicate (typed_expr, discrete_type), discrete_type

	| Parsed_state_predicate predicate ->
	    let typed_expr, discrete_type = type_check_parsed_state_predicate variable_infos infer_type_opt predicate in
	    Typed_state_predicate (typed_expr, discrete_type), discrete_type






(* - -- - - - -- - - - -*)
(* - -- - - - -- - - - -*)
(* - -- - - - -- - - - -*)
(*
let infer_discrete_number_type discrete_number_type target_number_type =
    match discrete_number_type, target_number_type with
    (* If discrete type is unknown number, replace by target type *)
    | Var_type_discrete_unknown_number, _ -> target_number_type
    (* Else keep the current discrete type *)
    | _ -> discrete_number_type

let rec infer_discrete_type discrete_type target_type =
    match discrete_type, target_type with
    | Var_type_discrete_number discrete_number_type, Var_type_discrete_number target_number_type ->
        Var_type_discrete_number (infer_discrete_number_type discrete_number_type target_number_type)
    | Var_type_discrete_array (inner_type, length), Var_type_discrete_array (target_inner_type, _) ->
        Var_type_discrete_array (infer_discrete_type inner_type target_inner_type, length)
    | Var_type_discrete_list inner_type, Var_type_discrete_list target_inner_type ->
        Var_type_discrete_list (infer_discrete_type inner_type target_inner_type)
    | x, y when x = y -> x
    | _ -> raise (InternalError "Target type for inference should have the same structure as inferred type.")
*)
(*
(* Infer is a top-down operation on typed tree *)
(* It 'downstairs' the type of top node, to descendants (if the type of top node is stronger than descendants types) *)
let rec infer_typed_global_expression top_type = function
    | Typed_global_expr (expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_global_expr (
            infer_typed_boolean_expression discrete_type expr,
            discrete_type
        )

and infer_typed_boolean_expression top_type = function
	| Typed_And (l_expr, r_expr) ->
	    Typed_And (
	        infer_typed_boolean_expression top_type l_expr,
	        infer_typed_boolean_expression top_type r_expr
	    )

	| Typed_Or (l_expr, r_expr) ->
        Typed_Or (
            infer_typed_boolean_expression top_type l_expr,
            infer_typed_boolean_expression top_type r_expr
        )

	| Typed_discrete_bool_expr (expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
	    Typed_discrete_bool_expr (
	        infer_typed_discrete_boolean_expression greater_type expr,
	        greater_type
	    )

and infer_typed_discrete_boolean_expression top_type = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_arithmetic_expr (
            infer_typed_discrete_arithmetic_expression greater_type expr,
            greater_type
        )

	| Typed_comparison (l_expr, relop, r_expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_comparison (
            infer_typed_discrete_boolean_expression greater_type l_expr,
            relop,
            infer_typed_discrete_boolean_expression greater_type r_expr,
            greater_type
        )

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in
        Typed_comparison_in (
            infer_typed_discrete_arithmetic_expression greater_type in_expr,
            infer_typed_discrete_arithmetic_expression greater_type lw_expr,
            infer_typed_discrete_arithmetic_expression greater_type up_expr,
            greater_number_type
        )

	| Typed_bool_expr expr ->
	    Typed_bool_expr (
	        infer_typed_boolean_expression top_type expr
        )
	| Typed_not_expr expr ->
	    Typed_not_expr (
	        infer_typed_boolean_expression top_type expr
        )

and infer_typed_discrete_arithmetic_expression top_type = function
	| Typed_plus (expr, term, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in

        Typed_plus (
            infer_typed_discrete_arithmetic_expression greater_type expr,
            infer_typed_discrete_term greater_type term,
            greater_number_type
        )

	| Typed_minus (expr, term, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in

        Typed_minus (
            infer_typed_discrete_arithmetic_expression greater_type expr,
            infer_typed_discrete_term greater_type term,
            greater_number_type
        )

	| Typed_term (term, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
	    Typed_term (
	        infer_typed_discrete_term greater_type term,
            greater_type
	    )

and infer_typed_discrete_term top_type = function
	| Typed_mul (term, factor, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in

        Typed_mul (
            infer_typed_discrete_term greater_type term,
            infer_typed_discrete_factor greater_type factor,
            greater_number_type
        )

	| Typed_div (term, factor, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in

        Typed_div (
            infer_typed_discrete_term greater_type term,
            infer_typed_discrete_factor greater_type factor,
            greater_number_type
        )

	| Typed_factor (factor, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
	    Typed_factor (
	        infer_typed_discrete_factor greater_type factor,
            greater_type
	    )

and infer_typed_discrete_factor top_type = function
	| Typed_variable (variable_name, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_variable (variable_name, greater_type)

	| Typed_constant (value, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_constant (value, greater_type)

	| Typed_array (array_expr, inner_type) ->
	    let inner_top_type =
            match top_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_inner_type = stronger_discrete_type_of inner_type inner_top_type in
        Typed_array (
            Array.map (infer_typed_boolean_expression greater_inner_type) array_expr,
            greater_inner_type
        )

	| Typed_list (list_expr, inner_type) ->
	    let inner_top_type =
            match top_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_inner_type = stronger_discrete_type_of inner_type inner_top_type in
        Typed_list (
            List.map (infer_typed_boolean_expression greater_inner_type) list_expr,
            greater_inner_type
        )

	| Typed_expr (expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
	    Typed_expr (
	        infer_typed_discrete_arithmetic_expression greater_type expr,
            greater_type
	    )

	| Typed_unary_min (factor, discrete_number_type) ->
	    let top_discrete_number_type =
            match top_type with
            | Var_type_discrete_number top_discrete_number_type -> top_discrete_number_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_number_type = stronger_discrete_number_type_of discrete_number_type top_discrete_number_type in
        let greater_type = Var_type_discrete_number greater_number_type in

        Typed_unary_min (
            infer_typed_discrete_factor greater_type factor,
            greater_number_type
        )

    | Typed_access (factor, index_expr, discrete_type, inner_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
	    let inner_top_type =
            match top_type with
            | Var_type_discrete_array (inner_type, _)
            | Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (InternalError "Infer type should have the same structure as inferred node type.")
        in
        let greater_inner_type = stronger_discrete_type_of inner_type inner_top_type in

        Typed_access (
            infer_typed_discrete_factor greater_type factor,
            infer_typed_discrete_arithmetic_expression (Var_type_discrete_number Var_type_discrete_unknown_number) index_expr,
            greater_type,
            greater_inner_type
        )

	| Typed_function_call (function_name, arg_expressions, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in

	    Typed_function_call (
	        function_name,
	        List.map (infer_typed_boolean_expression (Var_type_discrete_number Var_type_discrete_unknown_number)) arg_expressions,
	        greater_type
	    )




let infer_typed_loc_predicate = function
	| Typed_loc_predicate_EQ _
	| Typed_loc_predicate_NEQ _ as loc_predicate -> loc_predicate

let rec infer_typed_simple_predicate top_type = function
	| Typed_discrete_boolean_expression (expr, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_discrete_boolean_expression (
            infer_typed_discrete_boolean_expression greater_type expr,
            greater_type
        )

	| Typed_loc_predicate _ as loc_predicate -> loc_predicate
	| Typed_state_predicate_true -> Typed_state_predicate_true
	| Typed_state_predicate_false -> Typed_state_predicate_false
	| Typed_state_predicate_accepting -> Typed_state_predicate_accepting

and infer_typed_state_predicate top_type = function
	| Typed_state_predicate_OR (l_expr, r_expr) ->
	    Typed_state_predicate_OR (
	        infer_typed_state_predicate top_type l_expr,
	        infer_typed_state_predicate top_type r_expr
	    )

	| Typed_state_predicate_term (term, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_state_predicate_term (
            infer_typed_state_predicate_term greater_type term,
            greater_type
        )

and infer_typed_state_predicate_term top_type = function
	| Typed_state_predicate_term_AND (l_expr, r_expr) ->
	    Typed_state_predicate_term_AND (
	        infer_typed_state_predicate_term top_type l_expr,
	        infer_typed_state_predicate_term top_type r_expr
	    )

	| Typed_state_predicate_factor (factor, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_state_predicate_factor (
            infer_typed_state_predicate_factor greater_type factor,
            greater_type
        )

and infer_typed_state_predicate_factor top_type = function
	| Typed_state_predicate_factor_NOT factor ->
        Typed_state_predicate_factor_NOT (
            infer_typed_state_predicate_factor top_type factor
        )

	| Typed_simple_predicate (predicate, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_simple_predicate (
            infer_typed_simple_predicate greater_type predicate,
            greater_type
        )

	| Typed_state_predicate (predicate, discrete_type) ->
        let greater_type = stronger_discrete_type_of discrete_type top_type in
        Typed_state_predicate (
            infer_typed_state_predicate greater_type predicate,
            greater_type
        )


(* - -- - - - -- - - - -*)
(* - -- - - - -- - - - -*)
(* - -- - - - -- - - - -*)
*)



(* Check that an expression assigned to a variable is of the same type *)
(* If not, raise a TypeError exception with an error message *)
let check_type_assignment3 variable_infos variable_name variable_type expr =

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
    let variable_number_type_opt = DiscreteType.extract_number_of_discrete_type variable_type in
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
let check_discrete_init3 variable_infos variable_name expr =
    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer init expression: " ^ string_of_parsed_global_expression variable_infos expr);

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
    check_type_assignment3 variable_infos variable_name variable_type expr


let check_constant_expression variable_infos (name, expr, var_type) =
    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer constant expression: " ^ string_of_parsed_global_expression variable_infos expr);

    (* Get variable type *)
    let discrete_type = DiscreteType.discrete_type_of_var_type var_type in
    (* Check expression / variable type consistency *)
    check_type_assignment3 variable_infos name discrete_type expr


(* Type non-linear constraint *)
(* return a tuple containing the non-linear constraint uniformly typed and the resolved type of the expression *)
let check_nonlinear_constraint variable_infos nonlinear_constraint =

    let typed_nonlinear_constraint, discrete_type = type_check_parsed_discrete_boolean_expression variable_infos None nonlinear_constraint in

    ImitatorUtilities.print_message Verbose_high "-------------------------";
    ImitatorUtilities.print_message Verbose_high "Typed tree of nonlinear constraint: ";
    ImitatorUtilities.print_message Verbose_high "-------------------------";
    ImitatorUtilities.print_message Verbose_high (string_of_typed_discrete_boolean_expression variable_infos typed_nonlinear_constraint);
    ImitatorUtilities.print_message Verbose_high "-------------------------";

    (* Check that non-linear constraint is a Boolean expression *)
    match discrete_type with
    | DiscreteType.Var_type_discrete_bool -> typed_nonlinear_constraint
    | _ ->
        raise (TypeError (
            "Guard or invariant expression `"
            ^ string_of_parsed_nonlinear_constraint variable_infos nonlinear_constraint
            ^ "` is not a boolean expression"
        ))

(* Type check guard / invariant *)
(* return a tuple containing the expression uniformly typed and the resolved type of the expression *)
let check_guard variable_infos =
    List.map (check_nonlinear_constraint variable_infos)





(* Type check an update *)
let check_update variable_infos variable_access expr =

    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer update expression: " ^ string_of_parsed_global_expression variable_infos expr);

    (* Get assigned variable name *)
    let variable_name = ParsingStructureUtilities.variable_name_of_variable_access variable_access in
    (* Get assigned variable type *)
    let var_type = get_type_of_variable_by_name variable_infos variable_name in

    (* Eventually get a number type to infer *)
    let variable_number_type_opt = DiscreteType.extract_number_of_type var_type in
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
            ^ "` is not a boolean expression."
        ))


