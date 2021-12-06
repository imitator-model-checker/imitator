open ParsingStructure
open ParsingStructureUtilities
open ImitatorUtilities
open OCamlUtilities
open Exceptions
open FunctionSig
open TypeConstraintResolver
open CustomModules
open DiscreteType


type typed_global_expression =
    | Typed_global_expr of typed_boolean_expression * var_type_discrete

and typed_boolean_expression =
	| Typed_And of typed_boolean_expression * typed_boolean_expression
	| Typed_Or of typed_boolean_expression * typed_boolean_expression
	| Typed_discrete_bool_expr of typed_discrete_boolean_expression * var_type_discrete

and typed_discrete_boolean_expression =
    | Typed_arithmetic_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_comparison of typed_discrete_boolean_expression * parsed_relop * typed_discrete_boolean_expression * var_type_discrete * var_type_discrete
	| Typed_comparison_in of typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_bool_expr of typed_boolean_expression * var_type_discrete
	| Typed_not_expr of typed_boolean_expression

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
	| Typed_array of typed_boolean_expression array * var_type_discrete
	| Typed_list of typed_boolean_expression list * var_type_discrete
	| Typed_expr of typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_unary_min of typed_discrete_factor
    | Typed_access of typed_discrete_factor * typed_discrete_arithmetic_expression * var_type_discrete
	| Typed_function_call of string * typed_boolean_expression list * var_type_discrete

let type_of_typed_global_expression = function
    | Typed_global_expr (_, discrete_type) -> discrete_type

let type_of_typed_boolean_expression = function
	| Typed_And _
	| Typed_Or _ -> Var_type_discrete_bool
	| Typed_discrete_bool_expr (_, discrete_type) -> discrete_type

let type_of_typed_discrete_boolean_expression = function
    | Typed_bool_expr (_, discrete_type)
    | Typed_arithmetic_expr (_, discrete_type) -> discrete_type
	| Typed_comparison _
	| Typed_comparison_in _
	| Typed_not_expr _ -> Var_type_discrete_bool

let type_of_typed_discrete_arithmetic_expression = function
	| Typed_term (_, discrete_type) -> discrete_type
	| Typed_plus (_, _, discrete_number_type)
	| Typed_minus (_, _, discrete_number_type) -> Var_type_discrete_number discrete_number_type


let type_of_typed_discrete_term = function
	| Typed_mul (_, _, discrete_number_type)
	| Typed_div (_, _, discrete_number_type) -> Var_type_discrete_number discrete_number_type
	| Typed_factor (_, discrete_type) -> discrete_type

let type_of_typed_discrete_factor = function
	| Typed_unary_min _ -> Var_type_discrete_number Var_type_discrete_int
	| Typed_variable (_, discrete_type)
	| Typed_constant (_, discrete_type)
	| Typed_array (_, discrete_type)
	| Typed_list (_, discrete_type)
	| Typed_expr (_, discrete_type)
    | Typed_access (_, _, discrete_type)
	| Typed_function_call (_, _, discrete_type) -> discrete_type


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


let rec type_check_global_expression3 variable_infos = function
    | Parsed_global_expression expr ->
        let typed_expr = type_check_parsed_boolean_expression3 variable_infos expr in
        let discrete_type = type_of_typed_boolean_expression typed_expr in
        Typed_global_expr (typed_expr, discrete_type)

and type_check_parsed_boolean_expression3 variable_infos = function
	| Parsed_And (l_expr, r_expr) ->
        let l_typed_expr = type_check_parsed_boolean_expression3 variable_infos l_expr in
        let r_typed_expr = type_check_parsed_boolean_expression3 variable_infos r_expr in
        let l_type = type_of_typed_boolean_expression l_typed_expr in
        let r_type = type_of_typed_boolean_expression r_typed_expr in

        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_And (l_typed_expr, r_typed_expr)
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

        let l_typed_expr = type_check_parsed_boolean_expression3 variable_infos l_expr in
        let r_typed_expr = type_check_parsed_boolean_expression3 variable_infos r_expr in
        let l_type = type_of_typed_boolean_expression l_typed_expr in
        let r_type = type_of_typed_boolean_expression r_typed_expr in

        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_Or (l_typed_expr, r_typed_expr)
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
	    let typed_expr = type_check_parsed_discrete_boolean_expression3 variable_infos expr in
	    let discrete_type = type_of_typed_discrete_boolean_expression typed_expr in
	    Typed_discrete_bool_expr (typed_expr, discrete_type)

and type_check_parsed_discrete_boolean_expression3 variable_infos = function
    | Parsed_arithmetic_expression expr ->
	    let typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos expr in
	    let discrete_type = type_of_typed_discrete_arithmetic_expression typed_expr in
	    Typed_arithmetic_expr (typed_expr, discrete_type)

	| Parsed_expression (l_expr, relop, r_expr) ->

	    let l_typed_expr = type_check_parsed_discrete_boolean_expression3 variable_infos l_expr in
	    let r_typed_expr = type_check_parsed_discrete_boolean_expression3 variable_infos r_expr in
        let l_type = type_of_typed_discrete_boolean_expression l_typed_expr in
        let r_type = type_of_typed_discrete_boolean_expression r_typed_expr in

        if is_discrete_type_compatibles l_type r_type then
            Typed_comparison (l_typed_expr, relop, r_typed_expr, greater_defined l_type r_type, Var_type_discrete_bool)
        else
            raise (TypeError "b");

	| Parsed_expression_in (in_expr, lw_expr, up_expr) ->
	    let in_typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos in_expr in
	    let lw_typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos lw_expr in
	    let up_typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos up_expr in
        let in_type = type_of_typed_discrete_arithmetic_expression in_typed_expr in
        let lw_type = type_of_typed_discrete_arithmetic_expression lw_typed_expr in
        let up_type = type_of_typed_discrete_arithmetic_expression up_typed_expr in

        let in_lw_compatible = is_discrete_type_compatibles in_type lw_type in
        let in_up_compatible = is_discrete_type_compatibles in_type up_type in
        let lw_up_compatible = is_discrete_type_compatibles lw_type up_type in

        let inner_type = greater_defined (greater_defined in_type lw_type) up_type in

        if in_lw_compatible && in_up_compatible && lw_up_compatible then (
            Typed_comparison_in (in_typed_expr, lw_typed_expr, up_typed_expr, inner_type)
        )
        else
            raise (TypeError "c")

	| Parsed_boolean_expression expr ->
	    let typed_expr = type_check_parsed_boolean_expression3 variable_infos expr in
	    let discrete_type = type_of_typed_boolean_expression typed_expr in
	    Typed_bool_expr (typed_expr, discrete_type)

	| Parsed_Not expr ->
	    let typed_expr = type_check_parsed_boolean_expression3 variable_infos expr in
	    let discrete_type = type_of_typed_boolean_expression typed_expr in

	    (match discrete_type with
	    | Var_type_discrete_bool -> Typed_not_expr typed_expr
	    | _ -> raise (TypeError "d")
        )

and type_check_parsed_discrete_arithmetic_expression3 variable_infos = function
	| Parsed_DAE_plus (expr, term) ->
	    let l_typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos expr in
	    let r_typed_expr = type_check_parsed_discrete_term3 variable_infos term in
        let l_type = type_of_typed_discrete_arithmetic_expression l_typed_expr in
        let r_type = type_of_typed_discrete_term r_typed_expr in

        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            Typed_plus (l_typed_expr, r_typed_expr, greater_number_defined l_number_type r_number_type)
        | _ -> raise (TypeError "not compatible plus")
        )

	| Parsed_DAE_minus (expr, term) ->
	    let l_typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos expr in
	    let r_typed_expr = type_check_parsed_discrete_term3 variable_infos term in
        let l_type = type_of_typed_discrete_arithmetic_expression l_typed_expr in
        let r_type = type_of_typed_discrete_term r_typed_expr in

        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            Typed_minus (l_typed_expr, r_typed_expr, greater_number_defined l_number_type r_number_type)
        | _ -> raise (TypeError "not compatible minus")
        )

	| Parsed_DAE_term term ->
	    let typed_expr = type_check_parsed_discrete_term3 variable_infos term in
	    let discrete_type = type_of_typed_discrete_term typed_expr in
	    Typed_term (typed_expr, discrete_type)

and type_check_parsed_discrete_term3 variable_infos = function
    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)

    | Parsed_DT_div ((Parsed_DT_factor (Parsed_DF_constant lv) as term), (Parsed_DF_constant rv as factor)) ->

	    let l_typed_expr = type_check_parsed_discrete_term3 variable_infos term in
	    let r_typed_expr = type_check_parsed_discrete_factor3 variable_infos factor in
        let l_type = type_of_typed_discrete_term l_typed_expr in
        let r_type = type_of_typed_discrete_factor r_typed_expr in

        (* TODO benjamin IMPORTANT CHECK TYPES *)

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
        Typed_div (l_typed_expr, r_typed_expr, discrete_number_type)

    | Parsed_DT_mul (term, factor) ->
	    let l_typed_expr = type_check_parsed_discrete_term3 variable_infos term in
	    let r_typed_expr = type_check_parsed_discrete_factor3 variable_infos factor in
        let l_type = type_of_typed_discrete_term l_typed_expr in
        let r_type = type_of_typed_discrete_factor r_typed_expr in

        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            Typed_mul (l_typed_expr, r_typed_expr, greater_number_defined l_number_type r_number_type)
        | _ -> raise (TypeError "not compatible mul")
        )

	| Parsed_DT_div (term, factor) ->
	    let l_typed_expr = type_check_parsed_discrete_term3 variable_infos term in
	    let r_typed_expr = type_check_parsed_discrete_factor3 variable_infos factor in
        let l_type = type_of_typed_discrete_term l_typed_expr in
        let r_type = type_of_typed_discrete_factor r_typed_expr in



        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            Typed_div (l_typed_expr, r_typed_expr, greater_number_defined l_number_type r_number_type)
        | _ -> raise (TypeError "not compatible div")
        )

	| Parsed_DT_factor factor ->
	    let typed_expr = type_check_parsed_discrete_factor3 variable_infos factor in
	    let discrete_type = type_of_typed_discrete_factor typed_expr in
	    Typed_factor (typed_expr, discrete_type)

and type_check_parsed_discrete_factor3 variable_infos = function
	| Parsed_DF_variable variable_name ->
        let discrete_type = get_discrete_type_of_variable_by_name variable_infos variable_name in
        Typed_variable (variable_name, discrete_type)

	| Parsed_DF_constant value ->
        let discrete_type = DiscreteValue.discrete_type_of_value value in
        Typed_constant (value, discrete_type)

	| Parsed_DF_array array_expr ->
	    let type_check_parsed_boolean_expression_apply = type_check_parsed_boolean_expression3 variable_infos in
	    let list_expr = Array.to_list array_expr in
        let typed_expressions = List.map type_check_parsed_boolean_expression_apply list_expr in

        let discrete_types = List.map type_of_typed_boolean_expression typed_expressions in
        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        if all_compatibles then (
            let inner_type = List.nth discrete_types 0 in
            let discrete_type = Var_type_discrete_array (inner_type, List.length list_expr) in
            Typed_array (Array.of_list typed_expressions, discrete_type)
        )
        else
            raise (TypeError "e")

	| Parsed_DF_list list_expr ->
	    let type_check_parsed_boolean_expression_apply = type_check_parsed_boolean_expression3 variable_infos in
        let typed_expressions = List.map type_check_parsed_boolean_expression_apply list_expr in

        let discrete_types = List.map type_of_typed_boolean_expression typed_expressions in
        let all_compatibles = OCamlUtilities.for_all_in_arrangement (fun a b -> is_discrete_type_compatibles a b) discrete_types in

        if all_compatibles then (
            let inner_type = List.nth discrete_types 0 in
            let discrete_type = Var_type_discrete_list inner_type in
            Typed_list (typed_expressions, discrete_type)
        )
        else
            raise (TypeError "f")

    | Parsed_DF_access (factor, index_expr) ->
        let typed_factor = type_check_parsed_discrete_factor3 variable_infos factor in
        let typed_index = type_check_parsed_discrete_arithmetic_expression3 variable_infos index_expr in

        let factor_type = type_of_typed_discrete_factor typed_factor in
        let index_type = type_of_typed_discrete_arithmetic_expression typed_index in

        if not (is_discrete_type_number_type index_type) || is_discrete_type_rational_type index_type then
            raise (TypeError "Index cannot be another type than int.");

        (match factor_type with
        | Var_type_discrete_array (inner_type, _)
        | Var_type_discrete_list inner_type -> Typed_access (typed_factor, typed_index, factor_type)
        | _ -> raise (TypeError "Cannot make an access to another type than array or list.")
        )

	| Parsed_DF_expression expr ->
	    let typed_expr = type_check_parsed_discrete_arithmetic_expression3 variable_infos expr in
	    let discrete_type = type_of_typed_discrete_arithmetic_expression typed_expr in
	    Typed_expr (typed_expr, discrete_type)

	| Parsed_DF_unary_min factor ->
	    let typed_expr = type_check_parsed_discrete_factor3 variable_infos factor in
	    let discrete_type = type_of_typed_discrete_factor typed_expr in

	    if is_discrete_type_number_type discrete_type then
	        Typed_unary_min typed_expr
        else
            raise (TypeError "g")

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
        (* Get inferred types of arguments as a signature *)
        let type_check_parsed_boolean_expression_apply = type_check_parsed_boolean_expression3 variable_infos in

        let typed_expressions = List.map type_check_parsed_boolean_expression_apply argument_expressions in
        let call_signature = List.map (fun typed_expr -> type_of_typed_boolean_expression typed_expr) typed_expressions in

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

        Typed_function_call (function_name, typed_expressions, return_type)


let string_format_typed_node str_node discrete_type =
    "{" ^ str_node ^ ":" ^ string_of_var_type_discrete discrete_type ^ "}"

let rec convert_typed_global_expression target_type = function
    | Typed_global_expr (expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
        Typed_global_expr (
            convert_typed_boolean_expression target_type expr,
            target_type
        )

and convert_typed_boolean_expression target_type = function
    (* convert point *)
	| Typed_And (l_expr, r_expr) ->
	    Typed_And (
	        convert_typed_boolean_expression target_type l_expr,
	        convert_typed_boolean_expression target_type r_expr
	    )

	| Typed_Or (l_expr, r_expr) ->
	    Typed_Or (
	        convert_typed_boolean_expression target_type l_expr,
	        convert_typed_boolean_expression target_type r_expr
	    )

	| Typed_discrete_bool_expr (expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_discrete_bool_expr (
	        convert_typed_discrete_boolean_expression target_type expr,
	        target_type
	    )

and convert_typed_discrete_boolean_expression target_type = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
        Typed_arithmetic_expr (
            convert_typed_discrete_arithmetic_expression target_type expr,
            target_type
        )

	| Typed_comparison (l_expr, relop, r_expr, inner_type, discrete_type) ->
	    (* Convert point *)
        let target_type = default_type_if_needed (greater_defined inner_type target_type) in
	    Typed_comparison (
	        convert_typed_discrete_boolean_expression target_type l_expr,
	        relop,
	        convert_typed_discrete_boolean_expression target_type r_expr,
	        target_type,
	        discrete_type
	    )

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_type) ->
	    (* Convert point *)
        let target_type = default_type_if_needed (greater_defined discrete_type target_type) in
	    Typed_comparison_in (
	        convert_typed_discrete_arithmetic_expression target_type in_expr,
	        convert_typed_discrete_arithmetic_expression target_type lw_expr,
	        convert_typed_discrete_arithmetic_expression target_type up_expr,
	        target_type
	    )

	| Typed_bool_expr (expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_bool_expr (
	        convert_typed_boolean_expression target_type expr,
	        target_type
	    )

	| Typed_not_expr expr ->
	    Typed_not_expr (
	        convert_typed_boolean_expression target_type expr
	    )

and convert_typed_discrete_arithmetic_expression target_type = function
	| Typed_plus (l_expr, r_expr, discrete_type) ->
        let target_number_type =
	        match target_type with
	        | Var_type_discrete_number t -> greater_number_defined discrete_type t
	        | _ -> raise (InternalError "")
        in
        let target_type = Var_type_discrete_number target_number_type in
	    Typed_plus (
	        convert_typed_discrete_arithmetic_expression target_type l_expr,
	        convert_typed_discrete_term target_type r_expr,
            target_number_type
	    )

	| Typed_minus (l_expr, r_expr, discrete_type) ->
        let target_number_type =
	        match target_type with
	        | Var_type_discrete_number t -> greater_number_defined discrete_type t
	        | _ -> raise (InternalError "")
        in
        let target_type = Var_type_discrete_number target_number_type in
	    Typed_minus (
	        convert_typed_discrete_arithmetic_expression target_type l_expr,
	        convert_typed_discrete_term target_type r_expr,
            target_number_type
	    )

	| Typed_term (term, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_term (
	        convert_typed_discrete_term target_type term,
	        target_type
	    )

and convert_typed_discrete_term target_type = function
	| Typed_mul (l_expr, r_expr, discrete_type) ->
        let target_number_type =
	        match target_type with
	        | Var_type_discrete_number t -> greater_number_defined discrete_type t
	        | _ -> raise (InternalError "")
        in
        let target_type = Var_type_discrete_number target_number_type in
	    Typed_mul (
	        convert_typed_discrete_term target_type l_expr,
            convert_typed_discrete_factor target_type r_expr,
            target_number_type
	    )

	| Typed_div (l_expr, r_expr, discrete_type) ->
        let target_number_type =
	        match target_type with
	        | Var_type_discrete_number t -> greater_number_defined discrete_type t
	        | _ -> raise (InternalError "")
        in
        let target_type = Var_type_discrete_number target_number_type in
	    Typed_div (
	        convert_typed_discrete_term target_type l_expr,
            convert_typed_discrete_factor target_type r_expr,
	        target_number_type
	    )

	| Typed_factor (factor, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_factor (
            convert_typed_discrete_factor target_type factor,
            target_type
	    )


and convert_typed_discrete_factor target_type = function
	| Typed_variable (variable_name, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_variable (
	        variable_name,
	        target_type
	    )

	| Typed_constant (value, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_constant (
	        value,
	        target_type
	    )

	| Typed_array (array_expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in

        let target_inner_type =
            match target_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (InternalError "")
        in

	    Typed_array (
	        Array.map (convert_typed_boolean_expression target_inner_type) array_expr,
	        target_type
	    )

	| Typed_list (list_expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in

        let target_inner_type =
            match target_type with
            | Var_type_discrete_list inner_type -> inner_type
            | _ -> raise (InternalError "")
        in

        Typed_list (
	        List.map (convert_typed_boolean_expression target_inner_type) list_expr,
	        target_type
        )

	| Typed_expr (expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_expr (
            convert_typed_discrete_arithmetic_expression target_type expr,
	        target_type
	    )

	| Typed_unary_min factor ->
	    Typed_unary_min (
            convert_typed_discrete_factor target_type factor
	    )

    | Typed_access (factor, expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
        Typed_access (
            convert_typed_discrete_factor target_type factor,
            convert_typed_discrete_arithmetic_expression target_type expr,
	        target_type
        )

	| Typed_function_call (function_name, list_expr, discrete_type) ->
        let target_type = greater_defined discrete_type target_type in
	    Typed_function_call (
            function_name,
	        List.map (convert_typed_boolean_expression target_type) list_expr,
	        target_type
	    )


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
    | Typed_arithmetic_expr (expr, _) ->
        string_of_typed_discrete_arithmetic_expression variable_infos expr

	| Typed_comparison (l_expr, relop, r_expr, inner_type, discrete_type) ->
	    let str_node =
            string_of_typed_discrete_boolean_expression variable_infos l_expr
            ^ " ~ "
            ^ string_of_typed_discrete_boolean_expression variable_infos r_expr
        in
        string_format_typed_node str_node discrete_type

	| Typed_comparison_in (in_expr, lw_expr, up_expr, discrete_type) ->
	    let str_node =
            string_of_typed_discrete_arithmetic_expression variable_infos in_expr
            ^ Constants.default_string.in_operator
            ^ string_of_typed_discrete_arithmetic_expression variable_infos lw_expr
            ^ string_of_typed_discrete_arithmetic_expression variable_infos up_expr
        in
        string_format_typed_node str_node discrete_type

	| Typed_bool_expr (expr, _) ->
        string_of_typed_boolean_expression variable_infos expr

	| Typed_not_expr expr ->
	    Constants.default_string.not_operator
	    ^ "("
        ^ string_of_typed_boolean_expression variable_infos expr
        ^ ")"

and string_of_typed_discrete_arithmetic_expression variable_infos = function
	| Typed_plus (l_expr, r_expr, _) ->
	        string_of_typed_discrete_arithmetic_expression variable_infos l_expr
	        ^ Constants.default_arithmetic_string.plus_string
	        ^ string_of_typed_discrete_term variable_infos r_expr

	| Typed_minus (l_expr, r_expr, _) ->
	        string_of_typed_discrete_arithmetic_expression variable_infos l_expr
	        ^ Constants.default_arithmetic_string.minus_string
	        ^ string_of_typed_discrete_term variable_infos r_expr

	| Typed_term (term, _) ->
	        string_of_typed_discrete_term variable_infos term

and string_of_typed_discrete_term variable_infos = function
	| Typed_mul (l_expr, r_expr, _) ->
	        string_of_typed_discrete_term variable_infos l_expr
	        ^ Constants.default_arithmetic_string.mul_string
            ^ string_of_typed_discrete_factor variable_infos r_expr

	| Typed_div (l_expr, r_expr, _) ->
	        string_of_typed_discrete_term variable_infos l_expr
	        ^ Constants.default_arithmetic_string.div_string
            ^ string_of_typed_discrete_factor variable_infos r_expr

	| Typed_factor (factor, _) ->
            string_of_typed_discrete_factor variable_infos factor

and string_of_typed_discrete_factor variable_infos = function
	| Typed_variable (variable_name, discrete_type) ->
	    variable_name ^ ":" ^ DiscreteType.string_of_var_type_discrete discrete_type
	| Typed_constant (value, discrete_type) ->
	    DiscreteValue.string_of_value value ^ ":" ^ DiscreteType.string_of_var_type_discrete discrete_type
	| Typed_array (array_expr, discrete_type) ->
	    let l_del, r_del = Constants.default_array_string.array_literal_delimiter in
	    let str_array = Array.map (string_of_typed_boolean_expression variable_infos) array_expr in
	    let str_node = l_del ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_array ^ r_del in
	    string_format_typed_node str_node discrete_type

	| Typed_list (list_expr, discrete_type) ->
	    let l_del, r_del = Constants.default_array_string.array_literal_delimiter in
	    let str_list = List.map (string_of_typed_boolean_expression variable_infos) list_expr in
	    let str_node = l_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_list ^ r_del in
	    string_format_typed_node str_node discrete_type

	| Typed_expr (expr, discrete_type) ->
	    let str_node = "(" ^ string_of_typed_discrete_arithmetic_expression variable_infos expr ^ ")" in
        string_format_typed_node str_node discrete_type



    (* TODO benjamin IMPORTANT fill below *)
    | Typed_access (_, _, discrete_type) -> ""
	| Typed_function_call (_, _, discrete_type) as factor -> ""

	| Typed_unary_min factor ->
	    Constants.default_arithmetic_string.unary_min_string
        ^ string_of_typed_discrete_factor variable_infos factor









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


    (* Resolve typed expression *)
    let typed_expr = type_check_global_expression3 variable_infos expr in
    let expr_var_type_discrete = type_of_typed_global_expression typed_expr in

    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteType.is_discrete_type_compatibles variable_type expr_var_type_discrete in

    (* Not consistent ? raise a type error with appropriate message *)
    if not (is_consistent) then (
        raise (TypeError (get_error_message variable_name variable_type expr_var_type_discrete expr))
    )
    else (

        if DiscreteType.is_discrete_type_holding_unknown_number_type expr_var_type_discrete then
            (* Convert all unknown numbers to variable type *)
            convert_typed_global_expression variable_type typed_expr
        else
            (* Convert all unknown numbers to rational *)
            convert_typed_global_expression (Var_type_discrete_number Var_type_discrete_rational) typed_expr
    )

(* Check that a discrete variable initialization is well typed *)
let check_discrete_init3 variable_infos variable_name expr =

    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer init expression: " ^ string_of_parsed_global_expression variable_infos expr);

    (* Get the variable index *)
    let discrete_index = Hashtbl.find variable_infos.index_of_variables variable_name in
    (* Get variable type *)
    let var_type = get_type_of_variable variable_infos discrete_index in
    let var_discrete_type = DiscreteType.discrete_type_of_var_type var_type in

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

    let discrete_type = DiscreteType.discrete_type_of_var_type var_type in

    let typed_expr = check_type_assignment3 variable_infos name discrete_type expr in

    typed_expr

    (*

    let target_var_type = DiscreteType.discrete_type_of_var_type var_type in

    (* Infer expression type *)
    let typed_expr = type_of_typed_boolean_expression variable_infos expr in

    let discrete_type = type_of_typed_boolean_expression typed_expr in

    (* Check compatibility *)
    let is_compatible = DiscreteType.is_discrete_type_compatibles target_var_type discrete_type in

    (* If not compatibles, display an error message *)
    if not is_compatible then (
        print_error ("Constant "
            ^ name
            ^ " of type "
            ^ (DiscreteType.string_of_var_type_discrete target_var_type)
            ^ " is not compatible with expression `"
            ^ (ParsingStructureUtilities.string_of_parsed_global_expression variable_infos expr)
            ^ "` of type "
            ^ (DiscreteType.string_of_var_type_discrete discrete_type)
        );
        raise (TypeError "Bad constant declaration(s)")
    );

    let target_inner_type = DiscreteType.extract_inner_type target_var_type in

    typed_expr
    *)