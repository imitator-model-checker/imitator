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
open TypedStructure
open Constants

(* Type checking module *)
(* This module aim to type check expressions and create a typed tree from a parsed tree *)
module rec TypeChecker : sig

(* Check that a discrete init is well typed *)
val check_discrete_init : variable_infos -> variable_name -> parsed_boolean_expression -> typed_boolean_expression
(* Check that a constant declarations is well typed *)
val check_constant_expression : variable_infos -> variable_name * parsed_boolean_expression * DiscreteType.var_type -> typed_boolean_expression
(* Check that a guard is well typed *)
val check_guard : variable_infos -> guard -> typed_guard
(* Check that an update is well typed *)
val check_update : variable_infos -> parsed_scalar_or_index_update_type -> ParsingStructure.parsed_boolean_expression -> typed_scalar_or_index_update_type * typed_boolean_expression
(* Check that a condition is well typed *)
val check_conditional : variable_infos -> ParsingStructure.parsed_boolean_expression -> typed_boolean_expression
(* Check that a predicate is well typed *)
val check_state_predicate : variable_infos -> parsed_state_predicate -> typed_state_predicate
(* Check whether a parsed sequential bloc definition is well typed *)
val check_seq_code_bloc : variable_infos -> parsed_seq_code_bloc_list -> typed_seq_code_bloc_list
(* Check whether a function definition is well typed *)
val check_fun_definition : variable_infos -> parsed_fun_definition -> typed_fun_definition

end = struct

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

let rec type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt = function
	| Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) as outer_expr ->

        let l_typed_expr, l_type = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt l_expr in
        let r_typed_expr, r_type = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt r_expr in

        let typed_conj_dis =
            match parsed_conj_dis with
            | Parsed_and -> Typed_and
            | Parsed_or -> Typed_or
        in

        (* Check that left and right members are Boolean *)
        (match l_type, r_type with
        | Var_type_discrete_bool, Var_type_discrete_bool -> Typed_conj_dis (l_typed_expr, r_typed_expr, typed_conj_dis), Var_type_discrete_bool
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
	    let typed_expr, discrete_type = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_discrete_bool_expr (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt = function
    | Parsed_arithmetic_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_arithmetic_expr (typed_expr, discrete_type), discrete_type

	| Parsed_comparison (l_expr, relop, r_expr) as outer_expr ->

	    let l_typed_expr, l_type = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt l_expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_boolean_expression local_variables_opt variable_infos infer_type_opt r_expr in

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

	| Parsed_comparison_in (in_expr, lw_expr, up_expr) as outer_expr ->
	    let in_typed_expr, in_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt in_expr in
	    let lw_typed_expr, lw_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt lw_expr in
	    let up_typed_expr, up_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt up_expr in

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
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_bool_expr typed_expr, discrete_type

	| Parsed_Not expr as outer_expr ->
	    let typed_expr, discrete_type = type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt expr in

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

and type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt = function
	| Parsed_sum_diff (expr, term, sum_diff) as outer_expr ->
	    let l_typed_expr, l_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    let r_typed_expr, r_type = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in

        let typed_sum_diff =
            match sum_diff with
            | Parsed_plus -> Typed_plus
            | Parsed_minus -> Typed_minus
        in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_sum_diff (l_typed_expr, r_typed_expr, discrete_number_type, typed_sum_diff), Var_type_discrete_number discrete_number_type
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
	    let typed_expr, discrete_type = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    Typed_term (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt = function
    (* Specific case, literal rational => constant / constant *)
    (* Should be reduced before... *)

    | Parsed_product_quotient ((Parsed_DT_factor (Parsed_DF_constant lv) as term), (Parsed_DF_constant rv as factor), Parsed_div) as outer_expr ->

	    let l_typed_expr, l_type = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

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

            Typed_product_quotient (l_typed_expr, r_typed_expr, discrete_number_type, Typed_div), Var_type_discrete_number discrete_number_type

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
	    let l_typed_expr, l_type = type_check_parsed_discrete_term local_variables_opt variable_infos infer_type_opt term in
	    let r_typed_expr, r_type = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

        let typed_product_quotient =
            match parsed_product_quotient with
            | Parsed_mul -> Typed_mul
            | Parsed_div -> Typed_div
        in

        (* Check that members are numbers and compatible *)
        (match l_type, r_type with
        | Var_type_discrete_number l_number_type, Var_type_discrete_number r_number_type when is_discrete_type_number_compatibles l_number_type r_number_type ->
            let discrete_number_type = stronger_discrete_number_type_of l_number_type r_number_type in
            Typed_product_quotient (l_typed_expr, r_typed_expr, discrete_number_type, typed_product_quotient), Var_type_discrete_number discrete_number_type
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
	    let typed_expr, discrete_type = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in
	    Typed_factor (typed_expr, discrete_type), discrete_type

and type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt = function
	| Parsed_DF_variable variable_name ->
        (* If it's local variable, take it's type *)
        (* local variables are more priority and shadow global variables  *)
	    let discrete_type, scope =
            match local_variables_opt with
            | Some local_variables when Hashtbl.mem local_variables variable_name ->
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

        Typed_variable (variable_name, infer_discrete_type, scope), infer_discrete_type

	| Parsed_DF_constant value ->
        let discrete_type = ParsedValue.discrete_type_of_value value in

        (* If infer type is given and discrete type is unknown number *)
        (* we can infer directly unknown number to infer type *)
        let infer_discrete_type =
            match infer_type_opt, discrete_type with
            | Some ((Var_type_discrete_number _) as infer_type), Var_type_discrete_number Var_type_discrete_weak_number -> infer_type
            | _ -> discrete_type
        in

        Typed_constant (value, infer_discrete_type), infer_discrete_type

	| Parsed_sequence (list_expr, seq_type) as outer_expr ->
        let type_checks = List.map (type_check_parsed_boolean_expression local_variables_opt variable_infos infer_type_opt) list_expr in
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in
        let discrete_types = List.map (fun (_, discrete_type) -> discrete_type) type_checks in

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
            Typed_sequence (typed_expressions, inner_type, seq_type), discrete_type
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
        let typed_factor, factor_type = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in
        let typed_index, index_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos None (* None: mean no inference for index *) index_expr in

        if not (is_discrete_type_number_type index_type) || is_discrete_type_rational_type index_type then
            raise (TypeError "Index cannot be another type than int.");

        (* Check that accessed element is a collection *)
        (match factor_type with
        | Var_type_discrete_array (inner_type, _)
        | Var_type_discrete_list inner_type -> Typed_access (typed_factor, typed_index, factor_type, inner_type), inner_type
        | _ -> raise (TypeError "Cannot make an access to another type than array or list.")
        )

	| Parsed_DF_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_arithmetic_expression local_variables_opt variable_infos infer_type_opt expr in
	    Typed_expr (typed_expr, discrete_type), discrete_type

	| Parsed_DF_unary_min factor as outer_expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_factor local_variables_opt variable_infos infer_type_opt factor in

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
        (* Get function metadata *)
        let function_metadata = VariableInfo.function_metadata_by_name variable_infos function_name in
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
        let call_signature = List.map (fun (_, discrete_type) -> discrete_type) type_checks in
        (* Convert to typed arguments expressions *)
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in

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
        (* As typing system is static, only constant expressions can be considered for a value produced in dependent type *)
        let dependent_type_constraints = OCamlUtilities.rev_filter_map (fun (type_constraint, expr) ->
            match type_constraint with
            | Defined_type_constraint (Number_constraint (Defined_type_number_constraint Int_constraint (Int_name_constraint constraint_name))) ->

                let converted_expr = Convert.global_expression_of_typed_boolean_expression variable_infos expr in

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
        let typed_expressions = List.map (fun (typed_expr, _) -> typed_expr) type_checks in

        Typed_function_call (function_name, typed_expressions, return_type), return_type

let rec type_check_parsed_scalar_or_index_update_type local_variables variable_infos = function
    | Parsed_scalar_update variable_name ->
        (* Get assigned variable type *)

        let discrete_type =
            if Hashtbl.mem local_variables variable_name then (
                Hashtbl.find local_variables variable_name
            )
            else (
                let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos variable_name in
                discrete_type_of_var_type var_type
            )
        in

        Typed_scalar_update variable_name, discrete_type

    | Parsed_indexed_update (parsed_scalar_or_index_update_type, index_expr) as indexed_update ->

        let typed_scalar_or_index_update_type, discrete_type = type_check_parsed_scalar_or_index_update_type local_variables variable_infos parsed_scalar_or_index_update_type in
        let typed_index_expr_type, index_discrete_type = type_check_parsed_discrete_arithmetic_expression (Some local_variables) variable_infos (Some (Var_type_discrete_number Var_type_discrete_int)) index_expr in

        (* Check that index expression is an int expression *)
        if index_discrete_type <> Var_type_discrete_number Var_type_discrete_int then
            raise (TypeError ("Index of expression `" ^ ParsingStructureUtilities.string_of_parsed_scalar_or_index_update_type variable_infos indexed_update ^ "` is not an integer."));

        (* Check is an array *)
        let discrete_type =
            match discrete_type with
            | Var_type_discrete_array (inner_type, _) -> inner_type
            | _ -> raise (TypeError "Trying to make a write access to a non-array variable.")
        in
        Typed_indexed_update (typed_scalar_or_index_update_type, typed_index_expr_type, discrete_type), discrete_type

let rec type_check_seq_code_bloc local_variables variable_infos infer_type_opt (* parsed_seq_code_bloc *) =

    let rec type_check_seq_code_bloc_rec local_variables parsed_seq_code_bloc =
        List.map (type_check_parsed_instruction local_variables) parsed_seq_code_bloc

    and type_check_parsed_instruction local_variables = function
        | Parsed_local_decl (variable_name, variable_type, expr, _) ->

            (* Eventually get a number type to infer *)
            let variable_number_type_opt = Some (DiscreteType.extract_inner_type variable_type) in

            (* Type check and infer init expression of the local variable declaration *)
            let typed_init_expr, init_discrete_type = type_check_parsed_boolean_expression (Some local_variables) variable_infos variable_number_type_opt expr in

            (* Add local variable to hashtable *)
            Hashtbl.replace local_variables variable_name variable_type;

            (* Check compatibility between local variable declared type and it's init expression *)
            if not (is_discrete_type_compatibles variable_type init_discrete_type) then
                raise (TypeError (
                    ill_typed_variable_message variable_name (DiscreteType.string_of_var_type_discrete variable_type) (ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr) init_discrete_type
                ));

            (* All is ok, convert to a typed function local declaration *)
            Typed_local_decl (
                variable_name,
                variable_type,
                typed_init_expr
            )

        | Parsed_assignment (parsed_scalar_or_index_update_type, expr) ->

            (* Resolve typed scalar or index update type *)
            let typed_parsed_scalar_or_index_update_type, variable_type = type_check_parsed_scalar_or_index_update_type local_variables variable_infos parsed_scalar_or_index_update_type in

            (* Eventually get a number type to infer *)
            let variable_number_type_opt = Some (DiscreteType.extract_inner_type variable_type) in

            (* Resolve typed expression *)
            let typed_expr, expr_type = type_check_parsed_boolean_expression (Some local_variables) variable_infos variable_number_type_opt expr in

            let str_update_type = ParsingStructureUtilities.string_of_parsed_scalar_or_index_update_type variable_infos parsed_scalar_or_index_update_type in

            (* Check compatibility between assignee variable type and it's assigned expression *)
            if not (is_discrete_type_compatibles variable_type expr_type) then
                raise (TypeError (
                    ill_typed_variable_message str_update_type (DiscreteType.string_of_var_type_discrete variable_type) (ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr) expr_type
                ));

            (* Get assignment scope *)
            let variable_name = ParsingStructureMeta.variable_name_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type in

            let scope =
                if Hashtbl.mem local_variables variable_name then (
                    Ass_discrete_local
                )
                else (
                    let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos variable_name in
                    (match var_type with
                    | Var_type_clock -> Ass_clock
                    | _ -> Ass_discrete_global
                    )
                )
            in

            Typed_assignment ((typed_parsed_scalar_or_index_update_type, typed_expr), scope)

        | Parsed_instruction expr ->
            (* Resolve typed expression *)
            let typed_expr, _ = type_check_parsed_boolean_expression (Some local_variables) variable_infos None expr in
            Typed_instruction typed_expr

        | Parsed_for_loop (variable_name, from_expr, to_expr, loop_dir, inner_bloc, _) as outer_expr ->
            (* Add local variable for loop to hashtable *)
            let loop_local_variables = Hashtbl.copy local_variables in
            Hashtbl.replace loop_local_variables variable_name (Var_type_discrete_number Var_type_discrete_int);

            (* Resolve typed from expr *)
            let typed_from_expr, from_expr_type = type_check_parsed_discrete_arithmetic_expression (Some local_variables) variable_infos (Some (Var_type_discrete_number Var_type_discrete_int)) from_expr in
            (* Resolve typed to expr *)
            let typed_to_expr, to_expr_type = type_check_parsed_discrete_arithmetic_expression (Some local_variables) variable_infos (Some (Var_type_discrete_number Var_type_discrete_int)) to_expr in
            (* Resolve typed inner expr *)
            let typed_inner_bloc = type_check_seq_code_bloc_rec loop_local_variables inner_bloc in

            let typed_loop_dir =
                match loop_dir with
                | Parsed_for_loop_up -> Typed_for_loop_up
                | Parsed_for_loop_down -> Typed_for_loop_down
            in

            (* Check from and to expr type are int *)
            (match from_expr_type, to_expr_type with
            | Var_type_discrete_number Var_type_discrete_int, Var_type_discrete_number Var_type_discrete_int ->
                Typed_for_loop (variable_name, typed_from_expr, typed_to_expr, typed_loop_dir, typed_inner_bloc)
            | _ ->
                raise (TypeError (
                    ill_typed_message_of_expressions
                        [
                            string_of_parsed_arithmetic_expression variable_infos from_expr;
                            string_of_parsed_arithmetic_expression variable_infos to_expr
                        ]
                        [from_expr_type; to_expr_type]
                        (string_of_parsed_instruction variable_infos outer_expr)
                ))
            )

        | Parsed_while_loop (condition_expr, inner_bloc) as outer_expr ->

            let loop_local_variables = Hashtbl.copy local_variables in

            (* Resolve typed condition expr *)
            let typed_condition_expr, condition_expr_type = type_check_parsed_boolean_expression (Some local_variables) variable_infos None condition_expr in
            (* Resolve typed inner expr *)
            let typed_inner_bloc = type_check_seq_code_bloc_rec loop_local_variables inner_bloc in

            (match condition_expr_type with
            | Var_type_discrete_bool ->
                Typed_while_loop (typed_condition_expr, typed_inner_bloc)
            | _ ->
                raise (TypeError (
                    ill_typed_message_of_expressions
                        [string_of_parsed_boolean_expression variable_infos condition_expr]
                        [condition_expr_type]
                        (string_of_parsed_instruction variable_infos outer_expr)
                ))
            )

        | Parsed_if (condition_expr, then_bloc, else_bloc_opt) as outer_expr ->
            (* Resolve typed from expr *)
            let typed_condition_expr, condition_expr_type = type_check_parsed_boolean_expression (Some local_variables) variable_infos None condition_expr in

            let then_local_variables = Hashtbl.copy local_variables in


            (* Resolve typed then expr *)
            let typed_then_bloc = type_check_seq_code_bloc_rec then_local_variables then_bloc in

            (* Resolve typed else expr *)
            let typed_else_bloc_opt =
                match else_bloc_opt with
                | Some else_bloc ->
                    let else_local_variables = Hashtbl.copy local_variables in
                    let typed_else_bloc = type_check_seq_code_bloc_rec else_local_variables else_bloc in
                    Some typed_else_bloc
                | None ->
                    None
            in

            (match condition_expr_type with
            | Var_type_discrete_bool ->
                Typed_if (typed_condition_expr, typed_then_bloc, typed_else_bloc_opt)
            | _ ->
                raise (TypeError (
                    ill_typed_message_of_expressions
                        [string_of_parsed_boolean_expression variable_infos condition_expr]
                        [condition_expr_type]
                        (string_of_parsed_instruction variable_infos outer_expr)
                ))
            )

    in
    type_check_seq_code_bloc_rec local_variables (* parsed_seq_code_bloc *)

let type_check_parsed_fun_definition variable_infos (fun_def : ParsingStructure.parsed_fun_definition) =
    (* Get parameter types and return type of the function *)
    let parameter_names, parameter_discrete_types = List.split fun_def.parameters in
    let return_type = fun_def.return_type in
    (* Construct signature *)
    let signature = parameter_discrete_types @ [return_type] in

    (* Add parameters as local variables of the function *)
    let local_variables = Hashtbl.create (List.length fun_def.parameters) in
    List.iter (fun (param_name, param_type) -> Hashtbl.replace local_variables param_name param_type) fun_def.parameters;

    (* Eventually infer the body expression type of function to the return type underlying type of the function *)
    let infer_type_opt = Some (DiscreteType.extract_inner_type return_type) in

    let parsed_seq_code_bloc, return_expr_opt = fun_def.body in

    let typed_seq_code_bloc = type_check_seq_code_bloc local_variables variable_infos None parsed_seq_code_bloc in

    let typed_return_expr_opt, check_return_type, str_return_expr =
        match return_expr_opt with
        | Some return_expr ->
            let typed_return_expr, check_return_type = type_check_parsed_boolean_expression (Some local_variables) variable_infos infer_type_opt return_expr in
            Some typed_return_expr, check_return_type, string_of_typed_boolean_expression variable_infos typed_return_expr
        | None -> None, Var_type_void, ""
    in

    (* Check type compatibility between function body and return type *)
    let is_return_type_compatibles = is_discrete_type_compatibles check_return_type return_type in

    if not is_return_type_compatibles then
        raise (TypeError (
            "Function signature `"
            ^ fun_def.name
            ^ FunctionSig.string_of_signature signature
            ^ "` does not match with return expression `"
            ^ str_return_expr
            ^ "` of type "
            ^ DiscreteType.string_of_var_type_discrete check_return_type
            ^ "."
        ));

    let typed_fun_def = {
        name = fun_def.name;
        parameters = parameter_names;
        signature = signature;
        body = typed_seq_code_bloc, typed_return_expr_opt;
    }
    in

    typed_fun_def, return_type


let type_check_parsed_loc_predicate variable_infos infer_type_opt = function
	| Parsed_loc_predicate_EQ (automaton_name, loc_name) -> Typed_loc_predicate_EQ (automaton_name, loc_name), Var_type_discrete_bool
	| Parsed_loc_predicate_NEQ (automaton_name, loc_name) -> Typed_loc_predicate_NEQ (automaton_name, loc_name), Var_type_discrete_bool

let rec type_check_parsed_simple_predicate variable_infos infer_type_opt = function
	| Parsed_discrete_boolean_expression expr ->
	    let typed_expr, discrete_type = type_check_parsed_discrete_boolean_expression None variable_infos infer_type_opt expr in

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

    (* Eventually get a number type to infer *)
    let variable_number_type_opt = Some (DiscreteType.extract_inner_type variable_type) in
    (* Resolve typed expression *)
    let typed_expr, expr_var_type_discrete = type_check_parsed_boolean_expression None variable_infos variable_number_type_opt expr in
    (* Check eventual side effects in assignment *)
    let has_side_effects = ParsingStructureMeta.has_side_effect_parsed_boolean_expression variable_infos expr in

    (* Check if initialisation (init / constant) has side effects *)
    if has_side_effects then
        raise (TypeError (
            "Init or constant expression `"
            ^ ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr
            ^ "` has side effects."
        ));

    (* Check expression / variable type consistency *)
    let is_consistent = DiscreteType.is_discrete_type_compatibles variable_type expr_var_type_discrete in

    (* Not consistent ? raise a type error with appropriate message *)
    if not (is_consistent) then (
        raise (TypeError (ill_typed_variable_message variable_name (DiscreteType.string_of_var_type_discrete variable_type) (ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr) expr_var_type_discrete))
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
        ^ string_of_typed_boolean_expression variable_infos typed_expr
    );
    (*
    ResultProcessor.add_custom_details (
        "annot - inits - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_boolean_expression variable_infos typed_expr
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
        ^ string_of_typed_boolean_expression variable_infos typed_expr
    );
    typed_expr


(* Type non-linear constraint *)
(* return a tuple containing the non-linear constraint uniformly typed and the resolved type of the expression *)
let check_nonlinear_constraint variable_infos nonlinear_constraint =

    let typed_nonlinear_constraint, discrete_type = type_check_parsed_discrete_boolean_expression None variable_infos None nonlinear_constraint in
    (* Check eventual side effects in non-linear constraint *)
    let has_side_effects = ParsingStructureMeta.has_side_effect_parsed_discrete_boolean_expression variable_infos nonlinear_constraint in

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





(* TODO benjamin CLEAN UPDATES *)
(* Type check an update *)
let check_update variable_infos parsed_scalar_or_index_update_type expr =

    (* Get assigned variable name *)
    let variable_name = ParsingStructureMeta.variable_name_of_parsed_scalar_or_index_update_type parsed_scalar_or_index_update_type in

    (* Get assigned variable type *)
    let var_type = VariableInfo.var_type_of_variable_or_constant variable_infos variable_name in

    (* Eventually get a number type to infer *)
    let variable_number_type_opt =
        match var_type with
        | Var_type_clock
        | Var_type_parameter -> None
        | Var_type_discrete discrete_type -> Some (DiscreteType.extract_inner_type discrete_type)
    in

    (* Resolve typed expression *)
    let typed_expr, expr_type = type_check_parsed_boolean_expression None variable_infos variable_number_type_opt expr in
    (* Check eventual side effects in update expression (left and right member) *)
    let has_side_effects = ParsingStructureMeta.has_side_effect_parsed_normal_update variable_infos (parsed_scalar_or_index_update_type, expr) in

    (* TODO benjamin here VariableMap.empty for local variable because this function is used only for check updates *)
    (* TODO benjamin Soon, this function will be removed *)
    let typed_parsed_scalar_or_index_update_type, l_value_type = type_check_parsed_scalar_or_index_update_type (Hashtbl.create 0) variable_infos parsed_scalar_or_index_update_type in

    (* Check var_type_discrete is compatible with expression type, if yes, convert expression *)
     if not (DiscreteType.is_discrete_type_compatibles l_value_type expr_type) then (
        raise (TypeError (
            ill_typed_variable_message variable_name (DiscreteType.string_of_var_type var_type) (ParsingStructureUtilities.string_of_parsed_boolean_expression variable_infos expr) expr_type
            )
        )
    );

    (* Print type annotations *)
    ImitatorUtilities.print_message Verbose_high (
        "annot - updates - "
        ^ variable_name
        ^ " := "
        ^ string_of_typed_boolean_expression variable_infos typed_expr
    );

    typed_parsed_scalar_or_index_update_type,
    typed_expr

(* TODO benjamin CLEAN UPDATES *)
(* Type check a conditional expression *)
(* return a tuple containing the conditional expression uniformly typed and the resolved type of the expression *)
let check_conditional variable_infos expr =

    print_message Verbose_high "----------";
    print_message Verbose_high ("Infer conditional expression: " ^ string_of_parsed_boolean_expression variable_infos expr);

    let typed_expr, expr_type = type_check_parsed_boolean_expression None variable_infos None expr in

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
    (* Check eventual side effects in state predicate *)
    let has_side_effects = ParsingStructureMeta.has_side_effect_parsed_state_predicate variable_infos predicate in

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

(* Check whether a parsed sequential bloc definition is well typed *)
let check_seq_code_bloc variable_infos parsed_seq_code_bloc = type_check_seq_code_bloc (Hashtbl.create 0) variable_infos None parsed_seq_code_bloc

(* Check whether a function definition is well typed *)
let check_fun_definition variable_infos (parsed_fun_definition : parsed_fun_definition) =
    let typed_fun_definition, _ = type_check_parsed_fun_definition variable_infos parsed_fun_definition in
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

val linear_term_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> LinearConstraint.pxd_linear_term
val global_expression_of_typed_boolean_expression_by_type : variable_infos -> typed_boolean_expression -> DiscreteType.var_type_discrete -> DiscreteExpressions.global_expression
val global_expression_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> DiscreteExpressions.global_expression
val bool_expression_of_typed_boolean_expression : variable_infos -> typed_boolean_expression -> DiscreteExpressions.boolean_expression
val bool_expression_of_typed_discrete_boolean_expression : variable_infos -> typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val nonlinear_constraint_of_typed_nonlinear_constraint : variable_infos -> typed_discrete_boolean_expression -> DiscreteExpressions.discrete_boolean_expression
val scalar_or_index_update_type_of_typed_scalar_or_index_update_type : variable_infos -> typed_scalar_or_index_update_type -> DiscreteExpressions.scalar_or_index_update_type
val seq_code_bloc_of_typed_seq_code_bloc : variable_infos -> typed_seq_code_bloc_list -> DiscreteExpressions.seq_code_bloc_list
val clock_update_of_typed_seq_code_bloc : variable_infos -> bool -> typed_seq_code_bloc_list -> AbstractModel.clock_updates
val fun_definition_of_typed_fun_definition : variable_infos -> typed_fun_definition -> AbstractModel.fun_definition

end = struct

open AbstractModel
open DiscreteExpressions
open TypeChecker


type discrete_index = int



let user_function_meta variable_infos function_name =
    let fun_meta_opt = Hashtbl.find_opt variable_infos.fun_meta function_name in
    match fun_meta_opt with
    | Some fun_meta -> fun_meta
    | None -> raise (UndefinedFunction function_name)

(* Messages *)
let expression_must_have_type_message = "An expression should have a determined type. Maybe something has failed before."
let expr_type_doesnt_match_to_structure_message str_expr_type str_expr = "The deduced expression type indicate that it should be converted to a " ^ str_expr_type ^ " expression, but it's incompatible with this expression structure: `" ^ str_expr ^ "`. Maybe something failed in type checking or conversion."

(** Convert a Boolean operator to its abstract model *)
let relop_of_parsed_relop = function
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

let loop_dir_of_typed_loop_dir = function
    | Typed_for_loop_up -> Loop_up
    | Typed_for_loop_down -> Loop_down


(* Raise internal error when expected typed expression doesn't match with target abstract model expression *)
let raise_conversion_error str_structure_name str_expr =
    let fail_message = expr_type_doesnt_match_to_structure_message str_structure_name str_expr in
    raise (InternalError fail_message)

let rec global_expression_of_typed_boolean_expression variable_infos expr =
    let discrete_type =
        match expr with
        | Typed_conj_dis _ -> Var_type_discrete_bool
        | Typed_discrete_bool_expr (_, discrete_type) -> discrete_type
    in
    global_expression_of_typed_boolean_expression_by_type variable_infos expr discrete_type

and global_expression_of_typed_boolean_expression_by_type variable_infos expr = function
    | Var_type_void ->
        Void_expression (
            void_expression_of_typed_boolean_expression variable_infos expr
        )
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
    | Var_type_discrete_stack _ ->
        Stack_expression (
            stack_expression_of_typed_boolean_expression variable_infos expr
        )
    | Var_type_discrete_queue _ ->
        Queue_expression (
            queue_expression_of_typed_boolean_expression variable_infos expr
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

(* Bool expression conversion *)
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
    | Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        bool_expression_of_typed_factor variable_infos factor

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
    | _ as expr ->
        raise_conversion_error "Bool" (string_of_typed_discrete_boolean_expression variable_infos expr)

and bool_expression_of_typed_comparison variable_infos l_expr parsed_relop r_expr discrete_type =

    let relop = relop_of_parsed_relop parsed_relop in

    match discrete_type with
    | Var_type_discrete_number discrete_number_type ->
        Arithmetic_comparison (
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type l_expr,
            relop,
            discrete_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos discrete_number_type r_expr
        )
    | Var_type_discrete_bool ->
        Boolean_comparison (
            bool_expression_of_typed_discrete_boolean_expression variable_infos l_expr,
            relop,
            bool_expression_of_typed_discrete_boolean_expression variable_infos r_expr
        )
    | Var_type_discrete_binary_word length ->
        Binary_comparison (
            binary_expression_of_typed_discrete_boolean_expression variable_infos length l_expr,
            relop,
            binary_expression_of_typed_discrete_boolean_expression variable_infos length r_expr
        )
    | Var_type_discrete_array (inner_type, _) ->
        Array_comparison (
            array_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            relop,
            array_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_list inner_type ->
        List_comparison (
            list_expression_of_typed_discrete_boolean_expression variable_infos inner_type l_expr,
            relop,
            list_expression_of_typed_discrete_boolean_expression variable_infos inner_type r_expr
        )
    | Var_type_discrete_stack _ as discrete_type ->
        Stack_comparison (
            stack_expression_of_typed_boolean_expression variable_infos (Typed_discrete_bool_expr (l_expr, discrete_type)),
            relop,
            stack_expression_of_typed_boolean_expression variable_infos (Typed_discrete_bool_expr (r_expr, discrete_type))
        )
    | Var_type_discrete_queue _ as discrete_type ->
        Queue_comparison (
            queue_expression_of_typed_boolean_expression variable_infos (Typed_discrete_bool_expr (l_expr, discrete_type)),
            relop,
            queue_expression_of_typed_boolean_expression variable_infos (Typed_discrete_bool_expr (r_expr, discrete_type))
        )
    | Var_type_void
    | Var_type_weak ->
        raise (InternalError expression_must_have_type_message)

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

    | Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        bool_expression_of_typed_factor variable_infos factor

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Bool_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
        let fun_meta = user_function_meta variable_infos function_name in

        Bool_function_call (
            function_name,
            fun_meta.parameter_names,
            List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
        )

	| _ as expr ->
	    raise_conversion_error "Bool" (string_of_typed_discrete_factor variable_infos Var_type_weak expr)

(* Rational expression conversion *)
and rational_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, _) ->
        rational_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ as expr ->
        raise_conversion_error "rational" (string_of_typed_discrete_boolean_expression variable_infos expr)

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
        raise_conversion_error "rational" (string_of_typed_discrete_factor variable_infos Var_type_weak expr)

and rational_expression_of_typed_function_call variable_infos argument_expressions function_name =
    let fun_meta = user_function_meta variable_infos function_name in

    Rational_function_call (
        function_name,
        fun_meta.parameter_names,
        List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
    )

(* Int expression conversion *)
and int_arithmetic_expression_of_typed_discrete_boolean_expression variable_infos = function
    | Typed_arithmetic_expr (expr, discrete_type) ->
        int_arithmetic_expression_of_typed_arithmetic_expression variable_infos expr
    | _ as expr ->
        raise_conversion_error "int" (string_of_typed_discrete_boolean_expression variable_infos expr)

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
        let fun_meta = user_function_meta variable_infos function_name in

        Int_function_call (
            function_name,
            fun_meta.parameter_names,
            List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
        )

	| _ as expr ->
        raise_conversion_error "int" (string_of_typed_discrete_factor variable_infos Var_type_weak expr)

(* Binary word conversion *)
and binary_expression_of_typed_boolean_expression variable_infos length = function
    | Typed_discrete_bool_expr (expr, _) ->
        binary_expression_of_typed_discrete_boolean_expression variable_infos length expr
    | _ as expr ->
        raise_conversion_error "binary word" (string_of_typed_boolean_expression variable_infos expr)

and binary_expression_of_typed_discrete_boolean_expression variable_infos length = function
    | Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        binary_expression_of_typed_factor variable_infos length factor
    | _ as expr ->
        raise_conversion_error "binary word" (string_of_typed_discrete_boolean_expression variable_infos expr)

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

    | Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        binary_expression_of_typed_factor variable_infos length factor

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Binary_word_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

    | Typed_function_call (function_name, argument_expressions, _) ->
        let fun_meta = user_function_meta variable_infos function_name in

        Binary_word_function_call (
            function_name,
            fun_meta.parameter_names,
            List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
        )

    | _ as expr ->
        raise_conversion_error "binary word" (string_of_typed_discrete_factor variable_infos Var_type_weak expr)

(* Array expression conversion *)
and array_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
        raise_conversion_error Constants.array_string (string_of_typed_boolean_expression variable_infos expr)

and array_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        array_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
        raise_conversion_error Constants.array_string (string_of_typed_discrete_boolean_expression variable_infos expr)

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
        let expressions = List.map (fun expr -> global_expression_of_typed_boolean_expression_by_type variable_infos expr discrete_type) expr_list in
        Literal_array (Array.of_list expressions)

	| Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        array_expression_of_typed_factor variable_infos discrete_type factor

    | Typed_access (factor, index_expr, discrete_type, _) ->
        Array_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
        let fun_meta = user_function_meta variable_infos function_name in

        Array_function_call (
            function_name,
            fun_meta.parameter_names,
            List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
        )

	| _ as expr ->
        raise_conversion_error Constants.array_string (string_of_typed_discrete_factor variable_infos Var_type_weak expr)


(* List expression conversion *)
and list_expression_of_typed_boolean_expression variable_infos discrete_type = function
    | Typed_discrete_bool_expr (expr, _) ->
        list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type expr
    | _ as expr ->
        raise_conversion_error Constants.list_string (string_of_typed_boolean_expression variable_infos expr)

and list_expression_of_typed_discrete_boolean_expression variable_infos discrete_type = function
    | Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        list_expression_of_typed_factor variable_infos discrete_type factor
    | _ as expr ->
        raise_conversion_error Constants.list_string (string_of_typed_discrete_boolean_expression variable_infos expr)

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
        Literal_list (List.map (fun expr -> global_expression_of_typed_boolean_expression_by_type variable_infos expr discrete_type) expr_list)

	| Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
        list_expression_of_typed_factor variable_infos discrete_type factor

    | Typed_access (factor, index_expr, discrete_type, _) ->
        List_array_access (
            expression_access_type_of_typed_factor variable_infos factor discrete_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

	| Typed_function_call (function_name, argument_expressions, _) ->
        let fun_meta = user_function_meta variable_infos function_name in

        List_function_call (
            function_name,
            fun_meta.parameter_names,
            List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
        )

	| _ as expr ->
        raise_conversion_error Constants.list_string (string_of_typed_discrete_factor variable_infos Var_type_weak expr)

(* Stack expression conversion *)
and stack_expression_of_typed_boolean_expression variable_infos expr =

    let raise_error = raise_conversion_error Constants.stack_string in

    let rec stack_expression_of_typed_boolean_expression = function
        | Typed_discrete_bool_expr (Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _), _) ->
            stack_expression_of_typed_factor factor
        | _ as outer_expr ->
            raise_error (string_of_typed_boolean_expression variable_infos outer_expr)

    and stack_expression_of_typed_factor = function
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
        | Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
            stack_expression_of_typed_factor factor

        | Typed_access (factor, index_expr, discrete_type, _) ->
            Stack_array_access (
                expression_access_type_of_typed_factor variable_infos factor discrete_type,
                int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
            )

        | Typed_sequence (_, _, Typed_stack) -> Literal_stack

        | Typed_function_call (function_name, argument_expressions, _) ->
            let fun_meta = user_function_meta variable_infos function_name in

            Stack_function_call (
                function_name,
                fun_meta.parameter_names,
                List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
            )

        | _ as expr ->
            raise_error (string_of_typed_discrete_factor variable_infos Var_type_weak expr)
    in
    stack_expression_of_typed_boolean_expression expr

(* Queue expression conversion *)
and queue_expression_of_typed_boolean_expression variable_infos expr =

    let raise_error = raise_conversion_error Constants.queue_string in

    let rec queue_expression_of_typed_boolean_expression = function
        | Typed_discrete_bool_expr (Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _), _) ->
            queue_expression_of_typed_factor factor
        | _ as outer_expr ->
            raise_error (string_of_typed_boolean_expression variable_infos outer_expr)

    and queue_expression_of_typed_factor = function
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

        | Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
            queue_expression_of_typed_factor factor

        | Typed_access (factor, index_expr, discrete_type, _) ->
            Queue_array_access (
                expression_access_type_of_typed_factor variable_infos factor discrete_type,
                int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
            )

        | Typed_sequence (_, _, Typed_queue) -> Literal_queue

        | Typed_function_call (function_name, argument_expressions, _) ->
            let fun_meta = user_function_meta variable_infos function_name in

            Queue_function_call (
                function_name,
                fun_meta.parameter_names,
                List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
            )

        | _ as expr ->
            raise_error (string_of_typed_discrete_factor variable_infos Var_type_weak expr)
    in
    queue_expression_of_typed_boolean_expression expr

(* Void expression conversion *)
and void_expression_of_typed_boolean_expression variable_infos expr =

    let raise_error = raise_conversion_error Constants.void_string in

    let rec void_expression_of_typed_boolean_expression = function
        | Typed_discrete_bool_expr (Typed_arithmetic_expr (Typed_term (Typed_factor (factor, _), _), _), _) ->
           void_expression_of_typed_factor factor
        | _ as expr ->
            raise_error (string_of_typed_boolean_expression variable_infos expr)

    and void_expression_of_typed_factor = function
        | Typed_variable (variable_name, _, scope) ->
            (* Some code should control that variables and function parameters cannot be declared as void *)
            (* If this exception is raised, it mean that control was not made before properly *)
            raise (InternalError (
                "`"
                ^ Constants.void_string
                ^ "` keyword is reserved for function return type.
                Literals, constants, function parameters, local or global variables of type `"
                ^ Constants.void_string
                ^ "` cannot be declared.
                It should be checked before."
            ))

        | Typed_expr (Typed_term (Typed_factor (factor, _), _), _) ->
            void_expression_of_typed_factor factor

        | Typed_function_call (function_name, argument_expressions, _) ->
            let fun_meta = user_function_meta variable_infos function_name in

            Void_function_call (
                function_name,
                fun_meta.parameter_names,
                List.map (global_expression_of_typed_boolean_expression variable_infos) argument_expressions
            )

        | _ as expr ->
            raise_error (string_of_typed_discrete_factor variable_infos Var_type_weak expr)
    in
    void_expression_of_typed_boolean_expression expr

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

(* Alias *)
let nonlinear_constraint_of_typed_nonlinear_constraint = bool_expression_of_typed_discrete_boolean_expression

let rec scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos = function
    | Typed_scalar_update variable_name ->
        let variable_kind = variable_kind_of_variable_name variable_infos variable_name in

        (match variable_kind with
        | Constant_kind value -> raise (InternalError "Unable to set a constant expression. This should be checked before.")
        | Variable_kind discrete_index -> Scalar_update (Global_update discrete_index)
        )

    | Typed_indexed_update (typed_scalar_or_index_update_type, index_expr, _) ->
        Indexed_update (
            scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

let rec local_scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos = function
    | Typed_scalar_update variable_name ->
        Scalar_update (Local_update variable_name)

    | Typed_indexed_update (typed_scalar_or_index_update_type, index_expr, _) ->
        Indexed_update (
            local_scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
            int_arithmetic_expression_of_typed_arithmetic_expression variable_infos index_expr
        )

let clock_index_of_typed_scalar_or_index_update_type variable_infos = function
    | Typed_scalar_update clock_name -> VariableInfo.index_of_variable_name variable_infos clock_name
    | _ -> raise (InternalError "Unable to have indexed update on clock.")

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

(* TODO benjamin CLEAN UP *)
(*** NOTE: define a top-level function to avoid recursive passing of all common variables ***)
(*
let linear_term_of_typed_arithmetic_expression variable_infos pdae =

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
		| Typed_product_quotient (parsed_update_term, parsed_update_factor, _, Typed_mul) as outer_term ->

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
            | None, None ->
                raise (
                    InvalidExpression (
                        "Unable to convert expression `"
                        ^ string_of_typed_discrete_term variable_infos (Var_type_discrete_number Var_type_discrete_rat) outer_term
                        ^ "` to a linear expression. Expression linearity should be checked before."
                    )
                )
            )



		| Typed_product_quotient (parsed_update_term, parsed_update_factor, _, Typed_div) as outer_term ->

            (* Convert to abstract tree *)
            let converted_factor = rational_arithmetic_expression_of_typed_factor variable_infos parsed_update_factor in
            (* Try to evaluate the factor *)
            let numconst_valued_factor_opt = DiscreteExpressionEvaluator.eval_constant_rational_factor_opt None (* function table *) converted_factor in

            (* Update coefficients *)
            (match numconst_valued_factor_opt with
            | Some numconst_valued_factor ->
                update_coef_array_in_parsed_update_term (NumConst.div mult_factor numconst_valued_factor) parsed_update_term
            | None ->
                raise (
                    InvalidExpression (
                        "Unable to convert expression `"
                        ^ string_of_typed_discrete_term variable_infos (Var_type_discrete_number Var_type_discrete_rat) outer_term
                        ^ "` to a linear expression. Expression linearity should be checked before."
                    )
                )
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
            raise (
                InvalidExpression (
                    "Unable to convert expression `"
                    ^ string_of_typed_discrete_factor variable_infos (Var_type_discrete_number Var_type_discrete_rat) factor
                    ^ "` to a linear expression. Expression linearity should be checked before."
                )
            )
	in

	(* Call the recursive function updating the coefficients *)
	update_coef_array_in_typed_update_arithmetic_expression NumConst.one pdae;

	(* Create the linear term *)
	linear_term_of_array array_of_coef !constant
*)

type linear_term_element =
    | Lt_var of NumConst.t * variable_name * typed_variable_scope
    | Lt_cons of NumConst.t

(* Convert typed arithmetic expression to a linear term, if possible, and reduce it *)
let linear_term_of_typed_arithmetic_expression variable_infos expr =

    (* Get message when conversion fail *)
    let unable_to_convert_error_msg str_expr =
        "Unable to convert expression `"
        ^ str_expr
        ^ "` to a linear expression. Expression linearity should be checked before."
    in

    (* Reduce a list of weighted variables (variables with coef) and constants by adding them *)
    (* For all examples consider this linear expression : 1 - 2 + 3x - 2 + y - 2x *)
    let reduce_terms_list list =
        (* Separate constants and variables with coefs (ex: split into [1;-2;-2] and [(3,x);(1,y);(-2, x)]) *)
        let constants, weighted_variables = OCamlUtilities.partition_map (function Lt_cons k -> My_left k | Lt_var (coef, variable_name, scope) -> My_right (coef, variable_name, scope)) list in
        (* Compute value of the constant by adding constants together *)
        (* ex: [1;-2;-2] = -3 *)
        let constants_sum = List.fold_left NumConst.add NumConst.zero constants in
        (* Group variable by name, ex: [(x, [3;-2]); (y, [1])] *)
        let wv_grouped_by_variable_name = OCamlUtilities.group_by_and_map (fun (_, variable_name, _) -> variable_name) (fun (coef, _, scope) -> coef, scope) weighted_variables in
        (* Compute for each variable the coef by adding them together (ex: for [(x, [3;-2]); (y, [1])] we obtain [(x, 1); (y, 1)] ) *)
        let weighted_variables_without_duplicates = List.map (fun (variable_name, coefs_and_scopes) ->
            let coefs, scopes = List.split coefs_and_scopes in
            (* Take first scope found as scope for this variable, all scopes should be equals *)
            let scope = List.hd scopes in
            (* Sum all coefs and return triplet *)
            List.fold_left NumConst.add NumConst.zero coefs, variable_name, scope
        ) wv_grouped_by_variable_name in

        (* Reconstruct list of linear_term_elements *)
        let terms_list = (List.map (fun (coef, variable_name, scope) -> Lt_var (coef, variable_name, scope)) weighted_variables_without_duplicates) @ [Lt_cons (constants_sum)] in
        (* Return list of variables with coef and constant value at raw form, and terms list *)
        weighted_variables_without_duplicates, constants_sum, terms_list
    in

    let rec linear_coefs_of_typed_arithmetic_expression = function
        | Typed_sum_diff (expr, term, _, sum_diff) ->

            let expr_coefs = linear_coefs_of_typed_arithmetic_expression expr in
            let term_coefs = linear_coefs_of_typed_term term in

            (* Change sign of right expression *)
            let new_term_coefs =
                match sum_diff with
                | Typed_minus ->
                    (match term_coefs with
                    | Lt_var (c, v, s) :: t -> Lt_var (NumConst.neg c, v, s) :: t
                    | Lt_cons c :: t -> Lt_cons (NumConst.neg c) :: t
                    | [] -> []
                    )
                | _ -> term_coefs
            in

            expr_coefs @ new_term_coefs

        | Typed_term (term, _) ->
            linear_coefs_of_typed_term term

    and linear_coefs_of_typed_term = function
        | Typed_product_quotient (term, factor, _, product_quotient) as outer_term ->

            let term_coefs = linear_coefs_of_typed_term term in
            let factor_coefs = linear_coefs_of_typed_factor factor in

            (* Eventually prepare string representation of expression for the message when conversion fail *)
            let str_outer_term_lazy = lazy (string_of_typed_discrete_term variable_infos (Var_type_discrete_number Var_type_discrete_rat) outer_term) in

            let result =
            (match product_quotient with
            | Typed_mul ->

                let combination = OCamlUtilities.list_combination_2 term_coefs factor_coefs in
                let coefs_list = List.map (fun (a, b) -> match a, b with
                    | Lt_cons c1, Lt_cons c2 -> Lt_cons (NumConst.mul c1 c2)
                    | Lt_cons c1, Lt_var (c2, v, s)
                    | Lt_var (c1, v, s), Lt_cons c2 -> Lt_var (NumConst.mul c1 c2, v, s)
                    | Lt_var _, Lt_var _ -> raise (InvalidExpression (unable_to_convert_error_msg (Lazy.force str_outer_term_lazy)))
                ) combination
                in

                let _, _, term_list = reduce_terms_list coefs_list in term_list

            | Typed_div ->
                let combination = OCamlUtilities.list_combination_2 term_coefs factor_coefs in
                let coefs_list = List.map (fun (a, b) -> match a, b with
                    | Lt_cons c1, Lt_cons c2 -> Lt_cons (NumConst.div c1 c2)
                    | Lt_var (c1, v, s), Lt_cons c2 ->
                        let inverse_c = NumConst.div NumConst.one c2 in
                        Lt_var (NumConst.mul c1 inverse_c, v, s)
                    | Lt_cons _, Lt_var _
                    | Lt_var _, Lt_var _ -> raise (InvalidExpression (unable_to_convert_error_msg (Lazy.force str_outer_term_lazy)))
                ) combination
                in
                let _, _, term_list = reduce_terms_list coefs_list in term_list

            )
            in
            result

        | Typed_factor (factor, _) ->
            linear_coefs_of_typed_factor factor

    and linear_coefs_of_typed_factor = function
        | Typed_variable (variable_name, _, scope) ->

            (match scope with
            | Local -> [Lt_var (NumConst.one, variable_name, Local)]
            | Global ->
                let variable_kind = VariableInfo.variable_kind_of_variable_name variable_infos variable_name in
                (match variable_kind with
                | Constant_kind value -> [Lt_cons (AbstractValue.numconst_value value)]
                | Variable_kind _ -> [Lt_var (NumConst.one, variable_name, Global)]
                )
            )

        | Typed_constant (value, _) ->
            let numconst_value = ParsedValue.to_numconst_value value in
            [Lt_cons numconst_value]

		| Typed_unary_min (factor, _) ->
			let factors = linear_coefs_of_typed_factor factor in

            List.map (function
                | Lt_cons c -> Lt_cons (NumConst.neg c)
                | Lt_var (c, v, s) -> Lt_var (NumConst.neg c, v, s)
            ) factors

	    | Typed_expr (expr, _) ->
	        let coefs_list = linear_coefs_of_typed_arithmetic_expression expr in
	        (* Reduce sums of variables and constants *)
	        let _, _, term_list = reduce_terms_list coefs_list in term_list

        | factor ->
            raise (InvalidExpression (
                unable_to_convert_error_msg (string_of_typed_discrete_factor variable_infos (Var_type_discrete_number Var_type_discrete_rat) factor)
            ))
    in

    let coefs_list = linear_coefs_of_typed_arithmetic_expression expr in

    (* Reduce expression by computing coef for each variables and constant term *)
    let weighted_variables_without_duplicates, constants_sum, _ = reduce_terms_list coefs_list in

    (* Create linear term as sum of linear terms *)
    let linear_term = List.fold_right (fun (coef, variable_name, scope) acc ->

        let ir_var =
            match scope with
            | Local ->
                LinearConstraint.IR_Local_var variable_name
            | Global ->
                (* Get index of variable *)
                let variable_index = VariableInfo.index_of_variable_name variable_infos variable_name in
                LinearConstraint.IR_Var variable_index
        in

        (* Map to IR_Var or IR_Times *)
        let lt = if NumConst.equal coef NumConst.one then ir_var else LinearConstraint.IR_Times (coef, ir_var) in
        (* Append sum linear term *)
        LinearConstraint.IR_Plus (lt, acc)

    ) weighted_variables_without_duplicates (LinearConstraint.IR_Coef constants_sum) (* Add constant term to the end of the expression *)
    in
    linear_term

(* Convert typed boolean expression to a linear term, if possible, and reduce it *)
let linear_term_of_typed_boolean_expression variable_infos = function
    | Typed_discrete_bool_expr (Typed_arithmetic_expr (expr, _), _) ->
        linear_term_of_typed_arithmetic_expression variable_infos expr
    | expr ->
        raise (
            InvalidExpression (
                "Unable to convert expression `"
                ^ string_of_typed_boolean_expression variable_infos expr
                ^ "` to a linear expression. Expression linearity should be checked before."
            )
        )


let clock_update_of_typed_seq_code_bloc variable_infos is_only_resets seq_code_bloc =

    let rec clock_update_of_typed_seq_code_bloc_rec typed_seq_code_bloc =
        let clock_updates = List.map clock_update_of_typed_instruction typed_seq_code_bloc in
        List.concat clock_updates

    and clock_update_of_typed_instruction = function
        | Typed_assignment ((typed_scalar_or_index_update_type, expr), _) ->

            (* Check if updated variable is a clock *)
            (match typed_scalar_or_index_update_type with
            | Typed_scalar_update variable_name
            when VariableInfo.var_type_of_variable_or_constant_opt variable_infos variable_name = Some DiscreteType.Var_type_clock ->
                (* Get index of clock *)
                let clock_index = VariableInfo.index_of_variable_name variable_infos variable_name in
                (* Convert to update expression to a linear term *)
                [clock_index, linear_term_of_typed_boolean_expression variable_infos expr]
            | _ -> []
            )
        | Typed_for_loop (_, _, _, _, inner_bloc)
        | Typed_while_loop (_, inner_bloc) ->
            clock_update_of_typed_seq_code_bloc_rec inner_bloc
        | Typed_if (_, then_bloc, else_bloc_opt) ->
            let then_results = clock_update_of_typed_seq_code_bloc_rec then_bloc in
            let else_results =
                match else_bloc_opt with
                | Some else_bloc -> clock_update_of_typed_seq_code_bloc_rec else_bloc
                | None -> []
            in

            then_results @ else_results

        | Typed_local_decl _
        | Typed_instruction _ -> []

    in

    let converted_clock_updates = clock_update_of_typed_seq_code_bloc_rec seq_code_bloc in

    (* Differentiate between different kinds of clock updates *)
    (* Case 1: no update *)
    if converted_clock_updates = [] then (
        No_update
    )
    else (
        (* Case 2: resets only *)
        if is_only_resets then (
            (* Keep only the clock ids, not the linear terms *)
            let clocks_to_reset, _ = List.split converted_clock_updates in
            Resets (List.rev clocks_to_reset)
        ) else
            (* Case 3: complex with linear terms *)
            Updates (List.rev converted_clock_updates)
    )


let rec seq_code_bloc_of_typed_seq_code_bloc variable_infos typed_seq_code_bloc =

    let rec seq_code_bloc_of_typed_seq_code_bloc_rec typed_seq_code_bloc =
        List.map instruction_of_typed_instruction typed_seq_code_bloc

    and instruction_of_typed_instruction = function
        | Typed_local_decl (variable_name, discrete_type, typed_init_expr) ->
            Local_decl (
                variable_name,
                discrete_type,
                global_expression_of_typed_boolean_expression variable_infos typed_init_expr
            )

        | Typed_for_loop (variable_name, typed_from_expr, typed_to_expr, typed_loop_dir, typed_inner_bloc) ->
            For_loop (
                variable_name,
                int_arithmetic_expression_of_typed_arithmetic_expression variable_infos typed_from_expr,
                int_arithmetic_expression_of_typed_arithmetic_expression variable_infos typed_to_expr,
                loop_dir_of_typed_loop_dir typed_loop_dir,
                seq_code_bloc_of_typed_seq_code_bloc_rec typed_inner_bloc
            )

        | Typed_while_loop (typed_condition_expr, typed_inner_bloc) ->
            While_loop (
                bool_expression_of_typed_boolean_expression variable_infos typed_condition_expr,
                seq_code_bloc_of_typed_seq_code_bloc_rec typed_inner_bloc
            )

        | Typed_if (typed_condition_expr, typed_then_bloc, typed_else_bloc_opt) ->
            let condition_expr = bool_expression_of_typed_boolean_expression variable_infos typed_condition_expr in
            let then_bloc = seq_code_bloc_of_typed_seq_code_bloc_rec typed_then_bloc in

            let else_bloc_opt =
                match typed_else_bloc_opt with
                | Some typed_else_bloc ->
                    Some (seq_code_bloc_of_typed_seq_code_bloc_rec typed_else_bloc)
                | None -> None
            in

            If (
                condition_expr,
                then_bloc,
                else_bloc_opt
            )

        | Typed_assignment ((typed_scalar_or_index_update_type, typed_expr), scope) ->

            (match scope with
            | Ass_discrete_global ->
                Assignment (
                    (scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
                    global_expression_of_typed_boolean_expression variable_infos typed_expr)
                )
            | Ass_discrete_local ->
                Local_assignment (
                    (local_scalar_or_index_update_type_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
                    global_expression_of_typed_boolean_expression variable_infos typed_expr)
                )
            | Ass_clock ->
                Clock_assignment (
                    (clock_index_of_typed_scalar_or_index_update_type variable_infos typed_scalar_or_index_update_type,
                    linear_term_of_typed_boolean_expression variable_infos typed_expr)
                )
            )

        | Typed_instruction typed_expr ->
            Instruction (
                global_expression_of_typed_boolean_expression variable_infos typed_expr
            )

    in
    seq_code_bloc_of_typed_seq_code_bloc_rec typed_seq_code_bloc

let fun_definition_of_typed_fun_definition variable_infos (typed_fun_def : typed_fun_definition) : fun_definition =
    let typed_code_bloc, typed_return_expr_opt = typed_fun_def.body in

    (* Convert return expr if needed *)
    let converted_return_expr_opt =
        match typed_return_expr_opt with
        | Some typed_return_expr -> Some (global_expression_of_typed_boolean_expression variable_infos typed_return_expr)
        | None -> None
    in

    (* Search metadata of function to convert *)
    let meta = Hashtbl.find variable_infos.fun_meta typed_fun_def.name in
    {
        name = typed_fun_def.name;
        parameter_names = typed_fun_def.parameters;
        signature_constraint = FunctionSig.signature_constraint_of_signature typed_fun_def.signature;
        body = Fun_user (
            seq_code_bloc_of_typed_seq_code_bloc variable_infos typed_code_bloc,
            converted_return_expr_opt
        );
        side_effect = meta.side_effect
    }

end