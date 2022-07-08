open Automaton
open AbstractProperty
open Location

open DiscreteExpressions
open AbstractValue
open Exceptions

type functions_table = (variable_name, AbstractModel.fun_definition) Hashtbl.t

(* Record that contain context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Current local variables *)
    local_variables : variable_table;
}

(* Result returned on delayed update *)
type delayed_update_result =
    | Delayed_update_recorded
    | Delayed_update_already_updated of discrete_index


(* Create an evaluation context with a discrete valuation function and a local variables table *)
let [@inline] create_eval_context (discrete_valuation, discrete_setter) =
    { discrete_valuation = discrete_valuation; discrete_setter = discrete_setter; local_variables = Hashtbl.create 0 }

(* Create an evaluation context with a discrete valuation function and a local variables table *)
let [@inline] create_eval_context_opt = function
    | Some discrete_access ->
        Some (create_eval_context discrete_access)
    | None -> None

(* Get operator function from relop *)
let operator_of_relop = function
    | OP_L -> (<)
    | OP_LEQ -> (<=)
    | OP_EQ -> (=)
    | OP_NEQ -> (<>)
    | OP_GEQ -> (>=)
    | OP_G -> (>)

let list_hd_fail_message list_expr =
    let str_expr = DiscreteExpressions.string_of_list_expression (fun i -> "") list_expr in
    "Use of `list_hd` on empty list `" ^ str_expr ^ "`."

let list_tl_fail_message list_expr =
    let str_expr = DiscreteExpressions.string_of_list_expression (fun i -> "") list_expr in
    "Use of `list_tl` on empty list `" ^ str_expr ^ "`."

let stack_pop_fail_message stack_expr =
    let str_expr = DiscreteExpressions.string_of_stack_expression (fun i -> "") stack_expr in
    "Use of `stack_pop` on empty stack `" ^ str_expr ^ "`."

let stack_top_fail_message stack_expr =
    let str_expr = DiscreteExpressions.string_of_stack_expression (fun i -> "") stack_expr in
    "Use of `stack_top` on empty stack `" ^ str_expr ^ "`."

let queue_pop_fail_message queue_expr =
    let str_expr = DiscreteExpressions.string_of_queue_expression (fun i -> "") queue_expr in
    "Use of `queue_pop` on empty queue `" ^ str_expr ^ "`."

let queue_top_fail_message queue_expr =
    let str_expr = DiscreteExpressions.string_of_queue_expression (fun i -> "") queue_expr in
    "Use of `queue_top` on empty queue `" ^ str_expr ^ "`."

let sequence_operation_fail_message seq_expr str_func function_name =
    let str_expr = str_func (fun i -> "") seq_expr in
    "Use of `" ^ function_name ^ "` on empty queue `" ^ str_expr ^ "`."

(* Evaluate function on a sequence, raise an exception if sequence is empty *)
let eval_if_not_empty eval_length_function eval_function collection fail_message =
    if eval_length_function collection = 0 then
        raise (Empty_collection fail_message)
    else
        eval_function collection

let try_eval_list_hd seq (* fail_message *) = eval_if_not_empty List.length List.hd seq (* fail_message *)
let try_eval_list_tl seq (* fail_message *) = eval_if_not_empty List.length List.tl seq (* fail_message *)
let try_eval_stack_pop seq (* fail_message *) = eval_if_not_empty Stack.length Stack.pop seq (* fail_message *)
let try_eval_stack_top seq (* fail_message *) = eval_if_not_empty Stack.length Stack.top seq (* fail_message *)
let try_eval_queue_pop seq (* fail_message *) = eval_if_not_empty Queue.length Queue.pop seq (* fail_message *)
let try_eval_queue_top seq (* fail_message *) = eval_if_not_empty Queue.length Queue.top seq (* fail_message *)

(* Try evaluating a global variable value if an eval context is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_variable variable_index = function
    | Some eval_context -> eval_context.discrete_valuation variable_index
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

let try_eval_function function_name : functions_table option -> AbstractModel.fun_definition = function
    | Some functions_table -> Hashtbl.find functions_table function_name
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate an expression containing function calls without a functions table."))

(* Try evaluating a local variable value if an eval context is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_local_variable variable_name = function
    | Some eval_context -> Hashtbl.find eval_context.local_variables variable_name
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

(* Evaluate an expression *)
let rec eval_global_expression_with_context functions_table_opt eval_context_opt = function
    | Arithmetic_expression expr -> Abstract_scalar_value (eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt expr)
    | Bool_expression expr -> Abstract_scalar_value (Abstract_bool_value (eval_boolean_expression_with_context functions_table_opt eval_context_opt expr))
    | Binary_word_expression expr -> Abstract_scalar_value (Abstract_binary_word_value (eval_binary_word_expression_with_context functions_table_opt eval_context_opt expr))
    | Array_expression expr -> Abstract_container_value (Abstract_array_value (eval_array_expression_with_context functions_table_opt eval_context_opt expr))
    | List_expression expr -> Abstract_container_value (Abstract_list_value (eval_list_expression_with_context functions_table_opt eval_context_opt expr))
    | Stack_expression expr -> Abstract_container_value (Abstract_stack_value (eval_stack_expression_with_context functions_table_opt eval_context_opt expr))
    | Queue_expression expr -> Abstract_container_value (Abstract_queue_value (eval_queue_expression_with_context functions_table_opt eval_context_opt expr))

and eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt = function
    | Rational_arithmetic_expression expr ->
        Abstract_number_value (Abstract_rat_value (eval_rational_expression_with_context functions_table_opt eval_context_opt expr))
    | Int_arithmetic_expression expr ->
        Abstract_number_value (Abstract_int_value (eval_int_expression_with_context functions_table_opt eval_context_opt expr))
(*
and eval_new_arithmetic_expression_with_context functions_table_opt eval_context_opt = function
        | Sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> NumConst.add
                | Minus -> NumConst.sub
            in
            sum_function
                (eval_new_arithmetic_expression_with_context functions_table_opt eval_context_opt expr)
                (eval_new_term_with_context functions_table_opt eval_context_opt term)

        | Arithmetic_term term ->
            eval_new_term_with_context functions_table_opt eval_context_opt term

and eval_new_term_with_context functions_table_opt eval_context_opt = function
    | Product_quotient (term, factor, product_quotient) ->
        let a = eval_new_term_with_context functions_table_opt eval_context_opt term in
        let b = eval_new_factor_with_context functions_table_opt eval_context_opt factor in
        (match product_quotient with
        | Mul -> NumConst.mul a b
        | Div ->
            let numerator, denominator = a, b in
            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ NumConst.to_string numerator ^ " / " ^ NumConst.to_string denominator ^ ""))
            );
            (* Divide *)
            NumConst.div numerator denominator
        )

    | Arithmetic_factor factor ->
        eval_new_factor_with_context functions_table_opt eval_context_opt factor

and eval_new_factor_with_context functions_table_opt eval_context_opt = function
    | Arithmetic_global_variable variable_index ->
        numconst_value (try_eval_variable variable_index eval_context_opt)
    | Arithmetic_global_constant variable_value ->
        variable_value
    | Arithmetic_local_variable variable_name ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        numconst_value discrete_value
    | Arithmetic_nested_expression expr ->
        eval_rational_expression_with_context functions_table_opt eval_context_opt expr
    | Arithmetic_unary_min factor ->
        NumConst.neg (eval_rational_factor_with_context functions_table_opt eval_context_opt factor)

    | Arithmetic_pow (expr, exp) ->
        let x = eval_rational_expression_with_context functions_table_opt eval_context_opt expr in
        let exponent = eval_int_expression_with_context functions_table_opt eval_context_opt exp in
        NumConst.pow x exponent

    | Arithmetic_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        numconst_value value

    | Arithmetic_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        numconst_value result

*)

and eval_rational_expression_with_context functions_table_opt eval_context_opt = function
        | Rational_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> NumConst.add
                | Minus -> NumConst.sub
            in
            sum_function
                (eval_rational_expression_with_context functions_table_opt eval_context_opt expr)
                (eval_rational_term_with_context functions_table_opt eval_context_opt term)

        | Rational_term term ->
            eval_rational_term_with_context functions_table_opt eval_context_opt term

and eval_rational_term_with_context functions_table_opt eval_context_opt = function
    | Rational_product_quotient (term, factor, product_quotient) ->
        let a = eval_rational_term_with_context functions_table_opt eval_context_opt term in
        let b = eval_rational_factor_with_context functions_table_opt eval_context_opt factor in
        (match product_quotient with
        | Mul -> NumConst.mul a b
        | Div ->
            let numerator, denominator = a, b in
            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ NumConst.to_string numerator ^ " / " ^ NumConst.to_string denominator ^ ""))
            );
            (* Divide *)
            NumConst.div numerator denominator
        )

    | Rational_factor factor ->
        eval_rational_factor_with_context functions_table_opt eval_context_opt factor

and eval_rational_factor_with_context functions_table_opt eval_context_opt = function
    | Rational_variable variable_index ->
        numconst_value (try_eval_variable variable_index eval_context_opt)
    | Rational_constant variable_value ->
        variable_value
    | Rational_local_variable variable_name ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        numconst_value discrete_value
    | Rational_expression expr ->
        eval_rational_expression_with_context functions_table_opt eval_context_opt expr
    | Rational_unary_min factor ->
        NumConst.neg (eval_rational_factor_with_context functions_table_opt eval_context_opt factor)

    | Rational_pow (expr, exp) ->
        let x = eval_rational_expression_with_context functions_table_opt eval_context_opt expr in
        let exponent = eval_int_expression_with_context functions_table_opt eval_context_opt exp in
        NumConst.pow x exponent

    | Rational_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        numconst_value value

    | Rational_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        numconst_value result

and eval_int_expression_with_context functions_table_opt eval_context_opt (* expr *) =
    let rec eval_int_expression_with_context_rec = function
        | Int_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> Int32.add
                | Minus -> Int32.sub
            in
            sum_function
                (eval_int_expression_with_context_rec expr)
                (eval_int_term_with_context term)

        | Int_term term ->
            eval_int_term_with_context term

    and eval_int_term_with_context = function
        | Int_product_quotient (term, factor, product_quotient) ->
            let a = eval_int_term_with_context term in
            let b = eval_int_factor_with_context factor in
            (match product_quotient with
            | Mul -> Int32.mul a b
            | Div ->
                let numerator, denominator = a, b in

                (* Check for 0-denominator *)
                if Int32.equal denominator Int32.zero then (
                    raise (Exceptions.Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ ""))
                );

                (* Check for non-int division *)
                if OCamlUtilities.modulo numerator denominator <> Int32.zero then
                    ImitatorUtilities.print_warning (
                        "Non-integer division of type int was spotted! This means that an int variable was rounded down to the nearest integer instead of a rational result (`"
                        ^ Int32.to_string numerator ^ " / " ^ Int32.to_string denominator
                        ^ "`). The overall result may now be invalid."
                    );

                (* Divide *)
                Int32.div numerator denominator
            )

        | Int_factor factor ->
            eval_int_factor_with_context factor

    and eval_int_factor_with_context = function
        | Int_variable variable_index ->
            int_value (try_eval_variable variable_index eval_context_opt)
        | Int_constant variable_value ->
            variable_value;
        | Int_local_variable variable_name ->
                        (* Variable should exist as it was checked before *)
            let discrete_value = try_eval_local_variable variable_name eval_context_opt in
            int_value discrete_value

        | Int_expression expr ->
            eval_int_expression_with_context_rec expr
        | Int_unary_min factor ->
            Int32.neg (eval_int_factor_with_context factor)
        | Int_pow (expr, exp) ->
            let x = eval_int_expression_with_context_rec expr in
            let exponent = eval_int_expression_with_context_rec exp in
            OCamlUtilities.pow x exponent

        | Int_array_access (access_type, index_expr) ->
            let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
            int_value value

        | Int_function_call (function_name, param_names, expr_args) ->
            let fun_def = try_eval_function function_name functions_table_opt in
            let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
            int_value result

    in
    eval_int_expression_with_context_rec

(** Check if a boolean expression is satisfied *)
and eval_boolean_expression_with_context functions_table_opt eval_context_opt = function
    | True_bool -> true
    | False_bool -> false
    | Conj_dis (l_expr, r_expr, conj_dis) ->
        let conj_dis_function =
            match conj_dis with
            | And -> (&&)
            | Or -> (||)
        in
        conj_dis_function
            (eval_boolean_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_boolean_expression_with_context functions_table_opt eval_context_opt r_expr)

    | Discrete_boolean_expression dbe -> eval_discrete_boolean_expression_with_context functions_table_opt eval_context_opt dbe

(** Check if a discrete boolean expression is satisfied *)
and eval_discrete_boolean_expression_with_context functions_table_opt eval_context_opt = function
    | Bool_variable variable_index ->
        bool_value (try_eval_variable variable_index eval_context_opt)
    | Bool_constant value ->
        value
    | Bool_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        bool_value discrete_value
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* We just have to create a Rational_comparison and a Int_comparison to solve this *)
    | Arithmetic_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (eval_discrete_boolean_expression_with_context functions_table_opt eval_context_opt l_expr)
             (eval_discrete_boolean_expression_with_context functions_table_opt eval_context_opt r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_binary_word_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_binary_word_expression_with_context functions_table_opt eval_context_opt r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_array_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_array_expression_with_context functions_table_opt eval_context_opt r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_list_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_list_expression_with_context functions_table_opt eval_context_opt r_expr)
    | Stack_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_stack_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_stack_expression_with_context functions_table_opt eval_context_opt r_expr)
    | Queue_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_queue_expression_with_context functions_table_opt eval_context_opt l_expr)
            (eval_queue_expression_with_context functions_table_opt eval_context_opt r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt  discrete_arithmetic_expression_1 in
            (eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (eval_discrete_arithmetic_expression_with_context functions_table_opt eval_context_opt discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        eval_boolean_expression_with_context functions_table_opt eval_context_opt boolean_expression
    | Not_bool b ->
        not (eval_boolean_expression_with_context functions_table_opt eval_context_opt b) (* negation *)
    | Bool_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        bool_value value

    | Bool_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        bool_value result

and eval_binary_word_expression_with_context functions_table_opt eval_context_opt = function
    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        binary_word_value (try_eval_variable variable_index eval_context_opt)
    | Binary_word_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        binary_word_value discrete_value

    | Binary_word_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        binary_word_value value

    | Binary_word_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        binary_word_value result

and eval_array_expression_with_context functions_table_opt eval_context_opt = function
    | Literal_array array ->
        Array.map (fun expr -> eval_global_expression_with_context functions_table_opt eval_context_opt expr) array
    | Array_variable variable_index ->
        array_value (try_eval_variable variable_index eval_context_opt)
    | Array_constant values ->
        values
    | Array_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        array_value discrete_value

    | Array_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        array_value value

    | Array_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        array_value result

and eval_list_expression_with_context functions_table_opt eval_context_opt = function
    | Literal_list list ->
        List.map (fun expr -> eval_global_expression_with_context functions_table_opt eval_context_opt expr) list
    | List_variable variable_index ->
        list_value (try_eval_variable variable_index eval_context_opt)
    | List_constant values ->
        values
    | List_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        list_value discrete_value
    | List_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        list_value value

    | List_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        list_value result

and eval_stack_expression_with_context functions_table_opt eval_context_opt = function
    | Literal_stack -> Stack.create ()

    | Stack_variable variable_index ->
        stack_value (try_eval_variable variable_index eval_context_opt)

    | Stack_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        stack_value discrete_value

    | Stack_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        stack_value value

    | Stack_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        stack_value result

and eval_queue_expression_with_context functions_table_opt eval_context_opt = function
    | Literal_queue -> Queue.create ()

    | Queue_variable variable_index ->
        queue_value (try_eval_variable variable_index eval_context_opt)

    | Queue_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        queue_value discrete_value

    | Queue_array_access (access_type, index_expr) ->
        let value = get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr access_type in
        queue_value value

    | Queue_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name functions_table_opt in
        let result = eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_def.body in
        queue_value result

and get_array_value_at_with_context functions_table_opt eval_context_opt array_expr index_expr =

    let values = eval_array_expression_with_context functions_table_opt eval_context_opt array_expr in
    let index = eval_int_expression_with_context functions_table_opt eval_context_opt index_expr in
    let int_index = Int32.to_int index in

    if int_index >= Array.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep ", " (Array.map (fun value -> AbstractValue.string_of_value value) values) in
        raise (Out_of_bound ("Array index out of range: `" ^ str_index ^ "` for array " ^ str_values))
    );

    Array.get values int_index

and get_list_value_at_with_context functions_table_opt eval_context_opt array_expr index_expr =

    let values = eval_list_expression_with_context functions_table_opt eval_context_opt array_expr in
    let index = eval_int_expression_with_context functions_table_opt eval_context_opt index_expr in
    let int_index = Int32.to_int index in

    if int_index >= List.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (fun value -> AbstractValue.string_of_value value) values) in
        raise (Out_of_bound ("List index out of range: `" ^ str_index ^ "` for list " ^ str_values))
    );

    List.nth values int_index

and get_expression_access_value_with_context functions_table_opt eval_context_opt index_expr = function
    | Expression_array_access array_expr ->
        get_array_value_at_with_context functions_table_opt eval_context_opt array_expr index_expr
    | Expression_list_access list_expr ->
        get_list_value_at_with_context functions_table_opt eval_context_opt list_expr index_expr

and eval_inline_function_with_context functions_table_opt eval_context_opt param_names expr_args fun_decl =

    (* Compute arguments values *)
    let arg_values = List.map (eval_global_expression_with_context functions_table_opt eval_context_opt) expr_args in
    (* Associate each parameter with their value *)
    let param_names_with_arg_values = List.combine param_names arg_values in
    (* Get or create local variables table *)
    let local_variables = OCamlUtilities.hashtbl_of_tuples param_names_with_arg_values in

    (* Update (optional) context *)
    let new_eval_context_opt =
        match eval_context_opt with
        | Some eval_context -> Some {eval_context with local_variables = local_variables }
        | None -> None
    in

    (* Eval function body *)
    let rec eval_fun_body_with_context eval_context_opt = function
        | Fun_builtin builtin_f ->
            (* Execute built-in function given argument values *)
            (* TODO benjamin IMPLEMENT here str_expr *)
            builtin_f "TODO benjamin fill here" arg_values

        | Fun_local_decl (variable_name, _, expr, next_expr) ->

            (* TODO benjamin IMPLEMENT HERE group Fun_local_decl and Fun_instruction in Fun_user_function to make this checking one time only *)
            let eval_context =
                match eval_context_opt with
                | Some eval_context -> eval_context
                | None -> raise (InternalError
                    "Trying to evaluate a function without `eval_context`.
                    Only constant expression can be evaluated without context
                    and constant expression can't contains functions calls.
                    Some checks may failed before."
                )
            in

            let value = eval_global_expression_with_context functions_table_opt eval_context_opt expr in
            Hashtbl.add eval_context.local_variables variable_name value;

            eval_fun_body_with_context eval_context_opt next_expr

        | Fun_instruction (normal_update, next_expr) ->

            let eval_context =
                match eval_context_opt with
                | Some eval_context -> eval_context
                | None -> raise (InternalError
                    "Trying to evaluate a function without `eval_context`.
                    Only constant expression can be evaluated without context
                    and constant expression can't contains functions calls.
                    Some checks may failed before."
                )
            in

            direct_update_with_context functions_table_opt eval_context normal_update;
            eval_fun_body_with_context eval_context_opt next_expr

        | Fun_expr expr ->
            eval_global_expression_with_context functions_table_opt eval_context_opt expr
    in
    eval_fun_body_with_context new_eval_context_opt fun_decl


and compute_update_value_opt_with_context functions_table_opt eval_context (update_type, expr) =

    let rec discrete_index_of_parsed_scalar_or_index_update_type = function
        | Scalar_update discrete_index -> discrete_index
        | Indexed_update (scalar_or_index_update_type, _) ->
            discrete_index_of_parsed_scalar_or_index_update_type scalar_or_index_update_type
    in
    match update_type with
    | Variable_update scalar_or_index_update_type ->
        (* Get discrete index *)
        let discrete_index = discrete_index_of_parsed_scalar_or_index_update_type scalar_or_index_update_type in
        (* Get value before update as old value *)
        let old_value = eval_context.discrete_valuation discrete_index in

        (* Compute its new value *)
        let new_value = eval_global_expression_with_context functions_table_opt (Some eval_context) expr in
        let new_value = pack_value functions_table_opt (Some eval_context) old_value new_value update_type in

        Some (discrete_index, new_value)
    | Void_update ->
        let _ = eval_global_expression_with_context functions_table_opt (Some eval_context) expr in None

and direct_update_with_context functions_table_opt eval_context update =

    let discrete_index_new_value_pair_opt = compute_update_value_opt_with_context functions_table_opt eval_context update in
    match discrete_index_new_value_pair_opt with
    | None -> ()
    | Some (discrete_index, new_value) ->
        (* Direct update ! *)
        eval_context.discrete_setter discrete_index new_value

and delayed_update_with_context functions_table_opt eval_context updated_discrete update =

    let discrete_index_new_value_pair_opt = compute_update_value_opt_with_context functions_table_opt eval_context update in
    match discrete_index_new_value_pair_opt with
    | None ->
        Delayed_update_recorded (* update ok *)
    | Some (discrete_index, new_value) ->
        (* Check if already updated *)
        if Hashtbl.mem updated_discrete discrete_index then (
            (* Find its value *)
            let previous_new_value = Hashtbl.find updated_discrete discrete_index in
            (* Compare with the new one *)
            if AbstractValue.neq previous_new_value new_value then (
                (* If different, return already update result *)
                Delayed_update_already_updated discrete_index
            ) else
                Delayed_update_recorded

        ) else (
            (* Else keep it in memory for update *)
            Hashtbl.add updated_discrete discrete_index new_value;
            Delayed_update_recorded
        )



(* Wrap a scalar value to an array value according to the modified index of an old value *)
(* For example: `old_value[0] = 1` with old value = [0, 1], would wrap new_value into an array `new_value = [1, 1]` *)
(* This function is used to assign an element of an array at a given index *)
(* a = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] *)
(* a[1][1][0] = 0 *)
(* new_value = [[[1, 2], [3, 4]], [[5, 6], [0, 8]]] *)
(* a[1] = [[5, 6], [7, 8]] *)
(* a[1][1] = [7, 8] *)
(* a[1][1][0] = 7 *)
and pack_value (* variable_names *) functions_table_opt eval_context_opt old_value new_value parsed_update_type =

    let rec pack_value_scalar_or_index_update_type = function
        | Scalar_update discrete_index -> old_value, [||], None
        | Indexed_update (inner_scalar_or_index_update_type, index_expr) ->

            let old_value, _, _ = pack_value_scalar_or_index_update_type inner_scalar_or_index_update_type in

            (* Compute index *)
            let index = Int32.to_int (eval_int_expression_with_context functions_table_opt eval_context_opt index_expr) in
(*            ImitatorUtilities.print_message Verbose_standard ("access index: " ^ string_of_int index ^ "for " ^ string_of_value old_value);*)
            (* Get inner array of discrete value of old value *)
            let old_array = array_value old_value in

            (* Check bounds *)
            if index >= Array.length old_array || index < 0 then (
                (* TODO benjamin IMPLEMENT repair that *)
                let str_parsed_update_type = "" in
(*                let str_parsed_update_type = DiscreteExpressions.string_of_update_type variable_names parsed_update_type in*)
                raise (Out_of_bound ("Array index out of range: `" ^ str_parsed_update_type ^ "`"))
            );

            (* Get element at given index *)
            let unpacked_old_array = old_array.(index) in
(*            ImitatorUtilities.print_message Verbose_standard ("unpacked old array: " ^ AbstractValue.string_of_value unpacked_old_array);*)
            unpacked_old_array, old_array, Some index
    in
    let pack_value_rec = function
        | Variable_update scalar_or_index_update_type ->
            pack_value_scalar_or_index_update_type scalar_or_index_update_type
        | Void_update -> old_value, [||], None
    in

    let unpacked_old_array, old_array, some_index = pack_value_rec parsed_update_type in
    match some_index with
    | Some index ->
        old_array.(index) <- new_value;
(*        ImitatorUtilities.print_message Verbose_standard ("packed new value is: " ^ AbstractValue.string_of_value old_value);*)
        old_value
    | None -> new_value


(* Try to evaluate a constant global expression, if expression isn't constant, it raise an error *)
let try_eval_constant_global_expression functions_table_opt = eval_global_expression_with_context functions_table_opt None
(* Try to evaluate a constant rational term, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_term functions_table_opt = eval_rational_term_with_context functions_table_opt None
(* Try to evaluate a constant rational factor, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_factor functions_table_opt = eval_rational_factor_with_context functions_table_opt None

let direct_update functions_table_opt discrete_access = direct_update_with_context functions_table_opt (create_eval_context discrete_access)
let delayed_update functions_table_opt discrete_access = delayed_update_with_context functions_table_opt (create_eval_context discrete_access)

(* Try to evaluate a constant global expression, if expression isn't constant, it return None *)
let eval_constant_global_expression_opt functions_table_opt expr = try Some (try_eval_constant_global_expression functions_table_opt expr) with _ -> None
(* Try to evaluate a constant rational term, if expression isn't constant, it return None *)
let eval_constant_rational_term_opt functions_table_opt expr = try Some (try_eval_constant_rational_term functions_table_opt expr) with _ -> None
(* Try to evaluate a constant rational factor, if expression isn't constant, it return None *)
let eval_constant_rational_factor_opt functions_table_opt expr = try Some (try_eval_constant_rational_factor functions_table_opt expr) with _ -> None

(**)
let eval_global_expression functions_table_opt discrete_access_opt = eval_global_expression_with_context functions_table_opt (create_eval_context_opt discrete_access_opt)
(**)
let eval_boolean_expression functions_table_opt discrete_access_opt = eval_boolean_expression_with_context functions_table_opt (create_eval_context_opt discrete_access_opt)
(**)
let eval_discrete_boolean_expression functions_table_opt discrete_access_opt = eval_discrete_boolean_expression_with_context functions_table_opt (create_eval_context_opt discrete_access_opt)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint functions_table_opt discrete_access =
  List.for_all (eval_discrete_boolean_expression functions_table_opt (Some discrete_access))

(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(***TODO/NOTE: Might have been nicer to convert the acceptance condition during the ModelConverter phase :-/ ***)
let match_state_predicate functions_table_opt discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location =

    (* Match loc predicate *)
    let match_loc_predicate = function
        | Loc_predicate_EQ (automaton_index, location_index) ->
            get_location global_location automaton_index = location_index
        | Loc_predicate_NEQ (automaton_index, location_index) ->
            get_location global_location automaton_index <> location_index
    in

    (* Match simple predicate *)
    let match_simple_predicate = function
        | State_predicate_discrete_boolean_expression expr ->
            eval_discrete_boolean_expression functions_table_opt (Some discrete_access) expr

        | Loc_predicate loc_predicate ->
            match_loc_predicate loc_predicate

        | State_predicate_accepting ->
            is_accepting locations_acceptance_condition global_location

        | State_predicate_true -> true
        | State_predicate_false -> false
    in

    (* Match state predicate *)
    let rec match_state_predicate = function
        | State_predicate_OR (l_predicate, r_predicate) ->
            match_state_predicate l_predicate
            || match_state_predicate r_predicate

        | State_predicate_term predicate_term ->
            match_state_predicate_term predicate_term

    (* Match state predicate term *)
    and match_state_predicate_term = function
        | State_predicate_term_AND (l_predicate_term, r_predicate_term) ->
            match_state_predicate_term l_predicate_term
            && match_state_predicate_term r_predicate_term

        | State_predicate_factor predicate_factor ->
            match_state_predicate_factor predicate_factor

    (* Match state predicate factor *)
    and match_state_predicate_factor = function
        | State_predicate_factor_NOT predicate_factor ->
            not (match_state_predicate_factor predicate_factor)
        | Simple_predicate simple_predicate ->
            match_simple_predicate simple_predicate
        | State_predicate state_predicate ->
            match_state_predicate state_predicate
    in
    match_state_predicate

let bad_arguments_message str_expr =
    "Bad arguments on `" ^ str_expr ^ "`. Expected types or number of arguments doesn't match with actual."

let eval_pow str_expr = function
    | (Abstract_scalar_value (Abstract_number_value (Abstract_rat_value x))) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value exponent)) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_rat_value (NumConst.pow x exponent)))
    | Abstract_scalar_value (Abstract_number_value (Abstract_int_value x)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value exponent)) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (OCamlUtilities.pow x exponent)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_rational_of_int str_expr = function
    | Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        ImitatorUtilities.print_warning
            "Conversion of an int expression to a rational expression
            may cause overflow if your platform doesn't manage `int` as an exact 32 bits integer.";
        Abstract_scalar_value (Abstract_number_value (Abstract_rat_value (NumConst.numconst_of_int (Int32.to_int i))))

    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_shift_left str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.shift_left b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_shift_right str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.shift_right b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_fill_left str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.fill_left b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_fill_right str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.fill_right b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_and str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (l_binary_word)) :: Abstract_scalar_value (Abstract_binary_word_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.log_and l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_or str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (l_binary_word)) :: Abstract_scalar_value (Abstract_binary_word_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.log_or l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_xor str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (l_binary_word)) :: Abstract_scalar_value (Abstract_binary_word_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.log_xor l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_not str_expr = function
    | Abstract_scalar_value (Abstract_binary_word_value (binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.log_not binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_array_append str_expr = function
    | Abstract_container_value (Abstract_array_value l_array) :: Abstract_container_value (Abstract_array_value r_array) :: _ ->
        Abstract_container_value (Abstract_array_value (Array.append l_array r_array))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_array_mem str_expr = function
    | e :: Abstract_container_value (Abstract_array_value a) :: _ ->
        Abstract_scalar_value (Abstract_bool_value (Array.mem e a))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_array_length str_expr = function
    | Abstract_container_value (Abstract_array_value a) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (Int32.of_int (Array.length a))))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_is_empty str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        Abstract_scalar_value (Abstract_bool_value (List.length l = 0))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_cons str_expr = function
    | e :: Abstract_container_value (Abstract_list_value l) :: _ ->
        Abstract_container_value (Abstract_list_value (List.cons e l))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_hd str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        let fail_message = "Use of `list_hd` on empty list `" ^ str_expr ^ "`." in
        try_eval_list_hd l fail_message
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_tl str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        let fail_message = "Use of `list_hd` on empty list `" ^ str_expr ^ "`." in
        Abstract_container_value (Abstract_list_value (try_eval_list_tl l fail_message))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_rev str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        Abstract_container_value (Abstract_list_value (List.rev l))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_mem str_expr = function
    | e :: Abstract_container_value (Abstract_list_value l) :: _ ->
        Abstract_scalar_value (Abstract_bool_value (List.mem e l))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_length str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (Int32.of_int (List.length l))))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_push str_expr = function
    | e :: Abstract_container_value (Abstract_stack_value s) :: _ ->
        Stack.push e s; Abstract_container_value (Abstract_stack_value s)
    | e :: Abstract_container_value (Abstract_queue_value s) :: _ ->
        Queue.push e s; Abstract_container_value (Abstract_queue_value s)
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_pop str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        let fail_message = "Use of `stack_pop` on empty stack `" ^ str_expr ^ "`." in
        try_eval_stack_pop s fail_message
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        let fail_message = "Use of `queue_pop` on empty queue `" ^ str_expr ^ "`." in
        try_eval_queue_pop s fail_message
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_top str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        let fail_message = "Use of `stack_top` on empty stack `" ^ str_expr ^ "`." in
        try_eval_stack_top s fail_message
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        let fail_message = "Use of `queue_top` on empty queue `" ^ str_expr ^ "`." in
        try_eval_queue_top s fail_message
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_clear str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        Stack.clear s; Abstract_container_value (Abstract_stack_value s)
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        Queue.clear s; Abstract_container_value (Abstract_queue_value s)
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_is_empty str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        Abstract_scalar_value (Abstract_bool_value (Stack.is_empty s))
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        Abstract_scalar_value (Abstract_bool_value (Queue.is_empty s))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_length str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (Int32.of_int (Stack.length s))))
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (Int32.of_int (Queue.length s))))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

(* Tricky function to know if an expression is constant *)
let is_global_expression_constant functions_table_opt expr =
    try (
        let _ = try_eval_constant_global_expression functions_table_opt expr in
        true
    )
    with _ -> false