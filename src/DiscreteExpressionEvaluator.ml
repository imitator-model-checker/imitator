open Automaton
open AbstractProperty
open Location

open DiscreteExpressions
open DiscreteValue
open Exceptions

type functions_table = (variable_name, AbstractModel.fun_definition) Hashtbl.t

(* Record that contain context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Function table (current model) *)
    functions_table : functions_table;
    (* Current local variables *)
    local_variables : variable_table;
}

(* Result returned on delayed update *)
type delayed_update_result =
    | Delayed_update_recorded
    | Delayed_update_already_updated of discrete_index


(* Create an evaluation context with a discrete valuation function and a local variables table *)
let [@inline] create_eval_context functions_table (discrete_valuation, discrete_setter) =
    { discrete_valuation = discrete_valuation; discrete_setter = discrete_setter; functions_table = functions_table; local_variables = Hashtbl.create 0 }

(* Create an evaluation context with a discrete valuation function and a local variables table *)
let [@inline] create_eval_context_opt functions_table = function
    | Some discrete_access ->
        Some (create_eval_context functions_table discrete_access)
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

let try_eval_function function_name = function
    | Some eval_context -> Hashtbl.find eval_context.functions_table function_name
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without an eval context."))

(* Try evaluating a local variable value if an eval context is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_local_variable variable_name = function
    | Some eval_context -> Hashtbl.find eval_context.local_variables variable_name
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

(* Evaluate an expression *)
let rec eval_global_expression_with_context eval_context_opt = function
    | Arithmetic_expression expr -> eval_discrete_arithmetic_expression_with_context eval_context_opt expr
    | Bool_expression expr -> Bool_value (eval_boolean_expression_with_context eval_context_opt expr)
    | Binary_word_expression expr -> Binary_word_value (eval_binary_word_expression_with_context eval_context_opt expr)
    | Array_expression expr -> Array_value (eval_array_expression_with_context eval_context_opt expr)
    | List_expression expr -> List_value (eval_list_expression_with_context eval_context_opt expr)
    | Stack_expression expr -> Stack_value (eval_stack_expression_with_context eval_context_opt expr)
    | Queue_expression expr -> Queue_value (eval_queue_expression_with_context eval_context_opt expr)

and eval_discrete_arithmetic_expression_with_context eval_context_opt = function
    | Rational_arithmetic_expression expr ->
        Rational_value (eval_rational_expression_with_context eval_context_opt expr)
    | Int_arithmetic_expression expr ->
        Int_value (eval_int_expression_with_context eval_context_opt expr)

and eval_rational_expression_with_context eval_context_opt = function
        | Rational_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> NumConst.add
                | Minus -> NumConst.sub
            in
            sum_function
                (eval_rational_expression_with_context eval_context_opt expr)
                (eval_rational_term_with_context eval_context_opt term)

        | Rational_term term ->
            eval_rational_term_with_context eval_context_opt term

and eval_rational_term_with_context eval_context_opt = function
    | Rational_product_quotient (term, factor, product_quotient) ->
        let a = eval_rational_term_with_context eval_context_opt term in
        let b = eval_rational_factor_with_context eval_context_opt factor in
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
        eval_rational_factor_with_context eval_context_opt factor

and eval_rational_factor_with_context eval_context_opt = function
    | Rational_variable variable_index ->
        numconst_value (try_eval_variable variable_index eval_context_opt)
    | Rational_constant variable_value ->
        variable_value
    | Rational_local_variable variable_name ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        numconst_value discrete_value
    | Rational_expression expr ->
        eval_rational_expression_with_context eval_context_opt expr
    | Rational_unary_min factor ->
        NumConst.neg (eval_rational_factor_with_context eval_context_opt factor)

    | Rational_of_int expr ->
        ImitatorUtilities.print_warning
            "Conversion of an int expression to a rational expression may cause overflow if your platform does not manage `int` as an exact 32-bit integer.";
        NumConst.numconst_of_int (Int32.to_int (eval_int_expression_with_context eval_context_opt expr))
    | Rational_pow (expr, exp) ->
        let x = eval_rational_expression_with_context eval_context_opt expr in
        let exponent = eval_int_expression_with_context eval_context_opt exp in
        NumConst.pow x exponent

    | Rational_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        numconst_value value

    | Rational_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        numconst_value result

and eval_sequence_function_with_context eval_context_opt = function
    | Array_access (access_type, index_expr) ->
        get_expression_access_value_with_context eval_context_opt index_expr access_type

    | List_hd list_expr ->
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        let fail_message = list_hd_fail_message list_expr in
        try_eval_list_hd list fail_message

    | Stack_pop stack_expr ->
        let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
        let fail_message = stack_pop_fail_message stack_expr in
        try_eval_stack_pop stack fail_message

    | Stack_top stack_expr ->
        let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
        let fail_message = stack_top_fail_message stack_expr in
        try_eval_stack_top stack fail_message

    | Queue_pop queue_expr ->
        let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
        let fail_message = queue_pop_fail_message queue_expr in
        try_eval_queue_pop queue fail_message

    | Queue_top queue_expr ->
        let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
        let fail_message = queue_top_fail_message queue_expr in
        try_eval_queue_top queue fail_message

and eval_int_expression_with_context eval_context_opt (* expr *) =
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

        | Int_sequence_function func ->
            let value = eval_sequence_function_with_context eval_context_opt func in
            int_value value
        | Array_length array_expr ->
            let array = eval_array_expression_with_context eval_context_opt array_expr in
            Int32.of_int (Array.length array)
        | List_length list_expr ->
            let list = eval_list_expression_with_context eval_context_opt list_expr in
            Int32.of_int (List.length list)
        | Stack_length stack_expr ->
            let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
            Int32.of_int (Stack.length stack)
        | Queue_length queue_expr ->
            let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
            Int32.of_int (Queue.length queue)
        | Int_function_call (function_name, param_names, expr_args) ->
            let fun_def = try_eval_function function_name eval_context_opt in
            let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
            int_value result

    in
    eval_int_expression_with_context_rec

(** Check if a boolean expression is satisfied *)
and eval_boolean_expression_with_context eval_context_opt = function
    | True_bool -> true
    | False_bool -> false
    | Conj_dis (l_expr, r_expr, conj_dis) ->
        let conj_dis_function =
            match conj_dis with
            | And -> (&&)
            | Or -> (||)
        in
        conj_dis_function
            (eval_boolean_expression_with_context eval_context_opt l_expr)
            (eval_boolean_expression_with_context eval_context_opt r_expr)

    | Discrete_boolean_expression dbe -> eval_discrete_boolean_expression_with_context eval_context_opt dbe

(** Check if a discrete boolean expression is satisfied *)
and eval_discrete_boolean_expression_with_context eval_context_opt = function
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
            (eval_discrete_arithmetic_expression_with_context eval_context_opt l_expr)
            (eval_discrete_arithmetic_expression_with_context eval_context_opt r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (eval_discrete_boolean_expression_with_context eval_context_opt l_expr)
             (eval_discrete_boolean_expression_with_context eval_context_opt r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_binary_word_expression_with_context eval_context_opt l_expr)
            (eval_binary_word_expression_with_context eval_context_opt r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_array_expression_with_context eval_context_opt l_expr)
            (eval_array_expression_with_context eval_context_opt r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_list_expression_with_context eval_context_opt l_expr)
            (eval_list_expression_with_context eval_context_opt r_expr)
    | Stack_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_stack_expression_with_context eval_context_opt l_expr)
            (eval_stack_expression_with_context eval_context_opt r_expr)
    | Queue_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_queue_expression_with_context eval_context_opt l_expr)
            (eval_queue_expression_with_context eval_context_opt r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = eval_discrete_arithmetic_expression_with_context eval_context_opt  discrete_arithmetic_expression_1 in
            (eval_discrete_arithmetic_expression_with_context eval_context_opt discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (eval_discrete_arithmetic_expression_with_context eval_context_opt discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        eval_boolean_expression_with_context eval_context_opt boolean_expression
    | Not_bool b ->
        not (eval_boolean_expression_with_context eval_context_opt b) (* negation *)
    | Bool_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        bool_value value
    | List_mem (expr, list_expr) ->
        let value = eval_global_expression_with_context eval_context_opt expr in
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        List.mem value list
    | Array_mem (expr, array_expr) ->
        let value = eval_global_expression_with_context eval_context_opt expr in
        let array = eval_array_expression_with_context eval_context_opt array_expr in
        Array.mem value array
    | List_is_empty list_expr ->
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        List.length list = 0
    | Stack_is_empty stack_expr ->
        let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
        Stack.is_empty stack
    | Queue_is_empty queue_expr ->
        let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
        Queue.is_empty queue

    | Bool_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        bool_value result

and eval_binary_word_expression_with_context eval_context_opt = function
    | Logical_shift_left (binary_word, expr, _) ->
        BinaryWord.shift_left
            (eval_binary_word_expression_with_context eval_context_opt binary_word)
            (Int32.to_int (eval_int_expression_with_context eval_context_opt expr))
    | Logical_shift_right (binary_word, expr, _) ->
        BinaryWord.shift_right
            (eval_binary_word_expression_with_context eval_context_opt binary_word)
            (Int32.to_int (eval_int_expression_with_context eval_context_opt expr))
    | Logical_fill_left (binary_word, expr, _) ->
        BinaryWord.fill_left
            (eval_binary_word_expression_with_context eval_context_opt binary_word)
            (Int32.to_int (eval_int_expression_with_context eval_context_opt expr))
    | Logical_fill_right (binary_word, expr, _) ->
        BinaryWord.fill_right
            (eval_binary_word_expression_with_context eval_context_opt binary_word)
            (Int32.to_int (eval_int_expression_with_context eval_context_opt expr))
    | Logical_and (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_and
            (eval_binary_word_expression_with_context eval_context_opt l_binary_word)
            (eval_binary_word_expression_with_context eval_context_opt r_binary_word)
    | Logical_or (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_or
            (eval_binary_word_expression_with_context eval_context_opt l_binary_word)
            (eval_binary_word_expression_with_context eval_context_opt r_binary_word)
    | Logical_xor (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_xor
            (eval_binary_word_expression_with_context eval_context_opt l_binary_word)
            (eval_binary_word_expression_with_context eval_context_opt r_binary_word)
    | Logical_not (binary_word, _) ->
        BinaryWord.log_not
            (eval_binary_word_expression_with_context eval_context_opt binary_word)

    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        binary_word_value (try_eval_variable variable_index eval_context_opt)
    | Binary_word_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        binary_word_value discrete_value

    | Binary_word_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        binary_word_value value

    | Binary_word_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        binary_word_value result

and eval_array_expression_with_context eval_context_opt = function
    | Literal_array array ->
        Array.map (fun expr -> eval_global_expression_with_context eval_context_opt expr) array
    | Array_variable variable_index ->
        array_value (try_eval_variable variable_index eval_context_opt)
    | Array_constant values ->
        values
    | Array_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        array_value discrete_value

    | Array_concat (array_expr_0, array_expr_1) ->
        let array_0 = eval_array_expression_with_context eval_context_opt array_expr_0 in
        let array_1 = eval_array_expression_with_context eval_context_opt array_expr_1 in
        Array.append array_0 array_1
    | Array_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        array_value value

    | Array_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        array_value result

and eval_list_expression_with_context eval_context_opt = function
    | Literal_list list ->
        List.map (fun expr -> eval_global_expression_with_context eval_context_opt expr) list
    | List_variable variable_index ->
        list_value (try_eval_variable variable_index eval_context_opt)
    | List_constant values ->
        values
    | List_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        list_value discrete_value
    | List_cons (expr, list_expr) ->
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        let value = eval_global_expression_with_context eval_context_opt expr in
        value :: list
    | List_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        list_value value

    | List_list_tl list_expr ->
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        let fail_message = list_tl_fail_message list_expr in
        try_eval_list_tl list fail_message

    | List_rev list_expr ->
        let list = eval_list_expression_with_context eval_context_opt list_expr in
        List.rev list
    | List_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        list_value result

and eval_stack_expression_with_context eval_context_opt = function
    | Literal_stack -> Stack.create ()

    | Stack_variable variable_index ->
        stack_value (try_eval_variable variable_index eval_context_opt)

    | Stack_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        stack_value discrete_value

    | Stack_push (expr, stack_expr) ->
        let e = eval_global_expression_with_context eval_context_opt expr in
        let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
        Stack.push e stack; stack

    | Stack_clear stack_expr ->
        let stack = eval_stack_expression_with_context eval_context_opt stack_expr in
        Stack.clear stack; stack

    | Stack_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        stack_value value

    | Stack_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        stack_value result

and eval_queue_expression_with_context eval_context_opt = function
    | Literal_queue -> Queue.create ()

    | Queue_variable variable_index ->
        queue_value (try_eval_variable variable_index eval_context_opt)

    | Queue_local_variable variable_name ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_name eval_context_opt in
        queue_value discrete_value

    | Queue_push (expr, queue_expr) ->
        let e = eval_global_expression_with_context eval_context_opt expr in
        let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
        Queue.push e queue; queue

    | Queue_clear queue_expr ->
        let queue = eval_queue_expression_with_context eval_context_opt queue_expr in
        Queue.clear queue; queue

    | Queue_sequence_function func ->
        let value = eval_sequence_function_with_context eval_context_opt func in
        queue_value value

    | Queue_function_call (function_name, param_names, expr_args) ->
        let fun_def = try_eval_function function_name eval_context_opt in
        let result = eval_inline_function_with_context eval_context_opt param_names expr_args fun_def.body in
        queue_value result

and get_array_value_at_with_context eval_context_opt array_expr index_expr =

    let values = eval_array_expression_with_context eval_context_opt array_expr in
    let index = eval_int_expression_with_context eval_context_opt index_expr in
    let int_index = Int32.to_int index in

    if int_index >= Array.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep ", " (Array.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("Array index out of range: `" ^ str_index ^ "` for array " ^ str_values))
    );

    Array.get values int_index

and get_list_value_at_with_context eval_context_opt array_expr index_expr =

    let values = eval_list_expression_with_context eval_context_opt array_expr in
    let index = eval_int_expression_with_context eval_context_opt index_expr in
    let int_index = Int32.to_int index in

    if int_index >= List.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("List index out of range: `" ^ str_index ^ "` for list " ^ str_values))
    );

    List.nth values int_index

and get_expression_access_value_with_context eval_context_opt index_expr = function
    | Expression_array_access array_expr ->
        get_array_value_at_with_context eval_context_opt array_expr index_expr
    | Expression_list_access list_expr ->
        get_list_value_at_with_context eval_context_opt list_expr index_expr

and eval_inline_function_with_context eval_context_opt param_names expr_args fun_decl =
    (* Get or create local variables table *)
    let local_variables = Hashtbl.create 0 in

    (* Compute parameters values *)
    for i = 0 to (List.length param_names) - 1 do
        let param_name = List.nth param_names i in
        let expr_arg = List.nth expr_args i in
        let arg_val = eval_global_expression_with_context eval_context_opt expr_arg in
        Hashtbl.add local_variables param_name arg_val;
    done;

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

    (* Eval function body *)
    let new_eval_context = {eval_context with local_variables = local_variables } in
    eval_fun_body_with_context new_eval_context fun_decl

and eval_fun_body_with_context eval_context = function
    | Fun_local_decl (variable_name, _, expr, next_expr) ->
        let value = eval_global_expression_with_context (Some eval_context) expr in
        Hashtbl.add eval_context.local_variables variable_name value;

        eval_fun_body_with_context eval_context next_expr

    | Fun_instruction (normal_update, next_expr) ->
        direct_update_with_context eval_context normal_update;
        eval_fun_body_with_context eval_context next_expr

    | Fun_expr expr ->
        eval_global_expression_with_context (Some eval_context) expr

and compute_update_value_opt_with_context eval_context (update_type, expr) =

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
        let new_value = eval_global_expression_with_context (Some eval_context) expr in
        let new_value = pack_value (Some eval_context) old_value new_value update_type in

        Some (discrete_index, new_value)
    | Void_update ->
        let _ = eval_global_expression_with_context (Some eval_context) expr in None

and direct_update_with_context eval_context update =

    let discrete_index_new_value_pair_opt = compute_update_value_opt_with_context eval_context update in
    match discrete_index_new_value_pair_opt with
    | None -> ()
    | Some (discrete_index, new_value) ->
        (* Direct update ! *)
        eval_context.discrete_setter discrete_index new_value

and delayed_update_with_context eval_context updated_discrete update =

    let discrete_index_new_value_pair_opt = compute_update_value_opt_with_context eval_context update in
    match discrete_index_new_value_pair_opt with
    | None ->
        Delayed_update_recorded (* update ok *)
    | Some (discrete_index, new_value) ->
        (* Check if already updated *)
        if Hashtbl.mem updated_discrete discrete_index then (
            (* Find its value *)
            let previous_new_value = Hashtbl.find updated_discrete discrete_index in
            (* Compare with the new one *)
            if DiscreteValue.neq previous_new_value new_value then (
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
and pack_value (* variable_names *) eval_context_opt old_value new_value parsed_update_type =

    let rec pack_value_scalar_or_index_update_type = function
        | Scalar_update discrete_index -> old_value, [||], None
        | Indexed_update (inner_scalar_or_index_update_type, index_expr) ->

            let old_value, _, _ = pack_value_scalar_or_index_update_type inner_scalar_or_index_update_type in

            (* Compute index *)
            let index = Int32.to_int (eval_int_expression_with_context eval_context_opt index_expr) in
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
(*            ImitatorUtilities.print_message Verbose_standard ("unpacked old array: " ^ DiscreteValue.string_of_value unpacked_old_array);*)
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
(*        ImitatorUtilities.print_message Verbose_standard ("packed new value is: " ^ DiscreteValue.string_of_value old_value);*)
        old_value
    | None -> new_value


(* Try to evaluate a constant global expression, if expression isn't constant, it raise an error *)
let try_eval_constant_global_expression = eval_global_expression_with_context None
(* Try to evaluate a constant rational term, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_term = eval_rational_term_with_context None
(* Try to evaluate a constant rational factor, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_factor = eval_rational_factor_with_context None

let direct_update functions_table discrete_access = direct_update_with_context (create_eval_context functions_table discrete_access)
let delayed_update functions_table discrete_access = delayed_update_with_context (create_eval_context functions_table discrete_access)

(* Try to evaluate a constant global expression, if expression isn't constant, it return None *)
let eval_constant_global_expression_opt expr = try Some (try_eval_constant_global_expression expr) with _ -> None
(* Try to evaluate a constant rational term, if expression isn't constant, it return None *)
let eval_constant_rational_term_opt expr = try Some (try_eval_constant_rational_term expr) with _ -> None
(* Try to evaluate a constant rational factor, if expression isn't constant, it return None *)
let eval_constant_rational_factor_opt expr = try Some (try_eval_constant_rational_factor expr) with _ -> None

(**)
let eval_global_expression functions_table discrete_access_opt = eval_global_expression_with_context (create_eval_context_opt functions_table discrete_access_opt)
(**)
let eval_boolean_expression functions_table discrete_access_opt = eval_boolean_expression_with_context (create_eval_context_opt functions_table discrete_access_opt)
(**)
let eval_discrete_boolean_expression functions_table discrete_access_opt = eval_discrete_boolean_expression_with_context (create_eval_context_opt functions_table discrete_access_opt)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint functions_table discrete_access =
  List.for_all (eval_discrete_boolean_expression functions_table (Some discrete_access))

(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(*------------------------------------------------------------*)
(* Matching global_location predicates with a given global_location *)
(*------------------------------------------------------------*)

let match_loc_predicate global_location = function
	| Loc_predicate_EQ (automaton_index, location_index) ->
		get_location global_location automaton_index = location_index
	| Loc_predicate_NEQ (automaton_index, location_index) ->
		get_location global_location automaton_index <> location_index

(*------------------------------------------------------------*)
(* Matching simple predicates with a given global_location *)
(*------------------------------------------------------------*)

let match_simple_predicate functions_table discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location = function
	| State_predicate_discrete_boolean_expression discrete_boolean_expression ->
	    eval_discrete_boolean_expression functions_table (Some discrete_access) discrete_boolean_expression

	| Loc_predicate loc_predicate ->
	    match_loc_predicate global_location loc_predicate

	| State_predicate_accepting ->
	    is_accepting locations_acceptance_condition global_location

	| State_predicate_true -> true
	| State_predicate_false -> false

(*------------------------------------------------------------*)
(* Matching state predicates with a given global_location *)
(*------------------------------------------------------------*)

(***TODO/NOTE: Might have been nicer to convert the acceptance condition during the ModelConverter phase :-/ ***)

(* TODO benjamin CLEAN see here if we can remove global_location parameter, as it as discrete_access for write / read variables *)
let rec match_state_predicate_factor functions_table discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location = function
	| State_predicate_factor_NOT state_predicate_factor_neg ->
	    not (match_state_predicate_factor functions_table discrete_access locations_acceptance_condition global_location state_predicate_factor_neg)
	| Simple_predicate simple_predicate ->
	    match_simple_predicate functions_table discrete_access locations_acceptance_condition global_location simple_predicate
	| State_predicate state_predicate ->
	    match_state_predicate functions_table discrete_access locations_acceptance_condition global_location state_predicate

and match_state_predicate_term functions_table discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location = function
	| State_predicate_term_AND (state_predicate_term_1, state_predicate_term_2) ->
		match_state_predicate_term functions_table discrete_access locations_acceptance_condition global_location state_predicate_term_1
		&&
		match_state_predicate_term functions_table discrete_access locations_acceptance_condition global_location state_predicate_term_2

	| State_predicate_factor state_predicate_factor ->
	    match_state_predicate_factor functions_table discrete_access locations_acceptance_condition global_location state_predicate_factor

and match_state_predicate functions_table discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location = function
	| State_predicate_OR (state_predicate_1, state_predicate_2) ->
		match_state_predicate functions_table discrete_access locations_acceptance_condition global_location state_predicate_1
		||
		match_state_predicate functions_table discrete_access locations_acceptance_condition global_location state_predicate_2

	| State_predicate_term state_predicate_term ->
	    match_state_predicate_term functions_table discrete_access locations_acceptance_condition global_location state_predicate_term




(* TODO benjamin REPLACE BY A REAL EVALUATION OF CONSTANT and not this tricky function using try *)
let is_global_expression_constant expr =
    try (
        let _ = try_eval_constant_global_expression expr in
        true
    )
    with _ -> false