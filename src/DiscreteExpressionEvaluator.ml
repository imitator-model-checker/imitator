open DiscreteExpressions
open DiscreteValue
open Exceptions

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




(* Try evaluating variable value if a discrete valuation is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_variable variable_index = function
    | Some discrete_valuation -> discrete_valuation variable_index
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

(* Evaluate an expression *)
let rec eval_global_expression discrete_valuation = function
    | Arithmetic_expression expr -> eval_discrete_arithmetic_expression discrete_valuation expr
    | Bool_expression expr -> Bool_value (eval_boolean_expression discrete_valuation expr)
    | Binary_word_expression expr -> Binary_word_value (eval_binary_word_expression discrete_valuation expr)
    | Array_expression expr -> Array_value (eval_array_expression discrete_valuation expr)
    | List_expression expr -> List_value (eval_list_expression discrete_valuation expr)
    | Stack_expression expr -> Stack_value (eval_stack_expression discrete_valuation expr)
    | Queue_expression expr -> Queue_value (eval_queue_expression discrete_valuation expr)

and eval_discrete_arithmetic_expression discrete_valuation = function
    | Rational_arithmetic_expression expr ->
        Rational_value (eval_rational_expression discrete_valuation expr)
    | Int_arithmetic_expression expr ->
        Int_value (eval_int_expression discrete_valuation expr)

and eval_rational_expression discrete_valuation = function
        | Rational_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> NumConst.add
                | Minus -> NumConst.sub
            in
            sum_function
                (eval_rational_expression discrete_valuation expr)
                (eval_rational_term discrete_valuation term)

        | Rational_term term ->
            eval_rational_term discrete_valuation term

and eval_rational_term discrete_valuation = function
    | Rational_product_quotient (term, factor, product_quotient) ->
        let a = eval_rational_term discrete_valuation term in
        let b = eval_rational_factor discrete_valuation factor in
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
        eval_rational_factor discrete_valuation factor

and eval_rational_factor discrete_valuation = function
    | Rational_variable variable_index ->
        numconst_value (try_eval_variable variable_index discrete_valuation)
    | Rational_constant variable_value ->
        variable_value
    | Rational_expression expr ->
        eval_rational_expression discrete_valuation expr
    | Rational_unary_min factor ->
        NumConst.neg (eval_rational_factor discrete_valuation factor)

    | Rational_of_int expr ->
        ImitatorUtilities.print_warning
            "Conversion of an int expression to a rational expression
            may cause overflow if your platform doesn't manage `int` as an exact 32 bits integer.";
        NumConst.numconst_of_int (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Rational_pow (expr, exp) ->
        let x = eval_rational_expression discrete_valuation expr in
        let exponent = eval_int_expression discrete_valuation exp in
        NumConst.pow x exponent

    | Rational_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        numconst_value value

and eval_sequence_function discrete_valuation = function
    | Array_access (access_type, index_expr) ->
        get_expression_access_value discrete_valuation index_expr access_type

    | List_hd list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        let fail_message = list_hd_fail_message list_expr in
        try_eval_list_hd list fail_message

    | Stack_pop stack_expr ->
        let stack = eval_stack_expression discrete_valuation stack_expr in
        let fail_message = stack_pop_fail_message stack_expr in
        try_eval_stack_pop stack fail_message

    | Stack_top stack_expr ->
        let stack = eval_stack_expression discrete_valuation stack_expr in
        let fail_message = stack_top_fail_message stack_expr in
        try_eval_stack_top stack fail_message

    | Queue_pop queue_expr ->
        let queue = eval_queue_expression discrete_valuation queue_expr in
        let fail_message = queue_pop_fail_message queue_expr in
        try_eval_queue_pop queue fail_message

    | Queue_top queue_expr ->
        let queue = eval_queue_expression discrete_valuation queue_expr in
        let fail_message = queue_top_fail_message queue_expr in
        try_eval_queue_top queue fail_message

and eval_int_expression discrete_valuation (* expr *) =
    let rec eval_int_expression_rec = function
        | Int_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> Int32.add
                | Minus -> Int32.sub
            in
            sum_function
                (eval_int_expression_rec expr)
                (eval_int_term term)

        | Int_term term ->
            eval_int_term term

    and eval_int_term = function
        | Int_product_quotient (term, factor, product_quotient) ->
            let a = eval_int_term term in
            let b = eval_int_factor factor in
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
            eval_int_factor factor

    and eval_int_factor = function
        | Int_variable variable_index ->
            int_value (try_eval_variable variable_index discrete_valuation)
        | Int_constant variable_value ->
            variable_value;
        | Int_expression expr ->
            eval_int_expression_rec expr
        | Int_unary_min factor ->
            Int32.neg (eval_int_factor factor)
        | Int_pow (expr, exp) ->
            let x = eval_int_expression_rec expr in
            let exponent = eval_int_expression_rec exp in
            OCamlUtilities.pow x exponent

        | Int_sequence_function func ->
            let value = eval_sequence_function discrete_valuation func in
            int_value value
        | Array_length array_expr ->
            let array = eval_array_expression discrete_valuation array_expr in
            Int32.of_int (Array.length array)
        | List_length list_expr ->
            let list = eval_list_expression discrete_valuation list_expr in
            Int32.of_int (List.length list)
        | Stack_length stack_expr ->
            let stack = eval_stack_expression discrete_valuation stack_expr in
            Int32.of_int (Stack.length stack)
        | Queue_length queue_expr ->
            let queue = eval_queue_expression discrete_valuation queue_expr in
            Int32.of_int (Queue.length queue)
        | Int_function_call (function_name, expr) ->
            let result = eval_user_function_call function_name expr in
            int_value result
    in
    eval_int_expression_rec

(** Check if a boolean expression is satisfied *)
and eval_boolean_expression discrete_valuation = function
    | True_bool -> true
    | False_bool -> false
    | Conj_dis (l_expr, r_expr, conj_dis) ->
        let conj_dis_function =
            match conj_dis with
            | And -> (&&)
            | Or -> (||)
        in
        conj_dis_function
            (eval_boolean_expression discrete_valuation l_expr)
            (eval_boolean_expression discrete_valuation r_expr)

    | Discrete_boolean_expression dbe -> eval_discrete_boolean_expression discrete_valuation dbe

(** Check if a discrete boolean expression is satisfied *)
and eval_discrete_boolean_expression discrete_valuation = function
    | Bool_variable variable_index ->
        bool_value (try_eval_variable variable_index discrete_valuation)
    | Bool_constant value ->
        value
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* TODO benjamin WARNING here we compare a DiscreteValue.discrete_value type with operator it's bad *)
    (* We just have to create a Rational_comparison and a Int_comparison to solve this *)
    | Arithmetic_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_discrete_arithmetic_expression discrete_valuation l_expr)
            (eval_discrete_arithmetic_expression discrete_valuation r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (eval_discrete_boolean_expression discrete_valuation l_expr)
             (eval_discrete_boolean_expression discrete_valuation r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_binary_word_expression discrete_valuation l_expr)
            (eval_binary_word_expression discrete_valuation r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_array_expression discrete_valuation l_expr)
            (eval_array_expression discrete_valuation r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_list_expression discrete_valuation l_expr)
            (eval_list_expression discrete_valuation r_expr)
    | Stack_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_stack_expression discrete_valuation l_expr)
            (eval_stack_expression discrete_valuation r_expr)
    | Queue_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_queue_expression discrete_valuation l_expr)
            (eval_queue_expression discrete_valuation r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1 in
            (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        eval_boolean_expression discrete_valuation boolean_expression
    | Not_bool b ->
        not (eval_boolean_expression discrete_valuation b) (* negation *)
    | Bool_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        bool_value value
    | List_mem (expr, list_expr) ->
        let value = eval_global_expression discrete_valuation expr in
        let list = eval_list_expression discrete_valuation list_expr in
        List.mem value list
    | Array_mem (expr, array_expr) ->
        let value = eval_global_expression discrete_valuation expr in
        let array = eval_array_expression discrete_valuation array_expr in
        Array.mem value array
    | List_is_empty list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        List.length list = 0
    | Stack_is_empty stack_expr ->
        let stack = eval_stack_expression discrete_valuation stack_expr in
        Stack.is_empty stack
    | Queue_is_empty queue_expr ->
        let queue = eval_queue_expression discrete_valuation queue_expr in
        Queue.is_empty queue

and eval_binary_word_expression discrete_valuation = function
    | Logical_shift_left (binary_word, expr, _) ->
        BinaryWord.shift_left
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_shift_right (binary_word, expr, _) ->
        BinaryWord.shift_right
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_fill_left (binary_word, expr, _) ->
        BinaryWord.fill_left
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_fill_right (binary_word, expr, _) ->
        BinaryWord.fill_right
            (eval_binary_word_expression discrete_valuation binary_word)
            (Int32.to_int (eval_int_expression discrete_valuation expr))
    | Logical_and (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_and
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_or (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_or
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_xor (l_binary_word, r_binary_word, _) ->
        BinaryWord.log_xor
            (eval_binary_word_expression discrete_valuation l_binary_word)
            (eval_binary_word_expression discrete_valuation r_binary_word)
    | Logical_not (binary_word, _) ->
        BinaryWord.log_not
            (eval_binary_word_expression discrete_valuation binary_word)

    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        binary_word_value (try_eval_variable variable_index discrete_valuation)

    | Binary_word_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        binary_word_value value

and eval_array_expression discrete_valuation = function
    | Literal_array array ->
        Array.map (fun expr -> eval_global_expression discrete_valuation expr) array
    | Array_variable variable_index ->
        array_value (try_eval_variable variable_index discrete_valuation)
    | Array_constant values ->
        values
    | Array_concat (array_expr_0, array_expr_1) ->
        let array_0 = eval_array_expression discrete_valuation array_expr_0 in
        let array_1 = eval_array_expression discrete_valuation array_expr_1 in
        Array.append array_0 array_1
    | Array_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        array_value value

and eval_list_expression discrete_valuation = function
    | Literal_list list ->
        List.map (fun expr -> eval_global_expression discrete_valuation expr) list
    | List_variable variable_index ->
        list_value (try_eval_variable variable_index discrete_valuation)
    | List_constant values ->
        values
    | List_cons (expr, list_expr) ->
        let list = eval_list_expression discrete_valuation list_expr in
        let value = eval_global_expression discrete_valuation expr in
        value :: list
    | List_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        list_value value

    | List_list_tl list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        let fail_message = list_tl_fail_message list_expr in
        try_eval_list_tl list fail_message

    | List_rev list_expr ->
        let list = eval_list_expression discrete_valuation list_expr in
        List.rev list

and eval_stack_expression discrete_valuation = function
    | Literal_stack -> Stack.create ()
    | Stack_variable variable_index ->
        stack_value (try_eval_variable variable_index discrete_valuation)
    | Stack_push (expr, stack_expr) ->
        let e = eval_global_expression discrete_valuation expr in
        let stack = eval_stack_expression discrete_valuation stack_expr in
        Stack.push e stack; stack

    | Stack_clear stack_expr ->
        let stack = eval_stack_expression discrete_valuation stack_expr in
        Stack.clear stack; stack

    | Stack_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        stack_value value

and eval_queue_expression discrete_valuation = function
    | Literal_queue -> Queue.create ()
    | Queue_variable variable_index ->
        queue_value (try_eval_variable variable_index discrete_valuation)
    | Queue_push (expr, queue_expr) ->
        let e = eval_global_expression discrete_valuation expr in
        let queue = eval_queue_expression discrete_valuation queue_expr in
        Queue.push e queue; queue

    | Queue_clear queue_expr ->
        let queue = eval_queue_expression discrete_valuation queue_expr in
        Queue.clear queue; queue

    | Queue_sequence_function func ->
        let value = eval_sequence_function discrete_valuation func in
        queue_value value

and get_array_value_at discrete_valuation array_expr index_expr =

    let values = eval_array_expression discrete_valuation array_expr in
    let index = eval_int_expression discrete_valuation index_expr in
    let int_index = Int32.to_int index in

    if int_index >= Array.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_array_of_string_with_sep ", " (Array.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("Array index out of range: `" ^ str_index ^ "` for array " ^ str_values))
    );

    Array.get values int_index

and get_list_value_at discrete_valuation array_expr index_expr =

    let values = eval_list_expression discrete_valuation array_expr in
    let index = eval_int_expression discrete_valuation index_expr in
    let int_index = Int32.to_int index in

    if int_index >= List.length values || int_index < 0 then (
        let str_index = string_of_int int_index in
        let str_values = OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (fun value -> DiscreteValue.string_of_value value) values) in
        raise (Out_of_bound ("List index out of range: `" ^ str_index ^ "` for list " ^ str_values))
    );

    List.nth values int_index

and get_expression_access_value discrete_valuation index_expr = function
    | Expression_array_access array_expr ->
        get_array_value_at discrete_valuation array_expr index_expr
    | Expression_list_access list_expr ->
        get_list_value_at discrete_valuation list_expr index_expr

(* TODO benjamin IMPLEMENT implement eval of user functions *)
(*
and eval_fun_decl_or_expr discrete_valuation local_variables = function
    | Fun_local_decl (variable_name, var_type, expr, decl_or_expr) ->
        (* TODO benjamin IMPLEMENT missing local variable in global expr eval *)
        let value = eval_global_expression discrete_valuation expr in
        Hashtbl.add local_variables variable_name value;
        eval_fun_decl_or_expr discrete_valuation local_variables decl_or_expr
    | Fun_expr expr ->
        (* TODO benjamin IMPLEMENT missing local variable in global expr eval *)
        eval_global_expression discrete_valuation expr
*)
and eval_user_function_call function_name arguments =
    (*
    let fun_definition_opt = Hashtbl.find_opt Functions.fun_definitions function_name in
    let fun_definition =
        match fun_definition_opt with
        | Some fun_def -> fun_def
        | None -> raise (UndefinedFunction function_name)
    in
    eval_fun_decl_or_expr (Hashtbl.create ()) fun_definition.body
    *)
    Int_value Int32.zero

(* Wrap a scalar value to an array value according to the modified index of an old value *)
(* For example: `old_value[0] = 1` with old value = [0, 1], would wrap new_value into an array `new_value = [1, 1]` *)
(* This function is used to assign an element of an array at a given index *)
(* a = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] *)
(* a[1][1][0] = 0 *)
(* new_value = [[[1, 2], [3, 4]], [[5, 6], [0, 8]]] *)
(* a[1] = [[5, 6], [7, 8]] *)
(* a[1][1] = [7, 8] *)
(* a[1][1][0] = 7 *)
let pack_value variable_names discrete_valuation old_value new_value parsed_variable_update_type =

    let rec pack_value_rec = function
        | Void_update -> old_value, [||], None
        | Variable_update discrete_index -> old_value, [||], None
        | Indexed_update (inner_parsed_variable_update_type, index_expr) ->

            let old_value, _, _ = pack_value_rec inner_parsed_variable_update_type in

            (* Compute index *)
            let index = Int32.to_int (eval_int_expression discrete_valuation index_expr) in
(*            ImitatorUtilities.print_message Verbose_standard ("access index: " ^ string_of_int index ^ "for " ^ string_of_value old_value);*)
            (* Get inner array of discrete value of old value *)
            let old_array = array_value old_value in

            (* Check bounds *)
            if index >= Array.length old_array || index < 0 then (
                let str_parsed_variable_update_type = DiscreteExpressions.string_of_variable_update_type variable_names parsed_variable_update_type in
                raise (Out_of_bound ("Array index out of range: `" ^ str_parsed_variable_update_type ^ "`"))
            );

            (* Get element at given index *)
            let unpacked_old_array = old_array.(index) in
(*            ImitatorUtilities.print_message Verbose_standard ("unpacked old array: " ^ DiscreteValue.string_of_value unpacked_old_array);*)
            unpacked_old_array, old_array, Some index
    in
    let unpacked_old_array, old_array, some_index = pack_value_rec parsed_variable_update_type in
    match some_index with
    | Some index ->
        old_array.(index) <- new_value;
(*        ImitatorUtilities.print_message Verbose_standard ("packed new value is: " ^ DiscreteValue.string_of_value old_value);*)
        old_value
    | None -> new_value


(* Try to evaluate a constant global expression, if expression isn't constant, it raise an error *)
let try_eval_constant_global_expression = eval_global_expression None
(* Try to evaluate a constant rational term, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_term = eval_rational_term None
(* Try to evaluate a constant rational factor, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_factor = eval_rational_factor None

(* Try to evaluate a constant global expression, if expression isn't constant, it return None *)
let eval_constant_global_expression_opt expr = try Some (try_eval_constant_global_expression expr) with _ -> None
(* Try to evaluate a constant rational term, if expression isn't constant, it return None *)
let eval_constant_rational_term_opt expr = try Some (try_eval_constant_rational_term expr) with _ -> None
(* Try to evaluate a constant rational factor, if expression isn't constant, it return None *)
let eval_constant_rational_factor_opt expr = try Some (try_eval_constant_rational_factor expr) with _ -> None

(* TODO benjamin REPLACE BY A REAL EVALUATION OF CONSTANT and not this tricky function using try *)
let is_global_expression_constant expr =
    try (
        let _ = try_eval_constant_global_expression expr in
        true
    )
    with _ -> false
