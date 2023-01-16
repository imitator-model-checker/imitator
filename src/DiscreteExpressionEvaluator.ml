(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module allows to evaluate guards, invariants, user defined functions and update code blocs
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Utils modules *)
open CustomModules
open OCamlUtilities
open Exceptions

(* Abstract model modules *)
open AbstractModel
open AbstractProperty
open AbstractValue
open DiscreteExpressions
open LinearConstraint

(* Execution modules *)
open Automaton
open DiscreteState


(* Table of variable name by index *)
type variable_name_table = variable_index -> variable_name
(* Table of function (name, definition) *)
type functions_table = (variable_name, fun_definition) Hashtbl.t
(* Table of clock updates in (no order) *)
type clock_updates_table = (clock_index, pxd_linear_term) Hashtbl.t
(* Queue of ordered clock updates *)
type clock_updates_history = (clock_index * pxd_linear_term) Queue.t

(* Record that contains context (current location, current local variables) for evaluating an expression *)
type eval_context = {
    (* Valuation of global variables at the context (current location) *)
    discrete_valuation : discrete_valuation;
    (* Setter of global variables at the context (current location) *)
    discrete_setter : discrete_setter;
    (* Valuation of local variables at the context (current location) *)
    local_discrete_valuation : local_discrete_valuation;
    (* Setter of local variables at the context (current location) *)
    local_discrete_setter : local_discrete_setter;
    (* All clock updates *)
    updated_clocks : clock_updates_table;
    (* Ordered queue of clock updates *)
    updated_clocks_ordered : clock_updates_history;
}


(* Create an evaluation context with a discrete valuation function and a local variables table *)
let [@inline] create_eval_context (discrete_valuation, discrete_setter, local_discrete_valuation, local_discrete_setter) =
    {
        discrete_valuation = discrete_valuation;
        discrete_setter = discrete_setter;
        local_discrete_valuation = local_discrete_valuation;
        local_discrete_setter = local_discrete_setter;
        updated_clocks = Hashtbl.create 0;
        updated_clocks_ordered = Queue.create ()
    }

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

(* Message when trying to perform an operation on elements of an empty sequence *)
let empty_seq_fail_message str_expr =
    "Use of `" ^ str_expr ^ "` on empty sequence."

(* Evaluate function on a sequence, raise an exception if sequence is empty *)
let eval_if_not_empty eval_length_function eval_function collection fail_message =
    if eval_length_function collection = 0 then
        raise (Empty_collection fail_message)
    else
        eval_function collection

(* Eval list_hd *)
let try_eval_list_hd seq (* fail_message *) = eval_if_not_empty List.length List.hd seq (* fail_message *)
(* Eval list_tl *)
let try_eval_list_tl seq (* fail_message *) = eval_if_not_empty List.length List.tl seq (* fail_message *)
(* Eval stack_pop *)
let try_eval_stack_pop seq (* fail_message *) = eval_if_not_empty Stack.length Stack.pop seq (* fail_message *)
(* Eval stack_top *)
let try_eval_stack_top seq (* fail_message *) = eval_if_not_empty Stack.length Stack.top seq (* fail_message *)
(* Eval queue_pop *)
let try_eval_queue_pop seq (* fail_message *) = eval_if_not_empty Queue.length Queue.pop seq (* fail_message *)
(* Eval queue_top *)
let try_eval_queue_top seq (* fail_message *) = eval_if_not_empty Queue.length Queue.top seq (* fail_message *)

(* Try evaluating a global variable value if an eval context is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_variable variable_index = function
    | Some eval_context -> eval_context.discrete_valuation variable_index
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

(* Get local variable value of a given variable in the given eval_context *)
let eval_local_variable eval_context = eval_context.local_discrete_valuation

(* Try evaluating a local variable value if an eval context is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_local_variable variable_ref = function
    | Some eval_context ->
        eval_local_variable eval_context variable_ref
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate a non-constant expression without a discrete valuation."))

(* Set local variable value of a given variable in the given eval_context *)
let set_local_variable eval_context (* variable_ref value *) = eval_context.local_discrete_setter (* variable_ref value *)

(* Try evaluating a function if a function table is given *)
(* Otherwise, it means that we are trying to evaluate an expression that should have to be constant (without variable) *)
(* For example in constant declaration, in this case trying to evaluate a variable raise an error *)
let try_eval_function function_name : functions_table option -> fun_definition = function
    | Some functions_table -> Hashtbl.find functions_table function_name
    (* If error below is raised, it mean that you doesn't check that expression is constant before evaluating it *)
    | None -> raise (InternalError ("Unable to evaluate an expression containing function calls without a functions table."))

(* TODO benjamin CLEAN remove comments, old version when clock updates are pure linear expression *)
(* Now we use more complex expression for clock updates, so rewriting is made on more larger arithmetic expressions *)

(* Replace discrete variable by their current value in a linear expression that update a clock *)
(*
let rewrite_clock_update variable_names eval_context (* linear_expr *) =
    let rec rewrite_clock_update_rec = function
        | IR_Var variable_index ->
            (* Check if the variable is a clock previously updated *)
            if Hashtbl.mem eval_context.updated_clocks variable_index then (
                (* If yes, we replace variable by the linear expression that updated the clock *)
                Hashtbl.find eval_context.updated_clocks variable_index
            )
            else (
                try (
                    (* If no, it's a discrete variable and we replace variable by the current computed value of discrete value *)
                    let value = numconst_value (eval_context.discrete_valuation variable_index) in
                    IR_Coef value
                ) with _ -> (
                    IR_Var variable_index
                )
            )

        | IR_Coef _ as ir_coef -> ir_coef
        | IR_Plus (l_linear_term, r_linear_term) ->
            LinearConstraint.add_pxd_linear_terms (rewrite_clock_update_rec l_linear_term) (rewrite_clock_update_rec r_linear_term)
        | IR_Minus (l_linear_term, r_linear_term) ->
            LinearConstraint.sub_pxd_linear_terms (rewrite_clock_update_rec l_linear_term) (rewrite_clock_update_rec r_linear_term)
        | IR_Times (coef, linear_term) ->
            IR_Times (coef, rewrite_clock_update_rec linear_term)
    in
    rewrite_clock_update_rec (* linear_expr *)
*)

(* Get clocks that were updated effectively (clock are found in eval context after a code bloc evaluation) *)
let effective_clock_updates eval_context variable_names =

    (* Get ordered clock updates from context *)
    let updated_clocks = eval_context.updated_clocks_ordered |> Queue.to_seq |> List.of_seq in

    (* Display clock updates found only if verbose=high *)
    let lazy_str_clock_updates = lazy (
        let str_clock_updates =
            List.map (fun (clock_index, l) ->
                let str_linear_expr = LinearConstraint.string_of_pxd_linear_term variable_names l in
                "`" ^ variable_names clock_index ^ "=" ^ str_linear_expr ^ "`"
            ) updated_clocks
        in
        "Update clock(s): " ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_clock_updates
    )
    in
    ImitatorUtilities.print_message_lazy Verbose_high lazy_str_clock_updates;

    (* No clock updates found *)
    if List.length updated_clocks = 0 then (
        ImitatorUtilities.print_message_lazy Verbose_high (lazy "No clock updates.");
        No_update
    ) else (

        (* Check whether all clock updates are resets *)
        let is_all_resets =
            List.for_all (fun (_, linear_expr) ->
                match linear_expr with
                | LinearConstraint.IR_Coef coef -> NumConst.equal coef NumConst.zero
                | _ -> false
            ) updated_clocks
        in

        (* Only clock resets found *)
        if is_all_resets then (
            ImitatorUtilities.print_message_lazy Verbose_high (lazy "Only clock resets.");
            let clock_indexes = List.map first_of_tuple updated_clocks in
            Resets clock_indexes
        (* Else, some updates *)
        ) else (
            ImitatorUtilities.print_message_lazy Verbose_high (lazy (string_of_int (List.length updated_clocks) ^ " clock updates found."));
            Updates updated_clocks
        )
    )


(* Evaluate an expression *)
let rec eval_global_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Void_expression expr -> eval_void_expression_with_context variable_names functions_table_opt eval_context_opt expr
    | Arithmetic_expression expr -> Abstract_scalar_value (eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt expr)
    | Bool_expression expr -> Abstract_scalar_value (Abstract_bool_value (eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | Binary_word_expression expr -> Abstract_scalar_value (Abstract_bin_value (eval_binary_word_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | Array_expression expr -> Abstract_container_value (Abstract_array_value (eval_array_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | List_expression expr -> Abstract_container_value (Abstract_list_value (eval_list_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | Stack_expression expr -> Abstract_container_value (Abstract_stack_value (eval_stack_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | Queue_expression expr -> Abstract_container_value (Abstract_queue_value (eval_queue_expression_with_context variable_names functions_table_opt eval_context_opt expr))

(* Evaluate an arithmetic expression *)
and eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Rational_arithmetic_expression expr ->
        Abstract_number_value (Abstract_rat_value (eval_rational_expression_with_context variable_names functions_table_opt eval_context_opt expr))
    | Int_arithmetic_expression expr ->
        Abstract_number_value (Abstract_int_value (eval_int_expression_with_context variable_names functions_table_opt eval_context_opt expr))

(* Evaluate a rational arithmetic expression *)
and eval_rational_expression_with_context variable_names functions_table_opt eval_context_opt = function
        | Rational_sum_diff (expr, term, sum_diff) ->
            let sum_function =
                match sum_diff with
                | Plus -> NumConst.add
                | Minus -> NumConst.sub
            in
            sum_function
                (eval_rational_expression_with_context variable_names functions_table_opt eval_context_opt expr)
                (eval_rational_term_with_context variable_names functions_table_opt eval_context_opt term)

        | Rational_term term ->
            eval_rational_term_with_context variable_names functions_table_opt eval_context_opt term

(* Evaluate a rational term *)
and eval_rational_term_with_context variable_names functions_table_opt eval_context_opt = function
    | Rational_product_quotient (term, factor, product_quotient) ->
        let a = eval_rational_term_with_context variable_names functions_table_opt eval_context_opt term in
        let b = eval_rational_factor_with_context variable_names functions_table_opt eval_context_opt factor in
        (match product_quotient with
        | Mul -> NumConst.mul a b
        | Div ->
            let numerator, denominator = a, b in
            (* Check for 0-denominator *)
            if NumConst.equal denominator NumConst.zero then(
                raise (Division_by_0 ("Division by 0 found when trying to perform " ^ NumConst.to_string numerator ^ " / " ^ NumConst.to_string denominator ^ "."))
            );
            (* Divide *)
            NumConst.div numerator denominator
        )

    | Rational_factor factor ->
        eval_rational_factor_with_context variable_names functions_table_opt eval_context_opt factor

(* Evaluate a rational factor *)
and eval_rational_factor_with_context variable_names functions_table_opt eval_context_opt = function
    | Rational_variable variable_index ->
        numconst_value (try_eval_variable variable_index eval_context_opt)
    | Rational_constant variable_value ->
        variable_value
    | Rational_local_variable variable_ref ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        numconst_value discrete_value
    | Rational_nested_expression expr ->
        eval_rational_expression_with_context variable_names functions_table_opt eval_context_opt expr
    | Rational_unary_min factor ->
        NumConst.neg (eval_rational_factor_with_context variable_names functions_table_opt eval_context_opt factor)

    | Rational_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        numconst_value value

    | Rational_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        numconst_value result

(* Evaluate a int arithmetic expression *)
and eval_int_expression_with_context variable_names functions_table_opt eval_context_opt (* expr *) =

    (* Evaluate a int arithmetic expression *)
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

    (* Evaluate a int term *)
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
                    raise (Division_by_0 ("Division by 0 found when trying to perform " ^ (Int32.to_string numerator) ^ " / " ^ (Int32.to_string denominator) ^ "."))
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

    (* Evaluate a int factor *)
    and eval_int_factor_with_context = function
        | Int_variable variable_index ->
            int_value (try_eval_variable variable_index eval_context_opt)
        | Int_constant variable_value ->
            variable_value;
        | Int_local_variable variable_ref ->
                        (* Variable should exist as it was checked before *)
            let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
            int_value discrete_value

        | Int_nested_expression expr ->
            eval_int_expression_with_context_rec expr
        | Int_unary_min factor ->
            Int32.neg (eval_int_factor_with_context factor)

        | Int_indexed_expr (access_type, index_expr) ->
            let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
            int_value value

        | Int_function_call (function_name, param_refs, expr_args) ->
            let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
            int_value result

    in
    (* Call top-level *)
    eval_int_expression_with_context_rec

(* Evaluate a boolean expression *)
and eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | True_bool -> true
    | False_bool -> false
    | Conj_dis (l_expr, r_expr, conj_dis) ->
        let conj_dis_function =
            match conj_dis with
            | And -> (&&)
            | Or -> (||)
        in
        conj_dis_function
            (eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)

    | Discrete_boolean_expression dbe -> eval_discrete_boolean_expression_with_context variable_names functions_table_opt eval_context_opt dbe

(* Evaluate a discrete boolean expression *)
and eval_discrete_boolean_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Bool_variable variable_index ->
        bool_value (try_eval_variable variable_index eval_context_opt)
    | Bool_constant value ->
        value
    | Bool_local_variable variable_ref ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        bool_value discrete_value
    (** Discrete arithmetic expression of the form Expr ~ Expr *)
    (* We just have to create a Rational_comparison and a Int_comparison to solve this *)
    | Arithmetic_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | Boolean_comparison (l_expr, relop, r_expr) ->
         (operator_of_relop relop)
             (eval_discrete_boolean_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
             (eval_discrete_boolean_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | Binary_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_binary_word_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_binary_word_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | Array_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_array_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_array_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | List_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_list_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_list_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | Stack_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_stack_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_stack_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)
    | Queue_comparison (l_expr, relop, r_expr) ->
        (operator_of_relop relop)
            (eval_queue_expression_with_context variable_names functions_table_opt eval_context_opt l_expr)
            (eval_queue_expression_with_context variable_names functions_table_opt eval_context_opt r_expr)

    (** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
    | Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
        (* Compute the first one to avoid redundancy *)
        let expr1_evaluated = eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt  discrete_arithmetic_expression_1 in
            (eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt discrete_arithmetic_expression_2)
            <=
            expr1_evaluated
            &&
            expr1_evaluated
            <=
            (eval_discrete_arithmetic_expression_with_context variable_names functions_table_opt eval_context_opt discrete_arithmetic_expression_3)
    | Boolean_expression boolean_expression ->
        eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt boolean_expression
    | Not_bool b ->
        not (eval_boolean_expression_with_context variable_names functions_table_opt eval_context_opt b) (* negation *)
    | Bool_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        bool_value value

    | Bool_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        bool_value result

(* Evaluate a binary word expression *)
and eval_binary_word_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Binary_word_constant value -> value
    | Binary_word_variable (variable_index, _) ->
        binary_word_value (try_eval_variable variable_index eval_context_opt)
    | Binary_word_local_variable variable_ref ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        binary_word_value discrete_value

    | Binary_word_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        binary_word_value value

    | Binary_word_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        binary_word_value result

(* Evaluate an array expression *)
and eval_array_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Literal_array array ->
        Array.map (fun expr -> eval_global_expression_with_context variable_names functions_table_opt eval_context_opt expr) array
    | Array_variable variable_index ->
        array_value (try_eval_variable variable_index eval_context_opt)
    | Array_constant values ->
        values
    | Array_local_variable variable_ref ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        array_value discrete_value

    | Array_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        array_value value

    | Array_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        array_value result

(* Evaluate a list expression *)
and eval_list_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Literal_list list ->
        List.map (fun expr -> eval_global_expression_with_context variable_names functions_table_opt eval_context_opt expr) list
    | List_variable variable_index ->
        list_value (try_eval_variable variable_index eval_context_opt)
    | List_constant values ->
        values
    | List_local_variable variable_ref ->
                (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        list_value discrete_value
    | List_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        list_value value

    | List_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        list_value result

(* Evaluate a stack expression *)
and eval_stack_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Literal_stack -> Stack.create ()

    | Stack_variable variable_index ->
        stack_value (try_eval_variable variable_index eval_context_opt)

    | Stack_local_variable variable_ref ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        stack_value discrete_value

    | Stack_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        stack_value value

    | Stack_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        stack_value result

(* Evaluate a queue expression *)
and eval_queue_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Literal_queue -> Queue.create ()

    | Queue_variable variable_index ->
        queue_value (try_eval_variable variable_index eval_context_opt)

    | Queue_local_variable variable_ref ->
        (* Variable should exist as it was checked before *)
        let discrete_value = try_eval_local_variable variable_ref eval_context_opt in
        queue_value discrete_value

    | Queue_indexed_expr (access_type, index_expr) ->
        let value = get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr in
        queue_value value

    | Queue_function_call (function_name, param_refs, expr_args) ->
        let result = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        queue_value result

(* Evaluate a void expression *)
and eval_void_expression_with_context variable_names functions_table_opt eval_context_opt = function
    | Void_function_call (function_name, param_refs, expr_args) ->
        let _ = eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args in
        Abstract_void_value

(* Evaluate an access to an array / list *)
and get_expression_access_value_with_context variable_names functions_table_opt eval_context_opt access_type index_expr =

        (* Compute index *)
        let index = eval_int_expression_with_context variable_names functions_table_opt eval_context_opt index_expr in
        let int_index = Int32.to_int index in

        (* Create out of range fail message *)
        let str_expr =
            match variable_names with
            | Some variable_names -> lazy (DiscreteExpressions.string_of_expression_access variable_names access_type index_expr)
            | None -> lazy ""
        in

        (* Check access type (array or list ?) *)
        match access_type with
        | Array_access array_expr ->
            let values = eval_array_expression_with_context variable_names functions_table_opt eval_context_opt array_expr in

            if int_index >= Array.length values || int_index < 0 then (
                raise (Out_of_range ("Index out of range at `" ^ Lazy.force str_expr ^ "`."))
            );

            (* Get element at index *)
            Array.get values int_index

        | List_access list_expr ->
            let values = eval_list_expression_with_context variable_names functions_table_opt eval_context_opt list_expr in

            if int_index >= List.length values || int_index < 0 then (
                raise (Out_of_range ("Index out of range at `" ^ Lazy.force str_expr ^ "`."))
            );

            (* Get element at index *)
            List.nth values int_index


(* Rewrite a clock update arithmetic expression by evaluating all discrete variables and replacing all clock variables by their previously updated expression *)
(* e.g : for init y >= 0; z >= 0 and i = 1, the updates `x := y + z; w := x + i * i + 2` will be rewritten to `x := y + z; w := y + z + 1 + 2` *)
(* This function transform an arithmetic clock update expression to a linear expression that can be evaluated after by PPL *)
and rewrite_clock_update variable_names eval_context functions_table_opt (* expr *) =

    (* Prepare error message when non-linear operation was found on continuous *)
    let nonlinear_operation_message = lazy "A clock update contains non-linear operations on continuous clock / parameter variables. It should be checked before." in

    let rec rewrite_rational_arithmetic_expression = function
        | Rational_sum_diff (expr, term, sum_diff) ->
            let rewrited_expr = rewrite_rational_arithmetic_expression expr in
            let rewrited_term = rewrite_rational_term term in

            (match rewrited_expr, rewrited_term with
            (* Compute coef *)
            | IR_Coef c1, IR_Coef c2 ->
                (match sum_diff with
                | Plus -> IR_Coef (NumConst.add c1 c2)
                | Minus -> IR_Coef (NumConst.sub c1 c2)
                )
            | l_expr, r_expr ->
                let sum_diff_f =
                    match sum_diff with
                    | Plus ->
                        LinearConstraint.add_pxd_linear_terms
                    | Minus ->
                        LinearConstraint.sub_pxd_linear_terms
                in
                sum_diff_f rewrited_expr rewrited_term
            )

        | Rational_term term ->
            rewrite_rational_term term

    and rewrite_rational_term = function
        | Rational_product_quotient (term, factor, product_quotient) ->
            (* Only cases rejected are var*var, var/var, k/var *)
            let rewrited_term = rewrite_rational_term term in
            let rewrited_factor = rewrite_rational_factor factor in

            (match rewrited_term, rewrited_factor with
            | IR_Coef c1, IR_Coef c2 ->
                (* Two coef, compute mul or div *)
                (match product_quotient with
                | Mul -> IR_Coef (NumConst.mul c1 c2)
                | Div ->
                    (* Check for 0-denominator *)
                    if NumConst.equal c2 NumConst.zero then(
                        raise (Division_by_0 ("Division by 0 found when trying to perform " ^ NumConst.to_string c1 ^ " / " ^ NumConst.to_string c2 ^ "."))
                    );
                    (* Divide *)
                    IR_Coef (NumConst.div c1 c2)
                )
            (* k / var or k / c*var *)
            | IR_Coef c, linear_term when product_quotient = Div ->
                raise (InternalError (Lazy.force nonlinear_operation_message))
            | IR_Coef c, linear_term
            | linear_term, IR_Coef c ->
                (* Get coef according to requested operation *)
                let times_coef =
                    match product_quotient with
                    (* If mul, multiply *)
                    | Mul -> c
                    (* If div, multiply by inverse *)
                    | Div -> NumConst.div NumConst.one c
                in
                IR_Times (times_coef, linear_term)
            | _ ->
                raise (InternalError (Lazy.force nonlinear_operation_message))
            )

        | Rational_factor factor ->
            rewrite_rational_factor factor

    and rewrite_rational_factor = function
        | Rational_variable variable_index ->
            (* Check if the variable is a clock previously updated *)
            if Hashtbl.mem eval_context.updated_clocks variable_index then (
                (* If yes, we replace variable by the linear expression that updated the clock *)
                Hashtbl.find eval_context.updated_clocks variable_index
            )
            else (
                try (
                    (* If no, it's a discrete variable and we replace variable by the current computed value of discrete value *)
                    let value = numconst_value (eval_context.discrete_valuation variable_index) in
                    IR_Coef value
                ) with _ -> (
                    IR_Var variable_index
                )
            )
        | Rational_local_variable variable_ref ->
            (* Variable should exist as it was checked before *)
            let discrete_value = eval_local_variable eval_context variable_ref in
            let value = numconst_value discrete_value in
            IR_Coef value

        | Rational_constant value ->
            IR_Coef value

        | Rational_nested_expression expr ->
            rewrite_rational_arithmetic_expression expr

        | Rational_unary_min factor ->
            let rewrited_factor = rewrite_rational_factor factor in
            LinearConstraint.negate_linear_term rewrited_factor

        | Rational_indexed_expr (access_type, index_expr) ->
            let discrete_value = get_expression_access_value_with_context variable_names functions_table_opt (Some eval_context) access_type index_expr in
            let value = numconst_value discrete_value in
            IR_Coef value

        | Rational_function_call (function_name, param_refs, expr_args) ->
            let result = eval_user_function_with_context variable_names functions_table_opt (Some eval_context) function_name param_refs expr_args in
            let value = numconst_value result in
            IR_Coef value

    in
    rewrite_rational_arithmetic_expression (* expr *)

(* Evaluate sequential code bloc given an eval context *)
and eval_seq_code_bloc_with_context variable_names functions_table_opt eval_context seq_code_bloc =

    let rec eval_seq_code_bloc eval_context code_bloc =
        List.iter (eval_instruction eval_context) code_bloc

    and eval_instruction eval_context = function
        | Local_decl (variable_ref, _, expr) ->
            let value = eval_global_expression_with_context variable_names functions_table_opt (Some eval_context) expr in
            (* Set local variable with initial value *)
            set_local_variable eval_context variable_ref value

        | For_loop (variable_ref, from_expr, to_expr, loop_dir, inner_bloc) ->
            let from_value = eval_int_expression_with_context variable_names functions_table_opt (Some eval_context) from_expr in
            let to_value = eval_int_expression_with_context variable_names functions_table_opt (Some eval_context) to_expr in

            let execute_inner_bloc i =
                let value = AbstractValue.of_int (Int32.of_int i) in
                set_local_variable eval_context variable_ref value;
                (* Don't get any value as it was evaluated as void expression *)
                eval_seq_code_bloc eval_context inner_bloc
            in

            let i32_from_value, i32_to_value = Int32.to_int from_value, Int32.to_int to_value in

            (match loop_dir with
            | Loop_up ->
                for i = i32_from_value to i32_to_value do
                    execute_inner_bloc i;
                done
            | Loop_down ->
                for i = i32_from_value downto i32_to_value do
                    execute_inner_bloc i;
                done
            );

        | While_loop (condition_expr, inner_bloc) ->

            while (eval_boolean_expression_with_context variable_names functions_table_opt (Some eval_context) condition_expr) do
                (* Don't get any value as it was evaluated as void expression *)
                eval_seq_code_bloc eval_context inner_bloc;
            done

        | If (condition_expr, then_bloc, else_bloc_opt) ->

            (* Evaluation condition *)
            let condition_evaluated = eval_boolean_expression_with_context variable_names functions_table_opt (Some eval_context) condition_expr in

            (* Execute then or else bloc (if defined) *)
            if condition_evaluated then (
                eval_seq_code_bloc eval_context then_bloc
            ) else (
                match else_bloc_opt with
                | Some else_bloc ->
                    eval_seq_code_bloc eval_context else_bloc
                | None -> ()
            );

        | Instruction expr ->
            let _ = eval_global_expression_with_context variable_names functions_table_opt (Some eval_context) expr in ()

        | Assignment normal_update ->
            direct_update_with_context variable_names functions_table_opt eval_context normal_update

        | Clock_assignment (clock_index, expr) ->
            (* Rewrite the clock's update according to previous clock updates and current discrete value (context) *)
            let rewritten_linear_expr = rewrite_clock_update variable_names eval_context functions_table_opt expr in
            (* Add clock update into table *)
            Hashtbl.replace eval_context.updated_clocks clock_index rewritten_linear_expr;
            (* Push clock update on queue *)
            Queue.push (clock_index, rewritten_linear_expr) eval_context.updated_clocks_ordered;

            (* Prepare rewriting message, only computed if verbose >= Verbose_high *)
            let lazy_rewriting_message = lazy (
                match variable_names with
                | Some variable_names ->
                    let str_expr_before = DiscreteExpressions.string_of_rational_arithmetic_expression variable_names expr in
                    let str_linear_expr_after = LinearConstraint.string_of_pxd_linear_term variable_names rewritten_linear_expr in
                    "Clock rewriting: `" ^ variable_names clock_index ^ " = " ^ str_expr_before ^ " => " ^ variable_names clock_index ^ " = " ^ str_linear_expr_after ^ "`"
                | _ -> ""
            )
            in
            ImitatorUtilities.print_message_lazy Verbose_high lazy_rewriting_message;



    in
    (* Call top-level *)
    eval_seq_code_bloc eval_context seq_code_bloc

(* Evaluate a user function given an eval context *)
and eval_user_function_with_context variable_names functions_table_opt eval_context_opt function_name param_refs expr_args =

    (* Get function definition *)
    let fun_def = try_eval_function function_name functions_table_opt in
    (* Compute arguments values *)
    let arg_values = List.map (eval_global_expression_with_context variable_names functions_table_opt eval_context_opt) expr_args in
    (* Associate each parameter with their value *)
    let param_refs_with_arg_values = List.combine param_refs arg_values in

    (* Eval function *)
    let eval_fun_type_with_context eval_context_opt = function
        | Fun_builtin builtin_f ->
            (* Execute built-in function given argument values *)
            let l_del, r_del = Constants.default_paren_delimiter in
            (* Get formal parameter names *)
            let param_names = List.map first_of_tuple param_refs in
            let str_fun_call = function_name ^ l_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " param_names ^ r_del in
            builtin_f str_fun_call arg_values

        | Fun_user (code_bloc, return_expr_opt) ->
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

            (* Set parameter values to computed argument values *)
            List.iter (fun (param_ref, value) -> set_local_variable eval_context param_ref value) param_refs_with_arg_values;
            (* Eval function body *)
            eval_seq_code_bloc_with_context variable_names functions_table_opt eval_context code_bloc;

            match return_expr_opt with
            | Some return_expr ->
                eval_global_expression_with_context variable_names functions_table_opt (Some eval_context) return_expr
            | None -> Abstract_void_value
    in
    eval_fun_type_with_context eval_context_opt fun_def.body

(* Set variable with computed expression *)
and compute_update_value_opt_with_context variable_names functions_table_opt eval_context (scalar_or_index_update_type, expr) =

    let rec update_scope_of_parsed_scalar_or_index_update_type = function
        | Scalar_update update_scope -> update_scope
        | Indexed_update (scalar_or_index_update_type, _) ->
            update_scope_of_parsed_scalar_or_index_update_type scalar_or_index_update_type
    in
    (* Get discrete index *)
    let update_scope = update_scope_of_parsed_scalar_or_index_update_type scalar_or_index_update_type in
    (* Get value before update as old value *)
    let old_value =
        match update_scope with
        | Global_update variable_index ->
            eval_context.discrete_valuation variable_index
        | Local_update variable_ref ->
            eval_local_variable eval_context variable_ref
    in

    (* Compute its new value *)
    let new_value = eval_global_expression_with_context variable_names functions_table_opt (Some eval_context) expr in
    (* Pack value (e.g: x[0] := 1 for x = [0,1] before update will pack value to [1,1]) *)
    let new_value = pack_value variable_names functions_table_opt (Some eval_context) old_value new_value scalar_or_index_update_type in

    (* Return *)
    update_scope, new_value

(* Directly update a discrete variable *)
(* This function is used for sequential updates *)
and direct_update_with_context variable_names functions_table_opt eval_context update =

    let update_scope, new_value = compute_update_value_opt_with_context variable_names functions_table_opt eval_context update in
    match update_scope with
    | Global_update discrete_index ->
        (* Direct update ! *)
        eval_context.discrete_setter discrete_index new_value
    | Local_update variable_ref ->
        set_local_variable eval_context variable_ref new_value

(* Wrap a scalar value to an array value according to the modified index of an old value *)
(* For example: `old_value[0] = 1` with old value = [0, 1], would wrap new_value into an array `new_value = [1, 1]` *)
(* This function is used to assign an element of an array at a given index *)
(* a = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] *)
(* a[1][1][0] = 0 *)
(* new_value = [[[1, 2], [3, 4]], [[5, 6], [0, 8]]] *)
(* a[1] = [[5, 6], [7, 8]] *)
(* a[1][1] = [7, 8] *)
(* a[1][1][0] = 7 *)
and pack_value variable_names functions_table_opt eval_context_opt old_value new_value scalar_or_index_update_type =

    let rec pack_value_scalar_or_index_update_type = function
        | Scalar_update _ -> old_value, [||], None
        | Indexed_update (inner_scalar_or_index_update_type, index_expr) ->

            let old_value, _, _ = pack_value_scalar_or_index_update_type inner_scalar_or_index_update_type in

            (* Compute index *)
            let index = Int32.to_int (eval_int_expression_with_context variable_names functions_table_opt eval_context_opt index_expr) in
(*            ImitatorUtilities.print_message Verbose_standard ("access index: " ^ string_of_int index ^ "for " ^ string_of_value old_value);*)
            (* Get inner array of discrete value of old value *)
            let old_array = array_value old_value in

            (* Check bounds *)
            if index >= Array.length old_array || index < 0 then (

                (* Create out of range fail message *)
                let str_update_type =
                    match variable_names with
                    | Some variable_names -> DiscreteExpressions.string_of_scalar_or_index_update_type variable_names scalar_or_index_update_type
                    | None -> ""
                in

                raise (Out_of_range ("Index out of range at `" ^ str_update_type ^ "`."))
            );

            (* Get element at given index *)
            let unpacked_old_array = old_array.(index) in
(*            ImitatorUtilities.print_message Verbose_standard ("unpacked old array: " ^ AbstractValue.string_of_value unpacked_old_array);*)
            unpacked_old_array, old_array, Some index
    in

    let unpacked_old_array, old_array, some_index = pack_value_scalar_or_index_update_type scalar_or_index_update_type in
    match some_index with
    | Some index ->
        old_array.(index) <- new_value;
(*        ImitatorUtilities.print_message Verbose_standard ("packed new value is: " ^ AbstractValue.string_of_value old_value);*)
        old_value
    | None -> new_value


(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint_with_context variable_names functions_table_opt eval_context_opt =
  List.for_all (eval_discrete_boolean_expression_with_context variable_names functions_table_opt eval_context_opt)

(* Evaluate an expression *)
let eval_global_expression variable_names functions_table_opt discrete_access_opt = eval_global_expression_with_context variable_names functions_table_opt (create_eval_context_opt discrete_access_opt)
(* Evaluate a boolean expression *)
let eval_boolean_expression variable_names functions_table_opt discrete_access_opt = eval_boolean_expression_with_context variable_names functions_table_opt (create_eval_context_opt discrete_access_opt)
(* Evaluate a discrete boolean expression *)
let eval_discrete_boolean_expression variable_names functions_table_opt discrete_access_opt = eval_discrete_boolean_expression_with_context variable_names functions_table_opt (create_eval_context_opt discrete_access_opt)

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint variable_names functions_table_opt discrete_access =
  List.for_all (eval_discrete_boolean_expression variable_names functions_table_opt (Some discrete_access))

(* Try to evaluate a constant expression, if expression isn't constant, it raise an error *)
let try_eval_constant_global_expression functions_table_opt = eval_global_expression_with_context None functions_table_opt None
(* Try to evaluate a constant non linear constraint, if expression isn't constant, it raise an error *)
let try_eval_nonlinear_constraint functions_table_opt = check_nonlinear_constraint_with_context None functions_table_opt None
(* Try to evaluate a constant rational term, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_term functions_table_opt = eval_rational_term_with_context None functions_table_opt None
(* Try to evaluate a constant rational factor, if expression isn't constant, it raise an error *)
let try_eval_constant_rational_factor functions_table_opt = eval_rational_factor_with_context None functions_table_opt None

let direct_update variable_names functions_table_opt discrete_access = direct_update_with_context variable_names functions_table_opt (create_eval_context discrete_access)
let eval_seq_code_bloc variable_names functions_table_opt discrete_access = eval_seq_code_bloc_with_context variable_names functions_table_opt (create_eval_context discrete_access)

(* Try to evaluate a constant expression, if expression isn't constant, it return None *)
let eval_constant_global_expression_opt functions_table_opt expr = try Some (try_eval_constant_global_expression functions_table_opt expr) with _ -> None
(* Try to evaluate a constant non linear constraint, if expression isn't constant, it return None *)
let eval_nonlinear_constraint_opt functions_table_opt expr = try Some (try_eval_nonlinear_constraint functions_table_opt expr) with _ -> None
(* Try to evaluate a constant rational term, if expression isn't constant, it return None *)
let eval_constant_rational_term_opt functions_table_opt expr = try Some (try_eval_constant_rational_term functions_table_opt expr) with _ -> None
(* Try to evaluate a constant rational factor, if expression isn't constant, it return None *)
let eval_constant_rational_factor_opt functions_table_opt expr = try Some (try_eval_constant_rational_factor functions_table_opt expr) with _ -> None

(* Tricky function to know if an expression is constant *)
let is_global_expression_constant functions_table_opt expr =
    try (
        let _ = try_eval_constant_global_expression functions_table_opt expr in
        true
    )
    with _ -> false

(************************************************************)
(** Matching state predicates with a global location *)
(************************************************************)

(***TODO/NOTE: Might have been nicer to convert the acceptance condition during the ModelConverter phase :-/ ***)
let match_state_predicate variable_names functions_table_opt discrete_access (locations_acceptance_condition : automaton_index -> location_index -> bool) global_location =

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
            eval_discrete_boolean_expression variable_names functions_table_opt (Some discrete_access) expr

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

(* --- Builtin-function evaluations --- *)

(* Note: builtin-function eval are not ideal, because we do checking of parameter values at runtime *)

let bad_arguments_message str_expr =
    "Bad arguments on `" ^ str_expr ^ "`. Expected types or number of arguments doesn't match with actual."

let eval_pow str_expr = function
    | (Abstract_scalar_value (Abstract_number_value (Abstract_rat_value x))) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value exponent)) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_rat_value (NumConst.pow x exponent)))
    | Abstract_scalar_value (Abstract_number_value (Abstract_int_value x)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value exponent)) :: _ ->
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value (OCamlUtilities.pow x exponent)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_mod str_expr = function
    | (Abstract_scalar_value (Abstract_number_value (Abstract_int_value a))) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value b)) :: _ ->
        let i_a, i_b = Int32.to_int a, Int32.to_int b in
        let modulo = Int32.of_int (i_a mod i_b) in
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value modulo))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_int_div str_expr = function
    | (Abstract_scalar_value (Abstract_number_value (Abstract_int_value a))) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value b)) :: _ ->
        let result = Int32.div a b in
        Abstract_scalar_value (Abstract_number_value (Abstract_int_value result))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_rational_of_int str_expr = function
    | Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        ImitatorUtilities.print_warning
            "Conversion of an int expression to a rational expression
            may cause overflow if your platform doesn't manage `int` as an exact 32 bits integer.";
        Abstract_scalar_value (Abstract_number_value (Abstract_rat_value (NumConst.numconst_of_int (Int32.to_int i))))

    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_shift_left str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.shift_left b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_shift_right str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.shift_right b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_fill_left str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.fill_left b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_fill_right str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (b)) :: Abstract_scalar_value (Abstract_number_value (Abstract_int_value (i))) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.fill_right b (Int32.to_int i)))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_and str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (l_binary_word)) :: Abstract_scalar_value (Abstract_bin_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.log_and l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_or str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (l_binary_word)) :: Abstract_scalar_value (Abstract_bin_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.log_or l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_xor str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (l_binary_word)) :: Abstract_scalar_value (Abstract_bin_value (r_binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.log_xor l_binary_word r_binary_word))
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_log_not str_expr = function
    | Abstract_scalar_value (Abstract_bin_value (binary_word)) :: _ ->
        Abstract_scalar_value (Abstract_bin_value (BinaryWord.log_not binary_word))
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
        let fail_message = empty_seq_fail_message str_expr in
        try_eval_list_hd l fail_message
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_list_tl str_expr = function
    | Abstract_container_value (Abstract_list_value l) :: _ ->
        let fail_message = empty_seq_fail_message str_expr in
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
        let fail_message = empty_seq_fail_message str_expr in
        try_eval_stack_pop s fail_message
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        let fail_message = empty_seq_fail_message str_expr in
        try_eval_queue_pop s fail_message
    | _ -> raise (InternalError (bad_arguments_message str_expr))

let eval_stack_top str_expr = function
    | Abstract_container_value (Abstract_stack_value s) :: _ ->
        let fail_message = empty_seq_fail_message str_expr in
        try_eval_stack_top s fail_message
    | Abstract_container_value (Abstract_queue_value s) :: _ ->
        let fail_message = empty_seq_fail_message str_expr in
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