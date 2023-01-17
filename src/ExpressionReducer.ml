open DiscreteExpressions
open ParsingStructure
open ParsingStructureUtilities
open ParsedValue
open Exceptions
open Extensions

(* Only reduce rational expression *)
module RationalReducer = struct

    (* Term element *)
    type term_element =
        | Cons_term of NumConst.t
        | Var_term of NumConst.t * variable_ref
        | Term_term of NumConst.t * parsed_discrete_term

    (* Combine left and right terms when multiplying a list of terms by another *)
    (* e.g: (1 + 2 - 3) * (4 + 5) = 1*4 + 1*5 + 2*4 + 2*5 + -3*4 + -3*5 = 0 *)
    let combine_mul l_terms r_terms =
        let terms_combination = OCamlUtilities.list_combination_2 l_terms r_terms in

        let compute (a, b) = match a, b with
            | Cons_term c1, Cons_term c2 -> Cons_term (NumConst.mul c1 c2)
            | Cons_term c1, Var_term (c2, v)
            | Var_term (c2, v), Cons_term c1 -> Var_term (NumConst.mul c1 c2, v)
            | Var_term (c1, v1), Var_term (c2, v2) ->
                (* (c1 * c2) * v1 * v2 *)
                Term_term (
                    NumConst.mul c1 c2,
                    Parsed_product_quotient (
                        Parsed_factor (Parsed_variable v1),
                        Parsed_variable v2,
                        Parsed_mul
                    )
                )
            | Cons_term c1, Term_term (c2, t)
            | Term_term (c1, t), Cons_term c2 ->
                Term_term (NumConst.mul c1 c2, t)

            | Var_term (c1, v), Term_term (c2, t)
            | Term_term (c1, t), Var_term (c2, v) ->
                Term_term (
                    NumConst.mul c1 c2,
                    Parsed_product_quotient (
                        Parsed_factor (Parsed_variable v),
                        Parsed_nested_expr (Parsed_term t),
                        Parsed_mul
                    )
                )
            | Term_term (c1, t1), Term_term (c2, t2) ->
                Term_term (
                    NumConst.mul c1 c2,
                    Parsed_product_quotient (
                        Parsed_factor (Parsed_nested_expr (Parsed_term t1)),
                        Parsed_nested_expr (Parsed_term t2),
                        Parsed_mul
                    )
                )
        in

        List.map compute terms_combination

    (* Inverse a term (e.g: inverse (2a) = 1 / 2a ) *)
    let inverse = function
        | Cons_term c -> Cons_term (NumConst.div NumConst.one c)
        | Var_term (c, v) -> Var_term (NumConst.div NumConst.one c, v)
        | Term_term (c, t) ->
            Term_term (
                c,
                Parsed_product_quotient (
                    Parsed_factor (Parsed_constant (Rat_value NumConst.one)),
                    Parsed_nested_expr (Parsed_term t),
                    Parsed_div
                )
            )

    (* Negate a term (e.g: negate (2a) = -2a *)
    let negate = function
        | Cons_term c -> Cons_term (NumConst.mul c NumConst.minus_one)
        | Var_term (c, v) -> Var_term (NumConst.mul c NumConst.minus_one, v)
        | Term_term (c, t) -> Term_term (NumConst.mul c NumConst.minus_one, t)

    (* Function that reduce a list of terms by grouping similar terms *)
    let reduce_terms terms =
        (* Separate var and coefs, complex expression and compute the sum of all constants found *)
        let weighted_vars = List.filter_map (function Var_term (c, v) -> Some (c, v) | _ -> None) terms in
        let complex_expressions = List.filter_map (function Term_term (c, t) -> Some (c ,t) | _ -> None) terms in
        let constants_sum = terms |> List.filter_map (function Cons_term c -> Some c | _ -> None) |> List.fold_left NumConst.add NumConst.zero in

        (* Group variable by ref (name * id), ex: [(x, [3;-2]); (y, [1])] *)
        let wv_grouped_by_variable_ref = OCamlUtilities.group_by_and_map (fun (_, v) -> v) (fun (c, _) -> c) weighted_vars in
        (* Compute for each variable the coef by adding them together (ex: for [(x, [3;-2]); (y, [1])] we obtain [(x, 1); (y, 1)] ) *)
        let weighted_vars_without_duplicates = List.filter_map (fun (v, coefs) ->
            (* Sum all coefs *)
            let sum_coef = List.fold_left NumConst.add NumConst.zero coefs in
            (* If sum is zero, filter out, else keep variable with coef summed up *)
            if NumConst.equal sum_coef NumConst.zero then None else Some (Var_term (sum_coef, v))
        ) wv_grouped_by_variable_ref in

        (* Group expressions if identical *)
        let ce_grouped = OCamlUtilities.group_by_and_map (fun (_, t) -> t) (fun (c, _) -> c) complex_expressions in
        let ce_without_duplicates = List.filter_map (fun (t, coefs) ->
            (* Sum all coefs *)
            let sum_coef = List.fold_left NumConst.add NumConst.zero coefs in
            (* If sum is zero, filter out, else keep variable with coef summed up *)
            if NumConst.equal sum_coef NumConst.zero then None else Some (Term_term (sum_coef, t))
        ) ce_grouped in

        (* Return *)
        weighted_vars_without_duplicates @ ce_without_duplicates @ [Cons_term constants_sum]

    (* Convert an arithmetic expression to a list of terms *)
    (* This function progressively (recursively) reduce list of terms *)
    let rec terms_of_discrete_arithmetic_expression = function
        | Parsed_sum_diff (expr, term, parsed_sum_diff) ->

            let l_terms = terms_of_discrete_arithmetic_expression expr in
            let r_terms = terms_of_discrete_term term in

            (* As terms are added if minus we add a negate term on the right *)
            let r_terms =
                match parsed_sum_diff with
                | Parsed_plus -> r_terms
                | Parsed_minus ->
                    (match r_terms with
                    | [] -> []
                    | r_terms -> List.map negate r_terms
                    )
            in

            reduce_terms (l_terms @ r_terms)

        | Parsed_term term -> terms_of_discrete_term term

    and terms_of_discrete_term = function
        | Parsed_product_quotient (term, factor, parsed_product_quotient) as pq ->
            (match parsed_product_quotient with
            | Parsed_div ->
                let l_terms = terms_of_discrete_term term in
                let r_terms = terms_of_discrete_factor factor in
                (match r_terms with
                (* Only reducible case (a0, ..., an) / k => (a0, ..., an) * (1 / k) *)
                | x :: [] -> combine_mul [inverse x] l_terms
                | _ -> [Term_term (NumConst.one, pq)] (* return *)
                )
            | Parsed_mul ->
                let l_terms = terms_of_discrete_term term in
                let r_terms = terms_of_discrete_factor factor in
                combine_mul l_terms r_terms
            )

        | Parsed_factor factor -> terms_of_discrete_factor factor


    and terms_of_discrete_factor = function
        | Parsed_variable variable_ref -> [Var_term (NumConst.one, variable_ref)]
        | Parsed_constant value -> [Cons_term (ParsedValue.to_numconst_value value)]
        | Parsed_nested_expr expr ->
            let terms = terms_of_discrete_arithmetic_expression expr in
            reduce_terms terms

        | Parsed_unary_min factor ->
            List.map negate (terms_of_discrete_factor factor)

        | Parsed_sequence _
        | Parsed_access _
        | Parsed_function_call _ as factor -> [Term_term (NumConst.one, Parsed_factor factor)]

    (* Reduce inner arithmetic expressions of an expression *)
    let rec reduce_parsed_boolean_expression variable_infos = function
        | Parsed_conj_dis (l_expr, r_expr, parsed_conj_dis) ->
            Parsed_conj_dis (
                reduce_parsed_boolean_expression variable_infos l_expr,
                reduce_parsed_boolean_expression variable_infos r_expr,
                parsed_conj_dis
            )

        | Parsed_discrete_bool_expr expr ->
            Parsed_discrete_bool_expr (reduce_parsed_discrete_boolean_expression variable_infos expr)

    and reduce_parsed_discrete_boolean_expression variable_infos = function
        | Parsed_arithmetic_expr expr ->
            Parsed_arithmetic_expr (reduce_parsed_discrete_arithmetic_expression variable_infos expr)

        | Parsed_comparison (l_expr, parsed_relop, r_expr) ->
            Parsed_comparison (
                reduce_parsed_discrete_boolean_expression variable_infos l_expr,
                parsed_relop,
                reduce_parsed_discrete_boolean_expression variable_infos r_expr
            )

        | Parsed_comparison_in (lw_expr, md_expr, up_expr) ->
            Parsed_comparison_in (
                reduce_parsed_discrete_arithmetic_expression variable_infos lw_expr,
                reduce_parsed_discrete_arithmetic_expression variable_infos md_expr,
                reduce_parsed_discrete_arithmetic_expression variable_infos up_expr
            )

        | Parsed_nested_bool_expr expr ->
            Parsed_nested_bool_expr (reduce_parsed_boolean_expression variable_infos expr)
        | Parsed_not expr ->
            Parsed_not (reduce_parsed_boolean_expression variable_infos expr)

    and reduce_parsed_discrete_arithmetic_expression variable_infos expr =

        (* Convert term of terms list to parsed term of parsing structure *)
        (* Get absolute value of coefficient, because operator + or - will be deduced by sign_of_term function *)
        let term_to_parsed_term = function
            | Cons_term c ->
                Parsed_factor (Parsed_constant (Rat_value (NumConst.abs c)))
            | Var_term (c, v) ->
                Parsed_product_quotient (Parsed_factor (Parsed_constant (Rat_value (NumConst.abs c))), Parsed_variable v, Parsed_mul)
            | Term_term (c, t) ->
                Parsed_product_quotient (
                    Parsed_factor (Parsed_constant (Rat_value (NumConst.abs c))),
                    Parsed_nested_expr (Parsed_term t),
                    Parsed_mul
                )
        in

        (* Get sign of term *)
        let sign_of_term = function
            | Cons_term c
            | Var_term (c, _)
            | Term_term (c, _) ->
                (* c < 0 => Parsed_minus, else Parsed_plus *)
                if NumConst.l c NumConst.zero then
                    Parsed_minus
                else
                    Parsed_plus
        in

        (* Function that create arithmetic expression from list of terms *)
        let rec parsed_discrete_arithmetic_expression_of_terms = function
            | x :: xs ->
                Parsed_sum_diff (parsed_discrete_arithmetic_expression_of_terms xs, term_to_parsed_term x, sign_of_term x)
            | [] -> Parsed_term (Parsed_factor (Parsed_constant (Rat_value NumConst.zero)))
        in

        (* Simplify an arithmetic expression *)
        (* Remove multiplication by one, zero, addition by zero, and subtraction of zero *)
        let rec simplify_parsed_discrete_arithmetic_expression = function
            | Parsed_sum_diff (expr, term, parsed_sum_diff) ->
                let simplified_expr = simplify_parsed_discrete_arithmetic_expression expr in
                let simplified_term = simplify_parsed_discrete_term term in

                let is_plus = match parsed_sum_diff with Parsed_plus -> true | _ -> false in

                (match simplified_expr, simplified_term with
                (* 0 + v = v *)
                | Parsed_term (Parsed_factor (Parsed_constant (Rat_value c))), term when NumConst.equal c NumConst.zero && is_plus ->
                    Parsed_term term
                (* 0 - v = -v *)
                | Parsed_term (Parsed_factor (Parsed_constant (Rat_value c))), term when NumConst.equal c NumConst.zero && (not is_plus) ->
                    Parsed_term (Parsed_factor (Parsed_unary_min (Parsed_nested_expr (Parsed_term term))))
                (* v + 0 = v *)
                | expr, Parsed_factor (Parsed_constant (Rat_value c)) when NumConst.equal c NumConst.zero ->
                    expr
                (* a . b = a . b *)
                | expr, term ->
                    Parsed_sum_diff (expr, term, parsed_sum_diff)
                )
            | Parsed_term term ->
                Parsed_term (simplify_parsed_discrete_term term)

        and simplify_parsed_discrete_term = function
            | Parsed_product_quotient (term, factor, parsed_product_quotient) ->
                let simplified_term = simplify_parsed_discrete_term term in
                let simplified_factor = simplify_parsed_discrete_factor factor in

                (match parsed_product_quotient with
                | Parsed_mul ->
                    (match simplified_term, simplified_factor with
                    (* 1 * v = v *)
                    | Parsed_factor (Parsed_constant (Rat_value c)), factor when NumConst.equal c NumConst.one ->
                        Parsed_factor factor
                    (* v * 1 = v *)
                    | term, Parsed_constant (Rat_value c) when NumConst.equal c NumConst.one ->
                        term
                    (* 0 * v = 0 *)
                    | Parsed_factor (Parsed_constant (Rat_value c)), _
                    (* v * 0 = 0 *)
                    | _, Parsed_constant (Rat_value c) when NumConst.equal c NumConst.zero ->
                        Parsed_factor (Parsed_constant (Rat_value NumConst.zero))

                    | term, factor ->
                        Parsed_product_quotient (term, factor, parsed_product_quotient)
                    )
                | Parsed_div ->
                    (match simplified_term, simplified_factor with
                    (* 0 / x = 0 *)
                    | Parsed_factor (Parsed_constant (Rat_value c)), _ when NumConst.equal c NumConst.zero ->
                        Parsed_factor (Parsed_constant (Rat_value NumConst.zero))
                    (* x / 1 = x *)
                    | term, Parsed_constant (Rat_value c) when NumConst.equal c NumConst.one ->
                        term
                    | term, factor ->
                        Parsed_product_quotient (term, factor, parsed_product_quotient)
                    )
                )

            | Parsed_factor factor ->
                Parsed_factor (simplify_parsed_discrete_factor factor)

        and simplify_parsed_discrete_factor = function
            | Parsed_nested_expr expr ->
                let simplified_expr = simplify_parsed_discrete_arithmetic_expression expr in
                (match simplified_expr with
                | Parsed_term (Parsed_factor factor) -> factor
                | _ ->
                    Parsed_nested_expr simplified_expr
                )

            | Parsed_unary_min factor ->
                Parsed_unary_min (simplify_parsed_discrete_factor factor)

            | Parsed_variable _
            | Parsed_constant _
            | Parsed_sequence _
            | Parsed_access _
            | Parsed_function_call _ as x -> x
        in

        (* Three step to reduce expression *)

        (* 1. Get arithmetic expression as list of terms, and reduce progressively list of terms by grouping similar terms *)
        let terms = terms_of_discrete_arithmetic_expression expr in
        (* 2. Convert list of terms to arithmetic expression *)
        let reduced_expr = parsed_discrete_arithmetic_expression_of_terms (List.rev terms) in
        (* 3. Simplify reduced arithmetic expression *)
        let simplified_expr = simplify_parsed_discrete_arithmetic_expression reduced_expr in


        let lazy_reduce_message = lazy (
            (*
            (* Convert term to string *)
            let string_of_term = function
                | Cons_term c -> if NumConst.neq c NumConst.zero then NumConst.to_string c else ""
                | Var_term (c, (vn, _)) -> if NumConst.neq c NumConst.zero then (NumConst.to_string c) ^ vn else ""
                | Term_term (c, t) -> if NumConst.neq c NumConst.zero then (NumConst.to_string c) ^ Constants.default_arithmetic_string.mul_string ^ ParsingStructureUtilities.string_of_parsed_term variable_infos t else ""
            in

            let str_terms_list = List.map string_of_term terms in
            let str_terms = OCamlUtilities.string_of_list_of_string_with_sep_without_empty_strings " + " str_terms_list in
            *)
            let str_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos expr in
            let str_reduced_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos reduced_expr in
            let str_simplified_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos simplified_expr in

            "Reduce expression `" ^ str_expr ^ "` -> `" ^ str_reduced_expr ^ "` -> `" ^ str_simplified_expr ^ "`."
        )
        in

        let lazy_reduce_struct = lazy (
            let str_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos expr in
            let str_reduced_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos reduced_expr in
            let str_simplified_expr = ParsingStructureUtilities.string_of_parsed_arithmetic_expression variable_infos simplified_expr in

            JsonFormatter.Json_struct [
                "expr", JsonFormatter.Json_string str_expr;
                "reduced_expr", JsonFormatter.Json_string str_reduced_expr;
                "simplified_expr", JsonFormatter.Json_string str_simplified_expr
            ]
        )
        in

        (* Only compute string and print if verbose >= high *)
        ImitatorUtilities.print_message_lazy Verbose_high lazy_reduce_message;
        (* TODO create special verbose mode for logging *)
        ImitatorUtilities.log_detail_in_array_lazy Verbose_high "expr_reducing" lazy_reduce_struct;

        (* Return simplified expression *)
        simplified_expr

end