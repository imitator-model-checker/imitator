(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: General fonctions for map, filter, traverse parsing structure tree
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/05
 * Last modified     : 2021/03/05
 *
 ************************************************************)

open ParsingStructure

(* Map the leafs of an arithmetic expression according to map_function *)
(* Leafs are Parsed_DF_variable, Parsed_DF_constant *)
let map_parsed_arithmetic_expression_leafs map_function arithmetic_expr =

    let rec map_parsed_arithmetic_expression_leafs_rec = function
        | Parsed_DAE_plus (arithmetic_expression, term)
        | Parsed_DAE_minus (arithmetic_expression, term) ->
            List.rev_append (map_parsed_arithmetic_expression_leafs_rec arithmetic_expression) (map_parsed_term_leafs term)
        | Parsed_DAE_term term ->
            map_parsed_term_leafs term

    and map_parsed_term_leafs = function
        | Parsed_DT_mul (term, factor)
        | Parsed_DT_div (term, factor) ->
            List.rev_append (map_parsed_term_leafs term) (map_parsed_factor_leafs factor)
        | Parsed_DT_factor factor ->
            map_parsed_factor_leafs factor

    and map_parsed_factor_leafs = function
        | Parsed_DF_variable _
        | Parsed_DF_constant _ as leaf -> [map_function leaf]
        | Parsed_DF_unary_min factor -> map_parsed_factor_leafs factor
        | Parsed_DF_expression arithmetic_expr -> map_parsed_arithmetic_expression_leafs_rec arithmetic_expr
    in

    map_parsed_arithmetic_expression_leafs_rec arithmetic_expr