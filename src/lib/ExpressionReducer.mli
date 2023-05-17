(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Module that aims to reduce and simplify parsed expressions, this module is only used to reduce clock updates
 *
 * File contributors : Benjamin L.
 * Created           : 2023/01/06
 *
 ************************************************************)

open ParsingStructure
open ParsingStructureUtilities
open ParsedValue
open Exceptions
open NumConst
open DiscreteExpressions

(* Module that aims to reduce parsed rational arithmetic expressions *)
module RationalReducer : sig

    (* Reduce and simplify a parsed Boolean expression (that hold a rational arithmetic expression) *)
    val reduce_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> parsed_boolean_expression
    (* Reduce and simplify a parsed rational arithmetic expression *)
    val reduce_parsed_discrete_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> parsed_discrete_arithmetic_expression

end