open ParsingStructure
open ParsingStructureUtilities
open ParsedValue
open Exceptions
open NumConst
open DiscreteExpressions

module RationalReducer : sig

    val reduce_parsed_boolean_expression : variable_infos -> parsed_boolean_expression -> parsed_boolean_expression
    val reduce_parsed_discrete_arithmetic_expression : variable_infos -> parsed_discrete_arithmetic_expression -> parsed_discrete_arithmetic_expression

end

(*
module type Number_type = sig
    type t
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val equal : t -> t -> bool
    val one : t
    val zero : t
    val of_parsed_value : parsed_value -> t
    val to_parsed_value : t -> parsed_value
end

module MakeReducer (Number : Number_type) : sig

    type reduced_non_linear_arithmetic_expression =
        | Reduced_cons of Number.t
        | Reduced_var of Number.t * variable_ref
        | Reduced_term of parsed_discrete_term
        | Reduced_factor of parsed_discrete_factor

    val reduce_parsed_discrete_arithmetic_expression : parsed_discrete_arithmetic_expression -> parsed_discrete_arithmetic_expression
    val reduce_parsed_boolean_expression : parsed_boolean_expression -> parsed_boolean_expression
end
*)