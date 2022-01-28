open ParsingStructure
open DiscreteType
open DiscreteValue
open FunctionSig

val signature_constraint_of_function : string -> signature_constraint
val is_function_subject_to_side_effect : string -> bool
val string_of_function_signature_constraint : string -> string
val arity_of_function : string -> int