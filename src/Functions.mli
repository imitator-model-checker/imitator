open ParsingStructure
open DiscreteType
open DiscreteValue
open FunctionSig


val dynamic_function_call : string -> discrete_value list -> discrete_value
val signature_constraint_of_function : string -> signature_constraint
val string_of_function_signature_constraint : string -> string
val arity_of_function : string -> int