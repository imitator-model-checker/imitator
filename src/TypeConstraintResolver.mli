open DiscreteType
open FunctionSig
open ParsingStructure

type resolved_constraint =
    | Resolved_type_constraint of DiscreteType.var_type_discrete
    | Resolved_length_constraint of int

val string_of_resolved_constraints : (constraint_name * resolved_constraint) list -> string
val resolve_constraints : variable_infos -> signature_constraint -> signature -> (constraint_name * resolved_constraint) list * (constraint_name * resolved_constraint) list
val signature_of_signature_constraint : (constraint_name, resolved_constraint) Hashtbl.t -> signature_constraint -> signature