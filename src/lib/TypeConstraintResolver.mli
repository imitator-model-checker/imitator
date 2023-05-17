(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description:
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

(* Parsing structure modules *)
open ParsingStructure
open DiscreteType

(**)
open FunctionSig

(* Represent a resolved constraint *)
(* Resolved constraint can be a type or a length value (in case of array, binary word lengths) *)
type resolved_constraint =
    | Resolved_type_constraint of DiscreteType.var_type_discrete
    | Resolved_length_constraint of int

val string_of_resolved_constraints : (constraint_name * resolved_constraint) list -> string

(* Given a discrete type, resolve the constraint type or the constraint length of a type constraint *)
val resolve_constraints : variable_infos -> signature_constraint -> signature -> (constraint_name * resolved_constraint) list * (constraint_name * resolved_constraint) list

val signature_of_signature_constraint : (constraint_name, resolved_constraint) Hashtbl.t -> signature_constraint -> signature