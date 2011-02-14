open Global
open LinearConstraint
open Automaton
open AbstractImitatorFile


(** concretization function *)
val concretize: predicate list -> abstract_state -> state

(** get all consistent abstract states for a location,
    given a list of predicates *)
val get_abstract_states: predicate list -> location -> abstract_state list

(** convert abstract state to string *)
val string_of_abstract_state: abstract_state -> string

val continuous_successors: predicate list -> abstract_state -> abstract_state list
