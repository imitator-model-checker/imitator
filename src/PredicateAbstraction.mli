open Global
open LinearConstraint
open Automaton
open AbstractImitatorFile


(** concretization function *)
val concretize: predicate list -> abstract_state -> state

(** abstraction function *)
val abstract: predicate list -> state -> abstract_state list

(** get all consistent abstract states for a location,
    given a list of predicates *)
val get_abstract_states: predicate list -> location -> abstract_state list

(** get all feasible regions with respect to a list of predicates *)
val get_feasible_regions: predicate list -> linear_constraint list

(** convert abstract state to string *)
val string_of_abstract_state: abstract_state -> string

(** convert abstract state signature to string *)
val string_of_signature: bool list -> string

(** compare two signatures *)
val signatures_equal: bool list -> bool list -> bool
