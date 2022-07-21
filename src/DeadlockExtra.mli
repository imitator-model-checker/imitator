(* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

open LinearConstraint

type clock_updates = (Automaton.clock_index * pxd_linear_term) list

open StateSpace

val dl_inverse_time : state_space -> State.state_index -> pxd_linear_constraint -> unit

val dl_weakest_precondition: state_space -> State.state_index -> combined_transition -> State.state_index -> pxd_linear_constraint
