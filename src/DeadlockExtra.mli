(* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

open LinearConstraint

type clock_updates = (Automaton.clock_index * pxd_linear_term) list

val dl_inverse_time : px_linear_constraint -> px_linear_constraint

open StateSpace

val dl_weakest_precondition: state_space -> State.state_index -> combined_transition -> State.state_index -> px_linear_constraint
