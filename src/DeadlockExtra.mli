(************************************************************
 *
 *                       IMITATOR
 *
 * Module description: utilities for deadlock checking
 *
 * File contributors : Mikael Bisgaard Dahlsen-Jensen, Jaco van de Pol
 * Created           : 2022
 *
 ************************************************************)

 (* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

open LinearConstraint
open StateSpace

val dl_inverse_time : state_space -> State.state_index -> pxd_linear_constraint -> unit

val dl_weakest_precondition: state_space -> State.state_index -> combined_transition -> State.state_index -> pxd_linear_constraint

val dl_instantiate_discrete: state_space -> State.state_index -> pxd_linear_constraint -> px_linear_constraint