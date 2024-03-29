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

val dl_inverse_time : AbstractModel.abstract_model -> StateSpace.stateSpace -> State.state_index -> pxd_linear_constraint -> unit

val dl_weakest_precondition : AbstractModel.abstract_model -> StateSpace.stateSpace -> State.state_index -> combined_transition -> State.state_index -> pxd_linear_constraint

val dl_predecessor : AbstractModel.abstract_model -> StateSpace.stateSpace -> State.state_index -> px_linear_constraint -> pxd_linear_constraint -> px_linear_constraint -> combined_transition -> pxd_linear_constraint

val dl_instantiate_discrete : AbstractModel.abstract_model -> StateSpace.stateSpace -> State.state_index -> pxd_linear_constraint -> px_linear_constraint

(* Compute all live valuations, i.e., the global precondition allowing to leave a state to a successor via a transition *)
val live_valuations_precondition : AbstractModel.abstract_model -> StateSpace.stateSpace -> State.state_index -> StateSpace.combined_transition -> State.state_index -> px_linear_constraint
