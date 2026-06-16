
(** Controller-strategy synthesis and export for parametric timed games (PTG). *)

open AbstractModel
open Automaton
open StateSpace
open DiscreteState


type location_strategy_key =  global_location

type strategy_action_entry = {
  winning_move : LinearConstraint.px_linear_constraint;
  action : action_index;
  transition : combined_transition;
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
  destination : location_strategy_key
}

type strategy_wait_entry = {
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
}

type strategy_entry = 
  ActionEntry of strategy_action_entry |
  WaitEntry of strategy_wait_entry

type location_strategy = strategy_entry list
  
class locationStrategyMap : [location_strategy_key, location_strategy ref] DefaultHashTable.defaultHashTable

val print_strategy : abstract_model -> strategy:locationStrategyMap -> unit

val controller_synthesis : abstract_model -> stateSpace -> Options.imitator_options -> locationStrategyMap -> callback:(unit -> unit) -> export_controller:(abstract_model -> unit) -> unit