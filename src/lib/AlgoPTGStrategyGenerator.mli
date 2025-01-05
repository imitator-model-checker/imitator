open AbstractModel
open Automaton
open State
open StateSpace
open AbstractValue
open DiscreteState


type strategy_action = 
  | Wait
  | Action of action_index * combined_transition


type location_strategy_key = global_location_index * global_location

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
  

class locationStrategyMap : object 
	val mutable internal_tbl : (location_strategy_key, location_strategy ref) Hashtbl.t
	method replace : location_strategy_key -> location_strategy ref -> unit
	method find : location_strategy_key -> location_strategy ref    
    method iter : (location_strategy_key -> location_strategy ref -> unit) -> unit
    method fold : 'c. (location_strategy_key -> location_strategy ref -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end

class locationUnionZoneMap : object 
	val mutable internal_tbl : (global_location_index, LinearConstraint.px_nnconvex_constraint) Hashtbl.t
	method replace : global_location_index -> LinearConstraint.px_nnconvex_constraint -> unit
	method find : global_location_index -> LinearConstraint.px_nnconvex_constraint    
    method iter : (global_location_index -> LinearConstraint.px_nnconvex_constraint -> unit) -> unit
    method fold : 'c. (global_location_index -> LinearConstraint.px_nnconvex_constraint -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end


val print_strategy : abstract_model -> stateSpace -> strategy:locationStrategyMap -> unit

val controller_synthesis : abstract_model -> stateSpace -> Options.imitator_options -> locationStrategyMap -> unit