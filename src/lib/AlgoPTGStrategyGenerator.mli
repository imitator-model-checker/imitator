open AbstractModel
open Automaton
open State
open StateSpace
open AbstractValue


type strategy_action = 
  | Wait
  | Action of action_index * combined_transition



type strategy_action_entry = {
  winning_move : LinearConstraint.px_linear_constraint;
  action : action_index;
  transition : combined_transition;
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
  destination : DiscreteState.global_location
}

type strategy_wait_entry = {
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
}

type strategy_entry = 
  ActionEntry of strategy_action_entry |
  WaitEntry of strategy_wait_entry

type location_strategy = strategy_entry list
  

class locationStrategyMap : object 
	val mutable internal_tbl : (DiscreteState.global_location, location_strategy ref) Hashtbl.t
	method replace : DiscreteState.global_location -> location_strategy ref -> unit
	method find : DiscreteState.global_location -> location_strategy ref    
    method iter : (DiscreteState.global_location -> location_strategy ref -> unit) -> unit
    method fold : 'c. (DiscreteState.global_location -> location_strategy ref -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end

class locationUnionZoneMap : object 
	val mutable internal_tbl : (location_index list, LinearConstraint.px_nnconvex_constraint) Hashtbl.t
	method replace : location_index list -> LinearConstraint.px_nnconvex_constraint -> unit
	method find : location_index list -> LinearConstraint.px_nnconvex_constraint    
    method iter : (location_index list -> LinearConstraint.px_nnconvex_constraint -> unit) -> unit
    method fold : 'c. (location_index list -> LinearConstraint.px_nnconvex_constraint -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end


class winningMovesPerAction : object 
	val mutable internal_tbl : (action_index, LinearConstraint.px_nnconvex_constraint) Hashtbl.t
	method replace : action_index -> LinearConstraint.px_nnconvex_constraint -> unit
	method find : action_index -> LinearConstraint.px_nnconvex_constraint    
    method iter : (action_index -> LinearConstraint.px_nnconvex_constraint -> unit) -> unit
    method fold : 'c. (action_index -> LinearConstraint.px_nnconvex_constraint -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end

class winningMovesPerState : object 
	val mutable internal_tbl : (state_index, winningMovesPerAction) Hashtbl.t
	method replace : state_index -> winningMovesPerAction -> unit
	method find : state_index -> winningMovesPerAction    
    method iter : (state_index -> winningMovesPerAction -> unit) -> unit
    method fold : 'c. (state_index -> winningMovesPerAction -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
end

val print_strategy : abstract_model -> strategy:locationStrategyMap -> unit

val controller_synthesis : abstract_model -> Options.imitator_options -> stateSpace -> locationStrategyMap -> unit