open AbstractModel
open Automaton
open State
open StateSpace
open AbstractValue


type strategy_entry = {
  action : action_index;
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
  winning_move : LinearConstraint.px_nnconvex_constraint;
}

type location_strategy = strategy_entry list
type discrete_mapping = variable_index * abstract_value
type location_strategy_key = location_index list * discrete_mapping list


class locationStrategyMap : object 
	val mutable internal_tbl : (location_strategy_key, location_strategy ref) Hashtbl.t
	method replace : location_strategy_key -> location_strategy ref -> unit
	method find : location_strategy_key -> location_strategy ref    
    method iter : (location_strategy_key -> location_strategy ref -> unit) -> unit
    method fold : 'c. (location_strategy_key -> location_strategy ref -> 'c -> 'c) -> 'c -> 'c
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

val print_strategy : abstract_model -> strategy:locationStrategyMap -> state_space:StateSpace.stateSpace -> unit
val generate_controller : abstract_model -> (state_index -> winningMovesPerState) -> stateSpace -> Options.imitator_options -> unit