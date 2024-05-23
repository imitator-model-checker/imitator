open AbstractModel
open Automaton
open State
open StateSpace


type strategy_entry = {
  action : action_index;
  prioritized_winning_zone : LinearConstraint.px_nnconvex_constraint;
  winning_move : LinearConstraint.px_nnconvex_constraint;
}

type state_strategy = strategy_entry list

type strategy = state_index -> state_strategy

class winningMovesPerAction : object 
	val mutable internal_tbl : (action_index, LinearConstraint.px_nnconvex_constraint) Hashtbl.t
	method replace : action_index -> LinearConstraint.px_nnconvex_constraint -> unit
	method find : action_index -> LinearConstraint.px_nnconvex_constraint    
    method iter : (action_index -> LinearConstraint.px_nnconvex_constraint -> unit) -> unit
    method fold : 'c. (action_index -> LinearConstraint.px_nnconvex_constraint -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
    method bot : LinearConstraint.px_nnconvex_constraint
end

class winningMovesPerState : object 
	val mutable internal_tbl : (state_index, winningMovesPerAction) Hashtbl.t
	method replace : state_index -> winningMovesPerAction -> unit
	method find : state_index -> winningMovesPerAction    
    method iter : (state_index -> winningMovesPerAction -> unit) -> unit
    method fold : 'c. (state_index -> winningMovesPerAction -> 'c -> 'c) -> 'c -> 'c
	method is_empty : bool
    method bot : winningMovesPerAction
end

val print_strategy : abstract_model -> strategy:strategy -> state_indices:state_index list -> state_space:StateSpace.stateSpace -> unit
val generate_controller : abstract_model -> (state_index -> winningMovesPerState) -> stateSpace -> Options.imitator_options -> unit