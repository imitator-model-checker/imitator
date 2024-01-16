open AbstractModel
open Automaton
open State
open StateSpace


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


 

(************************************************************)
(** The abstract model *)
(************************************************************)
type simple_abstract_model = {
	(* General information **)
	(* Cardinality *)
	nb_automata   : int;
	nb_actions    : int;
	nb_clocks     : int;
	nb_discrete   : int;
	nb_rationals   : int;
	nb_parameters : int;
	nb_variables  : int;
	(* Nb of variables used in PPL constraint: clocks + parameters + rationals *)
	nb_ppl_variables : int;
	nb_locations  : int;
	nb_transitions: int;

	(* Is there any invariant in the model? *)
	has_invariants : bool;
	(* Is there any clock reset of another form than x := 0? *)
	has_complex_updates : bool;

	(* Are all parameters bounded in the initial state? *)
	bounded_parameters : bool;
	(* Function returning the bounds of each parameter *)
	parameters_bounds : parameter_index -> bounds;

	(* The list of clock indexes *)
	clocks : clock_index list;
	(* True for clocks, false otherwise *)
	is_clock : variable_index -> bool;
	(* The list of discrete indexes *)
	discrete : discrete_index list;
	(* The list of rational indexes *)
	discrete_rationals : discrete_index list;
	(* True for discrete, false otherwise *)
	is_discrete : variable_index -> bool;
	(* The list of parameter indexes *)
	parameters : parameter_index list;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete : variable_index list;
	(* The non clocks (parameters and discrete) *)
	parameters_and_discrete : variable_index list;
	(* The non discrete (clocks and parameters) *)
	parameters_and_clocks : variable_index list;
	(* The function : variable_index -> variable name *)
	variable_names : variable_index -> variable_name;
	(* All discrete variable names group by types *)
    discrete_names_by_type_group : (DiscreteType.var_type * (variable_name list)) list;
	(* The type of variables *)
	type_of_variables : variable_index -> DiscreteType.var_type;

	(* The automata *)
	automata : automaton_index list;
	(* The automata names *)
	automata_names : automaton_index -> automaton_name;

	(* The locations for each automaton *)
	locations_per_automaton : automaton_index -> location_index list;
	(* The location names for each automaton *)
	location_names : automaton_index -> location_index -> location_name;
	(* The acceptance for each location *)
	is_accepting : automaton_index -> location_index -> bool;
	(* The urgency for each location *)
	is_urgent : automaton_index -> location_index -> bool;

	(* All action indexes *)
	actions : action_index list;
	(* Only controllable action indexes *)
	controllable_actions : action_index list;
	(* Action names *)
	action_names : action_index -> action_name;
	(* The type of actions *)
	action_types : action_index -> action_type;
	(* The list of actions for each automaton *)
	actions_per_automaton : automaton_index -> (action_index list);
	(* The list of automatons for each action *)
	automata_per_action : action_index -> (automaton_index list);
	(* The list of actions for each automaton for each location *)
	actions_per_location : automaton_index -> location_index -> (action_index list);
	(* Is an action controllable? *)
	is_controllable_action : action_index -> bool;

	
	(* The invariant for each automaton and each location *)
	invariants : automaton_index -> location_index -> invariant;

	(* The transitions for each automaton and each location and each action *)
	transitions : automaton_index -> location_index -> action_index -> (transition_index list);
	(* An array transition_index -> transition *)
	transitions_description : transition_index -> transition;
	(* An array transition_index -> automaton_index *)
	automaton_of_transition : transition_index -> automaton_index;

	(* Initial location of the model *)
	initial_location : DiscreteState.global_location;
	(* Initial constraint of the model *)
	initial_constraint : LinearConstraint.px_linear_constraint;
	(* Initial constraint of the model projected onto P *)
	initial_p_constraint : LinearConstraint.p_linear_constraint;
	
}


val generate_controller : abstract_model -> (state_index -> winningMovesPerState) -> stateSpace -> unit