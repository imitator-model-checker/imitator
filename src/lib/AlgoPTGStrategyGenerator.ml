open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities

type winning_moves_key = Automaton.action_index * state_index
module TransitionSet = Set.Make(struct type t = {source : state_index; action : Automaton.action_index; dest : state_index} let compare = Stdlib.compare end)



class virtual ['a, 'b] hashTable = object (self)
  val mutable internal_tbl : ('a, 'b) Hashtbl.t = Hashtbl.create 100
	method replace key zone = Hashtbl.replace internal_tbl key zone
	method find key =  try Hashtbl.find internal_tbl key with
                      Not_found -> 
                        Hashtbl.replace internal_tbl key self#bot; self#bot
	method iter f = Hashtbl.iter f internal_tbl
  method fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c = 
    fun f init -> Hashtbl.fold f internal_tbl init
  method virtual bot : 'b
end 


class winningMovesPerAction = object 
  inherit([action_index, LinearConstraint.px_nnconvex_constraint] hashTable) 
  method bot = LinearConstraint.false_px_nnconvex_constraint ()
end

class winningMovesPerState = object
  inherit ([state_index, winningMovesPerAction] hashTable)
  method bot = new winningMovesPerAction
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



let generate_abstract_model (simple_model : simple_abstract_model) : abstract_model = 
  {
      nb_automata = simple_model.nb_automata;
      nb_actions = simple_model.nb_actions;
      nb_clocks  = simple_model.nb_clocks;
      nb_discrete = simple_model.nb_discrete;
      nb_rationals  = simple_model.nb_rationals;
      nb_parameters = simple_model.nb_parameters;
      nb_variables  = simple_model.nb_variables;
      nb_ppl_variables = simple_model.nb_ppl_variables;
      nb_locations  = simple_model.nb_transitions;
      nb_transitions = simple_model.nb_transitions;
      has_invariants = simple_model.has_invariants;
      has_non_1rate_clocks = false;
      has_complex_updates = simple_model.has_complex_updates;
      lu_status = PTA_notLU;
      strongly_deterministic = false;
      has_silent_actions = false;
      bounded_parameters = simple_model.bounded_parameters;
      parameters_bounds = simple_model.parameters_bounds;
      observer_pta =  None;
      is_observer = (fun _ -> false);
      clocks = simple_model.clocks;
      is_clock = simple_model.is_clock;
      special_reset_clock = None;
      clocks_without_special_reset_clock = simple_model.clocks;
      global_time_clock = None;
      discrete = simple_model.discrete;
      discrete_rationals = simple_model.discrete_rationals;
      is_discrete = simple_model.is_discrete;
      parameters = simple_model.parameters;
      clocks_and_discrete = simple_model.clocks_and_discrete;
      parameters_and_discrete = simple_model.parameters_and_discrete;
      parameters_and_clocks = simple_model.parameters_and_clocks;
      variable_names = simple_model.variable_names;
      discrete_names_by_type_group = simple_model.discrete_names_by_type_group;
      type_of_variables = simple_model.type_of_variables;
      automata = simple_model.automata;
      automata_names = simple_model.automata_names;
      locations_per_automaton = simple_model.locations_per_automaton;
      location_names = simple_model.location_names;
      is_accepting = simple_model.is_accepting;
      is_urgent = simple_model.is_urgent;
      actions = simple_model.actions;
      controllable_actions = simple_model.controllable_actions;
      action_names = simple_model.action_names;
      action_types = simple_model.action_types;
      actions_per_automaton = simple_model.actions_per_automaton;
      automata_per_action = simple_model.automata_per_action;
      actions_per_location = simple_model.actions_per_location;
      is_controllable_action = simple_model.is_controllable_action;
      costs = (fun _ _ -> None);
      invariants = simple_model.invariants;
      transitions = simple_model.transitions;
      stopwatches = (fun _ _ -> []);
      flow = (fun _ _ -> []);
      transitions_description = simple_model.transitions_description;
      automaton_of_transition = simple_model.automaton_of_transition;
      functions_table = Hashtbl.create 0;
      local_variables_table = Hashtbl.create 0;
      px_clocks_non_negative = LinearConstraint.px_false_constraint();
      initial_location = simple_model.initial_location;
      initial_constraint = simple_model.initial_constraint;
      initial_p_constraint = simple_model.initial_p_constraint;
      px_clocks_non_negative_and_initial_p_constraint = LinearConstraint.px_false_constraint();
  }



let generate_controller (system_model : AbstractModel.abstract_model) (get_winning_moves : state_index -> winningMovesPerState) (state_space : stateSpace) = 
  let initial_state = (state_space#get_state (state_space#get_initial_state_index)).global_location in
  let transition_descriptions = Hashtbl.create 100 in
  let transitions = Hashtbl.create 100 in 
  let transition_counter = ref 0 in 
  let found_transitions = [] in 
  let rec explore winning_moves = 
    winning_moves#iter (
      fun state_index winning_moves_per_action -> 
        winning_moves_per_action#iter (
        fun action_index nnconv_constr -> 
        if not @@ LinearConstraint.px_nnconvex_constraint_is_false nnconv_constr then
          begin 
            let constituent_constrs = LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint nnconv_constr in
            List.iter (
            fun px_linear_constr ->
              let pxd_linear_constr = LinearConstraint.pxd_of_px_constraint px_linear_constr in
              Hashtbl.add transition_descriptions !transition_counter 
              {action = action_index; guard = Continuous_guard pxd_linear_constr; updates = (No_potential_update, []); target = state_index};
              transition_counter := !transition_counter + 1
            ) constituent_constrs
          end);
        explore (get_winning_moves state_index)
      )
  in
  explore (get_winning_moves state_space#get_initial_state_index);

  (* DEBUG *)
  Hashtbl.iter 
  (fun transition_index {action = a; guard = g;_} -> 
    match g with 
    | Continuous_guard zone ->
      print_message Verbose_standard (Printf.sprintf "%d: {action: %s, guard: %s}" transition_index (system_model.action_names a)
      (LinearConstraint.string_of_pxd_linear_constraint system_model.variable_names zone))
    | _ -> ()
  )
  transition_descriptions;


  let controller : simple_abstract_model = {
    nb_automata = system_model.nb_automata;
    nb_actions = system_model.nb_actions;
    nb_clocks  = system_model.nb_clocks; 
    nb_discrete = system_model.nb_discrete; 
    nb_rationals  = system_model.nb_rationals;
    nb_parameters = system_model.nb_parameters;
    nb_variables  = system_model.nb_variables;
    nb_ppl_variables = system_model.nb_ppl_variables;
    nb_locations  = system_model.nb_transitions;
    nb_transitions = system_model.nb_transitions;
    has_invariants = system_model.has_invariants;
    has_complex_updates = system_model.has_complex_updates;
    bounded_parameters = system_model.bounded_parameters;
    parameters_bounds = (fun x -> {upper = AbstractModel.Unbounded; lower = AbstractModel.Unbounded});
    clocks = system_model.clocks;
    is_clock = (fun var_id -> false);
    discrete = [];
    discrete_rationals = [];
    is_discrete = (fun var_id -> false);
    parameters = [];
    clocks_and_discrete = [];
    parameters_and_discrete = [];
    parameters_and_clocks = [];
    variable_names = (fun var_id -> "name");
    discrete_names_by_type_group = [];
    type_of_variables = (fun var_id -> Var_type_clock);
    automata = [];
    automata_names = (fun automata_id -> "name");
    locations_per_automaton = (fun automata_id -> []);
    location_names = (fun automata_id -> fun location_id -> "name");
    is_accepting = (fun automata_id -> fun location_id -> false);
    is_urgent = (fun automata_id -> fun location_id -> false);
    actions = [];
    controllable_actions = [];
    action_names = (fun action_id -> "name");
    action_types = (fun action_id -> Action_type_sync);
    actions_per_automaton = (fun automata_id -> []);
    automata_per_action = (fun action_id -> []);
    actions_per_location = (fun automata_id -> fun location_id -> []);
    is_controllable_action = (fun action_id -> false);
    invariants = (fun automaton_index location_index -> True_guard);
    transitions = (fun automaton_index location_index action_index -> []);
    transitions_description = (fun transition_index -> {action = 0; guard = True_guard; updates = (No_potential_update, []); target = 0});
    automaton_of_transition = (fun transition_index -> 0);
    initial_location = DiscreteState.make_location [] [];
    initial_constraint = LinearConstraint.px_false_constraint();
    initial_p_constraint = LinearConstraint.p_false_constraint();
  } in
  ()