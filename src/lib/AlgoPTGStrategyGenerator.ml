open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities

type winning_moves_key = Automaton.action_index * state_index

class virtual ['a, 'b] hashTable = object (self)
  val mutable internal_tbl : ('a, 'b) Hashtbl.t = Hashtbl.create 100
	method replace key value = Hashtbl.replace internal_tbl key value
	method find key =  try Hashtbl.find internal_tbl key with
                      Not_found -> 
                        let x = self#bot in Hashtbl.replace internal_tbl key x; x 
	method iter f = Hashtbl.iter f internal_tbl
  method fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c = 
    fun f init -> Hashtbl.fold f internal_tbl init
  method is_empty = Hashtbl.length internal_tbl = 0
  method virtual bot : 'b
end 

class ['a] array (ls : 'a list) = object
  val internal_array : 'a Array.t = Array.of_list ls
  method get = Array.get internal_array 
end

class winningMovesPerAction = object 
  inherit([action_index, LinearConstraint.px_nnconvex_constraint] hashTable) 
  method bot = LinearConstraint.false_px_nnconvex_constraint ()
end

class winningMovesPerState = object
  inherit ([state_index, winningMovesPerAction] hashTable)
  method bot = new winningMovesPerAction
end

class transitionsPerAction = object
  inherit ([action_index, transition_index list] hashTable)
  method bot = []
end
class transitionsPerLocation = object 
  inherit ([location_index, transitionsPerAction] hashTable)
  method bot = new transitionsPerAction
end
class actionsPerLocation = object
  inherit ([location_index, stateIndexSet] hashTable)
  method bot = new stateIndexSet
end

class locationPerStateIndex = object 
  inherit ([state_index, location_index] hashTable)
  method bot = -1
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
  let initial_state_index = state_space#get_initial_state_index in
  let transition_descriptions = Hashtbl.create 100 in
  let transitions = new transitionsPerLocation in 
  let actions_per_location = new actionsPerLocation in
  let transition_counter = ref 0 in 
  let location_counter = ref 0 in 
  let explored = new stateIndexSet in 
  let invariants = ref [] in
  let accepting = ref [] in
  let location_names = ref [] in 
  let location_per_state_index = new locationPerStateIndex in

  let get_invariant state_index = 
    let zone = (state_space#get_state state_index).px_constraint in
    Continuous_guard (LinearConstraint.pxd_of_px_constraint zone)
  in

  let is_accepting state_index = 
    let global_location = state_space#get_location state_index in
    DiscreteState.is_accepting system_model.is_accepting global_location 
  in

  let location_name state_index = 
    let global_location = state_space#get_location state_index in 
    let locations = DiscreteState.get_locations global_location in 
    system_model.location_names 0 (Array.get locations 0)
  in

  let state_to_location state_index = 
    if location_per_state_index#find state_index != -1 then 
      location_per_state_index#find state_index
    else
      begin 
        let fresh_loc = !location_counter in
        location_counter := !location_counter + 1;
        location_per_state_index#replace state_index fresh_loc;

        let invariant = get_invariant state_index in
        invariants := invariant::!invariants;

        let is_accepting = is_accepting state_index in 
        accepting := is_accepting::!accepting;

        let name = location_name state_index in
        location_names := name::!location_names;

        fresh_loc
      end;
  in 

  let add_transition src (transition : transition) = 
    (* Update transition descriptions *)
    Hashtbl.add transition_descriptions !transition_counter transition;
    (* Update transitions table *)
    let transitions_per_action = transitions#find src in 
    let transition_list = transitions_per_action#find transition.action in 
    let transition_list' = !transition_counter::transition_list in 
    transitions_per_action#replace transition.action transition_list';
    transition_counter := !transition_counter + 1;
    (* Update actions per location table *)
    let actions = actions_per_location#find src in 
    actions#add transition.action
  in 

  (* Name: Winning-move-guided exploration *)
  let rec explore state_index location_index = 
    let continue_exploring state_index location_index = 
      if not @@ explored#mem state_index then
        begin
          explored#add state_index;
          explore state_index location_index;
        end
    in

    let successors_with_combined_transitions = state_space#get_successors_with_combined_transitions state_index in 
    let relevant_uncontrollable_successors = List.filter (
      fun (t, state_index') -> 
        let action = StateSpace.get_action_from_combined_transition system_model t in
        let is_uncontrollable = not @@ system_model.is_controllable_action action in
        let has_winning_moves = not @@ (get_winning_moves state_index')#is_empty in
        is_uncontrollable && has_winning_moves
      ) successors_with_combined_transitions in
    List.iter (
      fun (t,state_index') ->
        let location_index' = state_to_location state_index' in             
        let old_transition = system_model.transitions_description (List.hd t) in  (* TODO: Generalize to multiple automata *)
        let new_transition = {old_transition with target = location_index'; updates = (No_potential_update, [])} in 
        add_transition location_index new_transition;
        continue_exploring state_index' location_index'
    ) relevant_uncontrollable_successors;


    let winning_moves = get_winning_moves state_index in
    winning_moves#iter (
      fun state_index' winning_moves_per_action ->
        let location_index' = state_to_location state_index' in   
        winning_moves_per_action#iter (
        fun action_index nnconv_constr -> 
        if not @@ LinearConstraint.px_nnconvex_constraint_is_false nnconv_constr then
          begin 
            let constituent_constrs = LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint nnconv_constr in
            List.iter (
            fun px_linear_constr ->
              let pxd_linear_constr = LinearConstraint.pxd_of_px_constraint px_linear_constr in
              let transition = {
                action = action_index; 
                guard = Continuous_guard pxd_linear_constr; 
                updates = (No_potential_update, []); (* TODO *)
                target = location_index'}
                ; in
              add_transition location_index transition
              ) constituent_constrs
          end);
        continue_exploring state_index' location_index'
    )
  in
  explore initial_state_index (state_to_location initial_state_index);


  let invariants_array = new array @@ List.rev !invariants in
  let accepting_array = new array @@ List.rev !accepting in
  let location_names_array = new array @@ List.rev !location_names in 


  let print_list list f = print_message Verbose_standard (String.concat ";" (List.map f list))
  in

  print_list !accepting string_of_bool;
  print_list !location_names (fun x -> x);
    
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
  transitions#iter 
  (fun state_index transitions_per_action -> 
    transitions_per_action#iter 
    (fun action_index transition_index_list -> 
      print_message Verbose_standard 
      (Printf.sprintf "transitions for location %d, action %d: %s" state_index action_index 
      (String.concat ";" (List.map (fun n -> string_of_int n) transition_index_list)))
    )
  );

  actions_per_location#iter 
  (fun location_index actions -> 
      print_message Verbose_standard 
      (Printf.sprintf "actions for location %d: %s" location_index 
      (String.concat ";" (List.map (fun n -> string_of_int n) actions#all_elements)))
  );



  let controller : simple_abstract_model = {
    nb_automata = system_model.nb_automata;
    nb_actions = system_model.nb_actions;
    nb_clocks  = system_model.nb_clocks; 
    nb_discrete = system_model.nb_discrete; 
    nb_rationals  = system_model.nb_rationals;
    nb_parameters = system_model.nb_parameters;
    nb_variables  = system_model.nb_variables;
    nb_ppl_variables = system_model.nb_ppl_variables;
    nb_locations  = !location_counter;
    nb_transitions = !transition_counter;
    has_invariants = system_model.has_invariants;
    has_complex_updates = system_model.has_complex_updates;
    bounded_parameters = system_model.bounded_parameters;
    parameters_bounds = system_model.parameters_bounds;
    clocks = system_model.clocks;
    is_clock = system_model.is_clock;
    discrete = system_model.discrete;
    discrete_rationals = system_model.discrete_rationals;
    is_discrete = system_model.is_discrete;
    parameters = system_model.parameters;
    clocks_and_discrete = system_model.clocks_and_discrete;
    parameters_and_discrete = system_model.parameters_and_discrete;
    parameters_and_clocks = system_model.parameters_and_clocks;
    variable_names = system_model.variable_names;
    discrete_names_by_type_group = system_model.discrete_names_by_type_group;
    type_of_variables = system_model.type_of_variables;
    automata = [0];
    automata_names = (fun automata_index -> system_model.automata_names automata_index ^ "_controller");
    locations_per_automaton = (fun _ -> List.init (!location_counter) (fun x -> (x-1) + 1));
    location_names = (fun _ location_id -> location_names_array#get location_id ^ "__#" ^ string_of_int location_id);
    is_accepting = (fun _ -> accepting_array#get);
    is_urgent = (fun automata_id -> fun location_id -> false);
    actions = system_model.actions;
    controllable_actions = system_model.controllable_actions;
    action_names = system_model.action_names;
    action_types = system_model.action_types;
    actions_per_automaton = system_model.actions_per_automaton;
    automata_per_action = (fun _ -> [0]);
    actions_per_location = (fun _ -> fun location_index -> (actions_per_location#find location_index)#all_elements);
    is_controllable_action = system_model.is_controllable_action;
    invariants = (fun _ -> invariants_array#get);
    transitions = (fun _ location_index action_index -> (transitions#find location_index)#find action_index);
    transitions_description = (fun transition_index -> Hashtbl.find transition_descriptions transition_index);
    automaton_of_transition = (fun _ -> 0);
    initial_location = DiscreteState.make_location [] [];
    initial_constraint = LinearConstraint.px_false_constraint();
    initial_p_constraint = LinearConstraint.p_false_constraint();
  } in
  let abstract_model = generate_abstract_model controller in
  print_message Verbose_standard @@ ModelPrinter.string_of_model abstract_model;
  print_message Verbose_medium ("Translating model to a graphics…");
		let translated_model = PTA2JPG.string_of_model abstract_model in
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high ("\n" ^ translated_model ^ "\n");
		);
		(*** NOTE: not so nice… ***)

		let dot_created_file_option = Graphics.dot "pdf" ("test123-pta") translated_model in
		begin
		match dot_created_file_option with
		| None -> print_error "Oops…! Something went wrong with dot."
		| Some created_file -> print_message Verbose_standard ("File `" ^ created_file ^ "` successfully created.");
		end;