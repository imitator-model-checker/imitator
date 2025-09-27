open Lib
open AbstractModel
open Crowbar


let abstract_model : abstract_model gen = 
  map [int] (fun nb_automata -> 
    {
    (* General information **)
    (* Cardinality *)
    nb_automata;
    nb_actions = 0;
    nb_clocks = 0;
    nb_discrete = 0;
    nb_rationals = 0;
    nb_parameters = 0;
    nb_variables = 0;
    (* Nb of variables used in PPL constraint: clocks + parameters + rationals *)
    nb_ppl_variables = 0;
    nb_locations = 0;
    nb_transitions = 0;

    (* Is there any invariant in the model? *)
    has_invariants = false;
    (* Is there any clock going at a rate <> 1 in the model? *)
    has_non_1rate_clocks = false;
    (* Is there any clock reset of another form than x := 0? *)
    has_complex_updates = false;
    (* Is the model an L/U-PTA? *)
    lu_status = PTA_notLU;
    (* Is the model a strongly deterministic PTA? *)
    strongly_deterministic = false;
    (* Does the model contain any transition labeled by a silent, non-observable action? *)
    has_silent_actions = false;
    
    (* Are all parameters bounded in the initial state? *)
    bounded_parameters = false;
    (* Function returning the bounds of each parameter *)
    parameters_bounds = (fun _ -> {upper = Unbounded; lower = Unbounded});

    (* Content of the PTA **)
    (* The observer *)
    observer_pta = None;
    is_observer = (fun _ -> false);

    (* The list of clock indexes *)
    clocks = [];
    (* True for clocks, false otherwise *)
    is_clock = (fun _ -> false);
    (* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
    special_reset_clock = None;
    (* The list of clock indexes except the reset clock (used, e.g., to print the model *)
    clocks_without_special_reset_clock = [];
    (* Index of a special clock meant to measure the global time (how this clock is actually used is up to the model designer *)
    global_time_clock = None;
    (* The list of discrete indexes *)
    discrete = [];
    (* The list of rational indexes *)
    discrete_rationals = [];
    (* True for discrete, false otherwise *)
    is_discrete = (fun _ -> false);
    (* The list of parameter indexes *)
    parameters = [];
    (* The non parameters (clocks and discrete) *)
    clocks_and_discrete = [];
    (* The non clocks (parameters and discrete) *)
    parameters_and_discrete = [];
    (* The non discrete (clocks and parameters) *)
    parameters_and_clocks = [];
    (* The function : variable_index -> variable name *)
    variable_names = (fun _ -> "dummy_var");
    (* All discrete variable names group by types *)
    discrete_names_by_type_group = [];
    (* The type of variables *)
    type_of_variables = (fun _ -> Var_type_clock);

    (* The automata *)
    automata = [];
    (* The automata names *)
    automata_names = (fun _ -> "dummy_automaton");

    (* The locations for each automaton *)
    locations_per_automaton = (fun _ -> []);
    (* The location names for each automaton *)
    location_names = (fun _ _ -> "dummy_loc");
    (* The acceptance for each location *)
    is_accepting = (fun _ _ -> false);
    (* The urgency for each location *)
    is_urgent = (fun _ _ -> false);

    (* All action indexes *)
    actions = [];
    (* Only controllable action indexes *)
    controllable_actions = [];
    (* Has the model a defined list of (un)controllable actions? This is different from the aforementioned list, as the user can define an empty list of (un)controllable actions, in which case this flag still evaluates to true *)
    has_controllable_or_uncontrollable_actions = false;
    (* Action names *)
    action_names = (fun _ -> "dummy_action");
    (* The type of actions *)
    action_types = (fun _ -> Action_type_nosync);
    (* The list of actions for each automaton *)
    actions_per_automaton = (fun _ -> []);
    (* The list of automatons for each action *)
    automata_per_action = (fun _ -> []);
    (* The list of actions for each automaton for each location *)
    actions_per_location = (fun _ _ -> []);
    (* Is an action controllable? *)
    is_controllable_action = (fun _ -> false);

    (* The cost for each automaton and each location *)
    costs = (fun _ _ -> None);

    (* The invariant for each automaton and each location *)
    invariants = (fun _ _ -> True_guard);

    (* The transitions for each automaton and each location and each action *)
    transitions = (fun _ _ _ -> []);
    (* The list of clocks stopped for each automaton and each location *)
    stopwatches = (fun _ _ -> []);
    (* The list of pairs (clock, NumConst.t) defining the flow of some clocks at each automaton and each location *)
    flow = (fun _ _ -> []);
    (* An array transition_index -> transition *)
    transitions_description = (fun _ -> {
      guard = True_guard;
      action = 0;
      updates = No_potential_update, [];
      target = 0;
    });
    (* An array transition_index -> automaton_index *)
    automaton_of_transition = (fun _ -> 0);

    (* The list of declared functions *)
    functions_table = Hashtbl.create 100;

      (* Local variables table type *)
      (*** WARNING (ÉA, 2023/04/14): probably has nothing to do here ***)
    local_variables_table = Hashtbl.create 100;

    (* All clocks non-negative *)
    px_clocks_non_negative = LinearConstraint.px_false_constraint ();
    (* Initial location of the model *)
    initial_location = DiscreteState.make_location [] [];
    (* Initial constraint of the model *)
    initial_constraint = LinearConstraint.px_false_constraint ();
    (* Initial constraint of the model projected onto P *)
    initial_p_constraint = LinearConstraint.p_false_constraint ();
    (* Initial constraint of the model projected onto P and all clocks non-negative *)
    px_clocks_non_negative_and_initial_p_constraint = LinearConstraint.px_false_constraint ();
    }
  )  
