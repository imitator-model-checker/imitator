open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities
open AlgoPTGStrategyGeneratorUtilities
open DefaultHashTable
open AbstractValue
open DiscreteState

class ['a] array (ls : 'a list) = object
  val internal_array : 'a Array.t = Array.of_list ls
  method get = Array.get internal_array 
end

type strategy_action = 
  | Wait
  | Action of action_index * combined_transition

let (>>) f g x = g (f x)


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

class locationStrategyMap = 
[location_strategy_key, location_strategy ref] defaultHashTable
(fun _ -> ref [])

class locationUnionZoneMap = 
[global_location_index,  LinearConstraint.px_nnconvex_constraint] defaultHashTable 
LinearConstraint.false_px_nnconvex_constraint

let global_location_of_global_location_index state_space i = state_space#get_location i


let format_zone_string (string : string) = 
  let b = Buffer.create 10 in
  String.iter (fun c -> if c == '\n' then Buffer.add_char b ' ' else Buffer.add_char b c) string;
  Buffer.contents b




let string_of_location_list (model : abstract_model) location_list = 
  let location_names = List.mapi model.location_names location_list in 
  let location_list_string = "[" ^ (List.fold_left (fun acc str -> Printf.sprintf "%s%s, " acc str) ("") location_names) in
  String.sub location_list_string 0 (String.length location_list_string-2)  ^ "]"
 
let string_of_discrete_mapping_list (model : abstract_model) discrete_mapping_list = 
  if List.length discrete_mapping_list = 0 then 
    "[]"
  else
    let discrete_valuation_strings = List.map (fun (index, value) -> Printf.sprintf "%s ↦ %s" (model.variable_names index) (string_of_value value)) discrete_mapping_list in  
    let discrete_valuations_string = "[" ^ (List.fold_left (fun acc str -> Printf.sprintf "%s%s, " acc str) ("") discrete_valuation_strings) in 
    String.sub discrete_valuations_string 0 (String.length discrete_valuations_string-2)  ^ "]"  

let locations_and_discrete_of_global_location (model : abstract_model) (global_location : DiscreteState.global_location) =
  let locations = Array.to_list (DiscreteState.get_locations global_location) in 
  let discrete_mapping = List.map 
    (fun index -> (index, DiscreteState.get_discrete_value global_location index))
    model.discrete in
  locations, discrete_mapping
    
let string_of_global_location (model : abstract_model) (global_location : DiscreteState.global_location) =
  let location_list, discrete_mapping_list = locations_and_discrete_of_global_location model global_location in  
  Printf.sprintf "(%s, %s)" (string_of_location_list model location_list) (string_of_discrete_mapping_list model discrete_mapping_list)

let string_of_strategy_entry (model : abstract_model) (state_space : stateSpace) (strategy_entry : strategy_entry) =
  match strategy_entry with 
    WaitEntry {prioritized_winning_zone} -> 
      Printf.sprintf "\t(When: %s, then wait until: environment takes an action)" 
    (format_zone_string (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names prioritized_winning_zone))
  | ActionEntry {action;prioritized_winning_zone;winning_move; destination;_} -> 
      Printf.sprintf "\t(When: %s, then wait until: (%s) and then do action: %s to reach state %s)"
      (format_zone_string (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names prioritized_winning_zone))
      (format_zone_string (LinearConstraint.string_of_px_linear_constraint model.variable_names winning_move))
      (model.action_names action) 
      (string_of_global_location model (snd destination))


let string_of_state_strategy (model : abstract_model) (state_space : stateSpace) (state_strategy : location_strategy) = 
  let strategy_entry_strings = List.rev @@ List.map (string_of_strategy_entry model state_space) state_strategy in
  let state_strategy_string = List.fold_left (fun acc str -> Printf.sprintf "%s%s,\n" acc str) ("") strategy_entry_strings in
  String.sub state_strategy_string 0 (String.length state_strategy_string-2)

let print_strategy (model : abstract_model) state_space ~strategy = 
  print_message Verbose_standard "Printing strategy that ensures controller win:";
  strategy#iter (fun (_, global_location) location_strategy -> 
    let str = string_of_state_strategy model state_space !location_strategy in 
    print_message Verbose_standard @@ Printf.sprintf "%s -> \n%s\n" (string_of_global_location model global_location) str
  )


class winningMovesPerAction = [action_index, LinearConstraint.px_nnconvex_constraint] defaultHashTable LinearConstraint.false_px_nnconvex_constraint 

class winningMovesPerState = [state_index, winningMovesPerAction] defaultHashTable (fun _ -> new winningMovesPerAction)

class transitionsPerAction = [action_index, transition_index list] defaultHashTable (fun _ -> [])

class transitionsPerLocation = [location_index, transitionsPerAction] defaultHashTable (fun _ -> new transitionsPerAction)

class actionsPerLocation = [location_index, stateIndexSet] defaultHashTable (fun _ -> new stateIndexSet)

class locationPerGlobalLocation = [global_location_index, location_index option] defaultHashTable (fun _ -> None)

type location_info = {
  invariant : invariant;
  is_accepting : bool;
  is_urgent : bool;
  location_name : location_name;
}
let get_location_info_from_state (state_space : stateSpace) (model : abstract_model) state_index : location_info = 
  let zone = (state_space#get_state state_index).px_constraint in
  let global_location = state_space#get_location state_index in
  let locations = DiscreteState.get_locations global_location in 

  let invariant = Continuous_guard (LinearConstraint.pxd_of_px_constraint zone) in
  let is_accepting = DiscreteState.is_accepting model.is_accepting global_location in
  let is_urgent = AbstractModelUtilities.is_global_location_urgent model global_location in
  let location_name = model.location_names 0 (Array.get locations 0) in 

  {
    invariant = invariant;
    is_accepting;
    is_urgent;
    location_name;
  }

let cached_array_indexing_from_list list array_opt_ref =
  match !array_opt_ref with 
      None -> 
        let array = new array @@ List.rev !list in 
        array_opt_ref := Some array;
        array#get
    | Some array -> array#get

class locationManager initial_count original_initial_global_location = object
  val mutable initial_location_index = ref None
  val mutable invariants = ref []
  val mutable accepting = ref []
  val mutable urgent = ref []
  val mutable location_names = ref []
  val mutable location_counter = ref initial_count
  val location_per_global_location = new locationPerGlobalLocation
  method create_location (cache : global_location_index option)(location_info : location_info) = 
    let create_new_location () = 
      let fresh_loc = !location_counter in
      location_counter := !location_counter + 1;
      invariants := location_info.invariant::!invariants;
      accepting := location_info.is_accepting::!accepting;
      urgent := location_info.is_urgent::!urgent;
      location_names := location_info.location_name::!location_names;
      fresh_loc
    in 
    match cache with
    | Some global_location -> 
      (match location_per_global_location#find global_location with 
      | Some location_index -> location_index
      | None ->  
        let fresh_loc = create_new_location () in 
        location_per_global_location#replace global_location @@ Some fresh_loc;
        
        if global_location = original_initial_global_location then initial_location_index := Some fresh_loc;
        fresh_loc)
    | None -> 
      create_new_location ()
  val mutable accepting_array = ref None 
  val mutable invariants_array = ref None
  val mutable urgent_array = ref None 
  val mutable location_names_array = ref None
  method nb_locations = !location_counter
  method is_accepting = (fun (_ : state_index) -> cached_array_indexing_from_list accepting accepting_array)
  method invariants = (fun (_ : state_index) -> cached_array_indexing_from_list invariants invariants_array)
  method is_urgent =  (fun (_ : state_index) -> cached_array_indexing_from_list urgent urgent_array)
  method location_names =  (fun (_ : state_index) -> cached_array_indexing_from_list location_names location_names_array)
  method get_initial_location_index = match !initial_location_index with 
  | Some i -> i 
  | None -> raise (Exceptions.InternalError "Error in controller generation: Initial location was never set")
end

class transitionManager = object 
  val descriptions = Hashtbl.create 100
  val mutable transition_counter = ref 0
  val transitions_per_location = new transitionsPerLocation
  val actions_per_location = new actionsPerLocation
  method add_transition (src : location_index) (transition : transition) : unit =
    (* Update transition descriptions *)
    Hashtbl.add descriptions !transition_counter transition;

    (* Update transitions table *)
    let transitions_per_action = transitions_per_location#find src in 
    let transition_list = transitions_per_action#find transition.action in 
    let transition_list' = !transition_counter::transition_list in 
    transitions_per_action#replace transition.action transition_list';
    transition_counter := !transition_counter + 1;
    
    (* Update actions per location table *)
    let actions = actions_per_location#find src in 
    actions#add transition.action
  method actions_per_location = (fun (_ : state_index) location_index -> (actions_per_location#find location_index)#all_elements)
  method transitions = (fun (_ : state_index) location_index action_index -> (transitions_per_location#find location_index)#find action_index)
  method transitions_description = Hashtbl.find descriptions
  method nb_transitions = !transition_counter
end


let location_name_of_global_location (model : abstract_model) (global_location : DiscreteState.global_location) =
  let string_of_location_list (model : abstract_model) location_list = 
    let location_names = List.mapi model.location_names location_list in 
    let location_list_string = (List.fold_left (fun acc str -> Printf.sprintf "%s%s_" acc str) ("") location_names) in
    String.sub location_list_string 0 (String.length location_list_string-1)
  in

  let string_of_discrete_mapping_list (model : abstract_model) discrete_mapping_list = 
    if List.length discrete_mapping_list = 0 then 
      ""
    else
      let discrete_valuation_strings = List.map (fun (index, value) -> Printf.sprintf "%s_is_%s" (model.variable_names index) (string_of_value value)) discrete_mapping_list in  
      let discrete_valuations_string = (List.fold_left (fun acc str -> Printf.sprintf "%s%s_" acc str) ("") discrete_valuation_strings) in 
      String.sub discrete_valuations_string 0 (String.length discrete_valuations_string-1)
  in
  let location_list, discrete_mapping_list = locations_and_discrete_of_global_location model global_location in  
  Printf.sprintf "%s__%s" (string_of_location_list model location_list) (string_of_discrete_mapping_list model discrete_mapping_list)



let controller_synthesis (system_model : AbstractModel.abstract_model) (state_space : stateSpace) (options : Options.imitator_options) (strategy : locationStrategyMap) =
  let fresh_action, get_nb_actions, action_types, action_names, actions = 
     let nb_actions = ref 0 in 
     let action_types = Hashtbl.create 100 in 
     let action_names = Hashtbl.create 100 in 
     let actions_ref = ref [] in 
     (fun (action_type : action_type) name_opt -> 
      let fresh = !nb_actions in nb_actions := !nb_actions + 1;
      Hashtbl.add action_types fresh action_type;
      actions_ref := fresh :: (!actions_ref);
      (match name_opt with 
      | Some name -> Hashtbl.add action_names fresh name
      | None -> Hashtbl.add action_names fresh "tau");
      fresh),
      (fun () -> !nb_actions),
      Hashtbl.find action_types,
      Hashtbl.find action_names,
      (fun () -> !actions_ref)

  in 

  let fresh_trap_name = 
    let nb_traps = ref 0 in 
    fun () -> let fresh = !nb_traps in nb_traps := !nb_traps + 1; Printf.sprintf "trap_%d" fresh
  in

  let action_of_original_action = 
    let tbl = Hashtbl.create 100 in
    fun action_index -> match Hashtbl.find_opt tbl action_index with 
    Some action -> action 
    | None -> 
      let action_name = system_model.action_names action_index in 
      let action_type = Action_type_sync in 
      let action = fresh_action action_type (Some action_name) in Hashtbl.add tbl action_index action; action
  in 
     
  (* Build global_location -> global_location_index map *)
  let global_location_index_of_global_location = 
    let tbl = Hashtbl.create 100 in 
    strategy#iter (fun (global_location_index, global_location) _ -> 
      Hashtbl.add tbl global_location global_location_index
    );
    fun x -> try Some (Hashtbl.find tbl x) with Not_found -> None
  in
  
  let original_initial_global_location_index = state_space#get_global_location_index state_space#get_initial_state_index in 
  let location_manager = new locationManager 0 original_initial_global_location_index in 
  let transition_manager = new transitionManager in
  
  let create_urgent_choice_loc_of_global_location global_location_index_opt = 
    let location_name, is_accepting = match global_location_index_opt with
    Some global_location_index -> 
      let global_location = global_location_of_global_location_index state_space global_location_index in 
      if DiscreteState.is_accepting system_model.is_accepting global_location then
        Printf.sprintf "l%d_ACCEPTING" global_location_index, true (* Mark locations in controller as accepting for visualization purposes *)
      else 
        Printf.sprintf "l%d" global_location_index, false
    | None -> fresh_trap_name (), false
    in
    location_manager#create_location global_location_index_opt
    {
      invariant = True_guard;
      is_urgent = true;
      is_accepting;
      location_name;
    }
  in


  (* Add epsilon parameter *)
  let epsilon_param = system_model.nb_parameters in 
  let nb_parameters = system_model.nb_parameters + 1 in 
  let nb_variables = system_model.nb_variables + 1 in 
  let nb_ppl_variables = system_model.nb_ppl_variables + 1 in 
  let clocks = List.map ((+)1) system_model.clocks in 
  let discrete = List.map ((+)1) system_model.discrete in 
  let discrete_rationals = List.map ((+)1) system_model.discrete_rationals in
  let variable_names i = if i = epsilon_param then "epsilon" else if i < epsilon_param then system_model.variable_names i else  system_model.variable_names (i-1) in
  let parameters = epsilon_param :: system_model.parameters in 
  let parameters_and_discrete = parameters @ discrete in 
  let parameters_and_clocks = parameters @ clocks in
  let clocks_and_discrete = clocks @ discrete in 
  let transitions_description = fun t -> 
    let original_description = system_model.transitions_description t in
    let guard = original_description.guard in 
    let guard' = match guard with 
    | Continuous_guard pxd -> 
      let copy = LinearConstraint.pxd_copy pxd in 
      let pxd' = LinearConstraint.add_dimension_to_pxd_linear_constraint epsilon_param copy in
      Continuous_guard pxd'
    | Discrete_continuous_guard g -> 
      let copy = LinearConstraint.pxd_copy g.continuous_guard in 
      let pxd' = LinearConstraint.add_dimension_to_pxd_linear_constraint epsilon_param copy in 		
      Discrete_continuous_guard {g with continuous_guard = pxd'}
    | True_guard -> True_guard
    | False_guard -> False_guard
    | Discrete_guard x -> Discrete_guard x
    in
    
    {original_description with guard = guard'}
  in 
  let invariants = fun a l -> 
    let invariant = system_model.invariants a l in 
    let invariant' = match invariant with 
    | Continuous_guard pxd -> 
      let copy = LinearConstraint.pxd_copy pxd in 
      Continuous_guard (LinearConstraint.add_dimension_to_pxd_linear_constraint epsilon_param copy)
    | Discrete_continuous_guard g -> 
      let copy = LinearConstraint.pxd_copy g.continuous_guard in 
      let pxd' = LinearConstraint.add_dimension_to_pxd_linear_constraint epsilon_param copy in 
      Discrete_continuous_guard {g with continuous_guard = pxd'}
    | True_guard -> True_guard
    | False_guard -> False_guard
    | Discrete_guard x -> Discrete_guard x
    in
    invariant' in 

  let is_clock x = List.mem x clocks in 
  let is_discrete x = List.mem x discrete in 
  

  LinearConstraint.set_dimensions (nb_parameters) (system_model.nb_clocks) (system_model.nb_discrete);
  State.flush_invariant_cache ();

  let epsilon_term = LinearConstraint.make_p_linear_term [(NumConst.one,epsilon_param)] (NumConst.zero) in
  let initial_epsilon_constraint = LinearConstraint.make_p_constraint [LinearConstraint.make_p_linear_inequality epsilon_term Op_g] in 

  let initial_constraint = LinearConstraint.add_dimension_to_px_linear_constraint epsilon_param (LinearConstraint.px_copy system_model.initial_constraint) in 
  
  let initial_p_constraint = LinearConstraint.add_dimension_to_p_linear_constraint epsilon_param (LinearConstraint.p_copy system_model.initial_p_constraint) in   

  LinearConstraint.px_intersection_assign_p initial_constraint [initial_epsilon_constraint];
  
  let dimension_extended_system_model = {system_model with 
  nb_parameters;nb_variables;parameters_and_discrete;
  parameters_and_clocks;parameters; variable_names; 
  transitions_description;invariants; initial_constraint;initial_p_constraint;
  discrete;clocks} in 
  AlgoStateBased.compute_static_time_polyhedrons dimension_extended_system_model;



  let add_dimension_to_px_nnconvex_constraint variable k = 
    k |> 
    LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint |>
    List.map (fun k -> LinearConstraint.add_dimension_to_px_linear_constraint variable k) |>
    LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints 
  in 

  (* Extend parametric dimension pre-process*)
  strategy#iter (fun _ location_strategy -> 
    let location_strategy' = 
      !location_strategy |>
      List.map (fun entry ->
        match entry with 
        | ActionEntry e -> 
          let {winning_move; prioritized_winning_zone; _} = e in 
          let winning_move = LinearConstraint.add_dimension_to_px_linear_constraint epsilon_param winning_move in 
          let prioritized_winning_zone = add_dimension_to_px_nnconvex_constraint epsilon_param prioritized_winning_zone in 
          ActionEntry {e with winning_move; prioritized_winning_zone}
        | WaitEntry {prioritized_winning_zone} -> 
          let prioritized_winning_zone = add_dimension_to_px_nnconvex_constraint epsilon_param prioritized_winning_zone in 
          WaitEntry  {prioritized_winning_zone}
      ) in 
    location_strategy := location_strategy'
  );
  
  (* Pre-processing step *)
  let no_upper = LinearConstraint.is_px_linear_upper_bounded >> not in
  let px_nnconvex_leq_px_linear nn lin = 
    LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint nn |>
    List.for_all (fun k -> LinearConstraint.px_is_leq k lin)
  in
  strategy#iter (fun _ location_strategy -> 
    let location_strategy' = 
      !location_strategy |>
      List.map (fun entry -> 
        match entry with 
                                                        (* Optimization - urgent is enough if winning zone is included in move*)
        | ActionEntry e when no_upper e.winning_move && not @@ px_nnconvex_leq_px_linear e.prioritized_winning_zone e.winning_move -> 
          let {winning_move;prioritized_winning_zone; _} = e in 
          
          let intersection = LinearConstraint.px_nnconvex_copy prioritized_winning_zone in 
          LinearConstraint.px_nnconvex_px_intersection_assign intersection winning_move;
          let init = 
            if LinearConstraint.px_nnconvex_constraint_is_false intersection then 
              []
            else
              [ActionEntry {e with prioritized_winning_zone = intersection}]
          in

          let bound_in, bound_out = LinearConstraint.epsilon_temporal_lower_bound_px_linear_constraint epsilon_param winning_move in
          LinearConstraint.px_nnconvex_union_assign bound_in bound_out;
          bound_in |>
          LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint |>
          List.fold_left (
            fun acc k ->
              let pxd_past =  LinearConstraint.pxd_of_px_constraint k in  
              AlgoStateBased.apply_time_past_no_stopwatch pxd_past; (* TODO: use normal time past function ?*)
              let px_past = LinearConstraint.pxd_hide_discrete_and_collapse pxd_past in 
              let intersection = LinearConstraint.px_nnconvex_copy prioritized_winning_zone in 
              LinearConstraint.px_nnconvex_px_intersection_assign intersection px_past;
              let new_entry = ActionEntry {e with winning_move = k; prioritized_winning_zone = intersection}  in 
              new_entry::acc
          ) init
        | _ -> [entry]
      ) |>
      List.flatten 
    in 
    location_strategy := location_strategy'
  );


  (* Main synthesis step *)
  strategy#iter (fun (global_location_index, global_location) location_strategy -> 
    (* Create location for src symb state *)
    let src_urgent_loc = create_urgent_choice_loc_of_global_location @@ Some global_location_index in 

    List.iteri (fun i entry -> 
      let connect invariant is_urgent prioritized_winning_zone =
        let new_loc = location_manager#create_location None
        {
          invariant;
          is_urgent;
          is_accepting = false;
          location_name = Printf.sprintf "l%d_%d" global_location_index i
        } in
        List.iter (fun k ->
        let pxd = LinearConstraint.pxd_of_px_constraint k in 
        transition_manager#add_transition src_urgent_loc 
        {
          guard = Continuous_guard pxd;
          action = fresh_action Action_type_nosync None;
          updates = No_potential_update, [];
          target = new_loc;
        }
      ) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint prioritized_winning_zone;
        new_loc
      in
      let choose_loc, invariant = match entry with 
      | ActionEntry {winning_move;destination;action;prioritized_winning_zone; _} -> 
        let winning_move_pxd = LinearConstraint.pxd_of_px_constraint winning_move in 
        let winning_move_past_pxd = LinearConstraint.pxd_copy winning_move_pxd in 
        AlgoStateBased.apply_time_past dimension_extended_system_model global_location winning_move_past_pxd;
        (* Compute if q_i should be urgent *)
        let winning_move_nn = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint winning_move  in
        let winning_zone_included_in_winning_move = LinearConstraint.px_nnconvex_constraint_is_leq prioritized_winning_zone winning_move_nn in
        let winning_move_not_upper_bounded = not @@ LinearConstraint.is_px_linear_upper_bounded winning_move in 
        let is_urgent = winning_zone_included_in_winning_move && winning_move_not_upper_bounded in 
        (* For each entry in strategy create location *)
        let new_loc = connect (Continuous_guard winning_move_past_pxd) is_urgent prioritized_winning_zone in
        
        let target = create_urgent_choice_loc_of_global_location (Some (fst destination)) in 
        transition_manager#add_transition new_loc {
          guard = Continuous_guard winning_move_pxd;
          action = action_of_original_action action;
          updates = No_potential_update, [];
          target;
        };
        new_loc, LinearConstraint.pxd_hide_discrete_and_collapse winning_move_past_pxd
      | WaitEntry {prioritized_winning_zone} -> 
        connect (True_guard) false prioritized_winning_zone, LinearConstraint.px_true_constraint()
      in
      let symbolic_state_overapproximation : State.state = {global_location; px_constraint = invariant} in
      AlgoStateBased.combined_transitions_and_states_from_one_state_functional options dimension_extended_system_model symbolic_state_overapproximation |>
      List.map (fun (ct, s) -> 
        StateSpace.get_action_from_combined_transition dimension_extended_system_model ct, 
        global_location_index_of_global_location s.global_location
        ) |>
      List.filter (fst >> dimension_extended_system_model.is_controllable_action >> not) |>
      
      List.iter (fun (action, global_location_index) -> 
        let action' = action_of_original_action action in 
        let target = create_urgent_choice_loc_of_global_location global_location_index in 
        transition_manager#add_transition choose_loc {
          guard = True_guard;
          action = action';
          updates = No_potential_update, [];
          target;
        }
      )
      ) (!location_strategy)
  );

  let initial_location_index = location_manager#get_initial_location_index in
  let model = generate_abstract_controller_model system_model 
  ~nb_locations: location_manager#nb_locations 
  ~invariants: location_manager#invariants
  ~is_accepting: location_manager#is_accepting
  ~is_urgent: location_manager#is_urgent
  ~location_names: location_manager#location_names
  ~nb_transitions: transition_manager#nb_transitions
  ~transitions: transition_manager#transitions
  ~actions_per_location: transition_manager#actions_per_location
  ~transitions_description: transition_manager#transitions_description
  ~nb_parameters ~nb_variables ~parameters ~variable_names ~parameters_and_clocks ~parameters_and_discrete  
  ~action_types: action_types ~action_names: action_names 
  ~actions:(actions ()) ~actions_per_automaton:(fun _ -> actions()) ~nb_actions:(get_nb_actions ())
  ~has_controllable_or_uncontrollable_actions: system_model.has_controllable_or_uncontrollable_actions (*** TODO: Mikael, please double check ***)
  ~discrete ~clocks ~initial_constraint ~initial_p_constraint
  ~nb_ppl_variables ~is_clock ~discrete_rationals ~is_discrete
  ~clocks_and_discrete
  ~initial_location_index
  in
  
  print_highlighted_message Shell_result Verbose_standard ("\nStrategy generation results:");
  (* Write controller to file *)
  let imi_file_name = options#files_prefix ^ "-controller.imi" in
  let controller_imi_file = open_out imi_file_name in
  output_string controller_imi_file @@ ModelPrinter.string_of_model model; 
  close_out controller_imi_file;
  print_highlighted_message Shell_result Verbose_standard ("Controller model `" ^ imi_file_name ^ "` succesfully created.");

  (* Generate graphical representation of controller *)
  if options#ptg_controller_mode == AbstractAlgorithm.Draw then
    begin
      print_message Verbose_medium ("Translating generated controller model to a graphics…");
      let translated_model = PTA2dot.string_of_model options model in

      let dot_created_file_option = Graphics.dot "pdf" (options#files_prefix ^ "-controller") translated_model in
      begin
      match dot_created_file_option with
      | None -> print_error "Oops…! Something went wrong with dot."
      | Some created_file -> print_highlighted_message Shell_result Verbose_standard ("Graphical representation of controller `" ^ created_file ^ "` successfully created.");
      end;
    end;