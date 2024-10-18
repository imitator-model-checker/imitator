open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities
open AlgoPTGStrategyGeneratorUtilities
open DefaultHashTable
open AbstractValue

class ['a] array (ls : 'a list) = object
  val internal_array : 'a Array.t = Array.of_list ls
  method get = Array.get internal_array 
end

type strategy_action = 
  | Wait
  | Action of action_index * combined_transition

let (>>) f g x = g (f x)

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
class locationStrategyMap = 
[DiscreteState.global_location, location_strategy ref] defaultHashTable
(fun _ -> ref [])

class locationUnionZoneMap = 
[location_index list,  LinearConstraint.px_nnconvex_constraint] defaultHashTable 
LinearConstraint.false_px_nnconvex_constraint

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

let string_of_strategy_entry (model : abstract_model) (strategy_entry : strategy_entry) =
  match strategy_entry with 
    WaitEntry {prioritized_winning_zone} -> 
      Printf.sprintf "\t(When: %s, then wait until: environment takes an action)" 
    (format_zone_string (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names prioritized_winning_zone))
  | ActionEntry {action;prioritized_winning_zone;winning_move; destination;_} -> 
      Printf.sprintf "\t(When: %s, then wait until: (%s) and then do action: %s to reach state %s)"
      (format_zone_string (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names prioritized_winning_zone))
      (format_zone_string (LinearConstraint.string_of_px_linear_constraint model.variable_names winning_move))
      (model.action_names action) 
      (string_of_global_location model destination)


let string_of_state_strategy (model : abstract_model) (state_strategy : location_strategy) = 
  let strategy_entry_strings = List.rev @@ List.map (string_of_strategy_entry model) state_strategy in
  let state_strategy_string = List.fold_left (fun acc str -> Printf.sprintf "%s%s,\n" acc str) ("") strategy_entry_strings in
  String.sub state_strategy_string 0 (String.length state_strategy_string-2)

let print_strategy (model : abstract_model) ~strategy = 
  print_message Verbose_standard "Printing strategy that ensures controller win:";
  strategy#iter (fun abstract_state_info location_strategy -> 
    let str = string_of_state_strategy model !location_strategy in 
    print_message Verbose_standard @@ Printf.sprintf "%s -> \n%s\n" (string_of_global_location model abstract_state_info) str
  )





class winningMovesPerAction = [action_index, LinearConstraint.px_nnconvex_constraint] defaultHashTable LinearConstraint.false_px_nnconvex_constraint 

class winningMovesPerState = [state_index, winningMovesPerAction] defaultHashTable (fun _ -> new winningMovesPerAction)

class transitionsPerAction = [action_index, transition_index list] defaultHashTable (fun _ -> [])

class transitionsPerLocation = [location_index, transitionsPerAction] defaultHashTable (fun _ -> new transitionsPerAction)

class actionsPerLocation = [location_index, stateIndexSet] defaultHashTable (fun _ -> new stateIndexSet)

class locationPerGlobalLocation = [DiscreteState.global_location, location_index option] defaultHashTable (fun _ -> None)

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

class locationManager initial_count state_space model = object
  val mutable invariants = ref []
  val mutable accepting = ref []
  val mutable urgent = ref []
  val mutable location_names = ref []
  val mutable location_counter = ref initial_count
  val location_per_global_location = new locationPerGlobalLocation
  method create_location (cache : DiscreteState.global_location option)(location_info : location_info) = 
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



let iter_on_nnconvex_px f constr = 
  LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint constr |>
  List.map f |>
  LinearConstraint.px_time_past_assign

let controller_synthesis (system_model : AbstractModel.abstract_model) (options : Options.imitator_options) (state_space : stateSpace) (strategy : locationStrategyMap) =
  let fresh_action = 
     let action_count = ref 0 in 
     fun () -> let fresh = !action_count in action_count := !action_count + 1; fresh
  in 

  let action_of_original_action = 
    let tbl = Hashtbl.create 100 in
    fun lbl -> match Hashtbl.find_opt tbl lbl with 
    Some action -> action 
    | None -> let action = fresh_action () in Hashtbl.add tbl lbl action; action
  in 
     
  let location_manager = new locationManager 0 state_space system_model in 
  let transition_manager = new transitionManager in
  
  let create_urgent_choice_loc_of_global_location global_location = 
    location_manager#create_location (Some global_location) 
    {
      invariant = True_guard;
      is_urgent = true;
      is_accepting = false;
      location_name =
      let base_loc_name = string_of_global_location system_model global_location in
      Printf.sprintf "%s_make_choice" base_loc_name
    }
  in


  (* Add epsilon parameter *)
  let epsilon_param = system_model.nb_variables in 
  let nb_parameters = system_model.nb_parameters + 1 in 
  let nb_variables = system_model.nb_variables + 1 in 
  let variable_names i = if i = epsilon_param then "ε" else system_model.variable_names i in
  let parameters = epsilon_param :: system_model.parameters in 
  let parameters_and_discrete = epsilon_param :: system_model.parameters_and_discrete in 
  let parameters_and_clocks = epsilon_param :: system_model.parameters_and_clocks in



  (* Pre-processing step *)
  let no_upper = LinearConstraint.is_px_linear_upper_bounded >> not in 
  strategy#iter (fun _ location_strategy -> 
    let location_strategy' = 
      !location_strategy |>
      List.map (fun entry -> 
        match entry with 
        | ActionEntry e when no_upper e.winning_move -> 
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
              AlgoStateBased.apply_time_past_no_stopwatch pxd_past;
              let px_past = LinearConstraint.pxd_hide_discrete_and_collapse pxd_past in 
              let intersection = LinearConstraint.px_nnconvex_copy prioritized_winning_zone in 
              LinearConstraint.px_nnconvex_px_intersection_assign intersection px_past;
              let new_entry = ActionEntry {e with winning_move = px_past; prioritized_winning_zone = intersection}  in 
              new_entry::acc
          ) init
        | _ -> [entry]
      ) |>
      List.flatten 
    in 
    location_strategy := location_strategy'
  );

  

  (* Main synthesis step *)
  strategy#iter (fun global_location location_strategy -> 
    (* Create location for src symb state *)
    let base_loc_name = string_of_global_location system_model global_location in 
    let src_urgent_loc = create_urgent_choice_loc_of_global_location global_location in 

    List.iteri (fun i entry -> 
      let connect invariant prioritized_winning_zone =
        let new_loc = location_manager#create_location None
        {
          invariant;
          is_urgent = false;
          is_accepting = false;
          location_name = Printf.sprintf "%s_choose_%d" base_loc_name i
        } in
        List.iter (fun k ->
        let pxd = LinearConstraint.pxd_of_px_constraint k in 
        transition_manager#add_transition src_urgent_loc 
        {
          guard = Continuous_guard pxd;
          action = fresh_action();
          updates = No_potential_update, [];
          target = new_loc;
        }
      ) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint prioritized_winning_zone;
        new_loc
      in
      let choose_loc = match entry with 
      | ActionEntry {winning_move;destination;action;prioritized_winning_zone; transition} -> 
        let winning_move_pxd = LinearConstraint.pxd_of_px_constraint winning_move in 
        let winning_move_past_pxd = LinearConstraint.pxd_copy winning_move_pxd in 
        AlgoStateBased.apply_time_past_no_stopwatch winning_move_past_pxd;


        (* For each entry in strategy create location *)
        let new_loc = connect (Continuous_guard winning_move_past_pxd) prioritized_winning_zone in
        
        let target = create_urgent_choice_loc_of_global_location destination in 
        transition_manager#add_transition new_loc {
          guard = Continuous_guard winning_move_pxd;
          action;
          updates = No_potential_update, [];
          target;
        };
        new_loc
      | WaitEntry {prioritized_winning_zone} -> 
        connect (True_guard) prioritized_winning_zone
      in 
      let symbolic_state_overapproximation : State.state = {global_location; px_constraint = LinearConstraint.px_true_constraint()} in
      AlgoStateBased.combined_transitions_and_states_from_one_state_functional options system_model symbolic_state_overapproximation |>
      List.map (fun (ct, s) -> StateSpace.get_action_from_combined_transition system_model ct, s.global_location) |>
      List.filter (fst >> system_model.is_controllable_action >> not) |>
      
      List.iter (fun (action, global_location) -> 
        let action = action_of_original_action action in 
        let target = create_urgent_choice_loc_of_global_location global_location in 
        transition_manager#add_transition choose_loc {
          guard = True_guard;
          action;
          updates = No_potential_update, [];
          target;
        }
      )
      
      ) (!location_strategy)
  );

  
  
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