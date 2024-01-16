open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities
open AlgoPTGStrategyGeneratorUtilities

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



let generate_controller (system_model : AbstractModel.abstract_model) (get_winning_moves : state_index -> winningMovesPerState) (state_space : stateSpace) (options : Options.imitator_options) = 
  let initial_state_index = state_space#get_initial_state_index in
  let transitions_description_tbl = Hashtbl.create 100 in
  let transitions_tbl = new transitionsPerLocation in 
  let actions_per_location_tbl = new actionsPerLocation in
  let transition_counter = ref 0 in 
  let location_counter = ref 0 in 
  let explored = new stateIndexSet in 
  let invariants = ref [] in
  let accepting = ref [] in
  let urgent = ref [] in
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

  let is_urgent state_index = 
    let global_location = state_space#get_location state_index in   
    AbstractModelUtilities.is_global_location_urgent system_model global_location
  in

  let location_name state_index = 
    let global_location = state_space#get_location state_index in 
    let locations = DiscreteState.get_locations global_location in 
    system_model.location_names 0 (Array.get locations 0)
  in

  let get_location state_index = 
    (* Find it if we already created it *)
    if location_per_state_index#find state_index != -1 then 
      location_per_state_index#find state_index
    else
    (* Otherwise create a new one *)
      begin 
        let fresh_loc = !location_counter in
        location_counter := !location_counter + 1;
        location_per_state_index#replace state_index fresh_loc;

        let invariant = get_invariant state_index in
        invariants := invariant::!invariants;

        let is_accepting = is_accepting state_index in 
        accepting := is_accepting::!accepting;

        let is_urgent = is_urgent state_index in
        urgent := is_urgent::!urgent;

        let name = location_name state_index in
        location_names := name::!location_names;

        fresh_loc
      end;
  in 

  let add_transition src (transition : transition) = 
    (* Update transition descriptions *)
    Hashtbl.add transitions_description_tbl !transition_counter transition;
    
    (* Update transitions table *)
    let transitions_per_action = transitions_tbl#find src in 
    let transition_list = transitions_per_action#find transition.action in 
    let transition_list' = !transition_counter::transition_list in 
    transitions_per_action#replace transition.action transition_list';
    transition_counter := !transition_counter + 1;
    
    (* Update actions per location table *)
    let actions = actions_per_location_tbl#find src in 
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
        let location_index' = get_location state_index' in             
        let old_transition = system_model.transitions_description (List.hd t) in  (* TODO: Generalize to multiple automata *)
        let new_transition = {old_transition with target = location_index'; updates = (No_potential_update, [])} in 
        add_transition location_index new_transition;
        continue_exploring state_index' location_index'
    ) relevant_uncontrollable_successors;


    let winning_moves = get_winning_moves state_index in
    winning_moves#iter (
      fun state_index' winning_moves_per_action ->
        let location_index' = get_location state_index' in   
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
                updates = (No_potential_update, []);
                target = location_index'}
                ; in
              add_transition location_index transition
              ) constituent_constrs
          end);
        continue_exploring state_index' location_index'
    )
  in
  explore initial_state_index (get_location initial_state_index);

  let invariants_array = new array @@ List.rev !invariants in
  let accepting_array = new array @@ List.rev !accepting in
  let urgent_array = new array @@ List.rev !urgent in 
  let location_names_array = new array @@ List.rev !location_names in 

  let location_names = (fun _ location_id -> location_names_array#get location_id ^ "__#" ^ string_of_int location_id) in
  let actions_per_location = (fun _ -> fun location_index -> (actions_per_location_tbl#find location_index)#all_elements) in
  let transitions = (fun _ location_index action_index -> (transitions_tbl#find location_index)#find action_index) in
  let transitions_description = (fun transition_index -> Hashtbl.find transitions_description_tbl transition_index) in

  let model = generate_abstract_controller_model system_model 
    ~nb_locations:!location_counter 
    ~nb_transitions:!transition_counter
    ~invariants:(fun _ -> invariants_array#get)
    ~is_accepting:(fun _ -> accepting_array#get)
    ~is_urgent:(fun _ -> urgent_array#get)
    ~location_names
    ~transitions
    ~actions_per_location
    ~transitions_description
  in


  (* Write controller to file *)
  let imi_file_name = options#files_prefix ^ "-controller.imi" in
  let controller_imi_file = open_out imi_file_name in
  output_string controller_imi_file @@ ModelPrinter.string_of_model model; 
  close_out controller_imi_file;
  print_message Verbose_standard ("Controller file `" ^ imi_file_name ^ "` succesfully created.");

  (* Generate graphical representation of controller *)
  print_message Verbose_medium ("Translating generated controller model to a graphics…");
  let translated_model = PTA2JPG.string_of_model model in
  if verbose_mode_greater Verbose_high then(
    print_message Verbose_high ("\n" ^ translated_model ^ "\n");
  );

  let dot_created_file_option = Graphics.dot "pdf" (options#files_prefix ^ "-controller") translated_model in
  begin
  match dot_created_file_option with
  | None -> print_error "Oops…! Something went wrong with dot."
  | Some created_file -> print_message Verbose_standard ("File `" ^ created_file ^ "` successfully created.");
  end;