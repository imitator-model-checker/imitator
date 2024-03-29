open AbstractModel
open Automaton
open StateSpace
open State
open ImitatorUtilities
open AlgoPTGStrategyGeneratorUtilities

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
  inherit ([state_index, location_index option] hashTable)
  method bot = None
end

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
  val location_per_state_index = new locationPerStateIndex
  method get_location state_index = 
    match location_per_state_index#find state_index with 
    (* Find it if we already created it *)
      Some location_index -> location_index

    (* Otherwise create a new one *)
    | None ->
        begin 
          let fresh_loc = !location_counter in
          location_counter := !location_counter + 1;
          location_per_state_index#replace state_index @@ Some fresh_loc;

          let location_info = get_location_info_from_state state_space model state_index in
          invariants := location_info.invariant::!invariants;
          accepting := location_info.is_accepting::!accepting;
          urgent := location_info.is_urgent::!urgent;
          location_names := location_info.location_name::!location_names;

          fresh_loc
        end;
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



let generate_controller (system_model : AbstractModel.abstract_model) (get_winning_moves : state_index -> winningMovesPerState) (state_space : stateSpace) (options : Options.imitator_options) = 
  let initial_state_index = state_space#get_initial_state_index in
  let explored = new stateIndexSet in 
  let location_manager = new locationManager 0 state_space system_model in 
  let transition_manager = new transitionManager in

  (* Name: Winning-move-guided exploration *)
  let rec explore state_index location_index = 
    explored#add state_index;
    let continue_exploring state_index location_index = 
      if not @@ explored#mem state_index then
        begin
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
        let location_index' = location_manager#get_location state_index' in             
        let old_transition = system_model.transitions_description (List.hd t) in  (* TODO: Generalize to multiple automata *)
        let new_transition = {old_transition with target = location_index'; updates = (No_potential_update, [])} in 
        transition_manager#add_transition location_index new_transition;
        continue_exploring state_index' location_index'
    ) relevant_uncontrollable_successors;


    let winning_moves = get_winning_moves state_index in
    winning_moves#iter (
      fun state_index' winning_moves_per_action ->
        let location_index' = location_manager#get_location state_index' in   
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
              transition_manager#add_transition location_index transition
              ) constituent_constrs
          end);
        continue_exploring state_index' location_index'
    )
  in
  explore initial_state_index (location_manager#get_location initial_state_index);

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