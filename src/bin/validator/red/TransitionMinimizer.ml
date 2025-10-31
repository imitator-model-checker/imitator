open Lib
open ParsingStructure

module ActionCounter = struct 

  type t = {global: (string, int) Hashtbl.t; local: (string, int) Hashtbl.t array; mutable label_for_removal: (int * string) option}

  let create (model : parsed_model) = 
    let nb_automata = List.length model.automata in 
    let global = Hashtbl.create 32 in 
    let local = Array.init nb_automata (fun _ -> Hashtbl.create 32) in 

    List.iteri (fun aid (_ , _, locations) -> 
      List.iter (fun location ->
        List.iter (fun (_, _, sync, _) ->
          match sync with 
          | Sync label -> 
            (match Hashtbl.find_opt global label with 
            | Some count -> Hashtbl.replace global label (count + 1)
            | None -> Hashtbl.add global label 1);
            (match Hashtbl.find_opt local.(aid) label with 
            | Some count -> Hashtbl.replace local.(aid) label (count + 1)
            | None -> Hashtbl.add local.(aid) label 1)
          | NoSync -> ()
        ) location.transitions
      ) locations 
    ) model.automata; 

    {global; local; label_for_removal = None}

  let remove_label t label = t.label_for_removal <- Some label

  let commit {global;local;label_for_removal} = 
    match label_for_removal with 
    | None -> ()
    | Some (aid, label) -> 
      let global_count = Hashtbl.find global label in 
      let local_count = Hashtbl.find local.(aid) label in 
      Hashtbl.replace global label (global_count - 1);
      Hashtbl.replace local.(aid) label (local_count - 1)
  let revert t = t.label_for_removal <- None
  
  let exists_locally {local; label_for_removal; _} ~automaton_id label =
    let uncommitted_diff = match label_for_removal with 
    | Some (aid, label') -> if aid = automaton_id && label = label' then 1 else 0 
    | None -> 0 
    in
    match Hashtbl.find_opt local.(automaton_id) label with 
    | None -> false
    | Some count -> 
      if count - uncommitted_diff >= 1 then true else false
     
  let exists_globally {global; label_for_removal; _} label = 
    let uncommitted_diff = match label_for_removal with 
    | Some (_, label') -> if label = label' then 1 else 0 
    | None -> 0 
    in
    match Hashtbl.find_opt global label with 
    | None -> false
    | Some count -> 
      if count - uncommitted_diff >= 1 then true else false
end

let remove_transition_from_parsed_model (parsed_model : parsed_model) ~transition_to_remove ~action_counter =
  let curr_trans = ref 0 in
  let automata' =
    List.mapi (fun aid (name, actions, locations) ->
      let locations' =
        List.map (fun location ->
          let transitions' =
            List.filter (fun (_, _, sync, _) ->
              let keep = !curr_trans <> transition_to_remove in
              incr curr_trans;
              if not keep then 
                (match sync with 
                | NoSync -> ()
                | Sync label -> ActionCounter.remove_label action_counter (aid, label));
              keep
            ) location.transitions
          in
          { location with transitions = transitions' }
        ) locations
      in
      let actions' = List.filter (ActionCounter.exists_locally action_counter ~automaton_id:aid) actions in 
      (name, actions', locations')
    ) parsed_model.automata
  in
  
  let controllable_actions' = match parsed_model.controllable_actions with 
  | Parsed_no_controllable_actions -> Parsed_no_controllable_actions
  | Parsed_controllable_actions actions -> 
    Parsed_controllable_actions (List.filter (ActionCounter.exists_globally action_counter) actions)
  | Parsed_uncontrollable_actions actions -> 
    Parsed_uncontrollable_actions (List.filter (ActionCounter.exists_globally action_counter) actions)
  in

  { parsed_model with automata = automata'; controllable_actions = controllable_actions'}

let minimize (model : parsed_model) ~original_nb_transitions ~predicate =
  let action_counter = ActionCounter.create model in 
  let rec loop current idx n =
    if idx = n then current
    else
      let candidate = remove_transition_from_parsed_model current ~transition_to_remove:idx ~action_counter in
      if predicate candidate then
        (ActionCounter.commit action_counter;
        loop candidate idx (n - 1))
      else
        (ActionCounter.revert action_counter;
        loop current (idx + 1) n)
  in
  loop model 0 original_nb_transitions
