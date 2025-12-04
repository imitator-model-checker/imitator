open Lib
open ParsingStructure

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
                | Sync label -> ActionCounter.remove_label action_counter ~automaton_id:aid label);
              keep
            ) location.transitions
          in
          { location with transitions = transitions' }
        ) locations
      in
      let actions' = ActionCounter.filter_local_actions action_counter ~automaton_id:aid actions in 
      (name, actions', locations')
    ) parsed_model.automata
  in
  
  let controllable_actions' = ActionCounter.filter_controllable_actions action_counter parsed_model.controllable_actions in 

  { parsed_model with automata = automata'; controllable_actions = controllable_actions'}

let minimize (model : parsed_model) ~predicate ~printer =
  let nb_transitions = 
    model.automata
    |> List.fold_left (fun acc (_, _, locations) ->
      acc + List.fold_left (fun acc_loc location ->
        acc_loc + List.length location.transitions
      ) 0 locations
    ) 0
  in
  let transitions_removed = ref 0 in 
  let transitions_kept = ref 0 in 
  let update_info = fun () -> Printer.info printer "Removed %d and skipped %d out of %d transitions" (!transitions_removed) (!transitions_kept) nb_transitions in 

  Printer.start_section printer "Transition Minimizer";

  let action_counter = ActionCounter.create model in 
  let rec loop current idx n =
    if idx = n then current
    else
      let candidate = remove_transition_from_parsed_model current ~transition_to_remove:idx ~action_counter in
      if predicate candidate then (
        incr transitions_removed;
        update_info ();
        ActionCounter.commit action_counter;
        loop candidate idx (n - 1))
      else (
        incr transitions_kept;
        update_info ();
        ActionCounter.revert action_counter;
        loop current (idx + 1) n)
  in
  Printer.start_live printer;
  Fun.protect ~finally:(fun _ -> Printer.end_live printer; Printer.end_section printer) 
  (fun _ -> loop model 0 nb_transitions)
