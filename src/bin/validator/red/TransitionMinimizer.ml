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

let minimize (model : parsed_model) ~original_nb_transitions ~predicate ~printer =
  Printer.with_section printer "Transition Minimizer" @@ fun () ->
  let action_counter = ActionCounter.create model in 
  let rec loop current idx n =
    if idx = n then current
    else
      let candidate = remove_transition_from_parsed_model current ~transition_to_remove:idx ~action_counter in
      if predicate candidate then
        (
        Printer.info printer "Removed transition";
        flush_all ();
        ActionCounter.commit action_counter;
        loop candidate idx (n - 1))
      else
        (ActionCounter.revert action_counter;
        loop current (idx + 1) n)
  in
  loop model 0 original_nb_transitions
