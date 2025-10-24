open Lib

let remove_transition_from_parsed_model (parsed_model : ParsingStructure.parsed_model) ~transition_to_remove =
  let open ParsingStructure in
  let curr_trans = ref 0 in
  let automata' =
    List.map (fun (name, actions, locations) ->
      let locations' =
        List.map (fun location ->
          let transitions' =
            List.filter (fun _ ->
              let keep = !curr_trans <> transition_to_remove in
              incr curr_trans;
              keep
            ) location.transitions
          in
          { location with transitions = transitions' }
        ) locations
      in
      (name, actions, locations')
    ) parsed_model.automata
  in
  { parsed_model with automata = automata' }

let minimize model ~original_nb_transitions ~predicate =
  let rec loop current idx n =
    if idx = n then current
    else
      let candidate = remove_transition_from_parsed_model current ~transition_to_remove:idx in
      if predicate candidate then
        loop candidate idx (n - 1)
      else
        loop current (idx + 1) n
  in
  loop model 0 original_nb_transitions
