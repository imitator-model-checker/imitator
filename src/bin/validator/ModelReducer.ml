open Lib


let remove_transition_from_parsed_model (parsed_model : ParsingStructure.parsed_model) ~transition_id_to_remove = 
  let open ParsingStructure in 
  let curr_trans = ref 0 in   
  let automata' = 
  List.map (fun (name, actions, locations) -> 
    let locations'  = 
    List.map (fun location -> 
      let transitons' = List.filter (fun _ -> 
        let keep = !curr_trans <> transition_id_to_remove in 
        incr curr_trans;
        keep
      ) location.transitions in 
      {location with transitions = transitons'}
    ) locations in
    (name, actions, locations')
  ) parsed_model.automata in
  {parsed_model with automata = automata'}

let minimize_transitions (parsed_model : ParsingStructure.parsed_model) 
  ~options_a
  ~options_b
  ~parsed_property_option_a
  ~parsed_property_option_b
  ~original_nb_transitions  = 

  let minimized_model = ref parsed_model in 
  for target_trans = 0 to original_nb_transitions do
    let changed_model = remove_transition_from_parsed_model !minimized_model ~transition_id_to_remove:target_trans in 
    Input.set_options options_a;
    let model, property_a = ModelConverter.abstract_structures_of_parsing_structures options_a changed_model parsed_property_option_a in 
    let result_a = ImitatorRunner.run options_a model property_a in 

    Input.set_options options_b;
    let model, property_b = ModelConverter.abstract_structures_of_parsing_structures options_b changed_model parsed_property_option_b in 
    let result_b = ImitatorRunner.run options_b model property_b in 
    if not @@ Comparison.eq_result model result_a result_b then minimized_model := changed_model

  done;
  !minimized_model


let reduce parsed_model ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b ~original_nb_transitions = 
  minimize_transitions parsed_model ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b ~original_nb_transitions