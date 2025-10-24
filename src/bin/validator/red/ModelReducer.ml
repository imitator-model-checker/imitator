open Lib
open Runner
open Comp

module IntSet = Set.Make(Int)


let remove_transition_from_parsed_model (parsed_model : ParsingStructure.parsed_model) ~transition_to_remove = 
  let open ParsingStructure in 
  let curr_trans = ref 0 in   
  let automata' = 
  List.map (fun (name, actions, locations) -> 
    let locations'  = 
    List.map (fun location -> 
      let transitons' = List.filter (fun _ -> 
        let keep = !curr_trans <> transition_to_remove in 
        incr curr_trans;
        keep
      ) location.transitions in 
      {location with transitions = transitons'}
    ) locations in
    (name, actions, locations')
  ) parsed_model.automata in
  {parsed_model with automata = automata'}

let model_is_still_counter_example parsed_model ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b = 
  let result_a, _ = ModelRunner.run options_a parsed_model parsed_property_option_a in 
  let result_b, _ = ModelRunner.run options_b parsed_model parsed_property_option_b in 

  not @@ Comparison.eq_result result_a result_b

let minimize_transitions (parsed_model : ParsingStructure.parsed_model) ~original_nb_transitions ~model_is_still_counter_example  = 

  let rec find_local_minimum model transition_to_remove nb_transitions = 
    (* base case: we have tried every to remove every transition *)
    if transition_to_remove = nb_transitions then model else

    (* recursive case: we create a new model with a removed transition and keep it like that if it is still a counter example *)
    let model' = remove_transition_from_parsed_model model ~transition_to_remove in 
    if model_is_still_counter_example model' then 
      find_local_minimum model' transition_to_remove (nb_transitions - 1)
    else 
      find_local_minimum model (transition_to_remove + 1) nb_transitions
  in
  
  find_local_minimum parsed_model 0 original_nb_transitions
  
(* Convert a parsed automaton to a boolean adjacency matrix for conveniently applying reachability checks
   Loses information: for example two transitions from location a to location b is treated as one
*)
let adjacency_matrix_of_parsed_locations parsed_locations = 
  let open ParsingStructure in 
  let i_of_name = 
    let tbl = Hashtbl.create 32 in 
    List.iteri (fun i loc -> Hashtbl.add tbl loc.name i) parsed_locations;
    Hashtbl.find tbl 
  in 
  let nb_locations = List.length parsed_locations in
  let adj = Array.init nb_locations (fun _ ->
    Array.init nb_locations (fun _ ->
      false
    )
  ) in 
  List.iteri (fun i loc ->
      List.iteri (fun _ (_, _, _, dst) -> 
      adj.(i).(i_of_name dst) <- true
      ) loc.transitions
  ) parsed_locations;
  adj

let remove_islands_from_automaton ((names, actions, locations) : ParsingStructure.parsed_automaton) =
  let adj = adjacency_matrix_of_parsed_locations locations in 
  let n = Array.length adj in
  let indeg = Array.make n false in
  let outdeg = Array.make n false in

  Array.iteri (fun i row ->
    Array.iteri (fun j edge ->
      if edge then begin
        outdeg.(i) <- true;
        indeg.(j) <- true
      end
    ) row
  ) adj;

  let locations' = List.filteri (fun i _ -> outdeg.(i) || indeg.(i)) locations in 
  names, actions, locations'

let remove_islands (parsed_model : ParsingStructure.parsed_model) = 
  {parsed_model with automata = List.map remove_islands_from_automaton parsed_model.automata}
      

let reduce parsed_model ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b ~original_nb_transitions = 
  let model_is_still_counter_example = model_is_still_counter_example ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b in 
  parsed_model |>
  minimize_transitions ~original_nb_transitions ~model_is_still_counter_example |>
  remove_islands