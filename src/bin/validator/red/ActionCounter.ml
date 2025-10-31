open Lib
open ParsingStructure


type t = {
  model: parsed_model;
  global: (string, int) Hashtbl.t; 
  local: (string, int) Hashtbl.t array; 
  mutable label_for_removal: (int * string) option
}

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

  {model; global; local; label_for_removal = None}

let remove_label t ~automaton_id label = t.label_for_removal <- Some (automaton_id ,label)

let commit {global;local;label_for_removal; _} = 
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

let filter_controllable_actions t controllable_actions = 
  match controllable_actions with 
  | Parsed_no_controllable_actions -> Parsed_no_controllable_actions
  | Parsed_controllable_actions actions -> 
    Parsed_controllable_actions (List.filter (exists_globally t) actions)
  | Parsed_uncontrollable_actions actions -> 
    Parsed_uncontrollable_actions (List.filter (exists_globally t) actions)

let filter_local_actions t ~automaton_id actions = List.filter (exists_locally t ~automaton_id) actions
  