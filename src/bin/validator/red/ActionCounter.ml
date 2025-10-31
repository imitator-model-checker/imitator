open Lib
open ParsingStructure


type t = {
  global: (string, int) Hashtbl.t; 
  local: (string, int) Hashtbl.t array; 
  mutable for_removal: (int * string) list
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

  {global; local; for_removal = []}

let remove_label t ~automaton_id label = t.for_removal <- (automaton_id ,label) :: t.for_removal

let remove {global;local;_} aid label = 
  let global_count = Hashtbl.find global label in 
  let local_count = Hashtbl.find local.(aid) label in 
  Hashtbl.replace global label (global_count - 1);
  Hashtbl.replace local.(aid) label (local_count - 1)

let commit t = List.iter (fun (aid, label) -> remove t aid label) t.for_removal; t.for_removal <- []
let revert t = t.for_removal <- []

 

let exists_locally {local; for_removal; _} ~automaton_id label =
  let uncommitted_diff = 
    for_removal 
    |> List.filter ((=) (automaton_id, label))
    |> List.length 
  in
  match Hashtbl.find_opt local.(automaton_id) label with 
  | None -> false
  | Some count -> 
    if count - uncommitted_diff >= 1 then true else false
    
let exists_globally {global; for_removal; _} label = 
  let uncommitted_diff = 
    for_removal 
    |> List.map snd
    |> List.filter ((=) label)
    |> List.length 
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
  