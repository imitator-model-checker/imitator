open Lib
open ParsingStructure

module StringSet = Set.Make(String)


type candidate = {pair : string * string; automaton_id: int}
module CandidateSet = Set.Make(struct type t = candidate let compare = compare end)

module CoalescingWorklist : sig
  type t
  type result = MERGE_LEFT | MERGE_RIGHT | MERGE_FAILED
  val create : parsed_model -> t
  val step : t -> (candidate -> parsed_model * result) -> parsed_model option
  
end = struct 
  type result = MERGE_LEFT | MERGE_RIGHT | MERGE_FAILED
  type direction = LEFT | RIGHT
  type t = {mutable candidates: CandidateSet.t; mutable deferred: CandidateSet.t}
  let create (parsed_model : parsed_model) = 
    let candidate_of_transition automaton_id src (_, _, _, dst) =
      if src = dst then 
        CandidateSet.empty
      else ( 
        let pair = if src < dst then (src, dst) else (dst, src) in
        CandidateSet.singleton {pair; automaton_id}
      )
    in
    let candidates_of_location automaton_id location =
      location.transitions
      |> List.map (candidate_of_transition automaton_id location.name)
      |> List.fold_left CandidateSet.union CandidateSet.empty
    in
    let candidates_of_automaton automaton_id (_, _, locations) = 
      locations 
      |> List.map (candidates_of_location automaton_id)
      |> List.fold_left CandidateSet.union CandidateSet.empty
    in 
    let candidates = 
      parsed_model.automata
      |> List.mapi candidates_of_automaton
      |> List.fold_left CandidateSet.union CandidateSet.empty
    in 
    {
      candidates;
      deferred = CandidateSet.empty;
    }

  let apply_merge t candidate direction =
    let {pair = (a,b); automaton_id} = candidate in 

    let redirect_name name = 
      match direction with 
      | LEFT -> if name = b then a else name
      | RIGHT -> if name = a then b else name
    in

    let redirect_candidate candidate =
      let {pair = (name1, name2); automaton_id = automaton_id'} = candidate in  
      if automaton_id <> automaton_id' then candidate else
      let name1', name2' = redirect_name name1, redirect_name name2 in 
      let pair = if name1' < name2' then (name1', name2') else (name2', name1') in  
      {pair; automaton_id}
    in

    (* Candidates that failed under the current naming but must be retried 
       after a successful merge, since redirection may change feasibility. *)
    t.candidates <-
      t.candidates
      |> CandidateSet.union t.deferred 
      |> CandidateSet.remove candidate
      |> CandidateSet.map redirect_candidate; (* Note: map may collapse candidates after redirection *)
    
    t.deferred <- CandidateSet.empty

  let apply t candidate result = 
    match result with  
      | MERGE_LEFT -> 
        apply_merge t candidate LEFT
      | MERGE_RIGHT ->
        apply_merge t candidate RIGHT
      | MERGE_FAILED -> 
        (t.candidates <- CandidateSet.remove candidate t.candidates;
        t.deferred <- CandidateSet.add candidate t.deferred)
  
  let step t f = 
    (* Picks an arbitrary remaining candidate; no ordering guarantees *)
    match CandidateSet.choose_opt t.candidates with
    | Some candidate -> 
      let model', result = f candidate in 
      apply t candidate result; 
      Some model'
    | None -> None

end

let coalesce_two_locations ~action_counter (model : parsed_model) ~source ~target ~automaton_id = 

  let coalesce_in_automaton automaton source target =
    let (name, actions, locations) = automaton in 
    
    let rewire ((g, code, sync, name) as transition) = 
      if name = source then (g, code, sync, target) else transition in 

    (* Save the outgoing transitions of the location we are about to remove *)
    let source_to_target_transitions, source_transitions_except_target = 
      locations 
      |> List.find (fun loc -> loc.name = source)
      |> fun loc -> loc.transitions
      |> List.partition (fun (_, _, _, dst_name) -> dst_name = target)
    in

    let locations = 
      locations
      |> List.filter (fun loc -> loc.name <> source)
      |> List.map (fun loc -> 
        let transitions = 
          if loc.name = target then 
            let target_to_source_transitions, target_transitions_except_source = 
              loc.transitions
              |> List.partition (fun (_, _, _, dst_name) -> dst_name = source)
            in

            (* remove labels *)
            (target_to_source_transitions @ source_to_target_transitions)
            |> List.map (fun (_, _, sync, _) -> sync)
            |> List.filter_map (function Sync label -> Some label | NoSync -> None)
            |> List.iter (fun label -> ActionCounter.remove_label action_counter ~automaton_id label);

            (* transplant source transitions on target while disallowing any transitions between them *)
            target_transitions_except_source @ source_transitions_except_target
          else 
            loc.transitions
        in
        {loc with transitions = List.map rewire transitions }
      ) 
    in

    let actions = ActionCounter.filter_local_actions action_counter ~automaton_id actions in
    (name, actions, locations)
  in
  let automata = 
    model.automata
    |> List.mapi (fun i automaton -> 
      if i = automaton_id then 
        coalesce_in_automaton automaton source target 
      else 
        automaton)
  in
  let init_definition = 
    let aid_of_name = 
      let map = List.mapi(fun i (a_name, _, _) -> a_name, i) model.automata in
      fun name -> List.assoc name map
    in 
    model.init_definition
    |> List.map (function 
      | Parsed_loc_assignment (a_name, loc_name) when loc_name = source && aid_of_name a_name = automaton_id -> 
        Parsed_loc_assignment (a_name, target)
      | p -> p) 
  in 

  let controllable_actions = ActionCounter.filter_controllable_actions action_counter model.controllable_actions in 

  {model with automata; init_definition; controllable_actions}


let try_merge ~action_counter ~printer ~predicate ~source ~target ~automaton_id model =
  Printer.info printer "Attempting to coalesce `%s` into `%s` in automaton %d" source target automaton_id;

  let model' = coalesce_two_locations ~action_counter model ~source ~target ~automaton_id in

  if predicate model' then (
    ActionCounter.commit action_counter;
    Printer.info printer "Coalesced `%s` into `%s` in automaton %d" source target automaton_id;
    Printer.end_live printer;
    Printer.start_live printer;
    Some model'
  ) else (
    Printer.info printer "";
    ActionCounter.revert action_counter;
    None
  )

type direction = Left | Right

let endpoints = function
  | Right -> (fun a b -> a, b)
  | Left  -> (fun a b -> b, a)


let coalesce ~predicate model ~printer =
  Printer.with_section printer "Coalescing locations" @@ fun () ->
  Printer.with_live printer @@ fun () ->
  let action_counter = ActionCounter.create model in 
  let wl = CoalescingWorklist.create model in 

  let rec loop model =
    match CoalescingWorklist.step wl (fun {pair = (a,b); automaton_id} -> 
      let try_direction dir =
        let source, target = endpoints dir a b in
        try_merge ~action_counter ~printer ~predicate ~source ~target ~automaton_id model
      in
      match try_direction Right with
      | Some model' -> model', MERGE_RIGHT
      | None ->
        match try_direction Left with
        | Some model' -> model', MERGE_LEFT
        | None -> model, MERGE_FAILED 
      
    ) with 
    | Some model' -> loop model'
    | None -> model
  in
  
  loop model

  