open Lib
open ParsingStructure

module StringSet = Set.Make(String)

let true_constraint = [Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant (ParsedValue.Bool_value true))))]
let is_true_formula f = (f = true_constraint)

module Structural = struct
  let warning () = print_endline "Warning: Structural coalescing broke the counterexample. This suggests the bug might depend on structure or state order."

  type controllability = Controllable | Uncontrollable
  type action_type = Internal | Synchronized
  type action_classification = {controllability:controllability;action_type:action_type}

  module Action = struct 
    let classifier (model : parsed_model) = 
      let controllability_of_name, tau_controllability = 
        match model.controllable_actions with 
        | Parsed_controllable_actions xs ->
          let set = List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty xs in 
          (fun name -> if StringSet.mem name set then Controllable else Uncontrollable), Uncontrollable
        | Parsed_uncontrollable_actions xs ->
          let set = List.fold_left (fun acc x -> StringSet.add x acc) StringSet.empty xs in 
          (fun name -> if StringSet.mem name set then Uncontrollable else Controllable), Controllable
        | Parsed_no_controllable_actions -> (fun _ -> Uncontrollable), Uncontrollable
      in

      let appearances = Hashtbl.create 32 in 

      let incr_appeareance action = 
        match Hashtbl.find_opt appearances action with 
          | Some count -> Hashtbl.replace appearances action (count + 1)
          | None -> Hashtbl.add appearances action 1
      in
      let classifier = Hashtbl.create 32 in 

      List.iter (fun (_, actions, _) ->
        List.iter incr_appeareance actions
      ) model.automata;

      List.iter (fun (_, actions, _) -> 
        List.iter (fun action -> 
          let controllability = controllability_of_name action in 
          let action_type = if Hashtbl.find appearances action = 1 then Internal else Synchronized in 
          Hashtbl.add classifier action {action_type;controllability}
          ) actions
      ) model.automata;
      

      function Sync name -> Hashtbl.find classifier name 
            | NoSync -> {action_type = Internal; controllability = tau_controllability}
  end

  module Graph = struct 
    let build_succ_pred (model : parsed_model) =
      (* One successor table and one predecessor table per automaton *)
      let nb_aut = List.length model.automata in
      let succ =
        Array.init nb_aut (fun _ -> Hashtbl.create 32)
      in
      let pred =
        Array.init nb_aut (fun _ -> Hashtbl.create 32)
      in

      let add_pred tbl key v =
        match Hashtbl.find_opt tbl key with
        | Some xs -> Hashtbl.replace tbl key (v :: xs)
        | None    -> Hashtbl.add tbl key [v]
      in

      List.iteri
        (fun aid (_names, _actions, locations) ->
          List.iter
            (fun loc ->
              (* For successors: map src.name -> its outgoing transitions (as-is) *)
              Hashtbl.replace succ.(aid) loc.name loc.transitions;

              (* For predecessors: for each edge src->dst, store (guard,code,sync,src) under key=dst *)
              List.iter
                (fun ((g, code, sync, dst) as _tr) ->
                  add_pred pred.(aid) dst (g, code, sync, loc.name))
                loc.transitions)
            locations)
        model.automata;

      succ, pred
  end 

  module Coalesce = struct 
    let location_is_simple loc =
      loc.urgency = Parsed_location_nonurgent &&
      loc.stopped = [] &&
      loc.flow = [] &&
      loc.cost = None &&
      is_true_formula loc.invariant

    let can_coalesce_pair
      ~source:src_loc
      ~target:tgt_loc
      ~edge:(g,code,sync ,_)
      ~target_successors
      ~action_classifier = 

      let action_classification = action_classifier sync in 
      
      (* location checks *)
      src_loc.name <> tgt_loc.name &&
      location_is_simple src_loc &&
      location_is_simple tgt_loc &&
      (src_loc.acceptance = tgt_loc.acceptance) &&

      (* transition checks *)
      is_true_formula g &&
      code = [] &&

      (* action checks *)
      action_classification.action_type = Internal &&
      List.for_all (fun (_, _, sync, _) -> 
        action_classification.controllability = (action_classifier sync).controllability) 
        target_successors


    let compute_redirections 
      (succ : (string, transition list) Hashtbl.t)
      (pred : (string, transition list) Hashtbl.t) 
      (action_classifier : sync -> action_classification)
      (locations : parsed_location list) = 

      
      let redir = Hashtbl.create 32 in 
      List.iter (fun src -> 
        match Hashtbl.find_opt succ src.name with 
        | Some [ (g,code,sync,dst) ] when List.length (Hashtbl.find pred dst) = 1 ->
          let dst_succ = Option.default [] (Hashtbl.find_opt succ dst) in
          (match List.find_opt (fun l -> l.name = dst) locations with
          | Some tgt when can_coalesce_pair 
                          ~source:src 
                          ~target:tgt 
                          ~edge:(g,code,sync,dst)
                          ~action_classifier 
                          ~target_successors:dst_succ ->
            Hashtbl.add redir src.name dst
          | _ -> ())
        | _ -> ()
      ) locations;
      redir

    let apply ~redirs_per_automaton ~predicate (model : parsed_model) = 
      let resolve aid name =
        let rec aux visited n =
          match Hashtbl.find_opt redirs_per_automaton.(aid) n with
          | None -> n
          | Some s ->
              if StringSet.mem s visited then n
              else (aux (StringSet.add s visited) s)
        in
        aux (StringSet.singleton name) name 
      in

      let automata' = 
        List.mapi (fun aid (name, actions, locations) -> 
          let drop = Hashtbl.fold (fun k _ acc -> StringSet.add k acc) redirs_per_automaton.(aid) StringSet.empty in 
          let locations' = 
            locations 
            |> List.filter (fun loc -> not (StringSet.mem loc.name drop))
            |> List.map (fun loc ->
                let trans' =
                  List.map (fun (g,code,sync,dst) ->
                    let dst' = resolve aid dst in
                    (g,code,sync,dst')
                  ) loc.transitions
                in
                { loc with transitions = trans' }
              )
          in 
          (name, actions, locations')
        ) (model.automata) 
      in

      let aid_of_name = 
        let map = List.mapi(fun i (a_name, _, _) -> a_name, i) model.automata in
        fun name -> List.assoc name map
      in 

      let init_definition' = 
        model.init_definition
        |> List.map (function 
          | Parsed_loc_assignment (a_name, loc_name) -> 
            Parsed_loc_assignment (a_name, resolve (aid_of_name a_name) loc_name)
          | p -> p) in 

      let candidate = {model with automata = automata'; init_definition = init_definition'} in

      if predicate candidate then candidate else (warning (); model) 
  end

  let coalesce ~predicate (model : parsed_model) = 
    let succ, pred = Graph.build_succ_pred model in
    let classify = Action.classifier model in
    let redirs =
      model.automata
      |> List.mapi (fun aid (_, _, locs) ->
          Coalesce.compute_redirections succ.(aid) pred.(aid) classify locs)
      |> Array.of_list
    in
    Coalesce.apply ~redirs_per_automaton:redirs ~predicate model
end

module Predicate = struct
  let merge_pair (model : parsed_model) ~aid ~src ~dst = 
    let succesful = ref false in
    let rewire ((g, code, sync, name) as transition) = 
      if name = src then (succesful := true; (g, code, sync, dst)) else transition in 
    let automata' = 
        List.mapi (fun id (name, actions, locations) -> 
          let locations' = 
            if aid = id then 
              locations 
              |> List.filter (fun loc -> loc.name <> src)
              |> List.map (fun loc -> { loc with transitions = List.map rewire loc.transitions })
            else
              locations 
          in 
          (name, actions, locations')
        ) (model.automata) 
    in
    let aid_of_name = 
      let map = List.mapi(fun i (a_name, _, _) -> a_name, i) model.automata in
      fun name -> List.assoc name map
    in 

    let init_definition' = 
      model.init_definition
      |> List.map (function 
        | Parsed_loc_assignment (a_name, loc_name) when loc_name = src && aid_of_name a_name = aid -> 
          Parsed_loc_assignment (a_name, dst)
        | p -> p) in 

    {model with automata = automata'; init_definition = init_definition'}, !succesful

  let one_coalsecing_pass (model : parsed_model) ~predicate = 
    let redirs = Array.init (List.length model.automata) (fun _ -> Hashtbl.create 32) in 
    let resolve aid name =
      let rec aux visited n =
        match Hashtbl.find_opt redirs.(aid) n with
        | None -> n
        | Some s ->
            if StringSet.mem s visited then n
            else (aux (StringSet.add s visited) s)
      in
      aux (StringSet.singleton name) name 
    in    
    let candidate, _, changed =
    model.automata
    |> List.fold_left (fun (candidate, aid, changed) (_, _, locations) ->
       locations
       |> List.fold_left (fun (candidate, aid, changed) location ->
          location.transitions
          |> List.filter (fun (_, _, _, dst) -> dst <> location.name)
          |> List.fold_left (fun (candidate, aid, changed) (_, _, _, dst) -> 
             let src = resolve aid location.name in 
             let dst = resolve aid dst in 
             if src = dst then (candidate, aid, changed) else 
             let candidate', sucessful = merge_pair candidate ~aid ~src ~dst in
             if sucessful && predicate candidate' then 
              begin 
                Hashtbl.add redirs.(aid) location.name dst;
                (candidate', aid, true)
              end
             else (candidate, aid, changed)
             ) (candidate, aid, changed)
          ) (candidate, aid + 1, changed) 
       ) (model, -1, false) in 
    candidate, changed

  let coalesce ~predicate (model : parsed_model) =
    let rec loop model =
      let model', changed = one_coalsecing_pass model ~predicate
      in
      if changed then loop model' else model'
    in
    loop model
end

let coalesce ~predicate model =
  model
  |> Structural.coalesce ~predicate
  |> Predicate.coalesce ~predicate