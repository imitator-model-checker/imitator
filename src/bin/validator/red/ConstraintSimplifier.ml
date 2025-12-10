open Lib
open ParsingStructure

let true_constraint = [Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant (ParsedValue.Bool_value true))))]

let update_info ~printer ~nb_simplified ~nb_kept ~nb_total =  
  Printer.info printer "%d/%d/%d [simplified/kept/total]\t(%d %%)"
  nb_simplified nb_kept nb_total ((100 * (nb_kept + nb_simplified)) / (nb_total))
module Invariants = struct 
  let non_trivial_invariant_indices automata =
    automata
    |> List.mapi (fun ai (_, _, locs) ->
        List.filter_map
          (fun (li, loc) ->
              if loc.invariant <> true_constraint then Some (ai, li) else None)
          (List.mapi (fun li loc -> (li, loc)) locs))
    |> List.flatten

  let set_invariant_true (model : parsed_model) (ai, li) =
    let automata' =
      List.mapi (fun i (name, actions, locations) ->
        if i = ai then
          let locations' =
            List.mapi (fun j location ->
              if j = li then { location with invariant = true_constraint } else location
            ) locations
          in
          (name, actions, locations')
        else
          (name, actions, locations)
      ) model.automata
    in
    { model with automata = automata' }

  let simplify ~predicate ~printer (model : parsed_model) =
    Printer.with_section printer "Invariants" @@ fun () -> 
    Printer.with_live printer @@ fun () ->
    let invariant_indices = non_trivial_invariant_indices model.automata in
    let nb_simplified = ref 0 in
    let nb_kept = ref 0 in 
    let nb_total = List.length invariant_indices in 
    invariant_indices
    |> List.fold_left (fun acc (ai, li) ->
        let candidate = set_invariant_true acc (ai, li) in
        let result = 
          if predicate candidate then (
            incr nb_simplified;
            candidate 
          )
          else ( 
            incr nb_kept;
            acc
          ) in
        update_info ~printer ~nb_simplified:!nb_simplified ~nb_kept:!nb_kept ~nb_total;
        result
      ) model
end

module Guards = struct 
let non_trivial_guard_indices automata =
  automata
  |> List.mapi (fun ai (_, _, locs) ->
       locs
       |> List.mapi (fun li loc ->
            loc.transitions
            |> List.mapi (fun ti (guards, _, _, _) ->
                 if guards <> true_constraint then Some (ai, li, ti) else None)
            |> List.filter_map (fun x -> x))
       |> List.flatten)
  |> List.flatten

  let set_guard_true (model : parsed_model) (ai, li, ti) =
    let automata' =
      List.mapi (fun i (name, actions, locations) ->
        if i = ai then
          let locations' =
            List.mapi (fun j loc ->
              if j = li then
                let transitions' =
                  List.mapi (fun k (guards, code, sync, target) ->
                    if k = ti then
                      (true_constraint, code, sync, target)
                    else
                      (guards, code, sync, target)
                  ) loc.transitions
                in
                { loc with transitions = transitions' }
              else loc
            ) locations
          in
          (name, actions, locations')
        else
          (name, actions, locations)
      ) model.automata
    in
    { model with automata = automata' }
  let simplify ~predicate ~printer (model : parsed_model) =
    Printer.with_section printer "Guards" @@ fun () -> 
    Printer.with_live printer @@ fun () ->
    let guard_indices = non_trivial_guard_indices model.automata in
    let nb_simplified = ref 0 in
    let nb_kept = ref 0 in 
    let nb_total = List.length guard_indices in 
    guard_indices
    |> List.fold_left (fun acc idx ->
        let candidate = set_guard_true acc idx in
        let result = 
          if predicate candidate then (
            incr nb_simplified;
            candidate 
          )
          else ( 
            incr nb_kept;
            acc
          ) in
        update_info ~printer ~nb_simplified:!nb_simplified ~nb_kept:!nb_kept ~nb_total;
        result
      ) model
end

let simplify ~predicate model ~printer = 
  Printer.with_section printer "Simplifying" @@ fun () ->
  model 
  |> Invariants.simplify ~predicate ~printer
  |> Guards.simplify ~predicate ~printer 