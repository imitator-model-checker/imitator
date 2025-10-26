open Lib
open ParsingStructure

let true_constraint = [Parsed_arithmetic_expr (Parsed_term (Parsed_factor (Parsed_constant (ParsedValue.Bool_value true))))]


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

  let simplify ~predicate model =
    non_trivial_invariant_indices model.automata
    |> List.fold_left (fun acc (ai, li) ->
        let candidate = set_invariant_true acc (ai, li) in
        if predicate candidate then candidate else acc
      ) model
end

let simplify ~predicate model = 
  model |> 
  Invariants.simplify ~predicate