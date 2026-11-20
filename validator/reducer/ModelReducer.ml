exception Predicate_not_satisfied

(* Unlike the other steps, Reachability.remove_islands does not check the predicate *)
let remove_islands ~predicate ~printer parsed_model =
  let candidate = Reachability.remove_islands ~printer parsed_model in
  if predicate candidate then candidate else parsed_model

let reduce parsed_model ~predicate ~printer = 
  Printer.with_section printer "Reducer" @@ fun () ->

  if not (predicate parsed_model) then raise Predicate_not_satisfied;
  
  parsed_model
  |> TransitionMinimizer.minimize ~predicate ~printer
  |> remove_islands ~predicate ~printer
  |> ConstraintSimplifier.simplify ~predicate ~printer
  |> LocationCoalescer.coalesce ~predicate ~printer
