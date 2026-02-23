let reduce parsed_model ~predicate ~printer = 
  Printer.with_section printer "Reducer" @@ fun () ->

  if not (predicate parsed_model) then begin
    Printer.error printer "Initial model does not satisfy predicate.@, Aborting@,";
    exit 1
  end;
  
  parsed_model
  |> TransitionMinimizer.minimize ~predicate ~printer
  |> Reachability.remove_islands ~printer 
  |> ConstraintSimplifier.simplify ~predicate ~printer
  |> LocationCoalescer.coalesce ~predicate ~printer
