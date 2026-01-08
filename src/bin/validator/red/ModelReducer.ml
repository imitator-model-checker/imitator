let reduce parsed_model ~printer ~options_and_properties = 
  Printer.with_section printer "Reducer" @@ fun () ->
  let predicate =
    Checker.counter_example_predicate
      ~options_and_properties
  in

  if not (predicate parsed_model) then begin
    Printer.error printer "Initial model is not a valid counterexample.@, Aborting@,";
    exit 1
  end;
  
  parsed_model
  |> TransitionMinimizer.minimize ~predicate ~printer
  |> Reachability.remove_islands ~printer 
  |> ConstraintSimplifier.simplify ~predicate ~printer
  |> LocationCoalescer.coalesce ~predicate ~printer
