let reduce parsed_model ~printer ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b = 
  Printer.with_section printer "Reducer" @@ fun () ->
  let predicate =
    Checker.counter_example_predicate
      ~options_a ~options_b
      ~parsed_property_option_a ~parsed_property_option_b
  in

  if not (predicate parsed_model) then begin
    Printer.error printer "Initial model is not a valid counterexample.@, Aborting@,";
    exit 1
  end;
  
  parsed_model
  |> TransitionMinimizer.minimize ~predicate ~printer
  |> Reachability.remove_islands
  |> ConstraintSimplifier.simplify ~predicate
  |> LocationCoalescer.coalesce ~predicate
