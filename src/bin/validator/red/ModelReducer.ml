let reduce parsed_model ~options_a ~options_b ~parsed_property_option_a ~parsed_property_option_b ~original_nb_transitions =
  let predicate =
    Checker.counter_example_predicate
      ~options_a ~options_b
      ~parsed_property_option_a ~parsed_property_option_b
  in
  parsed_model
  |> TransitionMinimizer.minimize ~original_nb_transitions ~predicate
  |> Reachability.remove_islands
  |> ConstraintSimplifier.simplify ~predicate
  |> LocationCoalescer.coalesce ~predicate
