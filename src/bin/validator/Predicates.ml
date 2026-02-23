open Lib
let result_is_empty (imitator_result : Result.imitator_result) : CounterExampleFinder.predicate_result = 
  match imitator_result with 
  | Single_synthesis_result {result = Good_constraint (c, soundness); termination; _} -> 
    (match termination with 
    | Time_limit _ -> Time_out
    | _ ->
    (match soundness with 
    | Constraint_maybe_under | Constraint_exact -> 
      if LinearConstraint.p_nnconvex_constraint_is_false c then True else False
    | _ -> 
      False))
  | _ -> False