open Lib

let negate (predicate : CounterExampleFinder.predicate_result) : CounterExampleFinder.predicate_result = 
  match predicate with 
  | True -> False
  | False -> True
  | Time_out -> Time_out


(* if nothing timed out and all are single synthesis results: return True if the intersection of the good constraint is non-empty of all results*)
let results_intersect (imitator_results_and_polarity : (Result.imitator_result * bool) list) : CounterExampleFinder.predicate_result = 
  let intersection = LinearConstraint.true_p_nnconvex_constraint () in 
  let has_time_out = ref false in
  let error = ref false in
  List.iter (fun (res, negated) ->
    match (res : Result.imitator_result) with 
    | Single_synthesis_result {termination = Time_limit _; _} -> 
      has_time_out := true
    | Single_synthesis_result {result = Good_constraint (c, soundness); _} 
      when soundness = Constraint_exact -> 
        if negated then 
          LinearConstraint.p_nnconvex_difference_assign intersection c
        else 
          LinearConstraint.p_nnconvex_intersection_assign intersection c
    | _ -> error := true
  ) 
  imitator_results_and_polarity;
  if !has_time_out then Time_out
  else if !error then False
  else if not @@ LinearConstraint.p_nnconvex_constraint_is_false intersection then True
  else False


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