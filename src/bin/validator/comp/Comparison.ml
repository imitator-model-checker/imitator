open Lib


type result =
  | Equal
  | Not_Equal
  | Time_out
  | Not_supported
  | Incomparable
  | Error

let result_of_bool = function 
  | true -> Equal
  | false -> Not_Equal 

let eq_result (a : Result.imitator_result) (b : Result.imitator_result) : result =
  match a, b with 
  | Single_synthesis_result ra, Single_synthesis_result rb -> 
    (match ra.termination, rb.termination with Time_limit _, _ | _, Time_limit _ -> Time_out | _ -> 
    (match ra.result,rb.result with 
    | Good_constraint (constr_a, _), Good_constraint (constr_b, _)
    | Bad_constraint (constr_a, _), Bad_constraint (constr_b, _) ->
      result_of_bool @@ LinearConstraint.p_nnconvex_constraint_is_equal constr_a constr_b
    | Good_bad_constraint {good=(good_a, _) ;bad=(bad_a, _)}, 
      Good_bad_constraint {good=(good_b, _) ;bad=(bad_b, _)} -> 
      result_of_bool
      (LinearConstraint.p_nnconvex_constraint_is_equal good_a good_b &&
      LinearConstraint.p_nnconvex_constraint_is_equal bad_a bad_b)
    | _ -> Incomparable))
  | Error_result _, _ | _, Error_result _ -> 
    Error
  | _ -> Not_supported