open Lib
open Crowbar

exception ComparisonError of string
exception TimeOutResult 

let eq_result (a : Result.imitator_result) (b : Result.imitator_result) : bool =
  match a, b with 
  | Single_synthesis_result ra, Single_synthesis_result rb -> 
    (match ra.termination, rb.termination with Time_limit _, _ | _, Time_limit _ -> true | _ -> 
    (match ra.result,rb.result with 
    | Good_constraint (constr_a, _), Good_constraint (constr_b, _)
    | Bad_constraint (constr_a, _), Bad_constraint (constr_b, _) ->
      LinearConstraint.p_nnconvex_constraint_is_equal constr_a constr_b
    | Good_bad_constraint {good=(good_a, _) ;bad=(bad_a, _)}, 
      Good_bad_constraint {good=(good_b, _) ;bad=(bad_b, _)} -> 
      LinearConstraint.p_nnconvex_constraint_is_equal good_a good_b &&
      LinearConstraint.p_nnconvex_constraint_is_equal bad_a bad_b
    | _ -> false))
  | _ -> false
