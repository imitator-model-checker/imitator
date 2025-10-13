open Lib
open Crowbar

exception ComparisonError of string

let check_eq_p_nnconvex_constraint (model : AbstractModel.abstract_model) = 
  check_eq 
  ~eq:LinearConstraint.p_nnconvex_constraint_is_equal 
  ~pp:(fun f k -> Format.pp_print_string f (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names k))

let check_eq_result model (a : Result.imitator_result) (b : Result.imitator_result) = 
  match a, b with 
    | Single_synthesis_result ra, Single_synthesis_result rb -> 
      (match rb.termination with Time_limit _ -> () | _ -> 
      (match ra.result,rb.result with 
      | Good_constraint (constr_a, _), Good_constraint (constr_b, _)
      | Bad_constraint (constr_a, _), Bad_constraint (constr_b, _) ->
        check_eq_p_nnconvex_constraint model constr_a constr_b
      | Good_bad_constraint {good=(good_a, _) ;bad=(bad_a, _)}, 
        Good_bad_constraint {good=(good_b, _) ;bad=(bad_b, _)} -> 
        check_eq_p_nnconvex_constraint model good_a good_b; 
        check_eq_p_nnconvex_constraint model bad_a bad_b
      | _ -> fail "Results not equal"))
    | _ -> raise @@ ComparisonError "Validator can only compare Single Synthesis Results"
