open Lib
let result_is_witness (imitator_result : Result.imitator_result) = 
  match imitator_result with 
  | Single_synthesis_result {result = Good_constraint (c, soundness); _} -> 
    (match soundness with 
    | Constraint_maybe_under | Constraint_exact -> 
      Printf.printf "Constraint is %s\n" (LinearConstraint.string_of_p_nnconvex_constraint (fun _ -> "") c);
      not @@ LinearConstraint.p_nnconvex_constraint_is_false c
    | _ -> 
      Printf.printf "Constraint is %s\n" (LinearConstraint.string_of_p_nnconvex_constraint (fun _ -> "") c);
      
      false)
  | Error_result _ -> Printf.printf "ERROR"; false
  | Syntax_check_result _ -> Printf.printf "Syntax check result"; false
  | _ ->  Printf.printf "Result is not a good constraint\n\n";
    false