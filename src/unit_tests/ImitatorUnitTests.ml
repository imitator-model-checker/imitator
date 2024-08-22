
(* Main unit test module
   File contributors : Mikael Bisgaard Dahlsen-Jensen

   Intended structure: Each internal module exposes tests that are then added in the Alcotest.run method in the bottom
   Utility methods for testing can be added in UnitTestUtility.ml 
*)

open Lib
open UnitTestUtility


module LinearConstraintTests = struct

  let variables : LinearConstraintUtility.variables = {
    parameters = ["p";"q";"epsilon"]; 
    clocks= ["x";"y";"z"]; 
    discrete= ["d"]
  }  in 
  LinearConstraintUtility.initialize variables;
  
  open LinearConstraintUtility

  (* === TESTS FROM HERE === *)
  let test_p_true_constraint () = 
    let true_constraint = LinearConstraint.p_true_constraint () in
    Alcotest.(check p_linear_constraint) "sanity check: true should be true" (LinearConstraint.p_true_constraint()) true_constraint

  let close_clocks_true () = 
    let input = LinearConstraint.px_true_constraint () in 
    let expected = LinearConstraint.px_true_constraint () in 
    let result = LinearConstraint.close_clocks_px_linear_constraint input in
    Alcotest.(check px_linear_constraint) "close clocks on true should be unchanged" expected result
  let close_clocks_simple_strict () = 
    let input = simple_px_constraint "x" "<" 5 in
    let expected = simple_px_constraint "x" "<=" 5 in
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check px_linear_constraint) "close clocks on strict should become non-strict" expected result
  let close_clocks_simple_non_strict () = 
      let input = simple_px_constraint "x" "<=" 5 in
      let expected = simple_px_constraint "x" "<=" 5 in
      let result = LinearConstraint.close_clocks_px_linear_constraint input in 
      Alcotest.(check px_linear_constraint) "close clocks on non-strict should remain non-strict" expected result
  let close_clocks_simple_strict_param () = 
    let input = simple_px_constraint "p" "<" 5 in
    let expected = simple_px_constraint "p" "<" 5 in
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check px_linear_constraint) "close clocks on param strict should remain strict" expected result

  let close_clocks_complex_constr () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "y" 0;
      simple_px_constraint "q" "<" 6;
      diagonal_px_constraint "p" "<" "q" 0;
      simple_px_constraint "z" "<" 2;
      diagonal_px_constraint "p" "<" "x" 5;
      simple_px_constraint "y" ">" 2;
    ]  in 
    let expected = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "y" 0;
      simple_px_constraint "q" "<" 6;
      diagonal_px_constraint "p" "<" "q" 0;
      simple_px_constraint "z" "<=" 2;
      diagonal_px_constraint "p" "<=" "x" 5;
      simple_px_constraint "y" ">=" 2;
    ]  in 
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check @@ neg px_linear_constraint) "sanity check: complex example should not be false" (LinearConstraint.px_false_constraint()) input;
    Alcotest.(check px_linear_constraint) "close clocks on complex constr should be correct" expected result

  (* BOUND COMPUTING TESTS *)
  let precise_upper_bound_simple_constr_1 () =
    let input = simple_px_constraint "x" "<" 5 in
    let expected = nn_convex_px (simple_px_constraint "x" "=" 5) in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of simple constraint should be correct" expected result

  let precise_upper_bound_simple_constr_2 () =
    let input = simple_px_constraint "y" "<=" 8 in
    let expected = nn_convex_px (simple_px_constraint "y" "=" 8) in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of simple constraint should be correct" expected result
  let precise_upper_bound_complex_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "y" "<=" "x" 2;
      diagonal_px_constraint "x" "<=" "y" 3;
      simple_px_constraint "x" "<" 4;
      simple_px_constraint "y" "<" 4;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint[
        simple_px_constraint "y" "<=" 4;
        simple_px_constraint "y" ">=" 1;
        simple_px_constraint "x" "=" 4;
      ];
      intersection_px_constraint[
        simple_px_constraint "x" "<=" 4;
        simple_px_constraint "x" ">=" 2;
        simple_px_constraint "y" "=" 4;
      ]
    ]
    in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of complex constraint should be correct" expected result
  
  let precise_upper_bound_simple_param_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "p" 0;
    ] in 
    let expected = nn_convex_px (diagonal_px_constraint "x" "=" "p" 0) in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "bound of parameter constraint should be correct" expected result
  
  let precise_upper_bound_simple_param_constr_2 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" ">" "p" 0;
      diagonal_px_constraint "y" "<" "q" 2;
    ] in 
    let expected = nn_convex_px (
      intersection_px_constraint [
        diagonal_px_constraint "x" ">=" "p" 0;
        diagonal_px_constraint "y" "=" "q" 2;
      ]
    ) in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of parameter constraint should be correct" expected result

  let precise_upper_bound_complex_param_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" ">" "p" 0;
      simple_px_constraint "z" "<" 2;
      simple_px_constraint "z" "<" 4;
      diagonal_px_constraint "y" "<" "q" 2;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint [
        diagonal_px_constraint "x" ">=" "p" 0;
        diagonal_px_constraint "y" "<=" "q" 2;
        simple_px_constraint "z" "=" 2;
      ];
      intersection_px_constraint [
        diagonal_px_constraint "x" ">=" "p" 0;
        diagonal_px_constraint "y" "=" "q" 2;
        simple_px_constraint "z" "<=" 2;
      ];
     ] in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of parameter constraint should be correct" expected result
  
  let precise_upper_bound_complex_param_constr_2 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "p" 5;
      simple_px_constraint "x" "<" 2;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint [
        diagonal_px_constraint "x" "=" "p" 5;
        simple_px_constraint "x" "<=" 2;
      ];
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 5;
        simple_px_constraint "x" "=" 2;
      ];
      ] in
    let result = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "upper bound of parameter constraint should be correct" expected result
        

let precise_lower_bound_simple_constr_1 () =
    let input = simple_px_constraint "x" ">" 5 in
    let expected = nn_convex_px (simple_px_constraint "x" "=" 5) in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of simple constraint should be correct" expected result

  let precise_lower_bound_simple_constr_2 () =
    let input = simple_px_constraint "y" ">=" 8 in
    let expected = nn_convex_px (simple_px_constraint "y" "=" 8) in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of simple constraint should be correct" expected result
  let precise_lower_bound_complex_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "y" "<=" "x" 2;
      diagonal_px_constraint "x" "<=" "y" 3;
      simple_px_constraint "x" ">" 4;
      simple_px_constraint "y" ">" 4;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint[
        simple_px_constraint "y" ">=" 4;
        simple_px_constraint "y" "<=" 6;
        simple_px_constraint "x" "=" 4;
      ];
      intersection_px_constraint[
        simple_px_constraint "x" ">=" 4;
        simple_px_constraint "x" "<=" 7;
        simple_px_constraint "y" "=" 4;
      ]
    ]
    in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of complex constraint should be correct" expected result
  let precise_lower_bound_simple_param_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" ">" "p" 0;
    ] in 
    let expected = nn_convex_px (diagonal_px_constraint "x" "=" "p" 0) in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "bound of parameter constraint should be correct" expected result
  
  let precise_lower_bound_simple_param_constr_2 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "p" 0;
      diagonal_px_constraint "y" ">" "q" 2;
    ] in 
    let expected = nn_convex_px (
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 0;
        diagonal_px_constraint "y" "=" "q" 2;
      ]
    ) in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of parameter constraint should be correct" expected result

  let precise_lower_bound_complex_param_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "p" 0;
      simple_px_constraint "z" ">" 2;
      simple_px_constraint "z" ">" 4;
      diagonal_px_constraint "y" ">" "q" 2;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 0;
        diagonal_px_constraint "y" ">=" "q" 2;
        simple_px_constraint "z" "=" 4;
      ];
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 0;
        diagonal_px_constraint "y" "=" "q" 2;
        simple_px_constraint "z" ">=" 4;
      ];
     ] in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of parameter constraint should be correct" expected result
  
  let precise_lower_bound_complex_param_constr_2 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" ">" "p" 5;
      simple_px_constraint "x" ">" 2;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint [
        diagonal_px_constraint "x" "=" "p" 5;
        simple_px_constraint "x" ">=" 2;
      ];
      intersection_px_constraint [
        diagonal_px_constraint "x" ">=" "p" 5;
        simple_px_constraint "x" "=" 2;
      ];
      ] in
    let result = LinearConstraint.precise_temporal_lower_bound_px_linear_constraint input in 
    Alcotest.(check px_nnconvex_constraint) "lower bound of parameter constraint should be correct" expected result

  let epsilon_upper_bound_simple_constr_1 () =
    let input = simple_px_constraint "x" "<" 5 in
    let expected = nn_convex_px (
      intersection_px_constraint [
        simple_px_constraint "x" "<=" 5;
        diagonal_px_constraint "x" ">=" "-epsilon" 5
      ]) in
    let result = LinearConstraint.epsilon_temporal_upper_bound_px_linear_constraint (var_of_string "epsilon") input in 
    Alcotest.(check px_nnconvex_constraint) "epsilon upper bound of simple constraint should be correct" expected result


  let epsilon_upper_bound_complex_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "y" "<=" "x" 2;
      diagonal_px_constraint "x" "<=" "y" 3;
      simple_px_constraint "x" "<" 4;
      simple_px_constraint "y" "<" 4;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint[
        diagonal_px_constraint "y" "<=" "x" 2;
        diagonal_px_constraint "x" "<=" "y" 3;
        simple_px_constraint "x" "<=" 4;
        simple_px_constraint "y" "<=" 4;
        diagonal_px_constraint "x" ">=" "-epsilon" 4;
      ];
      intersection_px_constraint[
        diagonal_px_constraint "y" "<=" "x" 2;
        diagonal_px_constraint "x" "<=" "y" 3;
        simple_px_constraint "x" "<=" 4;
        simple_px_constraint "y" "<=" 4;
        diagonal_px_constraint "y" ">=" "-epsilon" 4;
      ]
    ]
    in
    let result = LinearConstraint.epsilon_temporal_upper_bound_px_linear_constraint (var_of_string "epsilon") input in 
    Alcotest.(check px_nnconvex_constraint) "epsilon upper bound of complex constraint should be correct" expected result
  
  let epsilon_lower_bound_simple_constr_1 () =
    let input = simple_px_constraint "x" ">" 5 in
    let expected = nn_convex_px (
      intersection_px_constraint [
        simple_px_constraint "x" ">=" 5;
        diagonal_px_constraint "x" "<=" "epsilon" 5
      ]) in
    let result = LinearConstraint.epsilon_temporal_lower_bound_px_linear_constraint (var_of_string "epsilon") input in 
    Alcotest.(check px_nnconvex_constraint) "epsilon lower bound of simple constraint should be correct" expected result


  let epsilon_lower_bound_complex_constr_1 () = 
    let input = intersection_px_constraint [
      diagonal_px_constraint "x" "<" "p" 0;
      simple_px_constraint "z" ">" 2;
      simple_px_constraint "z" ">" 4;
      diagonal_px_constraint "y" ">" "q" 2;
    ] in 
    let expected = union_px_constraint [
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 0;
        diagonal_px_constraint "y" ">=" "q" 2;
        diagonal_px_constraint "x" "<=" "p" 0;
        simple_px_constraint "z" ">=" 4;
        diagonal_px_constraint "z" "<=" "epsilon" 4;
      ];
      intersection_px_constraint [
        diagonal_px_constraint "x" "<=" "p" 0;
        diagonal_px_constraint "y" ">=" "q" 2;
        diagonal_px_constraint "x" "<=" "p" 0;
        simple_px_constraint "z" ">=" 4;
        multi_diagonal_px_constraint "y" "<=" "q" "epsilon" 2;
      ];
     ] in
    let result = LinearConstraint.epsilon_temporal_lower_bound_px_linear_constraint (var_of_string "epsilon") input in 
    Alcotest.(check px_nnconvex_constraint) "epsilon lower bound of complex constraint should be correct" expected result
end


let () =
  let open Alcotest in
run "Unit tests" [
      "linear-constraint", [
        test_case "True is True" `Quick LinearConstraintTests.test_p_true_constraint;
        test_case "Close clocks on True" `Quick LinearConstraintTests.close_clocks_true;
        test_case "Close clocks on simple constr" `Quick LinearConstraintTests.close_clocks_simple_strict;
        test_case "Close clocks on simple constr" `Quick LinearConstraintTests.close_clocks_simple_non_strict;
        test_case "Close clocks on simple p constr" `Quick LinearConstraintTests.close_clocks_simple_strict_param;
        test_case "Close clocks on complex constr" `Quick LinearConstraintTests.close_clocks_complex_constr;

        test_case "Upper bound on simple constr" `Quick LinearConstraintTests.precise_upper_bound_simple_constr_1;
        test_case "Upper bound on simple constr" `Quick LinearConstraintTests.precise_upper_bound_simple_constr_2;
        test_case "Upper bound on complex constr" `Quick LinearConstraintTests.precise_upper_bound_complex_constr_1;
        test_case "Upper bound on complex constr" `Quick LinearConstraintTests.precise_upper_bound_simple_param_constr_1;
        test_case "Upper bound on complex constr" `Quick LinearConstraintTests.precise_upper_bound_simple_param_constr_2;
        test_case "Upper bound on complex constr" `Quick LinearConstraintTests.precise_upper_bound_complex_param_constr_1;
        test_case "Upper bound on complex constr" `Quick LinearConstraintTests.precise_upper_bound_complex_param_constr_2;

        test_case "Lower bound on simple constr" `Quick LinearConstraintTests.precise_lower_bound_simple_constr_1;
        test_case "Lower bound on simple constr" `Quick LinearConstraintTests.precise_lower_bound_simple_constr_2;
        test_case "Lower bound on complex constr" `Quick LinearConstraintTests.precise_lower_bound_complex_constr_1;
        test_case "Lower bound on complex constr" `Quick LinearConstraintTests.precise_lower_bound_simple_param_constr_1;
        test_case "Lower bound on complex constr" `Quick LinearConstraintTests.precise_lower_bound_simple_param_constr_2;
        test_case "Lower bound on complex constr" `Quick LinearConstraintTests.precise_lower_bound_complex_param_constr_1;
        test_case "Lower bound on complex constr" `Quick LinearConstraintTests.precise_lower_bound_complex_param_constr_2;
        
        test_case "Upper ε-bound on simple constr" `Quick LinearConstraintTests.epsilon_upper_bound_simple_constr_1;
        test_case "Upper ε-bound on complex constr" `Quick LinearConstraintTests.epsilon_upper_bound_complex_constr_1;
        
        test_case "Lower ε-bound on simple constr" `Quick LinearConstraintTests.epsilon_lower_bound_simple_constr_1;
        test_case "Lower ε-bound on complex constr" `Quick LinearConstraintTests.epsilon_lower_bound_complex_constr_1;
        ];
  ]