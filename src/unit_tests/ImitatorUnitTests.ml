
(* Main unit test module
   File contributors : Mikael Bisgaard Dahlsen-Jensen

   Intended structure: Each internal module exposes tests that are then added in the Alcotest.run method in the bottom
   Utility methods for testing can be added in UnitTestUtility.ml 
*)

open Lib
open UnitTestUtility


module LinearConstraintTests = struct

  let variables : LinearConstraintUtility.variables = {
    parameters = ["p";"q"]; 
    clocks= ["x";"y";"z"]; 
    discrete= ["d"]
  }  in 
  LinearConstraintUtility.initialize variables;
  
  open LinearConstraintUtility

  (* === TESTS FROM HERE === *)
  let test_p_true_constraint () = 
    let true_constraint = LinearConstraint.p_true_constraint () in
    Alcotest.(check p_constraint) "sanity check: true should be true" (LinearConstraint.p_true_constraint()) true_constraint

  let close_clocks_true () = 
    let input = LinearConstraint.px_true_constraint () in 
    let expected = LinearConstraint.px_true_constraint () in 
    let result = LinearConstraint.close_clocks_px_linear_constraint input in
    Alcotest.(check px_constraint) "close clocks on true should be unchanged" expected result
  let close_clocks_simple_strict () = 
    let input = simple_px_constraint "x" "<" 5 in
    let expected = simple_px_constraint "x" "<=" 5 in
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check px_constraint) "close clocks on strict should become non-strict" expected result
  let close_clocks_simple_non_strict () = 
      let input = simple_px_constraint "x" "<=" 5 in
      let expected = simple_px_constraint "x" "<=" 5 in
      let result = LinearConstraint.close_clocks_px_linear_constraint input in 
      Alcotest.(check px_constraint) "close clocks on non-strict should remain non-strict" expected result
  let close_clocks_simple_strict_param () = 
    let input = simple_px_constraint "p" "<" 5 in
    let expected = simple_px_constraint "p" "<" 5 in
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check px_constraint) "close clocks on param strict should remain strict" expected result

  let close_clocks_complex_constr () = 
    let input = combination_px_constraint [
      diagonal_px_constraint "x" "<" "y" 0;
      simple_px_constraint "q" "<" 6;
      diagonal_px_constraint "p" "<" "q" 0;
      simple_px_constraint "z" "<" 2;
      diagonal_px_constraint "p" "<" "x" 5;
      simple_px_constraint "y" ">" 2;
    ]  in 
    let expected = combination_px_constraint [
      diagonal_px_constraint "x" "<=" "y" 0;
      simple_px_constraint "q" "<" 6;
      diagonal_px_constraint "p" "<" "q" 0;
      simple_px_constraint "z" "<=" 2;
      diagonal_px_constraint "p" "<=" "x" 5;
      simple_px_constraint "y" ">=" 2;
    ]  in 
    let result = LinearConstraint.close_clocks_px_linear_constraint input in 
    Alcotest.(check @@ neg px_constraint) "sanity check: complex example should not be false" (LinearConstraint.px_false_constraint()) input;
    Alcotest.(check px_constraint) "close clocks on complex constr should be correct" expected result
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
        test_case "Close clocks on complex constr" `Quick LinearConstraintTests.close_clocks_complex_constr      
        ];
  ]