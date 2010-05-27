open OUnit
open Global
open LinearConstraint
open Ppl_ocaml

let names = function
	| 0 -> "a"
	| 1 -> "b"
	| 2 -> "c"
	| 3 -> "a'"
	| 4 -> "b'"
	| 5 -> "c'"
	| 6 -> "d"
	| 7 -> "e"
	| _ -> "x"

let a = NumConst.numconst_of_float 0.4
let b = NumConst.numconst_of_frac 1 3
let c = NumConst.numconst_of_int 2
let d = NumConst.numconst_of_float 2.00
let e = NumConst.numconst_of_frac 306 153

		
let test_sat _ =
	set_debug_mode Debug_high;
	set_manager 0 6;
	
	let term = LinearConstraint.make_linear_term [(NumConst.one, 0); (NumConst.one, 1); (NumConst.numconst_of_frac 2 3, 2)] (NumConst.zero) in
	let ineq = LinearConstraint.make_linear_inequality term LinearConstraint.Op_ge in
	let poly = LinearConstraint.make [ineq] in
	
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names poly));
	assert_bool "constraint is unsatisfiable" (LinearConstraint.is_satisfiable poly);
	
	let poly_mapped = LinearConstraint.rename_variables [(0,3); (1,4)] poly in
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names poly_mapped));
	assert_bool "constraint is unsatisfiable" (LinearConstraint.is_satisfiable poly_mapped)
	
		
(** Build testsuite **)

let suite = "testLinearConstraint" >::: 
	["test_sat" >:: test_sat]
	 	
