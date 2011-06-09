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
	set_manager 0 7;
	
	let term = LinearConstraint.make_linear_term [(NumConst.one, 0); (NumConst.one, 1); (NumConst.numconst_of_frac 2 3, 2)] (NumConst.zero) in
	let ineq = LinearConstraint.make_linear_inequality term LinearConstraint.Op_ge in
	let poly = LinearConstraint.make [ineq] in
	
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names poly));
	assert_bool "constraint is unsatisfiable" (LinearConstraint.is_satisfiable poly);
	
	let poly_mapped = LinearConstraint.rename_variables [(0,3); (1,4)] poly in
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names poly_mapped));
	assert_bool "constraint is unsatisfiable" (LinearConstraint.is_satisfiable poly_mapped)
	
  
let test_inclusion _ =
	set_manager 0 7;
	
	(* make inequality c > 0 *)
	let c_greater_zero = make_linear_inequality_ppl (Var 2) Greater_Than_RS (Coef NumConst.zero) in
	(* make equality a = b *)
	let eq_ab = make_linear_inequality_ppl (Var 0) Equal_RS (Var 1) in
	(* make inequality a >= c *)
	let ineq_ac = make_linear_inequality_ppl (Var 0) Greater_Or_Equal_RS (Var 2) in
	(* make inequality b >= 2c *)
	let ineq_bc = make_linear_inequality_ppl (Var 1) Greater_Or_Equal_RS (Ti (c, Var 2)) in
	(* make constraint a = b & a >= c *)
	let constr1 = make [eq_ab; ineq_ac; c_greater_zero] in
	(* make constraint a = b & b >= 2c *)
	let constr2 = make [eq_ab; ineq_bc; c_greater_zero] in
	assert_bool "contraint not included" (is_leq constr2 constr1) 
		
		
(** Build testsuite **)

let suite = "testLinearConstraint" >::: 
	["test_sat"       >:: test_sat;
	 "test_inclusion" >:: test_inclusion]
	 	
