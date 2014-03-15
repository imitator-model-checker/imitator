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
	assert_bool "constraint is unsatisfiable" (LinearConstraint.is_satisfiable poly_mapped);
	
	let elapse = LinearConstraint.add_d 6 (NumConst.numconst_of_frac 2 3) [0; 1] poly in
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names elapse));
	
	(* try affine mapping *)
(*	let aff_expr = Plus (Variable 2, Variable 6) in                                                            *)
(*	let ppoly = LinearConstraint.to_ppl_polyhedron poly in                                                     *)
(*	ppl_Polyhedron_affine_preimage ppoly 2 aff_expr Gmp.Z.one;                                                 *)
(*	let image = LinearConstraint.from_ppl_polyhedron ppoly in                                                  *)
(*	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names image));*)
	
	(* try PPL's time elapse operator *)
	let my_poly = LinearConstraint.to_ppl_polyhedron (LinearConstraint.true_constraint ()) in
	ppl_Polyhedron_add_constraints my_poly [
		Equal (Variable 0, Coefficient Gmp.Z.zero);
		Equal (Variable 1, Coefficient Gmp.Z.zero);
		Equal (Variable 2, Coefficient Gmp.Z.one)	
	];
	let deriv = LinearConstraint.to_ppl_polyhedron (LinearConstraint.from_ppl_constraints [
		Equal (Variable 0, Coefficient Gmp.Z.one);
		Equal (Variable 1, Coefficient (Gmp.Z.from_int 2));
		Equal (Variable 2, Coefficient (Gmp.Z.from_int (-1)))
(*		Equal (Variable 3, Coefficient Gmp.Z.zero);*)
(*		Equal (Variable 4, Coefficient Gmp.Z.zero);*)
(*		Equal (Variable 5, Coefficient Gmp.Z.zero) *)
(*		Equal (Variable 6, Coefficient Gmp.Z.zero)		*)
	]) in
	print_message Debug_standard ("derivatives: " ^ (LinearConstraint.string_of_linear_constraint names (LinearConstraint.from_ppl_polyhedron deriv)));
	ppl_Polyhedron_time_elapse_assign my_poly deriv;
	let new_constr = LinearConstraint.from_ppl_polyhedron my_poly in
	print_message Debug_standard ("constraint: " ^ (LinearConstraint.string_of_linear_constraint names new_constr))	
	
		
(** Build testsuite **)

let suite = "testLinearConstraint" >::: 
	["test_sat" >:: test_sat]
	 	
