open Lib

(* Utility class for generating unit tests to avoid cluttering the main unit test class*)

exception UnitTestException of string

(* Utility module for testing LinearConstraint. It has useful Alcotest.testables as well as functions to help 
 build simple, diagonal or combined constraints *)
module LinearConstraintUtility = struct

  type variables = {parameters: string list; clocks: string list; discrete: string list}

  let flat_vars = ref []

  let initialize (variables : variables) = 
    LinearConstraint.set_dimensions 
    (List.length variables.parameters)
    (List.length variables.clocks)
    (List.length variables.discrete);
    flat_vars := variables.parameters @ variables.clocks @ variables.discrete

  let var_map (variable : int) = 
    List.nth (!flat_vars) variable
  
  let var_of_string (variable_str : string) : int =
    let rec index_of list elem ctr = 
      match list with 
      | [] -> raise @@ UnitTestException ("not a known variable: " ^ variable_str)
      | x :: xs -> if x = elem then ctr else index_of xs elem (ctr+1)
    in
    index_of (!flat_vars) variable_str 0 
  let op_of_string = function 
  | "<" -> LinearConstraint.Op_l
  | "<=" -> LinearConstraint.Op_le
  | ">" -> LinearConstraint.Op_g
  | ">=" -> LinearConstraint.Op_ge
  | "=" -> LinearConstraint.Op_eq
  | op -> raise @@ UnitTestException ("unknown operator: " ^ op)

  type sign = P | M 
  let neg = function P -> M | M -> P
  let numconst_of_sign = function P -> NumConst.one | M -> NumConst.minus_one

  let numconst_var_of_string sign var_string = 
    if String.sub var_string 0 1 = "-" then
      let remainder = String.sub var_string 1 (String.length var_string - 1) in
      (numconst_of_sign @@ neg sign, var_of_string remainder)
    else 
      (numconst_of_sign sign, var_of_string var_string) 
  let simple_px_constraint var op const =
    let term = LinearConstraint.make_px_linear_term 
      [numconst_var_of_string P var] (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]

  let simple_pxd_constraint var op const =
    let term = LinearConstraint.make_pxd_linear_term 
      [numconst_var_of_string P var] (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_pxd_linear_inequality term op in 
    LinearConstraint.make_pxd_constraint [inequal]

  let diagonal_px_constraint var1 op var2 const = 
    let term = LinearConstraint.make_px_linear_term 
      [numconst_var_of_string P var1; numconst_var_of_string M var2] 
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]


  let diagonal_px_constraint var1 op var2 const = 
    let term = LinearConstraint.make_px_linear_term 
      [numconst_var_of_string P var1; numconst_var_of_string M var2] 
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]

  let multi_diagonal_px_constraint var1 op var2 var3 const = 
    let term = LinearConstraint.make_px_linear_term 
      [numconst_var_of_string P var1; numconst_var_of_string M var2; numconst_var_of_string M var3] 
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]

  let diagonal_pxd_constraint var1 op var2 const = 
    let term = LinearConstraint.make_pxd_linear_term 
      [numconst_var_of_string P var1; numconst_var_of_string M var2]
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_pxd_linear_inequality term op in 
    LinearConstraint.make_pxd_constraint [inequal]

  let intersection_p_constraint = LinearConstraint.p_intersection
  let intersection_px_constraint = LinearConstraint.px_intersection
  let intersection_pxd_constraint = LinearConstraint.pxd_intersection

  let union_p_constraint = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraints
  let union_px_constraint = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints

  let nn_convex_px = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint

  let p_linear_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_p_linear_constraint var_map)) 
  LinearConstraint.p_is_equal
  let px_linear_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_px_linear_constraint var_map)) 
  LinearConstraint.px_is_equal
  let pxd_linear_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_pxd_linear_constraint var_map)) 
  LinearConstraint.pxd_is_equal
  let p_nnconvex_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_p_nnconvex_constraint var_map)) 
  LinearConstraint.p_nnconvex_constraint_is_equal
  let px_nnconvex_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_px_nnconvex_constraint var_map)) 
  LinearConstraint.px_nnconvex_constraint_is_equal
end