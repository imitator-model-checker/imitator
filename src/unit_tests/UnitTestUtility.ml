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
      | [] -> raise @@ UnitTestException "not a known variable"
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

  let simple_px_constraint var op const =
    let term = LinearConstraint.make_px_linear_term 
      [(NumConst.one, var_of_string var)] (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]

  let simple_pxd_constraint var op const =
    let term = LinearConstraint.make_pxd_linear_term 
      [(NumConst.one, var_of_string var)] (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_pxd_linear_inequality term op in 
    LinearConstraint.make_pxd_constraint [inequal]

  let diagonal_px_constraint var1 op var2 const = 
    let term = LinearConstraint.make_px_linear_term 
      [(NumConst.one, var_of_string var1); (NumConst.minus_one, var_of_string var2)] 
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_px_linear_inequality term op in 
    LinearConstraint.make_px_constraint [inequal]

  let diagonal_pxd_constraint var1 op var2 const = 
    let term = LinearConstraint.make_pxd_linear_term 
      [(NumConst.one, var_of_string var1); (NumConst.minus_one, var_of_string var2)] 
      (NumConst.neg (NumConst.numconst_of_int const)) in
    let op = op_of_string op in 
    let inequal = LinearConstraint.make_pxd_linear_inequality term op in 
    LinearConstraint.make_pxd_constraint [inequal]

  let combination_p_constraint = LinearConstraint.p_intersection
  let combination_px_constraint = LinearConstraint.px_intersection
  let combination_pxd_constraint = LinearConstraint.pxd_intersection

  let p_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_p_linear_constraint var_map)) 
  LinearConstraint.p_is_equal
  let px_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_px_linear_constraint var_map)) 
  LinearConstraint.px_is_equal
  let pxd_constraint = Alcotest.testable 
  (Fmt.of_to_string (LinearConstraint.string_of_pxd_linear_constraint var_map)) 
  LinearConstraint.pxd_is_equal
end