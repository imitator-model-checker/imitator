
(* Non linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression
(*type nonlinear_constraint = nonlinear_inequality list*)
type nonlinear_constraint =
  | True_nonlinear_constraint
  | False_nonlinear_constraint
  | Nonlinear_constraint of nonlinear_inequality list

(************************************************************)
(** Check whether a non linear constraint evaluates to true when valuated with a valuation *)
(************************************************************)
(* TODO benjamin to test *)

let check_nonlinear_inequality discrete_valuation nonlinear_inequality =
    let l_expr, relop, r_expr = nonlinear_inequality in
    let l_expr_eval, r_expr_eval =
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation l_expr,
      DiscreteExpressions.eval_discrete_arithmetic_expression discrete_valuation r_expr
    in
      DiscreteExpressions.eval_discrete_relop relop l_expr_eval l_expr_eval

(* if all true, it's satisfied *)
let check_nonlinear_inequalities discrete_valuation nonlinear_inequalities =
  List.for_all (check_nonlinear_inequality discrete_valuation) nonlinear_inequalities

(* Check if a nonlinear constraint is satisfied *)
let check_nonlinear_constraint discrete_valuation = function
  | True_nonlinear_constraint -> true
  | False_nonlinear_constraint -> false
  | Nonlinear_constraint nonlinear_inequalities -> check_nonlinear_inequalities discrete_valuation nonlinear_inequalities