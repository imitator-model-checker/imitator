
(* Non linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression
(*type nonlinear_constraint = nonlinear_inequality list*)
type nonlinear_constraint =
  | True_nonlinear_constraint (* TODO to remove ? *)
  | False_nonlinear_constraint (* TODO to remove ? *)
  | Nonlinear_constraint of nonlinear_inequality list (* TODO to replace with discrete_boolean_expression list ? *)

(* Evaluations *)
val check_nonlinear_constraint : DiscreteExpressions.discrete_valuation -> nonlinear_constraint -> bool

(* Strings *)
val customized_string_of_nonlinear_constraint : Constants.customized_string -> (Automaton.variable_index -> string) -> nonlinear_constraint -> string

val string_of_nonlinear_constraint : (Automaton.variable_index -> string) -> nonlinear_constraint -> string