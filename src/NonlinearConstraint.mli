(* Types *)

(* Re-declare variable as int for semantic *)
type variable = int

(* Non-linear custom expression without PPL *)
type nonlinear_inequality = DiscreteExpressions.discrete_arithmetic_expression * DiscreteExpressions.relop * DiscreteExpressions.discrete_arithmetic_expression

(*type nonlinear_constraint = nonlinear_inequality list*)
type nonlinear_constraint =
  | True_nonlinear_constraint
  | False_nonlinear_constraint
  | Nonlinear_constraint of DiscreteExpressions.discrete_boolean_expression list

(* Evaluations *)
val check_nonlinear_constraint : DiscreteExpressions.discrete_valuation -> nonlinear_constraint -> bool

(*(* Utility *)*)
(*val is_constrained : nonlinear_constraint -> variable -> bool*)

(*(** Compute the intersection of nonlinear constraints **)*)
(*val intersection : nonlinear_constraint list -> nonlinear_constraint*)

(* Strings *)
val customized_string_of_nonlinear_constraint : Constants.customized_string -> (Automaton.variable_index -> string) -> nonlinear_constraint -> string
val string_of_nonlinear_constraint : (Automaton.variable_index -> string) -> nonlinear_constraint -> string
(*JANI*)
val customized_strings_of_nonlinear_constraint_for_jani : Constants.customized_string -> (Automaton.variable_index -> string) -> nonlinear_constraint -> string list
val strings_of_nonlinear_constraint_for_jani : (Automaton.variable_index -> string) -> nonlinear_constraint -> string list
