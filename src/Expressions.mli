(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions over continuous and discrete variables
 *
 * File contributors : Étienne André
 * Created           : 2020/02/19
 * Last modified     : 2020/02/19
 *
 ************************************************************)

(************************************************************)
(** Guards and invariants *)
(************************************************************)

(*------------------------------------------------------------*)
(* Continuous expressions (for guards) *)
(*------------------------------------------------------------*)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G

type convex_continuous_expression =
	| CCE_plus  of convex_continuous_expression * convex_continuous_term
	| CCE_minus of convex_continuous_expression * convex_continuous_term
	| CCE_term  of convex_continuous_term

and convex_continuous_term =
	| CCT_mul    of convex_continuous_term * convex_continuous_factor
	| CCT_div    of convex_continuous_term * convex_continuous_factor
	| CCT_factor of convex_continuous_factor

and convex_continuous_factor =
	| CCF_variable   of Automaton.variable_index
	| CCF_constant   of Automaton.constant_value
	| CCF_expression of convex_continuous_expression
	| CCF_unary_min  of convex_continuous_factor

type convex_continuous_boolean_inequality = convex_continuous_expression * relop * convex_continuous_expression

(** Convex Boolean expression on discrete and continuous variables *)
type convex_continuous_boolean_expression =
	| CCBE_True (** True *)
	| CCBE_False (** False *)
	| CCBE_conjunction of convex_continuous_boolean_inequality list (** Conjunction *)


(*------------------------------------------------------------*)
(** Convex Boolean expression on discrete variables (for guards) *)
(*------------------------------------------------------------*)

(* A discrete Boolean expression is just a continuous Boolean expression without clock variables in it *)
type convex_discrete_boolean_expression = convex_continuous_boolean_expression




(************************************************************)
(** Updates *)
(************************************************************)

type rational_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| RBE_Expression    of convex_continuous_expression * relop * convex_continuous_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| RBE_Expression_in of convex_continuous_expression * convex_continuous_expression * (*rational_arithmetic_expression*)convex_continuous_expression


(*------------------------------------------------------------*)
(* Discrete Boolean expressions (for conditional expressions in updates) *)
(*------------------------------------------------------------*)

type convex_discrete_expression = convex_continuous_expression

(** Boolean expression on discrete variables (for updates) *)
type discrete_boolean_expression =
	| DBE_True (** True *)
	| DBE_False (** False *)
	| DBE_Not of discrete_boolean_expression (** Negation *)
	| DBE_And of discrete_boolean_expression * discrete_boolean_expression (** Conjunction *)
	| DBE_Or  of discrete_boolean_expression * discrete_boolean_expression (** Disjunction *)
	| DBE_Rational_boolean_expression of rational_boolean_expression


(*------------------------------------------------------------*)
(*** TODO ***)
(*------------------------------------------------------------*)
type string_term = string


