(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: evaluating expressions
 *
 * File contributors : Étienne André
 * Created           : 2020/02/19
 * Last modified     : 2020/02/22
 *
 ************************************************************)

 
(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open Expressions


(************************************************************)
(************************************************************)
(* Types *)
(************************************************************)
(************************************************************)

type rational_valuation = Automaton.discrete_index -> Automaton.rational_value


(************************************************************)
(************************************************************)
(* Functions *)
(************************************************************)
(************************************************************)

(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

val check_rational_boolean_expression : rational_valuation -> rational_boolean_expression -> bool


(************************************************************)
(* Valuate all variables in a given list with their valuation and returns the resulting convex_continuous_boolean_expression *)
(************************************************************)
val valuate_rationals_in_convex_continuous_boolean_expression : Automaton.discrete_index list -> rational_valuation -> convex_continuous_boolean_expression -> convex_continuous_boolean_expression
