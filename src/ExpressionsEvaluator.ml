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
 * Last modified     : 2020/02/19
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
(** Evaluate arithmetic expressions with a valuation *)
(************************************************************)

let rec eval_rational_factor rational_valuation = function
	| CCF_variable variable_index ->
		rational_valuation variable_index
		
	| CCF_constant variable_value ->
		variable_value
		
	| CCF_expression rational_arithmetic_expression ->
		eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression
	
	| CCF_unary_min rational_factor ->
		NumConst.neg (eval_rational_factor rational_valuation rational_factor)

and eval_rational_term rational_valuation = function
	| CCT_mul (rational_term, rational_factor) ->
		NumConst.mul
		(eval_rational_term rational_valuation rational_term)
		(eval_rational_factor rational_valuation rational_factor)
		
	| CCT_div (rational_term, rational_factor) ->
			(*** TODO: division by zero ***)
		NumConst.div
		(eval_rational_term rational_valuation rational_term)
		(eval_rational_factor rational_valuation rational_factor)
		
	| CCT_factor rational_factor ->
		eval_rational_factor rational_valuation rational_factor

and eval_rational_arithmetic_expression rational_valuation = function
	| CCE_plus (rational_arithmetic_expression, rational_term) ->
		NumConst.add
		(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression)
		(eval_rational_term rational_valuation rational_term)
		
	| CCE_minus (rational_arithmetic_expression, rational_term) ->
		NumConst.sub
		(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression)
		(eval_rational_term rational_valuation rational_term)
		
	| CCE_term rational_term ->
		eval_rational_term rational_valuation rational_term

	
	

(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

let eval_rational_relop relop value_1 value_2 : bool =
	match relop with
	| OP_L		-> value_1 <  value_2
	| OP_LEQ	-> value_1 <= value_2
	| OP_EQ		-> value_1 =  value_2
	| OP_NEQ	-> value_1 <> value_2
	| OP_GEQ	-> value_1 >= value_2
	| OP_G		-> value_1 >  value_2

let check_rational_boolean_expression rational_valuation = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| RBE_Expression (rational_arithmetic_expression_1, relop, rational_arithmetic_expression_2) ->
		eval_rational_relop
			relop
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_1)
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_2)
			
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| RBE_Expression_in (rational_arithmetic_expression_1, rational_arithmetic_expression_2, rational_arithmetic_expression_3) ->
		(* Compute the first one to avoid redundancy *)
		let expr1_evaluated = eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_1 in
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_2)
			<=
			expr1_evaluated
			&&
			expr1_evaluated
			<= 
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_3)
