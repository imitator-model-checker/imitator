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


(************************************************************)
(* Valuate all variables in a given list with their valuation and returns the resulting convex_continuous_boolean_expression *)
(************************************************************)

let rec valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression : convex_continuous_expression =
	match convex_continuous_expression with
	| CCE_plus  (convex_continuous_expression , convex_continuous_term) ->
		CCE_plus (
			valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression
			,
			valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term
			)

	| CCE_minus (convex_continuous_expression , convex_continuous_term) ->
		CCE_minus (
			valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression
			,
			valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term
			)
	
	| CCE_term  convex_continuous_term ->
		CCE_term (valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term)

and valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term : convex_continuous_term =
	match convex_continuous_term with
	| CCT_mul    (convex_continuous_term , convex_continuous_factor) ->
		(*** TODO: simplify if clock * discrete? ***)
		CCT_mul (
			valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term
			,
			valuate_rationals_in_convex_continuous_factor rational_variables rational_valuation convex_continuous_factor
			)
	
	| CCT_div    (convex_continuous_term , convex_continuous_factor) ->
		(*** TODO: simplify if clock / discrete? ***)
		CCT_div (
			valuate_rationals_in_convex_continuous_term rational_variables rational_valuation convex_continuous_term
			,
			valuate_rationals_in_convex_continuous_factor rational_variables rational_valuation convex_continuous_factor
			)
	
	| CCT_factor convex_continuous_factor ->
		CCT_factor (valuate_rationals_in_convex_continuous_factor rational_variables rational_valuation convex_continuous_factor)

and valuate_rationals_in_convex_continuous_factor rational_variables rational_valuation convex_continuous_factor : convex_continuous_factor =
	match convex_continuous_factor with
	| CCF_variable   variable_index ->
		(* If variables to be valuated *)
		if List.mem variable_index rational_variables then (
		
			(* Valuate *)
			CCF_constant (rational_valuation variable_index)
		
		)else(
		(* Else keep unchanged *)
			CCF_variable variable_index
		)
	
	| CCF_constant   constant_value -> CCF_constant   constant_value
	
	| CCF_expression convex_continuous_expression ->
		CCF_expression (valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression)
	
	| CCF_unary_min  convex_continuous_factor ->
		CCF_unary_min (valuate_rationals_in_convex_continuous_factor rational_variables rational_valuation convex_continuous_factor : convex_continuous_factor)


let valuate_rationals_in_convex_continuous_boolean_inequality rational_variables rational_valuation ((convex_continuous_expression_l , relop , convex_continuous_expression_r) : convex_continuous_boolean_inequality) : convex_continuous_boolean_inequality  =
	(valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression_l)
	,
	relop
	,
	(valuate_rationals_in_convex_continuous_expression rational_variables rational_valuation convex_continuous_expression_r)


let valuate_rationals_in_convex_continuous_boolean_inequalities rational_variables rational_valuation = List.map (valuate_rationals_in_convex_continuous_boolean_inequality rational_variables rational_valuation)


let valuate_rationals_in_convex_continuous_boolean_expression (rational_variables : Automaton.discrete_index list) (rational_valuation : rational_valuation) (convex_continuous_boolean_expression : convex_continuous_boolean_expression) : convex_continuous_boolean_expression =
	match convex_continuous_boolean_expression with
	| CCBE_True -> CCBE_True
	| CCBE_False -> CCBE_False
	| CCBE_conjunction convex_continuous_boolean_inequalities ->
		CCBE_conjunction (valuate_rationals_in_convex_continuous_boolean_inequalities rational_variables rational_valuation convex_continuous_boolean_inequalities)


