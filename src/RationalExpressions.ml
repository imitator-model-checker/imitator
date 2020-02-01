(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: arithmetic and Boolean expressions on discrete variables
 *
 * File contributors : Étienne André
 * Created           : 2019/12/10
 * Last modified     : 2020/02/01
 *
 ************************************************************)



(****************************************************************)
(** Operators *)
(****************************************************************)

(** Boolean operators *)

type relop = OP_L | OP_LEQ | OP_EQ | OP_NEQ | OP_GEQ | OP_G

(****************************************************************)
(** Valuation *)
(****************************************************************)
type rational_valuation = Automaton.discrete_index -> Automaton.rational_value


(****************************************************************)
(** Arithmetic expressions for rational discrete variables *)
(****************************************************************)
type rational_arithmetic_expression =
	| DAE_plus of rational_arithmetic_expression * rational_term
	| DAE_minus of rational_arithmetic_expression * rational_term
	| DAE_term of rational_term

and rational_term =
	| DT_mul of rational_term * rational_factor
	| DT_div of rational_term * rational_factor
	| DT_factor of rational_factor

and rational_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of Automaton.variable_value
	| DF_expression of rational_arithmetic_expression
	| DF_unary_min of rational_factor


(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

type rational_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of rational_arithmetic_expression * relop * rational_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of rational_arithmetic_expression * rational_arithmetic_expression * rational_arithmetic_expression




(************************************************************)
(** Evaluate arithmetic expressions with a valuation *)
(************************************************************)

let rec eval_rational_factor rational_valuation = function
	| DF_variable variable_index ->
		rational_valuation variable_index
		
	| DF_constant variable_value ->
		variable_value
		
	| DF_expression rational_arithmetic_expression ->
		eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression
	
	| DF_unary_min rational_factor ->
		NumConst.neg (eval_rational_factor rational_valuation rational_factor)

and eval_rational_term rational_valuation = function
	| DT_mul (rational_term, rational_factor) ->
		NumConst.mul
		(eval_rational_term rational_valuation rational_term)
		(eval_rational_factor rational_valuation rational_factor)
		
	| DT_div (rational_term, rational_factor) ->
			(*** TODO: division by zero ***)
		NumConst.div
		(eval_rational_term rational_valuation rational_term)
		(eval_rational_factor rational_valuation rational_factor)
		
	| DT_factor rational_factor ->
		eval_rational_factor rational_valuation rational_factor

and eval_rational_arithmetic_expression rational_valuation = function
	| DAE_plus (rational_arithmetic_expression, rational_term) ->
		NumConst.add
		(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression)
		(eval_rational_term rational_valuation rational_term)
		
	| DAE_minus (rational_arithmetic_expression, rational_term) ->
		NumConst.sub
		(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression)
		(eval_rational_term rational_valuation rational_term)
		
	| DAE_term rational_term ->
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
	| Expression (rational_arithmetic_expression_1, relop, rational_arithmetic_expression_2) ->
		eval_rational_relop
			relop
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_1)
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_2)
			
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (rational_arithmetic_expression_1, rational_arithmetic_expression_2, rational_arithmetic_expression_3) ->
		(* Compute the first one to avoid redundancy *)
		let expr1_evaluated = eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_1 in
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_2)
			<=
			expr1_evaluated
			&&
			expr1_evaluated
			<= 
			(eval_rational_arithmetic_expression rational_valuation rational_arithmetic_expression_3)
