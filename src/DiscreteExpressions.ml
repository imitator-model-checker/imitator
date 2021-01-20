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
 * Last modified     : 2020/01/07
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
type discrete_valuation = Automaton.discrete_index -> Automaton.discrete_value


(****************************************************************)
(** Arithmetic expressions for discrete variables *)
(****************************************************************)
type discrete_arithmetic_expression =
	| DAE_plus of discrete_arithmetic_expression * discrete_term
	| DAE_minus of discrete_arithmetic_expression * discrete_term
	| DAE_term of discrete_term

and discrete_term =
	| DT_mul of discrete_term * discrete_factor
	| DT_div of discrete_term * discrete_factor
	| DT_factor of discrete_factor

and discrete_factor =
	| DF_variable of Automaton.variable_index
	| DF_constant of Automaton.variable_value
	| DF_expression of discrete_arithmetic_expression
	| DF_unary_min of discrete_factor


(****************************************************************)
(** Boolean expressions for discrete variables *)
(****************************************************************)

type discrete_boolean_expression =
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression of discrete_arithmetic_expression * relop * discrete_arithmetic_expression
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in of discrete_arithmetic_expression * discrete_arithmetic_expression * discrete_arithmetic_expression


(************************************************************)
(** Evaluate arithmetic expressions with a valuation *)
(************************************************************)

let rec eval_discrete_factor discrete_valuation = function
	| DF_variable variable_index ->
		discrete_valuation variable_index
		
	| DF_constant variable_value ->
		variable_value
		
	| DF_expression discrete_arithmetic_expression ->
		eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression
	
	| DF_unary_min discrete_factor ->
		NumConst.neg (eval_discrete_factor discrete_valuation discrete_factor)

and eval_discrete_term discrete_valuation = function
	| DT_mul (discrete_term, discrete_factor) ->
		NumConst.mul
		(eval_discrete_term discrete_valuation discrete_term)
		(eval_discrete_factor discrete_valuation discrete_factor)
		
	| DT_div (discrete_term, discrete_factor) ->
			(*** TODO: division by zero ***)
		NumConst.div
		(eval_discrete_term discrete_valuation discrete_term)
		(eval_discrete_factor discrete_valuation discrete_factor)
		
	| DT_factor discrete_factor ->
		eval_discrete_factor discrete_valuation discrete_factor

and eval_discrete_arithmetic_expression discrete_valuation = function
	| DAE_plus (discrete_arithmetic_expression, discrete_term) ->
		NumConst.add
		(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression)
		(eval_discrete_term discrete_valuation discrete_term)
		
	| DAE_minus (discrete_arithmetic_expression, discrete_term) ->
		NumConst.sub
		(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression)
		(eval_discrete_term discrete_valuation discrete_term)
		
	| DAE_term discrete_term ->
		eval_discrete_term discrete_valuation discrete_term

	
	
	
(************************************************************)
(** Check whether a Boolean expression evaluates to true when valuated with a valuation *)
(************************************************************)

let eval_discrete_relop relop value_1 value_2 : bool =
	match relop with
	| OP_L		-> value_1 <  value_2
	| OP_LEQ	-> value_1 <= value_2
	| OP_EQ		-> value_1 =  value_2
	| OP_NEQ	-> value_1 <> value_2
	| OP_GEQ	-> value_1 >= value_2
	| OP_G		-> value_1 >  value_2

let check_discrete_boolean_expression discrete_valuation = function
	(** Discrete arithmetic expression of the form Expr ~ Expr *)
	| Expression (discrete_arithmetic_expression_1, relop, discrete_arithmetic_expression_2) ->
		eval_discrete_relop
			relop
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1)
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
			
	(** Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' *)
	| Expression_in (discrete_arithmetic_expression_1, discrete_arithmetic_expression_2, discrete_arithmetic_expression_3) ->
		(* Compute the first one to avoid redundancy *)
		let expr1_evaluated = eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_1 in
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_2)
			<=
			expr1_evaluated
			&&
			expr1_evaluated
			<= 
			(eval_discrete_arithmetic_expression discrete_valuation discrete_arithmetic_expression_3)


