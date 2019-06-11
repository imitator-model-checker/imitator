(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: "EF min" algorithm: minimization of a parameter valuation for which there exists a run leading to some states
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2018/08/16
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFopt


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFmin :
	object inherit algoEFopt
		(************************************************************)
		(* Class variables *)
		(************************************************************)

		method algorithm_name : string

		(*------------------------------------------------------------*)
		(* Instantiating min/max *)
		(*------------------------------------------------------------*)
		(* Function to remove upper bounds (if minimum) or lower bounds (if maximum) *)
		method remove_bounds : Automaton.parameter_index list -> Automaton.parameter_index list -> LinearConstraint.p_linear_constraint -> unit
		
		(* The closed operator (>= for minimization, and <= for maximization) *)
		method closed_op : LinearConstraint.op

		(* Function to negate an inequality *)
		method negate_inequality : LinearConstraint.p_linear_constraint -> LinearConstraint.p_linear_constraint

		
		(* Various strings *)
		method str_optimum : string
		method str_upper_lower : string

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)


end