(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: "EF max" algorithm: maximization of a parameter valuation for which there exists a run leading to some states
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2017/05/02
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoEFopt


(************************************************************)
(* Class definition *)
(************************************************************)
class algoEFmax :
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
		
		(* Function to negate an inequality *)
		method negate_inequality : LinearConstraint.p_linear_constraint -> LinearConstraint.p_linear_constraint

		(* Various strings *)
		method str_optimum : string
		method str_upper_lower : string

		
		(************************************************************)
		(* Class methods *)
		(************************************************************)


end