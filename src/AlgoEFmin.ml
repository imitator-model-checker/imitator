(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: "EF min" algorithm: minimization of a parameter valuation for which there exists a run leading to some states [ABPP19]
 * 
 * File contributors : Étienne André
 * Created           : 2017/05/02
 * Last modified     : 2020/04/16
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoEFopt
open Statistics



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFmin (state_predicate : AbstractProperty.state_predicate) (parameter_index : Automaton.parameter_index) =
	object (self) inherit algoEFopt state_predicate parameter_index as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	
	(*------------------------------------------------------------*)
	(* Instantiating min/max *)
	(*------------------------------------------------------------*)
	(* Function to remove upper bounds (if minimum) or lower bounds (if maximum) *)
	method remove_bounds = LinearConstraint.p_grow_to_infinity_assign
	
	(* The closed operator (>= for minimization, and <= for maximization) *)
	method closed_op = LinearConstraint.Op_ge
	
	(* Function to negate an inequality *)
	method negate_inequality = LinearConstraint.negate_single_inequality_p_constraint
	
	
	
	(* Various strings *)
	method str_optimum = "minimum"
	method str_upper_lower = "upper"
	
	


	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFmin"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)



	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
