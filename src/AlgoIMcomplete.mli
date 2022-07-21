(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: IM algorithm with complete result, i.e., possibly non-convex and without random selection [AM15]
 * 
 * File contributors : Étienne André
 * Created           : 2017/03/21
 *
 ************************************************************)


(************************************************************)
(* Modules *)
(************************************************************)
open AlgoIMK


(************************************************************)
(* Class definition *)
(************************************************************)
class algoIMcomplete : PVal.pval ->
	object inherit algoIMK
		(************************************************************)
		(* Class variables *)
		(************************************************************)


		(************************************************************)
		(* Class methods *)
		(************************************************************)
		method algorithm_name : string
		
		method run : unit -> Result.imitator_result
		
		method initialize_variables : unit
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(* Checks a new state for pi0-compatibility .*)
		(* constr            : new state constraint            *)
		(*------------------------------------------------------------*)
		(* returns true if the state is pi0-compatible, and false otherwise *)
		(*------------------------------------------------------------*)
		(* side effect: add the negation of the p_constraint to all computed states *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		method check_pi0compatibility : LinearConstraint.px_linear_constraint -> bool

		method compute_result : Result.imitator_result
end