(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Abstract tiles manager class to manage the tiles received in the cartography algorithms.
 * 
 * File contributors : Étienne André
 * Created           : 2016/08/15
 * Last modified     : 2019/08/08
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
open Result




(************************************************************)
(************************************************************)
(* Class-independent functions *)
(************************************************************)
(************************************************************)

(*------------------------------------------------------------*)
(** 'good_or_bad_constraint_union_assign good_or_bad_constraint1 good_or_bad_constraint2' performs the union of two good_or_bad_constraint, by assigning the result to the first and leaving the second unchanged *)
(*------------------------------------------------------------*)
let good_or_bad_constraint_union_assign (good_or_bad_constraint1 : good_or_bad_constraint) (good_or_bad_constraint2 : good_or_bad_constraint) =
	match (good_or_bad_constraint1, good_or_bad_constraint2) with

	| Good_bad_constraint good_and_bad_constraint1, Good_constraint good_constraint2 ->
		(* Get good part *)
		let constraint1, _ = good_and_bad_constraint1.good in
		let constraint2, _ = good_constraint2 in
		(* Add second good part to the first *)
		LinearConstraint.p_nnconvex_union_assign constraint1 constraint2
	
	| Good_bad_constraint good_and_bad_constraint1, Bad_constraint bad_constraint2 ->
		(* Get bad part *)
		let constraint1, _ = good_and_bad_constraint1.bad in
		let constraint2, _ = bad_constraint2 in
		(* Add second bad part to the first *)
		LinearConstraint.p_nnconvex_union_assign constraint1 constraint2
	
	| Good_bad_constraint good_and_bad_constraint1, Good_bad_constraint good_and_bad_constraint2 ->
		(* Add second good part to the first *)
		let gconstraint1, _ = good_and_bad_constraint1.good in
		let gconstraint2, _ = good_and_bad_constraint2.good in
		LinearConstraint.p_nnconvex_union_assign gconstraint1 gconstraint2;
		(* Add second bad part to the first *)
		let bconstraint1, _ = good_and_bad_constraint1.bad in
		let bconstraint2, _ = good_and_bad_constraint2.bad in
		LinearConstraint.p_nnconvex_union_assign bconstraint1 bconstraint2
	
	| _ -> raise (InternalError("Case not supported in good_or_bad_constraint_union_assign"))


(*------------------------------------------------------------*)
(** Check if a good_or_bad_constraint is sound *)
(*------------------------------------------------------------*)
let is_good_or_bad_constraint_sound (good_or_bad_constraint : good_or_bad_constraint) =
	match good_or_bad_constraint with
	| Good_constraint (_, Constraint_exact)
	| Good_constraint (_, Constraint_maybe_under)
	| Bad_constraint (_, Constraint_exact)
	| Bad_constraint (_, Constraint_maybe_under)
	-> true
	| Good_constraint (_, Constraint_maybe_over)
	| Good_constraint (_, Constraint_maybe_invalid)
	| Bad_constraint (_, Constraint_maybe_over)
	| Bad_constraint (_, Constraint_maybe_invalid)
	-> false
	| Good_bad_constraint good_and_bad_constraint ->
		let _, good_soundness = good_and_bad_constraint.good in
		let _, bad_soundness = good_and_bad_constraint.bad in
		let is_sound =
		match (good_soundness, bad_soundness) with
		| Constraint_exact, Constraint_exact
		| Constraint_exact, Constraint_maybe_under
		| Constraint_maybe_under, Constraint_exact
		| Constraint_maybe_under, Constraint_maybe_under
		-> true
		| _ -> false
		in
		is_sound


(*------------------------------------------------------------*)
(** Check if a good_or_bad_constraint is empty, i.e., contains no valuation *)
(*------------------------------------------------------------*)
let is_good_or_bad_constraint_empty (good_or_bad_constraint : good_or_bad_constraint) =
	match good_or_bad_constraint with
	(* Only good valuations *)
	| Good_constraint (p_nnconvex_constraint, _)
	(* Only bad valuations *)
	| Bad_constraint (p_nnconvex_constraint, _)
		-> LinearConstraint.p_nnconvex_constraint_is_false p_nnconvex_constraint
		
	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint ->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		LinearConstraint.p_nnconvex_constraint_is_false good_p_nnconvex_constraint
		&&
		LinearConstraint.p_nnconvex_constraint_is_false bad_p_nnconvex_constraint


(*------------------------------------------------------------*)
(** Check if a parameter valuation belongs to a good_or_bad_constraint *)
(*------------------------------------------------------------*)
let pval_in_good_or_bad_constraint pval (good_or_bad_constraint : good_or_bad_constraint) =
	match good_or_bad_constraint with
(*	| LinearConstraint.Convex_p_constraint p_linear_constraint -> LinearConstraint.is_pi0_compatible pval#get_value p_linear_constraint
	| LinearConstraint.Nonconvex_p_constraint p_nnconvex_constraint -> LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value p_nnconvex_constraint*)
	
	(*** NOTE: we do not investigate soundness; i.e., even if the tile is invalid, we still check whether the valuation belongs to the tile ***)
	
	(* Only good valuations *)
	| Good_constraint (p_nnconvex_constraint, _)
	(* Only bad valuations *)
	| Bad_constraint (p_nnconvex_constraint, _)
		-> LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value p_nnconvex_constraint
		
	(* Both good and bad valuations *)
	| Good_bad_constraint good_and_bad_constraint ->
		let good_p_nnconvex_constraint, _ = good_and_bad_constraint.good in
		let bad_p_nnconvex_constraint, _ = good_and_bad_constraint.bad in
		LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value good_p_nnconvex_constraint
		||
		LinearConstraint.p_nnconvex_constraint_is_pi0_compatible pval#get_value bad_p_nnconvex_constraint



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class virtual tilesManager =
	object (self)
	
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Flag to remember whether a valuation was skipped *)
	val mutable point_skipped = false
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Initialize the manager *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual initialize : unit
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the number of results processed and stored *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual get_nb_results : int
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check if a parameter valuation belongs to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual pval_in_tiles : PVal.pval -> bool

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Remembers the fact that at least one point was skipped, i.e., the resulting constraint is not valid *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method set_point_skipped : unit =
		point_skipped <- true
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process a new tile, i.e., add it to the tiles *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_tile : Result.abstract_point_based_result -> unit

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Process the result of the cartography *)
	(* If forced_coverage_option <> None, then the coverage is set to this argument; otherwise, it is computed as exepected (*** NOTE: used in Random to force the coverage to Unknown ***) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual process_result : float -> NumConst.t -> int -> Result.bc_algorithm_termination -> Result.bc_coverage option -> Result.imitator_result


(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
