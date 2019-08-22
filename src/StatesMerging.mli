(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Merging states the union of which is convex [AFS13, ATVA]
 * 
 * File contributors : Étienne André
 * Created           : 2015/11/27
 * Last modified     : 2015/11/27
 *
 ************************************************************)

(*------------------------------------------------------------*)
(* Merge states in a ref list (action_index list, location, constraint) *)
(* Return nothing as the list ref is already modified *)
(*------------------------------------------------------------*)
val merge : (Automaton.action_index list * Location.global_location * LinearConstraint.px_linear_constraint) list ref -> unit
