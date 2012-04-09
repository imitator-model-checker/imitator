(*****************************************************************
 *
 *                     HYMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2010/07/13
 *
 ****************************************************************)

(**************************************************)
(* Functions *)
(**************************************************)
open LinearConstraint
open AbstractModel


(** Plot polyhedron corresponding to a convex constraint, projected onto the first two variables *)
val plot_2d : variable -> variable -> linear_constraint -> string

(** Plot the cartography *)
val cartography : linear_constraint list ->  int list->  int -> string -> unit

(** Compute the coverage of the rectangle v0 by a list of zones (constraints) *)
val coverage : linear_constraint list -> float
