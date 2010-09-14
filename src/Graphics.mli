(*****************************************************************
 *
 *                     IMITATOR
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
open AbstractImitatorFile


(** Plot polyhedron corresponding to a convex constraint, projected onto the first two variables *)
val plot_2d : variable -> variable -> linear_constraint -> string

(** Plot the cartography *)
val cartography : abstract_program ->  pi0cube -> linear_constraint list ->  int -> string -> unit

(** Compute the coverage of the rectangle v0 by a list of zones (constraints) *)
val coverage : abstract_program -> pi0cube -> linear_constraint list -> float
