(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2016/01/28
 *
 ************************************************************)

(************************************************************)
(* Modules *)
(************************************************************)

open Global



(************************************************************)
(* Constants *)
(************************************************************)
val dot_colors : string list


(************************************************************)
(* Functions *)
(************************************************************)

(** Draw the cartography corresponding to a list of constraints. Takes as second argument the file name prefix. *)
val draw_cartography : (LinearConstraint.p_convex_or_nonconvex_constraint * StateSpace.tile_nature) list ->  string -> unit

(** Execute the 'dot' with a source file name as argument *)
val dot : string -> string -> unit

(** Draw the state space using dot *)
val draw_statespace : StateSpace.state_space -> string -> unit
