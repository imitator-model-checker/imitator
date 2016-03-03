(************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre, Ulrich Kuehne
 * Created:       2010/07/05
 * Last modified: 2016/03/03
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
val draw_cartography : (LinearConstraint.p_convex_or_nonconvex_constraint * StateSpace.statespace_nature) list ->  string -> unit

(** Execute the 'dot' with a source file name as argument *)
val dot : string -> string -> unit

(** 'draw_statespace state_space algorithm_name radical' draws the state space using dot *)
val draw_statespace : StateSpace.state_space -> string -> string -> unit
