(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: All graphics handling (cartography, trace set…)
 * 
 * File contributors : Étienne André, Ulrich Kühne
 * Created           : 2010/07/05
 * Last modified     : 2017/06/27
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

(** Execute the 'dot' utility with as argument the image format, the radical, and the source file *)
val dot : string -> string -> string -> unit

(** 'draw_statespace state_space algorithm_name radical' draws the state space using dot *)
val draw_statespace : StateSpace.state_space -> string -> string -> unit
