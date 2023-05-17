(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13 & CNRS (France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: All graphics handling (cartography, trace set…)
 * 
 * File contributors : Étienne André, Ulrich Kühne
 * Created           : 2010/07/05
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

(** Draw (using the plotutils graph utility) the cartography corresponding to a list of constraints. Takes as second argument the file name prefix. *)
val draw_cartography : (LinearConstraint.p_convex_or_nonconvex_constraint * StateSpace.statespace_nature) list ->  string -> unit


(** Draw (using the plotutils graph utility) the evolution of clock and discrete variables valuations according to time. *)
val draw_concrete_run : StateSpace.concrete_run -> string -> unit
val draw_impossible_concrete_run : StateSpace.impossible_concrete_run -> string -> unit


(** Execute the `dot` utility with as argument the image format, the radical, and the source file. Returns `Some file_name` if successful, or None otherwise *)
val dot : string -> string -> string -> (string option)


(** `draw_statespace state_space algorithm_name radical` draws the state space using dot, if required by the options. *)
val draw_statespace_if_requested : StateSpace.stateSpace -> string -> string -> unit
