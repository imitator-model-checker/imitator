(************************************************************
 *                       IMITATOR
 * Hull heuristic policy for PTG convex-hull abstraction.
 ************************************************************)

(** Opaque policy object holding per-location state. *)
type t

val create : unit -> t

(** Apply the hull policy for one step at the given location.
    The hull method and all thresholds are read from [options].
    Returns the new representative constraint. *)
val apply :
  t ->
  options:Options.imitator_options ->
  location_index:int ->
  representative:LinearConstraint.px_linear_constraint ->
  incoming:LinearConstraint.px_linear_constraint ->
  LinearConstraint.px_linear_constraint
