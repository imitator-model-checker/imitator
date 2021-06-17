(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: clocks extrapolation
 *
 * File contributors : Étienne André, Johan Arcile
 * Created           : 2021/06/17
 * Last modified     : 2021/06/17
 *
 ************************************************************)
 

 
(*------------------------------------------------------------*)
(* Function for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
val prepare_extrapolation : unit -> unit

(*------------------------------------------------------------*)
(* Apply extrapolation *)
(*------------------------------------------------------------*)
val px_lu_extrapolation : Automaton.clock_index -> LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list

