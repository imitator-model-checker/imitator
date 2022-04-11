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
 *
 ************************************************************)
 
 
(*------------------------------------------------------------*)
(* Function for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
val prepare_extrapolation : unit -> unit


(*------------------------------------------------------------*)
(* Apply M extrapolation *)
(*------------------------------------------------------------*)
val px_m_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list


(*------------------------------------------------------------*)
(* Apply Mglobal extrapolation *)
(*------------------------------------------------------------*)
val px_mglobal_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list


(*------------------------------------------------------------*)
(* Apply LU extrapolation *)
(*------------------------------------------------------------*)
val px_lu_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list


(*------------------------------------------------------------*)
(* Apply LU extrapolation *)
(*------------------------------------------------------------*)
val px_luglobal_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list

