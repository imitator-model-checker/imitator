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
 
open LinearConstraint
 
(*------------------------------------------------------------*)
(* Function for preparing data structures for extrapolation *)
(*------------------------------------------------------------*)
val prepare_extrapolation : unit -> unit


(* M-extrapolation: returns (the constraint ^ x <= M) , (the constraint ^ x >= M) *)
val m_extrapolation_of_x : NumConst.t -> variable -> LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list


(* LU-extrapolation: returns (the constraint ^ x <= smaller bound) , (the constraint ^ x > smaller bound ^ x <= greater bound) , (the constraint ^ x > greater bound) *)
val lu_extrapolation_of_x : NumConst.t -> NumConst.t -> variable -> LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list

(*------------------------------------------------------------*)
(* Apply M extrapolation *)
(*------------------------------------------------------------*)
val px_m_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list


(*------------------------------------------------------------*)
(* Apply LU extrapolation *)
(*------------------------------------------------------------*)
val px_lu_extrapolation : LinearConstraint.px_linear_constraint -> LinearConstraint.px_linear_constraint list

