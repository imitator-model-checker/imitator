(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2010/03/04
 * Last modified: 2013/01/30
 *
 ****************************************************************)
 
(**************************************************)
(* Type definition *)
(**************************************************)

type t


(**************************************************)
(* Functions *)
(**************************************************)

(**************************************************)
(** {2 Constants} *)
(**************************************************)

val zero : t
val one : t
val minus_one : t


(**************************************************)
(** {2 User Conversions} *)
(**************************************************)

val numconst_of_string : string -> t
val numconst_of_int : int -> t
val numconst_of_float : float -> t
val numconst_of_frac : int -> int -> t
val numconst_of_zfrac : Gmp.Z.t -> Gmp.Z.t -> t
val numconst_of_mpq : Gmp.Q.t -> t
val numconst_of_mpz : Gmp.Z.t -> t

val mpq_of_numconst : t -> Gmp.Q.t
val string_of_numconst : t -> string

val get_num : t -> Gmp.Z.t
val get_den : t -> Gmp.Z.t

(**************************************************)
(** {2 Arithmetic Functions} *)
(**************************************************)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val neg : t -> t
val abs : t -> t

(**************************************************)
(** {2 Comparison Functions} *)
(**************************************************)
val equal : t -> t -> bool
val neq : t -> t -> bool
val l : t -> t -> bool
val le : t -> t -> bool
val ge : t -> t -> bool
val g : t -> t -> bool

(**************************************************)
(** {2 Test Functions} *)
(**************************************************)
val is_integer : t -> bool
