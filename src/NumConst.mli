(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: unbounded exact rational computation using GMP
 * 
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2010/03/04
 * Last modified     : 2021/03/09
 *
 ************************************************************)
 

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
(* From num and den *)
val numconst_of_zfrac : Gmp.Z.t -> Gmp.Z.t -> t
val numconst_of_mpq : Gmp.Q.t -> t
val numconst_of_mpz : Gmp.Z.t -> t

val mpq_of_numconst : t -> Gmp.Q.t
val to_string : t -> string
val string_of_numconst : t -> string
val jani_string_of_numconst : t -> string

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

val min : t -> t -> t
val max : t -> t -> t

(** Find the closest multiple of step from base_number below (or equal to) number *)
val find_multiple_below : t -> t -> t -> t
(** Find the closest multiple of step from base_number above (or equal to) number *)
val find_multiple_above : t -> t -> t -> t

(** Generates a random integer NumConst in the interval [min , max] *)
val random_integer : t -> t -> t


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
(* Check if a NumConst is an integer *)
val is_integer : t -> bool

(* Check if a number is of type 'int', i.e., if it is an integer, and small enough to be represented as an int *)
val is_int : t -> bool


(**************************************************)
(** {2 Conversion Functions} *)
(**************************************************)
(* Converts to int; raise Failure in case of impossible cast *)
val to_int : t -> int

(** Convert a NumConst to a float thanks to the GMP.Q conversion *)
val to_float : t -> float
