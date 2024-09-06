(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: unbounded exact rational computation using GMP
 * 
 * File contributors : Étienne André, Dylan Marinho
 * Created           : 2010/03/04
 *
 ************************************************************)
 
(**************************************************)
(**************************************************)
(* GMP multi-precision integers *)
(**************************************************)
(**************************************************)

(**************************************************)
(* Type definition *)
(**************************************************)
type gmpz = Gmp.Z.t

(**************************************************)
(** {2 Constants} *)
(**************************************************)

(* 0, 1, -1 *)
val gmpz_zero      : gmpz
val gmpz_one       : gmpz
val gmpz_minus_one : gmpz

(**************************************************)
(** {2 Conversion functions} *)
(**************************************************)

(* Convert an integer to a Gmp.Z *)
val gmpz_of_int : int -> gmpz

(* Convert a Gmp.Z to a string *)
val string_of_gmpz : gmpz -> string

(**************************************************)
(** {2 Arithmetic functions} *)
(**************************************************)

(* Negation *)
val gmpz_neg : gmpz -> gmpz

(* Compute ceiling division *)
val gmpz_cdiv : gmpz -> gmpz -> gmpz

(* Compute floor division *)
val gmpz_fdiv : gmpz -> gmpz -> gmpz

(* Absolute *)
val gmpz_abs : gmpz -> gmpz


(**************************************************)
(** {2 Comparison functions} *)
(**************************************************)

(* Equal *)
val gmpz_equal : gmpz -> gmpz -> bool

(* Not equal *)
val gmpz_neq : gmpz -> gmpz -> bool

(**************************************************)
(**************************************************)
(* GMP multi-precision rationals *)
(**************************************************)
(**************************************************)

(**************************************************)
(* Type definition *)
(**************************************************)

type t = Gmp.Q.t


(**************************************************)
(* Functions *)
(**************************************************)

(**************************************************)
(* {2 Constants} *)
(**************************************************)

val zero : t
val one : t
val minus_one : t


(**************************************************)
(* {2 User Conversions} *)
(**************************************************)

val numconst_of_string : string -> t
val numconst_of_int : int -> t
val numconst_of_float : float -> t
val numconst_of_frac : int -> int -> t
(* From num and den *)
val numconst_of_zfrac : gmpz -> gmpz -> t
val numconst_of_mpq : Gmp.Q.t -> t
val numconst_of_mpz : gmpz -> t

val mpq_of_numconst : t -> Gmp.Q.t
val to_string : t -> string
val string_of_numconst : t -> string
val jani_string_of_numconst : t -> string

val get_num : t -> gmpz
val get_den : t -> gmpz

(**************************************************)
(* {2 Arithmetic Functions} *)
(**************************************************)

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val pow : t -> Int32.t -> t
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
(* {2 Comparison Functions} *)
(**************************************************)
val equal : t -> t -> bool
val neq : t -> t -> bool
val l : t -> t -> bool
val le : t -> t -> bool
val ge : t -> t -> bool
val g : t -> t -> bool

(**************************************************)
(* {2 Test Functions} *)
(**************************************************)
(** Check if a NumConst is an integer *)
val is_integer : t -> bool

(** Check if a number is of type 'int', i.e., if it is an integer, and small enough to be represented as an int *)
val is_int : t -> bool


(**************************************************)
(* {2 Conversion Functions} *)
(**************************************************)

(** Converts to int; raise Failure in case of impossible cast *)
val to_int : t -> int

val to_bounded_int : t -> int

(** Convert a NumConst to a float thanks to the GMP.Q conversion *)
val to_float : t -> float
