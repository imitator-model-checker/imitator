(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module hold Binary Word type for IMITATOR
 *
 * File contributors : Benjamin L.
 * Created           : 2020/??/??
 *
 ************************************************************)

(* Binary word type *)
type t = int * bool array

(* Get a zero binary word of length l *)
val zero : int -> t
(* Get the length of the binary word *)
val length : t -> int

(* --- Strings --- *)

(* Convert string value to binary word type *)
val binaryword_of_string : string -> t
(* Convert binary word to string *)
val string_of_binaryword : t -> string
(* Convert binary word to string (alias of string_of_binaryword) *)
val to_string : t -> string

(* --- Operations --- *)

val shift_left : t -> int -> t
val shift_right : t -> int -> t
val fill_left : t -> int -> t
val fill_right : t -> int -> t
val log_not : t -> t
val log_and : t -> t -> t
val log_or : t -> t -> t
val log_xor : t -> t -> t

(* --- Comparisons --- *)

(* Hash of binary word *)
val hash : t -> int

val l : t -> t -> bool
val leq : t -> t -> bool
val g : t -> t -> bool
val geq : t -> t -> bool
val equal : t -> t -> bool
val neq : t -> t -> bool

val (=) : t -> t -> bool
val (<>) : t -> t -> bool
val (<) : t -> t -> bool
val (<=) : t -> t -> bool
val (>) : t -> t -> bool
val (>=) : t -> t -> bool

(* --- Conversions --- *)

(* Convert a binary word to array of bool *)
val to_array : t -> bool array
(* Convert a binary word to int *)
val to_int : t -> int
