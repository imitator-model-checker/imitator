(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all types and functions for operations onto discrete value
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/01
 *
 ************************************************************)

open Constants
open DiscreteType


(* Discrete value of different specific types *)

type parsed_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t
    | Array_value of parsed_value array
    | List_value of parsed_value list
    | Stack_value of parsed_value Stack.t
    | Queue_value of parsed_value Queue.t

(** Values and types  **)

(* Get var type of a discrete value *)
val var_type_of_value : parsed_value -> var_type
(* Get discrete var type of a discrete value *)
val discrete_type_of_value : parsed_value -> var_type_discrete
(* Check if a value is compatible with given type *)
val check_value_compatible_with_type : parsed_value -> var_type -> bool


(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
val customized_string_of_value : customized_string -> parsed_value -> string
val string_of_value : parsed_value -> string

(** Check value type  **)

(* Check whether the value is rational *)
val is_rational_value : parsed_value -> bool
(* Check whether the value is int *)
val is_int_value : parsed_value -> bool
(* Check whether the value is boolean *)
val is_bool_value : parsed_value -> bool
(* Check whether the value is binary word *)
val is_binary_word_value : parsed_value -> bool

(** Default values  **)

(* Get default discrete value of discrete type *)
val default_discrete_value : var_type_discrete -> parsed_value
(* Get default discrete value of var type *)
val default_value : var_type -> parsed_value

(* Get zero value of Rational_value *)
val rational_zero : parsed_value
(* Get false value of Bool_value *)
val bool_value_false : parsed_value
(* Get true value of Bool_value *)
val bool_value_true : parsed_value

(** Get / Convert values  **)

(* Get NumConst.t value of discrete value *)
val numconst_value : parsed_value -> NumConst.t
(* Get Int32.t value of discrete value *)
val int_value : parsed_value -> Int32.t
(* Get bool value of discrete value *)
val bool_value : parsed_value -> bool
(* Get binary word value of discrete value *)
val binary_word_value : parsed_value -> BinaryWord.t
(* Get array value of discrete value *)
val array_value : parsed_value -> parsed_value array
(* Get list value of discrete value *)
val list_value : parsed_value -> parsed_value list
(* Get stack value of discrete value *)
val stack_value : parsed_value -> parsed_value Stack.t
(* Get queue value of discrete value *)
val queue_value : parsed_value -> parsed_value Queue.t

val is_zero : parsed_value -> bool

(* Convert any discrete value to NumConst.t value, if possible *)
val to_numconst_value : parsed_value -> NumConst.t
(* Convert any discrete value to Int32 value, if possible *)
val to_int_value : parsed_value -> Int32.t
(* Convert any discrete value to float value, if possible *)
val to_float_value : parsed_value -> float

val convert_to_numconst : parsed_value -> NumConst.t

(* Get discrete value from NumConst.t *)
val of_numconst : NumConst.t -> parsed_value
(* Get discrete value from Int32.t *)
val of_int : Int32.t -> parsed_value
(* Get discrete value from bool *)
val of_bool : bool -> parsed_value


(*(* Convert discrete value to another var type *)*)
(*val convert_value : parsed_value -> var_type -> parsed_value*)
(* Convert discrete value to another discrete type *)
(*val convert_value_to_discrete_type : parsed_value -> var_type_discrete -> parsed_value*)

(* Convert any discrete value to a Rational_value *)
val convert_to_rational_value : parsed_value -> parsed_value

(* Hash code of discrete value *)
val hash : parsed_value -> int

(** Computing operation on values  **)

(* Check if a discrete value is equal to another discrete value *)
val equal : parsed_value -> parsed_value -> bool
(* Check if a discrete value is not equal to another discrete value *)
val neq : parsed_value -> parsed_value -> bool
