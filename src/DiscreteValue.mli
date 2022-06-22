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

type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t
    (* TODO benjamin REFACTOR maybe Collection_value *)
    | Array_value of discrete_value array
    | List_value of discrete_value list
    | Stack_value of discrete_value Stack.t
    | Queue_value of discrete_value Queue.t

(** Values and types  **)

(* Get var type of a discrete value *)
val var_type_of_value : discrete_value -> var_type
(* Get discrete var type of a discrete value *)
val discrete_type_of_value : discrete_value -> var_type_discrete
(* Check if a value is compatible with given type *)
val check_value_compatible_with_type : discrete_value -> var_type -> bool


(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
val customized_string_of_value : customized_string -> discrete_value -> string
val string_of_value : discrete_value -> string

(** Check value type  **)

(* Check whether the value is rational *)
val is_rational_value : discrete_value -> bool
(* Check whether the value is int *)
val is_int_value : discrete_value -> bool
(* Check whether the value is boolean *)
val is_bool_value : discrete_value -> bool
(* Check whether the value is binary word *)
val is_binary_word_value : discrete_value -> bool

(** Default values  **)

(* Get default discrete value of discrete type *)
val default_discrete_value : var_type_discrete -> discrete_value
(* Get default discrete value of var type *)
val default_value : var_type -> discrete_value

(* Get zero value of Rational_value *)
val rational_zero : discrete_value
(* Get false value of Bool_value *)
val bool_value_false : discrete_value
(* Get true value of Bool_value *)
val bool_value_true : discrete_value

(** Get / Convert values  **)

(* Get NumConst.t value of discrete value *)
val numconst_value : discrete_value -> NumConst.t
(* Get Int32.t value of discrete value *)
val int_value : discrete_value -> Int32.t
(* Get bool value of discrete value *)
val bool_value : discrete_value -> bool
(* Get binary word value of discrete value *)
val binary_word_value : discrete_value -> BinaryWord.t
(* Get array value of discrete value *)
val array_value : discrete_value -> discrete_value array
(* Get list value of discrete value *)
val list_value : discrete_value -> discrete_value list
(* Get stack value of discrete value *)
val stack_value : discrete_value -> discrete_value Stack.t
(* Get queue value of discrete value *)
val queue_value : discrete_value -> discrete_value Queue.t

val is_zero : discrete_value -> bool

(* Convert any discrete value to NumConst.t value, if possible *)
val to_numconst_value : discrete_value -> NumConst.t
(* Convert any discrete value to Int32 value, if possible *)
val to_int_value : discrete_value -> Int32.t
(* Convert any discrete value to float value, if possible *)
val to_float_value : discrete_value -> float

val convert_to_numconst : discrete_value -> NumConst.t

(* Get discrete value from NumConst.t *)
val of_numconst : NumConst.t -> discrete_value
(* Get discrete value from Int32.t *)
val of_int : Int32.t -> discrete_value
(* Get discrete value from bool *)
val of_bool : bool -> discrete_value


(*(* Convert discrete value to another var type *)*)
(*val convert_value : discrete_value -> var_type -> discrete_value*)
(* Convert discrete value to another discrete type *)
(*val convert_value_to_discrete_type : discrete_value -> var_type_discrete -> discrete_value*)

(* Convert any discrete value to a Rational_value *)
val convert_to_rational_value : discrete_value -> discrete_value

(* Hash code of discrete value *)
val hash : discrete_value -> int

(** Computing operation on values  **)

(* Check if a discrete value is equal to another discrete value *)
val equal : discrete_value -> discrete_value -> bool
(* Check if a discrete value is not equal to another discrete value *)
val neq : discrete_value -> discrete_value -> bool
