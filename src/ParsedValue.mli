(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all types and functions for operations onto parsed discrete value
 *
 * File contributors : Benjamin L.
 * Created           : 2021/03/01
 *
 ************************************************************)

(* Utils modules *)
open Constants

(* Parsing structure modules *)
open DiscreteType


(* Discrete value of different specific types *)

type parsed_value =
    | Void_value
    | Weak_number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t
    | Array_value of parsed_value array
    | List_value of parsed_value list
    | Stack_value of parsed_value Stack.t
    | Queue_value of parsed_value Queue.t

(** Values and types  **)

(* Get discrete var type of a discrete value *)
val discrete_type_of_value : parsed_value -> var_type_discrete

(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
val customized_string_of_value : customized_string -> parsed_value -> string
val string_of_value : parsed_value -> string

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
