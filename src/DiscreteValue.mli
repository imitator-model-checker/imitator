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
 * Last modified     : 2021/03/01
 *
 ************************************************************)

open Constants

(************************************************************)
(** Types  *)
(************************************************************)

(* Specific type of number *)
type var_type_discrete_number =
    | Var_type_discrete_rational
    | Var_type_discrete_int
    | Var_type_discrete_unknown_number

(* Specific type of discrete variables *)
type var_type_discrete =
    | Var_type_discrete_bool
    | Var_type_discrete_number of var_type_discrete_number
    | Var_type_discrete_binary_word of int

(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

(* Shortcuts to types *)
val var_type_rational : var_type
val var_type_int : var_type
val var_type_unknown_number : var_type
val var_type_bool : var_type

(* Discrete value of different specific types *)
type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t

(************************************************************)
(** Type functions  *)
(************************************************************)

(** String of types  **)

(* String of var type *)
val string_of_var_type : var_type -> string
(* String of discrete var type *)
val string_of_var_type_discrete : var_type_discrete -> string
(* String of number var type *)
val string_of_var_type_discrete_number : var_type_discrete_number -> string

(** Check types **)

val is_discrete_type : var_type -> bool
(* Check if a Var_type is a Var_type_number *)
val is_number_type : var_type -> bool
val is_discrete_type_number_type : var_type_discrete -> bool

(* Check if discrete type is a Var_type_unknown_number *)
val is_discrete_type_unknown_number_type : var_type_discrete -> bool
(* Check if discrete type is not a Var_type_unknown_number *)
val is_discrete_type_known_number_type : var_type_discrete -> bool
(* Check if discrete type is a Var_type_rational *)
val is_rational_type : var_type -> bool

(* Check if discrete type is a Var_type_discrete_rational *)
val is_discrete_type_rational_type : var_type_discrete -> bool
(* Check if discrete type is a Var_type_discrete_int *)
val is_discrete_type_int_type : var_type_discrete -> bool
(* Check if discrete type is a Var_type_discrete_bool *)
val is_discrete_type_bool_type : var_type_discrete -> bool
(* Check if discrete type is a Var_type_discrete_binary_word *)
val is_discrete_type_binary_word_type : var_type_discrete -> bool

(* Check if two discrete types are compatible *)
val is_discrete_type_compatibles : var_type_discrete -> var_type_discrete -> bool
(* Check if a value is compatible with given type *)
val check_value_compatible_with_type : discrete_value -> var_type -> bool

(** Values and types  **)

(* Get var type of a discrete value *)
val var_type_of_value : discrete_value -> var_type
(* Get discrete var type of a discrete value *)
val discrete_type_of_value : discrete_value -> var_type_discrete
(* Get discrete type of a var type *)
(* Note : clocks / parameter are of rational type *)
val discrete_type_of_var_type : var_type -> var_type_discrete










(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
val customized_string_of_value : customized_boolean_string -> discrete_value -> string
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

(* Get default NumConst.t value *)
val numconst_default_value : NumConst.t
(* Get default Int32.t value *)
val int_default_value : Int32.t
(* Get default bool value *)
val bool_default_value : bool
(* Get default binary word value *)
val binary_word_default_value : int -> BinaryWord.t
(* Get default discrete value *)
val default_value : var_type -> discrete_value

(* Get zero value of Rational_value *)
val rational_zero : discrete_value
(* Get false value of Bool_value *)
val bool_value_false : discrete_value
(* Get true value of Bool_value *)
val bool_value_true : discrete_value

(* Get a zero discrete value according to given discrete value type *)
val zero_of : discrete_value -> discrete_value
(* Get a one discrete value according to given discrete value type *)
val one_of : discrete_value -> discrete_value

(** Get / Convert values  **)

(* Get NumConst.t value of discrete value *)
val numconst_value : discrete_value -> NumConst.t
(* Get Int32.t value of discrete value *)
val int_value : discrete_value -> Int32.t
(* Get bool value of discrete value *)
val bool_value : discrete_value -> bool
(* Get binary word value of discrete value *)
val binary_word_value : discrete_value -> BinaryWord.t

(* Convert any discrete value to NumConst.t value, if possible *)
val to_numconst_value : discrete_value -> NumConst.t
(* Convert any discrete value to Int32 value, if possible *)
val to_int_value : discrete_value -> Int32.t
(* Convert any discrete value to float value, if possible *)
val to_float_value : discrete_value -> float


(* Get discrete value from NumConst.t *)
val of_numconst : NumConst.t -> discrete_value
(* Get discrete value from Int32.t *)
val of_int : Int32.t -> discrete_value
(* Get discrete value from bool *)
val of_bool : bool -> discrete_value


(*(* Convert discrete value to another var type *)*)
(*val convert_value : discrete_value -> var_type -> discrete_value*)
(* Convert discrete value to another discrete type *)
val convert_value_to_discrete_type : discrete_value -> var_type_discrete -> discrete_value

(* Convert any discrete value to a Rational_value *)
val convert_to_rational_value : discrete_value -> discrete_value

(* Hash code of discrete value *)
val hash : discrete_value -> int

(** Computing operation on values  **)

(* Check if a discrete value is equal to another discrete value *)
val equal : discrete_value -> discrete_value -> bool
(* Check if a discrete value is not equal to another discrete value *)
val neq : discrete_value -> discrete_value -> bool

(* Add two discrete value *)
val add : discrete_value -> discrete_value -> discrete_value
(* Subtract two discrete value *)
val sub : discrete_value -> discrete_value -> discrete_value
(* Multiply two discrete value *)
val mul : discrete_value -> discrete_value -> discrete_value
(* Divide two discrete value *)
val div : discrete_value -> discrete_value -> discrete_value
(* Negate a discrete value *)
val neg : discrete_value -> discrete_value

(* Logical and on two discrete value *)
val _and : discrete_value -> discrete_value -> discrete_value
(* Logical or on two discrete value *)
val _or : discrete_value -> discrete_value -> discrete_value
(* Logical not on a discrete value *)
val not : discrete_value -> discrete_value
(* Logical equality on two discrete value *)
val bool_equal : discrete_value -> discrete_value -> discrete_value
(* Logical equality inverse on two discrete value *)
val bool_neq : discrete_value -> discrete_value -> discrete_value

(* Comparison, less between two discrete value *)
val l : discrete_value -> discrete_value -> discrete_value
(* Comparison, less or equal between two discrete value *)
val leq : discrete_value -> discrete_value -> discrete_value
(* Comparison, greater between two discrete value *)
val g : discrete_value -> discrete_value -> discrete_value
(* Comparison, greater or equal between two discrete value *)
val geq : discrete_value -> discrete_value -> discrete_value

val shift_left : int -> discrete_value -> discrete_value
val shift_right : int -> discrete_value -> discrete_value
val fill_left : int -> discrete_value -> discrete_value
val fill_right : int -> discrete_value -> discrete_value
val log_and : discrete_value -> discrete_value -> discrete_value
val log_or : discrete_value -> discrete_value -> discrete_value
val log_xor : discrete_value -> discrete_value -> discrete_value
val log_not : discrete_value -> discrete_value