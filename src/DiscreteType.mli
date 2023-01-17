(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all discrete types
 *
 * File contributors : Benjamin L., Étienne André
 * Created           : 2021/11/08
 *
 ************************************************************)

(************************************************************)
(** Types  *)
(************************************************************)

(* Specific type of number *)
type var_type_discrete_number =
    | Dt_rat
    | Dt_int
    | Dt_weak_number

(* Specific type of discrete variables *)
type var_type_discrete =
    | Dt_weak
    | Dt_void
    | Dt_bool
    | Dt_number of var_type_discrete_number
    | Dt_bin of int
    | Dt_array of var_type_discrete * int
    | Dt_list of var_type_discrete
    | Dt_stack of var_type_discrete
    | Dt_queue of var_type_discrete

(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

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

val string_of_var_type_constructor : var_type -> string

(** Check types **)

val is_discrete_type : var_type -> bool
(* Check if a Var_type is a Var_type_number *)
val is_discrete_type_number_type : var_type_discrete -> bool

(* Check if discrete type is a Var_type_unknown_number *)
val is_discrete_type_unknown_number_type : var_type_discrete -> bool
(* Check if discrete type is not a Var_type_unknown_number *)
val is_discrete_type_known_number_type : var_type_discrete -> bool

val is_discrete_type_holding_number_type : var_type_discrete -> bool
val is_discrete_type_holding_unknown_number_type : var_type_discrete -> bool
val is_discrete_type_holding_known_number_type : var_type_discrete -> bool

val extract_inner_type : var_type_discrete -> var_type_discrete

(* Check if discrete type is a rational *)
val is_discrete_type_rational_type : var_type_discrete -> bool
(* Check if discrete type is a int *)
val is_discrete_type_int_type : var_type_discrete -> bool
(* Check if discrete type is a bool *)
val is_discrete_type_bool_type : var_type_discrete -> bool
(* Check if discrete type is a binary word *)
val is_discrete_type_binary_word_type : var_type_discrete -> bool
(* Extract discrete type of var type, note that: clocks and parameter are rational *)
val discrete_type_of_var_type : var_type -> var_type_discrete

(* Check if two discrete number types are compatible *)
val is_discrete_type_number_compatibles : var_type_discrete_number -> var_type_discrete_number -> bool
(* Check if two discrete types are compatible *)
val is_discrete_type_compatibles : var_type_discrete -> var_type_discrete -> bool

(* Get the stronger number type between two given number types *)
(* order: number < int = rat *)
val stronger_discrete_number_type_of : var_type_discrete_number -> var_type_discrete_number -> var_type_discrete_number
(* Get the stronger type between two given types, see stronger_discrete_number_type_of *)
val stronger_discrete_type_of : var_type_discrete -> var_type_discrete -> var_type_discrete