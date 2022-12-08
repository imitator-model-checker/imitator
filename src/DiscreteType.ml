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

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter


(************************************************************)
(** Type functions  *)
(************************************************************)

(** String of types  **)

(* String of number var type *)
let string_of_var_type_discrete_number = function
    | Dt_rat -> "rational"
    | Dt_int -> "int"
    | Dt_weak_number -> "number"

(* String of discrete var type *)
let rec string_of_var_type_discrete = function
    | Dt_weak -> "weak"
    | Dt_void -> "void"
    | Dt_number x -> string_of_var_type_discrete_number x
    | Dt_bool -> "bool"
    | Dt_bin l -> "binary(" ^ string_of_int l ^ ")"
    | Dt_array (inner_type, length) -> string_of_var_type_discrete inner_type ^ " array(" ^ string_of_int length ^ ")"
    | Dt_list inner_type -> string_of_var_type_discrete inner_type ^ " list"
    | Dt_stack inner_type -> string_of_var_type_discrete inner_type ^ " stack"
    | Dt_queue inner_type -> string_of_var_type_discrete inner_type ^ " queue"

(* String of var type *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete var_type_discrete -> string_of_var_type_discrete var_type_discrete
	| Var_type_parameter -> "parameter"

let string_of_var_type_constructor = function
    | Var_type_clock
    | Var_type_parameter as var_type -> string_of_var_type var_type
    | Var_type_discrete _ -> "variable"

(** Check types **)

(* Check if a Var_type is a Var_type_discrete of anything *)
let is_discrete_type = function
    | Var_type_discrete _ -> true
    | _ -> false

let is_discrete_type_number_type = function
   | Dt_number _ -> true
   | _ -> false

(* Check if discrete type is a Var_type_unknown_number *)
let is_discrete_type_unknown_number_type = function
    | Dt_number Dt_weak_number -> true
    | _ -> false

(* Check if discrete type is not a Var_type_unknown_number *)
let is_discrete_type_known_number_type = function
    | Dt_number Dt_weak_number -> false
    | Dt_number _ -> true
    | _ -> false

(* Check if discrete type is, or holding a inner type that is unknown number type *)
let rec is_discrete_type_holding_unknown_number_type = function
    | Dt_number Dt_weak_number -> true
    | Dt_array (inner_type, _)
    | Dt_list inner_type
    | Dt_stack inner_type
    | Dt_queue inner_type -> is_discrete_type_holding_unknown_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_known_number_type = function
    | Dt_number Dt_weak_number -> false
    | Dt_number _ -> true
    | Dt_array (inner_type, _)
    | Dt_list inner_type
    | Dt_stack inner_type
    | Dt_queue inner_type -> is_discrete_type_holding_known_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_number_type = function
    | Dt_number _ -> true
    | Dt_array (inner_type, _)
    | Dt_list inner_type
    | Dt_stack inner_type
    | Dt_queue inner_type -> is_discrete_type_holding_number_type inner_type
    | _ -> false

let rec extract_inner_type = function
    | Dt_array (inner_type, _)
    | Dt_list inner_type
    | Dt_stack inner_type
    | Dt_queue inner_type -> extract_inner_type inner_type
    | _ as discrete_type -> discrete_type

(* Get default discrete type of any type that is, or holding a inner type that is unknown number type *)
(* For example : 1 is unknown number, it will be a rational, [1,2] is an array of unknown number, it will be *)
(* an array of rational *)
let rec default_type_of_type_holding_unknown_number_type = function
    | Dt_number Dt_weak_number -> Dt_number Dt_rat
    | Dt_array (inner_type, length) -> Dt_array (default_type_of_type_holding_unknown_number_type inner_type, length)
    | Dt_list inner_type -> Dt_list (default_type_of_type_holding_unknown_number_type inner_type)
    | Dt_stack inner_type -> Dt_stack (default_type_of_type_holding_unknown_number_type inner_type)
    | Dt_queue inner_type -> Dt_queue (default_type_of_type_holding_unknown_number_type inner_type)
    | _ as discrete_type -> discrete_type

(* Check if discrete type is a rational *)
let is_discrete_type_rational_type = function
    | Dt_number Dt_rat -> true
    | _ -> false

(* Check if discrete type is a int *)
let is_discrete_type_int_type = function
    | Dt_number Dt_int -> true
    | _ -> false

(* Check if discrete type is a bool *)
let is_discrete_type_bool_type = function
    | Dt_bool -> true
    | _ -> false

(* Check if discrete type is a binary word *)
let is_discrete_type_binary_word_type = function
    | Dt_bin _ -> true
    | _ -> false

let discrete_type_of_var_type = function
    | Var_type_clock
    | Var_type_parameter -> Dt_number Dt_rat
    | Var_type_discrete x -> x

(* Check if two discrete number types are compatible *)
let is_discrete_type_number_compatibles type_number_a type_number_b =
    match type_number_a, type_number_b with
    | Dt_rat, Dt_int
    | Dt_int, Dt_rat -> false
    | _ -> true

(* Check if two discrete types are compatible *)
let rec is_discrete_type_compatibles var_type expr_type =
    match var_type, expr_type with
    (* Any var type with weak type is ok *)
    | _, Dt_weak
    | Dt_weak, _ -> true
    (* any number type with literal number *)
    | Dt_number _, Dt_number Dt_weak_number
    | Dt_number Dt_weak_number, Dt_number _ -> true
    (* Two array of same type are compatibles *)
    | Dt_array (l_inner_type, l_length), Dt_array (r_inner_type, r_length) when l_length = r_length ->
        is_discrete_type_compatibles l_inner_type r_inner_type
    | Dt_list l_inner_type, Dt_list r_inner_type
    | Dt_stack l_inner_type, Dt_stack r_inner_type
    | Dt_queue l_inner_type, Dt_queue r_inner_type ->
        is_discrete_type_compatibles l_inner_type r_inner_type
    (* any equals types *)
    | ta, tb when ta = tb -> true
    (* other are not compatibles *)
    | _, _ -> false

(* Get the stronger number type between two given number types *)
(* order: number < int = rat *)
let stronger_discrete_number_type_of discrete_number_type_a discrete_number_type_b =
    match discrete_number_type_a, discrete_number_type_b with
    | Dt_weak_number, Dt_int
    | Dt_weak_number, Dt_rat -> discrete_number_type_b
    | _ -> discrete_number_type_a

(* Get the stronger type between two given types, see stronger_discrete_number_type_of *)
let rec stronger_discrete_type_of discrete_type_a discrete_type_b =
    match discrete_type_a, discrete_type_b with
    | _, Dt_weak -> discrete_type_a
    | Dt_weak, _ -> discrete_type_b
    | Dt_number discrete_number_type_a, Dt_number discrete_number_type_b ->
        Dt_number (stronger_discrete_number_type_of discrete_number_type_a discrete_number_type_b)
    | Dt_array (inner_type_a, length), Dt_array (inner_type_b, _) ->
        Dt_array ((stronger_discrete_type_of inner_type_a inner_type_b), length)
    | Dt_list inner_type_a, Dt_list inner_type_b ->
        Dt_list (stronger_discrete_type_of inner_type_a inner_type_b)
    | Dt_stack inner_type_a, Dt_stack inner_type_b ->
        Dt_stack (stronger_discrete_type_of inner_type_a inner_type_b)
    | Dt_queue inner_type_a, Dt_queue inner_type_b ->
        Dt_queue (stronger_discrete_type_of inner_type_a inner_type_b)
    | _ ->
        discrete_type_a


let rec extract_number_of_discrete_type = function
    | Dt_number discrete_number_type -> Some discrete_number_type
    | Dt_array (inner_type, _)
    | Dt_list inner_type
    | Dt_stack inner_type
    | Dt_queue inner_type -> extract_number_of_discrete_type inner_type
    | _ -> None

let extract_number_of_type = function
    | Var_type_discrete discrete_type -> extract_number_of_discrete_type discrete_type
    | _ -> None