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
 * Last modified     : 2021/11/08
 *
 ************************************************************)

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
    | Var_type_discrete_array of var_type_discrete * int
    | Var_type_discrete_list of var_type_discrete

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

(* Shortcuts to types *)
let var_type_rational = Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
let var_type_int = Var_type_discrete (Var_type_discrete_number Var_type_discrete_int)
let var_type_unknown_number = Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number)
let var_type_bool = Var_type_discrete Var_type_discrete_bool
let var_type_binary_word l = Var_type_discrete (Var_type_discrete_binary_word l)


(************************************************************)
(** Type functions  *)
(************************************************************)

(** String of types  **)

(* String of number var type *)
let string_of_var_type_discrete_number = function
    | Var_type_discrete_rational -> "rational"
    | Var_type_discrete_int -> "int"
    | Var_type_discrete_unknown_number -> "number"

(* String of discrete var type *)
let rec string_of_var_type_discrete = function
    | Var_type_discrete_number x -> string_of_var_type_discrete_number x
    | Var_type_discrete_bool -> "bool"
    | Var_type_discrete_binary_word l -> "binary(" ^ string_of_int l ^ ")"
    | Var_type_discrete_array (inner_type, length) -> string_of_var_type_discrete inner_type ^ " array(" ^ string_of_int length ^ ")"
    | Var_type_discrete_list inner_type -> string_of_var_type_discrete inner_type ^ " list"

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
   | Var_type_discrete_number _ -> true
   | _ -> false

(* Check if discrete type is a Var_type_unknown_number *)
let is_discrete_type_unknown_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> true
    | _ -> false

(* Check if discrete type is not a Var_type_unknown_number *)
let is_discrete_type_known_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> false
    | Var_type_discrete_number _ -> true
    | _ -> false

(* Check if discrete type is, or holding a inner type that is unknown number type *)
let rec is_discrete_type_holding_unknown_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> true
    | Var_type_discrete_array (inner_type, _)
    | Var_type_discrete_list inner_type -> is_discrete_type_holding_unknown_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_known_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> false
    | Var_type_discrete_number _ -> true
    | Var_type_discrete_array (inner_type, _)
    | Var_type_discrete_list inner_type -> is_discrete_type_holding_known_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_number_type = function
    | Var_type_discrete_number _ -> true
    | Var_type_discrete_array (inner_type, _)
    | Var_type_discrete_list inner_type -> is_discrete_type_holding_number_type inner_type
    | _ -> false

let rec extract_inner_type = function
    | Var_type_discrete_array (inner_type, _)
    | Var_type_discrete_list inner_type -> extract_inner_type inner_type
    | _ as discrete_type -> discrete_type

(* Get default discrete type of any type that is, or holding a inner type that is unknown number type *)
(* For example : 1 is unknown number, it will be a rational, [1,2] is an array of unknown number, it will be *)
(* an array of rational *)
let rec default_type_of_type_holding_unknown_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete_array (inner_type, length) -> Var_type_discrete_array (default_type_of_type_holding_unknown_number_type inner_type, length)
    | Var_type_discrete_list inner_type -> Var_type_discrete_list (default_type_of_type_holding_unknown_number_type inner_type)
    | _ as discrete_type -> discrete_type

(* Check if discrete type is a Var_type_discrete_rational *)
let is_discrete_type_rational_type = function
    | Var_type_discrete_number Var_type_discrete_rational -> true
    | _ -> false

(* Check if discrete type is a Var_type_discrete_int *)
let is_discrete_type_int_type = function
    | Var_type_discrete_number Var_type_discrete_int -> true
    | _ -> false

(* Check if discrete type is a Var_type_discrete_bool *)
let is_discrete_type_bool_type = function
    | Var_type_discrete_bool -> true
    | _ -> false

(* Check if discrete type is a Var_type_discrete_binary_word *)
let is_discrete_type_binary_word_type = function
    | Var_type_discrete_binary_word _ -> true
    | _ -> false

let discrete_type_of_var_type = function
    | Var_type_clock
    | Var_type_parameter -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete x -> x

(* Check if two discrete number types are compatible *)
let is_discrete_type_number_compatibles type_number_a type_number_b =
    match type_number_a, type_number_b with
    | Var_type_discrete_rational, Var_type_discrete_int
    | Var_type_discrete_int, Var_type_discrete_rational -> false
    | _ -> true

(* Check if two discrete types are compatible *)
let rec is_discrete_type_compatibles var_type expr_type =
    match var_type, expr_type with
    (* any number type with literal number *)
    | Var_type_discrete_number _, Var_type_discrete_number Var_type_discrete_unknown_number
    | Var_type_discrete_number Var_type_discrete_unknown_number, Var_type_discrete_number _ -> true
    (* Two array of same type are compatibles *)
    | Var_type_discrete_array (l_inner_type, l_length), Var_type_discrete_array (r_inner_type, r_length) when l_length = r_length ->
        is_discrete_type_compatibles l_inner_type r_inner_type
    | Var_type_discrete_list l_inner_type, Var_type_discrete_list r_inner_type ->
        is_discrete_type_compatibles l_inner_type r_inner_type
    (* any equals types *)
    | ta, tb when ta = tb -> true
    (* other are not compatibles *)
    | _, _ -> false

let greater_number_defined discrete_number_type_a discrete_number_type_b =
    match discrete_number_type_a, discrete_number_type_b with
    | Var_type_discrete_unknown_number, Var_type_discrete_int
    | Var_type_discrete_unknown_number, Var_type_discrete_rational -> discrete_number_type_b
    | _ -> discrete_number_type_a

let rec greater_defined discrete_type_a discrete_type_b =
    match discrete_type_a, discrete_type_b with
    | Var_type_discrete_number discrete_number_type_a, Var_type_discrete_number discrete_number_type_b ->
        Var_type_discrete_number (greater_number_defined discrete_number_type_a discrete_number_type_b)
    | Var_type_discrete_array (inner_type_a, length), Var_type_discrete_array (inner_type_b, _) ->
        Var_type_discrete_array ((greater_defined inner_type_a inner_type_b), length)
    | Var_type_discrete_list inner_type_a, Var_type_discrete_list inner_type_b ->
        Var_type_discrete_list (greater_defined inner_type_a inner_type_b)
    | _ ->
        discrete_type_a



let rec default_type_if_needed = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete_array (inner_type, length) -> Var_type_discrete_array (default_type_if_needed inner_type, length)
    | Var_type_discrete_list inner_type -> Var_type_discrete_list (default_type_if_needed inner_type)
    | discrete_type -> discrete_type