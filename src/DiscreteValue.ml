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
 * File contributors : Benjamin L., Étienne André
 * Created           : 2021/03/01
 * Last modified     : 2021/09/23
 *
 ************************************************************)

open Constants
open Exceptions

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

(* Discrete value of different specific types *)
type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t
    | Array_value of discrete_value array

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
    | Var_type_discrete_array (discrete_type, length) -> string_of_var_type_discrete discrete_type ^ " array(" ^ string_of_int length ^ ")"

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
    | Var_type_discrete_array (inner_type, _) -> is_discrete_type_holding_unknown_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_known_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> false
    | Var_type_discrete_number _ -> true
    | Var_type_discrete_array (inner_type, _) -> is_discrete_type_holding_known_number_type inner_type
    | _ -> false

let rec is_discrete_type_holding_number_type = function
    | Var_type_discrete_number _ -> true
    | Var_type_discrete_array (inner_type, _) -> is_discrete_type_holding_number_type inner_type
    | _ -> false

let rec extract_inner_type = function
    | Var_type_discrete_array (inner_type, _) -> extract_inner_type inner_type
    | _ as discrete_type -> discrete_type

(* Get default discrete type of any type that is, or holding a inner type that is unknown number type *)
(* For example : 1 is unknown number, it will be a rational, [1,2] is an array of unknown number, it will be *)
(* an array of rational *)
let rec default_type_of_type_holding_unknown_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete_array (inner_type, length) -> Var_type_discrete_array (default_type_of_type_holding_unknown_number_type inner_type, length)
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


(* Get discrete var type of a discrete value *)
let rec discrete_type_of_value = function
    | Number_value _ -> Var_type_discrete_number Var_type_discrete_unknown_number
    | Rational_value _ -> Var_type_discrete_number Var_type_discrete_rational
    | Int_value _ -> Var_type_discrete_number Var_type_discrete_int
    | Bool_value _ -> Var_type_discrete_bool
    | Binary_word_value b -> Var_type_discrete_binary_word (BinaryWord.length b)
    | Array_value a ->
        if Array.length a = 0 then
            Var_type_discrete_array (Var_type_discrete_number Var_type_discrete_rational, 0)
        else
            Var_type_discrete_array (discrete_type_of_value (Array.get a 0), Array.length a)

(* Get var type of a discrete value *)
let rec var_type_of_value = function
    | Number_value _ -> var_type_unknown_number
    | Rational_value _ -> var_type_rational
    | Int_value _ -> var_type_int
    | Bool_value _ -> var_type_bool
    | Binary_word_value b -> var_type_binary_word (BinaryWord.length b)
    | Array_value a -> Var_type_discrete (Var_type_discrete_array (discrete_type_of_value (Array.get a 0), Array.length a))


let discrete_type_of_var_type = function
    | Var_type_clock
    | Var_type_parameter -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete x -> x

(* Check if two discrete types are compatible *)
let rec is_discrete_type_compatibles var_type expr_type =
    match var_type, expr_type with
    (* any number type with literal number *)
    | Var_type_discrete_number _, Var_type_discrete_number Var_type_discrete_unknown_number
    | Var_type_discrete_number Var_type_discrete_unknown_number, Var_type_discrete_number _ -> true
    (* Two array of same type are compatibles *)
    | Var_type_discrete_array (l_discrete_type, l_length), Var_type_discrete_array (r_discrete_type, r_length) when l_length = r_length ->
        is_discrete_type_compatibles l_discrete_type r_discrete_type
    (* any equals types *)
    | ta, tb when ta = tb -> true
    (* other are not compatibles *)
    | _, _ -> false

(* Check if a value is compatible with given type *)
let check_value_compatible_with_type value var_type =
    (* Get discrete type of value *)
    let value_type = discrete_type_of_value value in
    (* Get discrete type of var type *)
    let var_type_discrete = discrete_type_of_var_type var_type in
    (* Check compatibility of discrete types *)
    is_discrete_type_compatibles var_type_discrete value_type

(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
let rec customized_string_of_value customized_boolean_string = function
    | Number_value x
    | Rational_value x -> NumConst.string_of_numconst x
    | Bool_value x -> if x then customized_boolean_string.true_string else customized_boolean_string.false_string
    | Int_value x -> Int32.to_string x
    | Binary_word_value b -> BinaryWord.string_of_binaryword b
    | Array_value a ->
        let string_array = Array.map (fun x -> customized_string_of_value customized_boolean_string x) a in
        "[" ^ OCamlUtilities.string_of_array_of_string_with_sep ", " string_array ^ "]"

let string_of_value = customized_string_of_value default_string

(** Check value type  **)

(* Check whether the value is rational *)
let is_rational_value = function
    | Rational_value _ -> true
    | _ -> false

(* Check whether the value is int *)
let is_int_value = function
    | Int_value _ -> true
    | _ -> false

(* Check whether the value is boolean *)
let is_bool_value = function
    | Bool_value _ -> true
    | _ -> false

(* Check whether the value is binary word *)
let is_binary_word_value = function
    | Binary_word_value _ -> true
    | _ -> false

(** Default values  **)

(* Get default NumConst.t value *)
let numconst_default_value = NumConst.zero
(* Get default Int32.t value *)
let int_default_value = Int32.zero
(* Get default bool value *)
let bool_default_value = false
(* Get default binary word value *)
let binary_word_default_value l = BinaryWord.zero l

(* Get default discrete number value *)
let default_discrete_number_value = function
    | Var_type_discrete_unknown_number
    | Var_type_discrete_rational -> Rational_value numconst_default_value
    | Var_type_discrete_int -> Int_value int_default_value

(* Get default discrete value *)
let rec default_discrete_value = function
    | Var_type_discrete_number x -> default_discrete_number_value x
    | Var_type_discrete_bool -> Bool_value bool_default_value
    | Var_type_discrete_binary_word l -> Binary_word_value (BinaryWord.zero l)
    | Var_type_discrete_array (discrete_type, length) -> Array_value (Array.make length (default_discrete_value discrete_type))

(* Get default discrete value *)
let default_value = function
    | Var_type_clock -> Rational_value numconst_default_value
    | Var_type_parameter -> Rational_value numconst_default_value
    | Var_type_discrete var_type_discrete -> default_discrete_value var_type_discrete

(* Get zero value of Rational_value *)
let rational_zero = Rational_value NumConst.zero
(* Get false value of Bool_value *)
let bool_value_false = Bool_value false
(* Get true value of Bool_value *)
let bool_value_true = Bool_value true

(* Get a zero discrete value according to given discrete value type *)
let zero_of = function
    | Rational_value _ -> Rational_value NumConst.zero
    | Int_value _ -> Int_value Int32.zero
    | _ as value -> raise (InternalError ("Computing exception at `zero_of " ^ string_of_value value ^ "`"))

(* Get a one discrete value according to given discrete value type *)
let one_of = function
    | Rational_value _ -> Rational_value NumConst.one
    | Int_value _ -> Int_value Int32.one
    | _ as value -> raise (InternalError ("Computing exception at `one_of " ^ string_of_value value ^ "`"))

(** Convert values  **)

(* Get NumConst.t value of rational discrete value *)
let numconst_value = function
    | Rational_value x -> x
    | value -> raise (InternalError ("Unable to get rational value of non-rational discrete value: " ^ string_of_value value))


(* Get Int32.t value of int32 discrete value *)
let int_value = function
    | Int_value x -> x
    | _ as value -> raise (InternalError ("Unable to get int value of non-int discrete value: " ^ string_of_value value ^ ":" ^ string_of_var_type (var_type_of_value value)))

(* Get bool value of bool discrete value *)
let bool_value = function
    | Bool_value x -> x
    | _ as t -> raise (InternalError ("Unable to get bool value of non-bool discrete value" ^ string_of_value t))

(* Get array value of discrete value *)
let array_value = function
    | Array_value x -> x
    | _ -> raise (InternalError "Unable to get array value of non-array discrete value")

(* Convert any discrete value to NumConst.t value, if possible *)
let to_numconst_value = function
    | Number_value x
    | Rational_value x -> x
    | Bool_value x -> if x then NumConst.one else NumConst.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> NumConst.numconst_of_int (Int32.to_int x)
    | Binary_word_value x -> NumConst.numconst_of_int (BinaryWord.hash x)
    | Array_value _ -> raise (InternalError "Unable to convert array to NumConst.t value")

(* Convert any discrete value to Int32 value, if possible *)
let to_int_value = function
    (* Warning !!!! conversion to int should be dependant of the platform ! *)
    | Number_value x
    | Rational_value x -> Int32.of_int (NumConst.to_int x)
    | Bool_value x -> if x then Int32.one else Int32.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> x
    | Binary_word_value x -> Int32.of_int (BinaryWord.hash x)
    | Array_value _ -> raise (InternalError "Unable to convert array to Int32.t value")

(* Convert any discrete value to float value, if possible *)
let to_float_value = function
    | Number_value x
    | Rational_value x -> (NumConst.to_float x)
    | Bool_value x -> if x then 0.0 else 0.0
    | Int_value x -> Int32.to_float x
    | Binary_word_value x -> float_of_int (BinaryWord.hash x)
    | Array_value _ -> raise (InternalError "Unable to convert array to float value")

(* Get binary word value of discrete value *)
let binary_word_value = function
    | Binary_word_value x -> x
    | _ as value -> raise (InternalError ("Unable to get binary word value of non binary word `" ^ string_of_value value ^ "`"))


(* Get discrete value from NumConst.t *)
let of_numconst x = Rational_value x
(* Get discrete value from Int32.t *)
let of_int x = Int_value x
(* Get discrete value from bool *)
let of_bool x = Bool_value x

(* Convert any discrete value to a Rational_value *)
let convert_to_rational_value value =
    Rational_value (to_numconst_value value)

(* Convert discrete value to another discrete type *)
(* Use for implicit conversion *)
let convert_value_to_discrete_type value target_type =
    match value, target_type with
    (* Source and target type are identical *)
    | Rational_value _, Var_type_discrete_number Var_type_discrete_rational
    | Int_value _, Var_type_discrete_number Var_type_discrete_int
    | Number_value _, Var_type_discrete_number Var_type_discrete_unknown_number -> value
    (* Number_value to Rational_value *)
    | Number_value _, Var_type_discrete_number Var_type_discrete_rational
    (* Int_value to Rational_value *)
    | Int_value _, Var_type_discrete_number Var_type_discrete_rational ->
        Rational_value (to_numconst_value value)
    (* Number_value to Int_value *)
    | Number_value _, Var_type_discrete_number Var_type_discrete_int
    (* Rational_value to Int_value *)
    | Rational_value _, Var_type_discrete_number Var_type_discrete_int ->
        Int_value (to_int_value value)
    (* Other are not supported *)
    | x, t -> failwith (
        "Implicit conversion of value "
        ^ (string_of_value x)
        ^ " to "
        ^ (string_of_var_type_discrete t)
        ^ " type is not supported"
    )

(* Hash code of discrete value *)
let rec hash = function
    | Number_value x
    | Rational_value x -> Gmp.Z.to_int (NumConst.get_num x)
    | Bool_value x -> if x then 1 else 0
    | Int_value x -> Int32.to_int x
    | Binary_word_value b -> BinaryWord.hash b
    (* Arbitrary *)
    | Array_value a -> Array.fold_left (fun acc x -> acc + (hash x)) 0 a

(** Dynamic computing operations on values  **)

(* Check if a discrete value is equal to another discrete value *)
let equal a b =
    match a, b with
    | Number_value a, Number_value b
    | Rational_value a, Rational_value b -> NumConst.equal a b
    | Bool_value a, Bool_value b -> a = b
    | Int_value a, Int_value b -> Int32.equal a b
    | Binary_word_value a, Binary_word_value b -> BinaryWord.equal a b
    | Array_value a, Array_value b -> a = b
    | lt, rt -> raise (
        InternalError ("Computing exception on `"
            ^ string_of_var_type_discrete (discrete_type_of_value lt)
            ^ " = "
            ^ string_of_var_type_discrete (discrete_type_of_value rt)
            ^ "`"
        )
    )

(* Check if a discrete value is not equal to another discrete value *)
let neq a b =
    not (equal a b)

(* Add two discrete value *)
let add a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Rational_value (NumConst.add a b)
        | Int_value a, Int_value b -> Int_value (Int32.add a b)
        | _ -> raise (InternalError ("Computing exception at `add " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Subtract two discrete value *)
let sub a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Rational_value (NumConst.sub a b)
        | Int_value a, Int_value b -> Int_value (Int32.sub a b)
        | _ -> raise (InternalError ("Computing exception at `sub " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Multiply two discrete value *)
let mul a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Rational_value (NumConst.mul a b)
        | Int_value a, Int_value b -> Int_value (Int32.mul a b)
        | _ -> raise (InternalError ("Computing exception at `mul " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Divide two discrete value *)
let div a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Rational_value (NumConst.div a b)
        | Int_value a, Int_value  b -> Int_value  (Int32.div a b)
        | _ -> raise (InternalError ("Computing exception at `div " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Negate a discrete value *)
let neg = function
    | Number_value x
    | Rational_value x -> Rational_value (NumConst.neg x)
    | Int_value x -> Int_value (Int32.neg x)
    | _ as value -> raise (InternalError ("Computing exception at `neg " ^ string_of_value value ^ "`"))

(* Logical and on two discrete value *)
let _and a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a && b)
        | _ -> raise (InternalError ("Computing exception at `and " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Logical or on two discrete value *)
let _or a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a || b)
        | _ -> raise (InternalError ("Computing exception at `or " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Logical not on a discrete value *)
let not = function
    | Bool_value a -> Bool_value (not (a))
    | _ as value -> raise (InternalError ("Computing exception at `not " ^ string_of_value value ^ "`"))

(* Logical equality on two discrete value *)
let bool_equal a b = Bool_value (equal a b)

(* Logical equality inverse on two discrete value *)
let bool_neq a b = Bool_value (neq a b)

(* Comparison, less between two discrete value *)
let l a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Bool_value (NumConst.l a b)
        | Int_value a, Int_value b -> Bool_value (a < b)
        | Array_value a, Array_value b -> Bool_value (a < b)
        | _ -> raise (InternalError ("Computing exception at `l " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Comparison, less or equal between two discrete value *)
let leq a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Bool_value (NumConst.le a b)
        | Int_value a, Int_value  b -> Bool_value (a < b)
        | Array_value a, Array_value b -> Bool_value  (a <= b)
        | _ -> raise (InternalError ("Computing exception at `leq " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Comparison, greater between two discrete value *)
let g a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Bool_value (NumConst.g a b)
        | Int_value a, Int_value  b -> Bool_value (a < b)
        | Array_value a, Array_value b -> Bool_value  (a > b)
        | _ -> raise (InternalError ("Computing exception at `g " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

(* Comparison, greater or equal between two discrete value *)
let geq a b =
    match a, b with
        | Number_value a, Number_value b
        | Rational_value a, Rational_value b -> Bool_value (NumConst.ge a b)
        | Int_value a, Int_value  b -> Bool_value (a < b)
        | Array_value a, Array_value b -> Bool_value  (a >= b)
        | _ -> raise (InternalError ("Computing exception at `geq " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

let access i = function
    | Array_value a -> a.(i)
    | _ as value -> raise (InternalError ("Computing exception at `access " ^ string_of_value value ^ "`"))

let shift_left i = function
    | Binary_word_value x -> Binary_word_value (BinaryWord.shift_left x i)
    | _ as value -> raise (InternalError ("Computing exception at `shift_left " ^ string_of_value value ^ "`"))

let shift_right i = function
    | Binary_word_value x -> Binary_word_value (BinaryWord.shift_right x i)
    | _ as value -> raise (InternalError ("Computing exception at `shift_right " ^ string_of_value value ^ "`"))

let fill_left i = function
    | Binary_word_value x -> Binary_word_value (BinaryWord.fill_left x i)
    | _ as value -> raise (InternalError ("Computing exception at `fill_left " ^ string_of_value value ^ "`"))

let fill_right i = function
    | Binary_word_value x -> Binary_word_value (BinaryWord.fill_right x i)
    | _ as value -> raise (InternalError ("Computing exception at `fill_right " ^ string_of_value value ^ "`"))

let log_and a b =
    match a, b with
    | Binary_word_value b1, Binary_word_value b2 -> Binary_word_value (BinaryWord.log_and b1 b2)
    | _ -> raise (InternalError ("Computing exception at `log_and " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

let log_or a b =
    match a, b with
    | Binary_word_value b1, Binary_word_value b2 -> Binary_word_value (BinaryWord.log_or b1 b2)
    | _ -> raise (InternalError ("Computing exception at `log_or " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

let log_xor a b =
    match a, b with
    | Binary_word_value b1, Binary_word_value b2 -> Binary_word_value (BinaryWord.log_xor b1 b2)
    | _ -> raise (InternalError ("Computing exception at `log_xor " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))

let log_not = function
    | Binary_word_value b -> Binary_word_value (BinaryWord.log_not b)
    | _ as value -> raise (InternalError ("Computing exception at `log_not " ^ string_of_value value ^ "`"))

let array_concat a b =
    match a, b with
    | Array_value a, Array_value b -> Array_value (Array.append a b)
    | _ -> raise (InternalError ("Computing exception at `array_concat " ^ string_of_value a ^ " " ^ string_of_value b ^ "`"))