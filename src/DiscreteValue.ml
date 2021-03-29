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

(* Should never happen, if correctly checked before in ModelConverter *)
exception ComputingException of string

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

(* Discrete value of different specific types *)
type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Int_value of Int32.t
    | Bool_value of bool

(************************************************************)
(** Type functions  *)
(************************************************************)

(** String of types  **)

(* String of number var type *)
let string_of_var_type_discrete_number = function
    | Var_type_discrete_rational -> "discrete"
    | Var_type_discrete_int -> "int"
    | Var_type_discrete_unknown_number -> "number"

(* String of discrete var type *)
let string_of_var_type_discrete = function
    | Var_type_discrete_number x -> string_of_var_type_discrete_number x
    | Var_type_discrete_bool -> "bool"

(* String of var type *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete var_type_discrete -> string_of_var_type_discrete var_type_discrete
	| Var_type_parameter -> "parameter"

(** Check types **)

(* Check if two discrete types are compatible *)
let is_discrete_type_compatibles type_a type_b =
    match type_a, type_b with
    | Var_type_discrete_number _, Var_type_discrete_bool
    | Var_type_discrete_bool, Var_type_discrete_number _ -> false
    | _, _ -> true

(* Check if two types are compatible *)
(* To be compatible, two type should have the same kind (number, boolean, ...) *)
(* TODO benjamin refactor with use of is_discrete_type_compatibles function *)
let is_type_compatibles type_a type_b =
    match type_a, type_b with
    | a, b when a = b -> true

    | Var_type_clock, Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational), Var_type_clock
    | Var_type_clock, Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_clock

    | Var_type_parameter, Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational), Var_type_parameter
    | Var_type_parameter, Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_parameter

    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational), Var_type_discrete (Var_type_discrete_number Var_type_discrete_int)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_int), Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_discrete (Var_type_discrete_number _)
    | Var_type_discrete (Var_type_discrete_number _), Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number)
    -> true
    | _ -> false

(* Check if a Var_type is a Var_type_discrete of anything *)
let is_discrete_type = function
    | Var_type_discrete _ -> true
    | _ -> false

(* Check if a Var_type is a Var_type_number *)
let is_number_type = function
   | Var_type_clock
   | Var_type_parameter
   | Var_type_discrete Var_type_discrete_number _ -> true
   | _ -> false

let is_discrete_type_number_type = function
   | Var_type_discrete_number _ -> true
   | _ -> false

(* Check if discrete type is a Var_type_unknown_number *)
let is_discrete_type_unknown_number_type = function
    | Var_type_discrete_number Var_type_discrete_unknown_number -> true
    | _ -> false

(* Check if discrete type is a Var_type_rational *)
let is_rational_type = function
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational) -> true
    | _ -> false

(* Check if discrete type is a Var_type_bool *)
let is_discrete_type_bool_type = function
    | Var_type_discrete_bool -> true
    | _ -> false

(* Get var type of a discrete value *)
let var_type_of_value = function
    | Number_value _ -> var_type_unknown_number
    | Rational_value _ -> var_type_rational
    | Int_value _ -> var_type_int
    | Bool_value _ -> var_type_bool

(* Get discrete var type of a discrete value *)
let var_type_discrete_of_value = function
    | Number_value _ -> Var_type_discrete_number Var_type_discrete_unknown_number
    | Rational_value _ -> Var_type_discrete_number Var_type_discrete_rational
    | Int_value _ -> Var_type_discrete_number Var_type_discrete_int
    | Bool_value _ -> Var_type_discrete_bool

let discrete_type_of_var_type = function
    | Var_type_clock
    | Var_type_parameter -> Var_type_discrete_number Var_type_discrete_rational
    | Var_type_discrete x -> x

(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
let customized_string_of_value customized_boolean_string = function
    | Number_value x
    | Rational_value x -> NumConst.string_of_numconst x
    | Bool_value x -> if x then customized_boolean_string.true_string else customized_boolean_string.false_string
    | Int_value x -> Int32.to_string x

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

(** Default values  **)

(* Get default NumConst.t value *)
let numconst_default_value = NumConst.zero
(* Get default Int32.t value *)
let int_default_value = Int32.zero
(* Get default bool value *)
let bool_default_value = false

(* Get default discrete number value *)
let default_discrete_number_value = function
    | Var_type_discrete_unknown_number
    | Var_type_discrete_rational -> Rational_value numconst_default_value
    | Var_type_discrete_int -> Int_value int_default_value

(* Get default discrete value *)
let default_discrete_value = function
    | Var_type_discrete_number x -> default_discrete_number_value x
    | Var_type_discrete_bool -> Bool_value bool_default_value

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
    | _ -> raise (ComputingException "zero_of")

(* Get a one discrete value according to given discrete value type *)
let one_of = function
    | Rational_value _ -> Rational_value NumConst.one
    | Int_value _ -> Int_value Int32.one
    | _ -> raise (ComputingException "one_of")

(** Convert values  **)

(* Get NumConst.t value of discrete value *)
let numconst_value = function
    | Number_value x
    | Rational_value x -> x
    | Bool_value x -> if x then NumConst.one else NumConst.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> NumConst.numconst_of_int (Int32.to_int x)

(* Get Int32.t value of discrete value *)
let int_value = function
    (* Warning !!!! conversion to int should be dependant of the platform ! *)
    | Number_value x
    | Rational_value x -> Int32.of_int (NumConst.to_int x)
    | Bool_value x -> if x then Int32.one else Int32.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> x

(* Get bool value of discrete value *)
let bool_value = function
    | Number_value x
    | Rational_value x -> if NumConst.equal x NumConst.one then true else false
    | Bool_value x -> x
    | Int_value x -> if Int32.equal x Int32.one then true else false

(* Get float value of discrete value *)
let float_value = function
    | Number_value x
    | Rational_value x -> (NumConst.to_float x)
    | Bool_value x -> if x then 0.0 else 0.0
    | Int_value x -> Int32.to_float x

(* Convert any discrete value to a Rational_value *)
let convert_to_rational_value value =
    Rational_value (numconst_value value)

(* Convert any discrete value to a Int_value *)
let convert_to_int_value value =
    Int_value (int_value value)

(* Convert discrete value to another discrete type *)
let convert_value_to_discrete_type value target_type =
    match value, target_type with
    (* Source and target type are identical *)
    | Rational_value _, Var_type_discrete_number Var_type_discrete_rational
    | Int_value _, Var_type_discrete_number Var_type_discrete_int
    | Bool_value _, Var_type_discrete_bool
    | Number_value _, Var_type_discrete_number Var_type_discrete_unknown_number -> value
    (* Number_value to Rational_value *)
    | Number_value _, Var_type_discrete_number Var_type_discrete_rational
    (* Int_value to Rational_value *)
    | Int_value _, Var_type_discrete_number Var_type_discrete_rational ->
        convert_to_rational_value value
    (* Number_value to Int_value *)
    | Number_value _, Var_type_discrete_number Var_type_discrete_int
    (* Rational_value to Int_value *)
    | Rational_value _, Var_type_discrete_number Var_type_discrete_int ->
        convert_to_int_value value
    (* Other are not supported *)
    | x, t -> failwith (
        "Conversion of value "
        ^ (string_of_value x)
        ^ " to "
        ^ (string_of_var_type_discrete t)
        ^ " type is not supported"
    )

(* Convert discrete value to another var type *)
let convert_value value target_type =
    let target_discrete_type = discrete_type_of_var_type target_type in
    convert_value_to_discrete_type value target_discrete_type


(* Hash code of discrete value *)
let hash = function
    | Number_value x
    | Rational_value x -> Gmp.Z.to_int (NumConst.get_num x)
    | Bool_value x -> if x then 1 else 0
    | Int_value x -> Int32.to_int x

(** Computing operation on values  **)

(* Check if a discrete value is equal to another discrete value *)
let equal a b =
    match a, b with
    | Rational_value a, Rational_value b -> NumConst.equal a b
    | Bool_value a, Bool_value b -> a = b
    | Int_value a, Int_value b -> Int32.equal a b
    | _ -> false

(* Check if a discrete value is not equal to another discrete value *)
let neq a b =
    not (equal a b)

(* Add two discrete value *)
let add a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.add a b)
        | Int_value a, Int_value b -> Int_value (Int32.add a b)
        | _ -> raise (ComputingException ("add : " ^ (string_of_var_type_discrete (var_type_discrete_of_value a)) ^ "," ^ (string_of_var_type_discrete (var_type_discrete_of_value b))))

(* Subtract two discrete value *)
let sub a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.sub a b)
        | Int_value a, Int_value b -> Int_value (Int32.sub a b)
        | x, y -> raise (ComputingException ("sub " ^ (string_of_var_type_discrete (var_type_discrete_of_value y)) ))

(* Multiply two discrete value *)
let mul a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.mul a b)
        | Int_value a, Int_value b -> Int_value (Int32.mul a b)
        | _ -> raise (ComputingException "mul")

(* Divide two discrete value *)
let div a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.div a b)
        | Int_value a, Int_value  b -> Int_value  (Int32.div a b)
        | _ -> raise (ComputingException "div")

(* Negate a discrete value *)
let neg = function
    | Rational_value x -> Rational_value (NumConst.neg x)
    | Int_value x -> Int_value (Int32.neg x)
    | _ -> raise (ComputingException "neg")

(* Logical and on two discrete value *)
let _and a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a && b)
        | _ -> raise (ComputingException "and")

(* Logical or on two discrete value *)
let _or a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a || b)
        | _ -> raise (ComputingException "or")

(* Logical not on a discrete value *)
let not = function
    | Bool_value a -> Bool_value (not (a))
    | _ -> raise (ComputingException "not")

(* Logical equality on two discrete value *)
let bool_equal a b = Bool_value (equal a b)

(* Logical equality inverse on two discrete value *)
let bool_neq a b = Bool_value (neq a b)

(* Comparison, less between two discrete value *)
let l a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.l a b)
        | Int_value a, Int_value  b -> Bool_value  (a < b)
        | _ -> raise (ComputingException "l")

(* Comparison, less or equal between two discrete value *)
let leq a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.le a b)
        | Int_value a, Int_value  b -> Bool_value  (a <= b)
        | _ -> raise (ComputingException "leq")

(* Comparison, greater between two discrete value *)
let g a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.g a b)
        | Int_value a, Int_value  b -> Bool_value  (a > b)
        | _ -> raise (ComputingException "g")

(* Comparison, greater or equal between two discrete value *)
let geq a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.ge a b)
        | Int_value a, Int_value  b -> Bool_value  (a >= b)
        | _ -> raise (ComputingException "geq")