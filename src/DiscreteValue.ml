open Constants

exception ComputingException of string (* Should never happen, if correctly checked before in ModelConverter *)

type var_type_discrete_number =
    | Var_type_discrete_rational
    | Var_type_discrete_int
    | Var_type_discrete_unknown_number

type var_type_discrete =
    | Var_type_discrete_bool
    | Var_type_discrete_number of var_type_discrete_number

(** Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

let var_type_rational = Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
let var_type_int = Var_type_discrete (Var_type_discrete_number Var_type_discrete_int)
let var_type_unknown_number = Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number)
let var_type_bool = Var_type_discrete Var_type_discrete_bool

(* Discrete value of different specific types *)
type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Bool_value of bool
    | Int_value of Int32.t

let var_type_of_value = function
    | Rational_value _ -> var_type_rational
    | Bool_value _ -> var_type_bool
    | Int_value _ -> var_type_int
    | Number_value _ -> var_type_unknown_number



let discrete_type_of_var_type = function
    | Var_type_clock
    | Var_type_parameter -> var_type_rational
    | other_type -> other_type



(* Check if two types are compatible *)
(* To be compatible, two type should have the same kind (number, boolean, ...) *)
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



let string_of_var_type_discrete_number = function
    | Var_type_discrete_rational -> "discrete"
    | Var_type_discrete_int -> "int"
    | Var_type_discrete_unknown_number -> "number"

(* String of discrete var type *)
let string_of_var_type_discrete = function
    | Var_type_discrete_number x -> string_of_var_type_discrete_number x
    | Var_type_discrete_bool -> "bool"

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete var_type_discrete -> string_of_var_type_discrete var_type_discrete
	| Var_type_parameter -> "parameter"



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

let is_unknown_number_type = function
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number) -> true
    | _ -> false

(* Check if a Var_type is a Var_type_rational *)
let is_rational_type = function
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational) -> true
    | _ -> false

(* Check if a Var_type is a Var_type_bool *)
let is_bool_type = function
    | Var_type_discrete Var_type_discrete_bool -> true
    | _ -> false

let is_rational_value = function
    | Rational_value _ -> true
    | _ -> false

let is_int32_value = function
    | Int_value _ -> true
    | _ -> false

let is_bool_value = function
    | Bool_value _ -> true
    | _ -> false

let numconst_default_value = NumConst.zero
let int32_default_value = Int32.zero
let bool_default_value = false

let default_discrete_number_value = function
    | Var_type_discrete_rational -> Rational_value numconst_default_value
    | Var_type_discrete_int -> Int_value int32_default_value

let default_discrete_value = function
    | Var_type_discrete_number x -> default_discrete_number_value x
    | Var_type_discrete_bool -> Bool_value bool_default_value

let default_value = function
    | Var_type_clock -> Rational_value numconst_default_value
    | Var_type_parameter -> Rational_value numconst_default_value
    | Var_type_discrete var_type_discrete -> default_discrete_value var_type_discrete


let numconst_value = function
    | Number_value x
    | Rational_value x -> x
    | Bool_value x -> if x then NumConst.one else NumConst.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> NumConst.numconst_of_int (Int32.to_int x)

let int32_value = function
    (* Warning !!!! conversion to int should be dependant of the platform ! *)
    | Number_value x
    | Rational_value x -> Int32.of_int (NumConst.to_int x)
    | Bool_value x -> if x then Int32.one else Int32.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> x

let bool_value = function
    | Rational_value x -> if NumConst.equal x NumConst.one then true else false
    | Bool_value x -> x
    | Int_value x -> if Int32.equal x Int32.one then true else false

let float_value = function
    | Rational_value x -> (NumConst.to_float x)
    | Bool_value x -> if x then 0.0 else 0.0
    | Int_value x -> Int32.to_float x

(* Convert any discrete value to a rational value *)
let convert_to_rational_value value =
    Rational_value (numconst_value value)

(* Convert any discrete value to a rational value *)
let convert_to_int_value value =
    Int_value (int32_value value)

let convert_value value target_type =
    let source_type = var_type_of_value value in
    match source_type, target_type with
    (* Source and target type identical *)
    |  s, t when s = t -> value
    (* Number to int *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_discrete (Var_type_discrete_number Var_type_discrete_int)
    (* Rational to int *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational), Var_type_discrete (Var_type_discrete_number Var_type_discrete_int) -> convert_to_int_value value
    (* Number to rational *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational)
    (* Number to parameter *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_parameter
    (* Number to clock *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_unknown_number), Var_type_clock
    (* Int to rational *)
    | Var_type_discrete (Var_type_discrete_number Var_type_discrete_int), Var_type_discrete (Var_type_discrete_number Var_type_discrete_rational) -> convert_to_rational_value value

    (* Other conversions, don't do anything *)
    | _ -> value (* TODO benjamin SHOULD RAISE AN EXCEPTION ! *)



let hash = function
     | Rational_value x -> Gmp.Z.to_int (NumConst.get_num x)
     | Bool_value x -> if x then 1 else 0
     | Int_value x -> Int32.to_int x

let rational_zero = Rational_value NumConst.zero
let bool_value_false = Bool_value false
let bool_value_true = Bool_value true

let equal_to_zero = function
    | Rational_value x -> NumConst.equal x NumConst.zero
    | Bool_value x -> not x
    | Int_value x -> Int32.equal x Int32.zero

let equal_to_one = function
    | Rational_value x -> NumConst.equal x NumConst.one
    | Bool_value x -> not x
    | Int_value x -> Int32.equal x Int32.one

let equal a b =
    match a, b with
    | Rational_value a, Rational_value b -> NumConst.equal a b
    | Bool_value a, Bool_value b -> a = b
    | Int_value a, Int_value b -> Int32.equal a b
    | _ -> false

let neq a b =
    not (equal a b)

let customized_string_of_value customized_boolean_string = function
    | Number_value x
    | Rational_value x -> NumConst.string_of_numconst x
    | Bool_value x -> if x then customized_boolean_string.true_string else customized_boolean_string.false_string
    | Int_value x -> Int32.to_string x

let string_of_value = customized_string_of_value default_string

let add a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.add a b)
        | Int_value a, Int_value b -> Int_value (Int32.add a b)
        | _ -> raise (ComputingException ("add : " ^ (string_of_var_type (var_type_of_value a)) ^ "," ^ (string_of_var_type (var_type_of_value b))))

let sub a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.sub a b)
        | Int_value a, Int_value b -> Int_value (Int32.sub a b)
        | x, y -> raise (ComputingException ("sub " ^ (string_of_var_type (var_type_of_value y)) ))

let mul a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.mul a b)
        | Int_value a, Int_value b -> Int_value (Int32.mul a b)
        | _ -> raise (ComputingException "mul")

let div a b =
    match a, b with
        | Rational_value a, Rational_value b -> Rational_value (NumConst.div a b)
        | Int_value a, Int_value  b -> Int_value  (Int32.div a b)
        | _ -> raise (ComputingException "div")

let neg = function
    | Rational_value x -> Rational_value (NumConst.neg x)
    | Int_value x -> Int_value (Int32.neg x)
    | _ -> raise (ComputingException "neg")

let zero_of = function
    | Rational_value _ -> Rational_value NumConst.zero
    | Int_value _ -> Int_value Int32.zero
    | _ -> raise (ComputingException "zero_of")

let one_of = function
    | Rational_value _ -> Rational_value NumConst.one
    | Int_value _ -> Int_value Int32.one
    | _ -> raise (ComputingException "one_of")

let _and a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a && b)
        | _ -> raise (ComputingException "and")

let _or a b =
    match a, b with
        | Bool_value a, Bool_value b -> Bool_value (a || b)
        | _ -> raise (ComputingException "or")

let not = function
    | Bool_value a -> Bool_value (not (a))
    | _ -> raise (ComputingException "not")

let bool_equal a b = Bool_value (equal a b)
let bool_neq a b = Bool_value (neq a b)

let l a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.l a b)
        | Int_value a, Int_value  b -> Bool_value  (a < b)
        | _ -> raise (ComputingException "l")

let leq a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.le a b)
        | Int_value a, Int_value  b -> Bool_value  (a <= b)
        | _ -> raise (ComputingException "leq")

let g a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.g a b)
        | Int_value a, Int_value  b -> Bool_value  (a > b)
        | _ -> raise (ComputingException "g")

let geq a b =
    match a, b with
        | Rational_value a, Rational_value b -> Bool_value (NumConst.ge a b)
        | Int_value a, Int_value  b -> Bool_value  (a >= b)
        | _ -> raise (ComputingException "geq")