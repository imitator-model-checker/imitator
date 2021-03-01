
type var_type_discrete =
    | Var_type_discrete_rational
    | Var_type_discrete_bool
    | Var_type_discrete_int

type numeric_value =
    | Rational_value of NumConst.t
    | Int_value of Int32.t

(* Discrete value of different specific types *)
type discrete_value =
    | Rational_value of NumConst.t
    | Bool_value of bool
    | Int_value of Int32.t

let var_type_of_value = function
    | Rational_value _ -> Var_type_discrete_rational
    | Bool_value _ -> Var_type_discrete_bool
    | Int_value _ -> Var_type_discrete_int

let numconst_value = function
    | Rational_value x -> x
    | Bool_value x -> if x then NumConst.one else NumConst.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> NumConst.numconst_of_int (Int32.to_int x)

let bool_value = function
    | Rational_value x -> if NumConst.equal x NumConst.one then true else false
    | Bool_value x -> x
    | Int_value x -> if Int32.equal x Int32.one then true else false

let float_value = function
    | Rational_value x -> (NumConst.to_float x)
    | Bool_value x -> if x then 0.0 else 0.0
    | Int_value x -> Int32.to_float x

let hash = function
     | Rational_value x -> Gmp.Z.to_int (NumConst.get_num x)
     | Bool_value x -> if x then 1 else 0
     | Int_value x -> Int32.to_int x

let rational_zero = Rational_value NumConst.zero

let equal_to_zero = function
    | Rational_value x -> NumConst.equal x NumConst.zero
    | Bool_value x -> not x
    | Int_value x -> Int32.equal x Int32.zero

let equal_to_one = function
    | Rational_value x -> NumConst.equal x NumConst.one
    | Bool_value x -> not x
    | Int_value x -> Int32.equal x Int32.one

let equal x = function
    | Rational_value b -> match x with | Rational_value a -> NumConst.equal a b | _ -> false
    | Bool_value b -> match x with | Bool_value a -> a = b | _ -> false
    | Int_value b -> match x with | Int_value a -> a = b | _ -> false

let string_of_value = function
    | Rational_value x -> NumConst.string_of_numconst x
    | Bool_value x -> if x then "true" else "false"
    | Int_value x -> Int32.to_string x

(*let bool_of_rational_value x = if x > NumConst.zero then true else false*)
(*let bool_value_of_rational_value x = Bool_value (bool_of_rational_value x)*)