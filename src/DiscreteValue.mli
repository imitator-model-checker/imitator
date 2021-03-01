type var_type_discrete =
    | Var_type_discrete_rational
    | Var_type_discrete_bool
    | Var_type_discrete_int

(* Discrete value of different specific types *)
type discrete_value =
    | Rational_value of NumConst.t
    | Bool_value of bool
    | Int_value of Int32.t

val var_type_of_value : discrete_value -> var_type_discrete

val numconst_value : discrete_value -> NumConst.t
val bool_value : discrete_value -> bool
val float_value : discrete_value -> float

val hash : discrete_value -> int

val rational_zero : discrete_value

val equal_to_zero : discrete_value -> bool
val equal_to_one : discrete_value -> bool

val equal : discrete_value -> discrete_value -> bool

val string_of_value : discrete_value -> string