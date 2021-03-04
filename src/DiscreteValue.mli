(* Specific type of number *)
type var_type_discrete_number =
    | Var_type_discrete_rational
    | Var_type_discrete_int

(* Specific type of discrete variables *)
type var_type_discrete =
    | Var_type_discrete_bool
    | Var_type_discrete_number of var_type_discrete_number

(* Type of variable in declarations *)
type var_type =
	| Var_type_clock
	| Var_type_discrete of var_type_discrete
	| Var_type_parameter

(* Discrete value of different specific types *)
type discrete_value =
    | Rational_value of NumConst.t
    | Bool_value of bool
    | Int_value of Int32.t


val is_discrete_type : var_type -> bool

val is_rational_value : discrete_value -> bool
val is_int32_value : discrete_value -> bool
val is_bool_value : discrete_value -> bool

val string_of_var_type : var_type -> string
val string_of_var_type_discrete : var_type_discrete -> string

val var_type_of_value : discrete_value -> var_type_discrete


val numconst_default_value : NumConst.t
val int32_default_value : Int32.t
val bool_default_value : bool
val default_value : var_type -> discrete_value

val numconst_value : discrete_value -> NumConst.t
val int32_value : discrete_value -> Int32.t
val bool_value : discrete_value -> bool
val float_value : discrete_value -> float

val hash : discrete_value -> int

val rational_zero : discrete_value

val equal_to_zero : discrete_value -> bool
val equal_to_one : discrete_value -> bool

val equal : discrete_value -> discrete_value -> bool
val neq : discrete_value -> discrete_value -> bool

val string_of_value : discrete_value -> string

val add : discrete_value -> discrete_value -> discrete_value
val sub : discrete_value -> discrete_value -> discrete_value
val mul : discrete_value -> discrete_value -> discrete_value
val div : discrete_value -> discrete_value -> discrete_value
val neg : discrete_value -> discrete_value
val zero_of : discrete_value -> discrete_value
val one_of : discrete_value -> discrete_value