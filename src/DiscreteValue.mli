open Constants

(* Specific type of number *)
type var_type_discrete_number =
    | Var_type_discrete_rational
    | Var_type_discrete_int
    | Var_type_discrete_unknown_number

(* Specific type of discrete variables *)
type var_type_discrete =
    | Var_type_discrete_bool
    | Var_type_discrete_number of var_type_discrete_number

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

type expression_type =
    | Expression_type_discrete_bool of var_type_discrete
    | Expression_type_discrete_number of var_type_discrete_number

(* Discrete value of different specific types *)
type discrete_value =
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    | Bool_value of bool
    | Int_value of Int32.t


val is_discrete_type : var_type -> bool
(* Check if a Var_type is a Var_type_number *)
val is_number_type : var_type -> bool

val is_unknown_number_type : var_type -> bool
(* Check if a Var_type is a Var_type_rational *)
val is_rational_type : var_type -> bool
(* Check if a Var_type is a Var_type_bool *)
val is_bool_type : var_type -> bool

val is_rational_value : discrete_value -> bool
val is_int32_value : discrete_value -> bool
val is_bool_value : discrete_value -> bool

val string_of_var_type : var_type -> string
val string_of_var_type_discrete : var_type_discrete -> string

val string_of_expression_type : expression_type -> string

val inner_type_of : var_type -> var_type
val var_type_of_value : discrete_value -> var_type

(* Check if two types are compatible *)
(* To be compatible, two type should have the same kind (number, boolean, ...) *)
val is_type_compatibles : var_type -> var_type -> bool

(* Check if a variable type is compatible with an expression type *)
val is_var_type_compatible_with_expr_type : var_type -> expression_type -> bool
(* Check if an expression is a boolean expression *)
val is_bool_expression : expression_type -> bool

val numconst_default_value : NumConst.t
val int32_default_value : Int32.t
val bool_default_value : bool
val default_value : var_type -> discrete_value

val numconst_value : discrete_value -> NumConst.t
val int32_value : discrete_value -> Int32.t
val bool_value : discrete_value -> bool
val float_value : discrete_value -> float
val convert_value : discrete_value -> var_type -> discrete_value

val hash : discrete_value -> int

val rational_zero : discrete_value
val bool_value_false : discrete_value
val bool_value_true : discrete_value

val equal_to_zero : discrete_value -> bool
val equal_to_one : discrete_value -> bool

val equal : discrete_value -> discrete_value -> bool
val neq : discrete_value -> discrete_value -> bool

val customized_string_of_value : customized_boolean_string -> discrete_value -> string
val string_of_value : discrete_value -> string

val add : discrete_value -> discrete_value -> discrete_value
val sub : discrete_value -> discrete_value -> discrete_value
val mul : discrete_value -> discrete_value -> discrete_value
val div : discrete_value -> discrete_value -> discrete_value
val neg : discrete_value -> discrete_value
val zero_of : discrete_value -> discrete_value
val one_of : discrete_value -> discrete_value

val _and : discrete_value -> discrete_value -> discrete_value
val _or : discrete_value -> discrete_value -> discrete_value
val not : discrete_value -> discrete_value
val bool_equal : discrete_value -> discrete_value -> discrete_value
val bool_neq : discrete_value -> discrete_value -> discrete_value


val l : discrete_value -> discrete_value -> discrete_value
val leq : discrete_value -> discrete_value -> discrete_value
val g : discrete_value -> discrete_value -> discrete_value
val geq : discrete_value -> discrete_value -> discrete_value