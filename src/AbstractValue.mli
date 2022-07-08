open DiscreteType

type abstract_number_value =
    | Abstract_rat_value of NumConst.t
    | Abstract_int_value of Int32.t

type abstract_scalar_value =
    | Abstract_number_value of abstract_number_value
    | Abstract_bool_value of bool
    | Abstract_binary_word_value of BinaryWord.t

type abstract_value =
    | Abstract_scalar_value of abstract_scalar_value
    | Abstract_container_value of abstract_container_value

and abstract_container_value =
    | Abstract_array_value of abstract_value array
    | Abstract_list_value of abstract_value list
    | Abstract_stack_value of abstract_value Stack.t
    | Abstract_queue_value of abstract_value Queue.t

val hash : abstract_value -> int
val rational_zero : abstract_value
val to_float_value : abstract_value -> float
val equal : abstract_value -> abstract_value -> bool
val neq : abstract_value -> abstract_value -> bool
val default_value : var_type -> abstract_value
val deep_copy : abstract_value -> abstract_value
val string_of_value : abstract_value -> string

(* Get NumConst.t value of discrete value *)
val numconst_value : abstract_value -> NumConst.t
(* Get Int32.t value of discrete value *)
val int_value : abstract_value -> Int32.t
(* Get bool value of discrete value *)
val bool_value : abstract_value -> bool
(* Get binary word value of discrete value *)
val binary_word_value : abstract_value -> BinaryWord.t
(* Get array value of discrete value *)
val array_value : abstract_value -> abstract_value array
(* Get list value of discrete value *)
val list_value : abstract_value -> abstract_value list
(* Get stack value of discrete value *)
val stack_value : abstract_value -> abstract_value Stack.t
(* Get queue value of discrete value *)
val queue_value : abstract_value -> abstract_value Queue.t

val discrete_type_of_value : abstract_value -> var_type_discrete

val is_rational_value : abstract_value -> bool
val to_int_value : abstract_value -> Int32.t
val to_rat_value : abstract_value -> NumConst.t

val of_parsed_value : DiscreteValue.parsed_value -> abstract_value