(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module helps to log to res file
 *
 * File contributors : Benjamin L.
 * Created           : 2022/07/19
 *
 ************************************************************)

 (* Parsing structure module *)
open ParsedValue
open DiscreteType

(* Number values *)
type abstract_number_value =
    | Abstract_rat_value of NumConst.t
    | Abstract_int_value of Int32.t

(* Scalar values (no composed values) *)
type abstract_scalar_value =
    | Abstract_number_value of abstract_number_value
    | Abstract_bool_value of bool
    | Abstract_bin_value of BinaryWord.t

(* Values *)
type abstract_value =
    | Abstract_void_value
    | Abstract_scalar_value of abstract_scalar_value
    | Abstract_container_value of abstract_container_value

(* Container values (values that contains another value(s)) *)
and abstract_container_value =
    | Abstract_array_value of abstract_value array
    | Abstract_list_value of abstract_value list
    | Abstract_stack_value of abstract_value Stack.t
    | Abstract_queue_value of abstract_value Queue.t

(* --- Strings --- *)

(* Convert value to string *)
val string_of_value : abstract_value -> string

(* -- Values --- *)

(* Get NumConst.t value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val numconst_value : abstract_value -> NumConst.t
(* Get Int32.t value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val int_value : abstract_value -> Int32.t
(* Get bool value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val bool_value : abstract_value -> bool
(* Get BinaryWord.t value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val binary_word_value : abstract_value -> BinaryWord.t
(* Get array value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val array_value : abstract_value -> abstract_value array
(* Get list value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val list_value : abstract_value -> abstract_value list
(* Get stack value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val stack_value : abstract_value -> abstract_value Stack.t
(* Get queue value of abstract value, raise an error if abstract value doesn't hold the asked type *)
val queue_value : abstract_value -> abstract_value Queue.t

(* Get hash of an abstract value *)
val hash : abstract_value -> int

(* Get abstract value of a zero rational *)
val rational_zero : abstract_value

(* Convert an abstract value to float, if possible, else raise an error *)
val to_float_value : abstract_value -> float
(* Check if an abstract value is equal to another abstract value *)
(* Note that two abstract value must hold the same type to be equals, else it raise an error *)
val equal : abstract_value -> abstract_value -> bool
(* Check if an abstract value is not equal to another abstract value *)
(* Note that two abstract value must hold the same type to be equals, else it raise an error *)
val neq : abstract_value -> abstract_value -> bool
(* Get default abstract value of the given var type *)
val default_value : var_type -> abstract_value
(* Get default abstract value of the given discrete type *)
val default_value_of_discrete_type : var_type_discrete -> abstract_value
(* Get discrete type of an abstract value *)
val discrete_type_of_value : abstract_value -> var_type_discrete
(* Make a deep copy of an abstract value *)
(* Deep copy make a recursive copy of value contained in container values *)
val deep_copy : abstract_value -> abstract_value
(* Check whether the value is rational *)
val is_rational_value : abstract_value -> bool

(* Convert any abstract value to Int32 value, if possible, else raise an error *)
val to_int_value : abstract_value -> Int32.t
(* Convert any abstract value to NumConst.t value, if possible, else raise an error *)
val to_rat_value : abstract_value -> NumConst.t

(* Get abstract value of a given int *)
val of_int : Int32.t -> abstract_value
(* Get abstract value of a given rat *)
val of_rat : NumConst.t -> abstract_value

(* Convert parsed value to an abstract one *)
val of_parsed_value : parsed_value -> abstract_value