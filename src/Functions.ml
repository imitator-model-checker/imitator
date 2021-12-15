(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: This module enable definition of customs IMITATOR functions
 * In order to define a new IMITATOR function, just create a module typed as FunctionImplementation
 * and fill the let-bindings.
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 * Last modified     : 2021/11/20
 *
 ************************************************************)

open Exceptions
open DiscreteType
open DiscreteValue
open DiscreteExpressions
open FunctionSig

module type FunctionImplementation = sig

    val name : string
    val arity : int
    val signature : signature_constraint

end

(* 'a number -> int -> 'a number *)
module PowFunction : FunctionImplementation = struct

    let name = "pow"
    let arity = 2

    let signature =
    [
        Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
        Defined_type_constraint (Number_constraint (Number_type_name_constraint "a"))
    ]

end

(* int -> rat *)
module RationalOfIntFunction : FunctionImplementation = struct

    let name = "rational_of_int"
    let arity = 1

    let signature = [Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint))); Defined_type_constraint (Number_constraint (Defined_type_number_constraint Rat_constraint))]

end

(* binary(l) -> l -> binary(l) *)
let shift_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> l':int -> binary(l + l') *)
let fill_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "l1")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint (Int_name_constraint "l"))));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_plus_constraint ("l", Length_constraint_expression (Length_scalar_constraint "l1")))))
    ]

(* binary(l) -> binary(l) *)
let unary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> binary(l) -> binary(l) *)
let binary_log_signature =
    [
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")));
        Defined_type_constraint (Binary_constraint (Length_constraint_expression (Length_scalar_constraint "a")))
    ]

(* binary(l) -> i -> binary(l) *)
module ShiftLeftFunction : FunctionImplementation = struct

    let name = "shift_left"
    let arity = 2
    let signature = shift_signature

end

(* binary(l) -> i -> binary(l) *)
module ShiftRightFunction : FunctionImplementation = struct

    let name = "shift_right"
    let arity = 2
    let signature = shift_signature

end

(* binary(l) -> l':i -> binary(l+l') *)
module FillLeftFunction : FunctionImplementation = struct

    let name = "fill_left"
    let arity = 2
    let signature = fill_signature

end

(* binary(l) -> l':i -> binary(l+l') *)
module FillRightFunction : FunctionImplementation = struct

    let name = "fill_right"
    let arity = 2
    let signature = fill_signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogAndFunction : FunctionImplementation = struct

    let name = "logand"
    let arity = 2
    let signature = binary_log_signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogOrFunction : FunctionImplementation = struct

    let name = "logor"
    let arity = 2
    let signature = binary_log_signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogXorFunction : FunctionImplementation = struct

    let name = "logxor"
    let arity = 2
    let signature = binary_log_signature

end

(* binary(l) -> binary(l) *)
module LogNotFunction : FunctionImplementation = struct

    let name = "lognot"
    let arity = 1
    let signature = unary_log_signature

end

(* 'a array(l1) -> 'a array(l2) -> 'a array(l1+l2) *)
module ArrayAppendFunction : FunctionImplementation = struct

    let name = "array_append"
    let arity = 2

    let signature =
    [
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l1")));
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l2")));
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_plus_constraint ("l1", Length_constraint_expression (Length_scalar_constraint "l2")))));
    ]

end

(* 'a array(l) -> int *)
module ArrayLengthFunction : FunctionImplementation = struct

    let name = "array_length"
    let arity = 1

    let signature =
    [
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l")));
        Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint)))
    ]

end

(* 'a -> 'a list -> 'a list *)
module ListConsFunction : FunctionImplementation = struct

    let name = "list_cons"
    let arity = 2

    let signature =
    [
        Type_name_constraint "a";
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        Defined_type_constraint (List_constraint (Type_name_constraint "a"))
    ]

end

(* 'a list -> 'a *)
module ListHdFunction : FunctionImplementation = struct

    let name = "list_hd"
    let arity = 1
    let signature =
    [
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        Type_name_constraint "a";
    ]

end

(* 'a list -> 'a list *)
module ListTlFunction : FunctionImplementation = struct

    let name = "list_tl"
    let arity = 1
    let signature =
    [
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
    ]

end

(* 'a list -> 'a list *)
module ListRevFunction : FunctionImplementation = struct

    let name = "list_rev"
    let arity = 1
    let signature =
    [
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
    ]

end

(* 'a -> 'a list -> bool *)
module ListMemFunction : FunctionImplementation = struct

    let name = "list_mem"
    let arity = 2
    let signature =
    [
        Type_name_constraint "a";
        Defined_type_constraint (List_constraint (Type_name_constraint "a"));
        Defined_type_constraint Bool_constraint
    ]

end

module FakeFunction : FunctionImplementation = struct

    let name = "fake"
    let arity = 1
    let signature =
    [
        Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
        Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
    ]

end



(* Get the function implementation module matching with the function name *)
(* Register module as a function *)
let function_module_by_name : string -> (module FunctionImplementation) = function
    | "pow" -> (module PowFunction)
    | "rational_of_int" -> (module RationalOfIntFunction)
    | "shift_left" -> (module ShiftLeftFunction)
    | "shift_right" -> (module ShiftRightFunction)
    | "fill_left" -> (module FillLeftFunction)
    | "fill_right" -> (module FillRightFunction)
    | "logand" -> (module LogAndFunction)
    | "logor" -> (module LogOrFunction)
    | "logxor" -> (module LogXorFunction)
    | "lognot" -> (module LogNotFunction)
    | "array_append" -> (module ArrayAppendFunction)
    | "array_length" -> (module ArrayLengthFunction)
    | "list_cons" -> (module ListConsFunction)
    | "list_hd" -> (module ListHdFunction)
    | "list_tl" -> (module ListTlFunction)
    | "list_rev" -> (module ListRevFunction)
    | "list_mem" -> (module ListMemFunction)
    | "fake" -> (module FakeFunction)
    | function_name -> raise (UndefinedFunction function_name)


(* Get signature constraint of a function given it's name *)
let signature_constraint_of_function function_name =
    let module M = (val function_module_by_name function_name : FunctionImplementation) in M.signature

(* Get arity of a function given it's name *)
let arity_of_function function_name =
    (List.length (signature_constraint_of_function function_name)) - 1

(* String representation of the function signature constraint *)
let string_of_function_signature_constraint function_name =
     string_of_signature_constraint (signature_constraint_of_function function_name)