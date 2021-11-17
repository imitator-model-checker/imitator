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
    val def : (discrete_value list -> discrete_value) * signature_constraint
    val dynamic_call : discrete_value list -> discrete_value
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

    let dynamic_call values =
        (* Get arguments values *)
        let value = List.nth values 0 in
        let exponent = List.nth values 1 in

        (* we have to know type of expr *)
        let value_type = discrete_type_of_value value in
        (match value_type with
        | Var_type_discrete_number Var_type_discrete_rational ->
            let numconst_expr = numconst_value value in
            let int_exp = int_value exponent in
            let numconst_result = NumConst.pow numconst_expr int_exp in
            of_numconst numconst_result
        | Var_type_discrete_number Var_type_discrete_int ->
            let int_expr = int_value value in
            let int_exp = int_value exponent in
            let int_result = OCamlUtilities.pow int_expr int_exp in
            of_int int_result
        (* Should never happen *)
        | _ as t ->
            raise (InternalError (
                "Try to reduce a pow function on a "
                ^ string_of_var_type_discrete t
                ^ " expression, although it was checked before by the type checker. Maybe type checking has failed before"
            ))
        )

    let def = dynamic_call, signature

end

(* int -> rat *)
module RationalOfIntFunction : FunctionImplementation = struct

    let name = "rational_of_int"
    let arity = 1

    let signature = [Defined_type_constraint (Number_constraint (Defined_type_number_constraint (Int_constraint Int_type_constraint))); Defined_type_constraint (Number_constraint (Defined_type_number_constraint Rat_constraint))]

    let dynamic_call values =
        let rational_value = List.nth values 0 in
        Rational_value (DiscreteValue.to_numconst_value rational_value)

    let def = dynamic_call, signature

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

    let dynamic_call values =
        let binary_word = List.nth values 0 |> DiscreteValue.binary_word_value in
        let i =  List.nth values 1 |> DiscreteValue.int_value |> Int32.to_int in
        Binary_word_value (BinaryWord.shift_left binary_word i)

    let def = dynamic_call, signature

end

(* binary(l) -> i -> binary(l) *)
module ShiftRightFunction : FunctionImplementation = struct

    let name = "shift_right"
    let arity = 2
    let signature = shift_signature

    let dynamic_call values =
        let binary_word = List.nth values 0 |> DiscreteValue.binary_word_value in
        let i =  List.nth values 1 |> DiscreteValue.int_value |> Int32.to_int in
        Binary_word_value (BinaryWord.shift_right binary_word i)

    let def = dynamic_call, signature

end

(* binary(l) -> l':i -> binary(l+l') *)
module FillLeftFunction : FunctionImplementation = struct

    let name = "fill_left"
    let arity = 2
    let signature = fill_signature

    let dynamic_call values =
        let binary_word = List.nth values 0 |> DiscreteValue.binary_word_value in
        let i =  List.nth values 1 |> DiscreteValue.int_value |> Int32.to_int in
        Binary_word_value (BinaryWord.fill_left binary_word i)

    let def = dynamic_call, signature

end

(* binary(l) -> l':i -> binary(l+l') *)
module FillRightFunction : FunctionImplementation = struct

    let name = "fill_right"
    let arity = 2
    let signature = fill_signature

    let dynamic_call values =
        let binary_word = List.nth values 0 |> DiscreteValue.binary_word_value in
        let i =  List.nth values 1 |> DiscreteValue.int_value |> Int32.to_int in
        Binary_word_value (BinaryWord.fill_right binary_word i)

    let def = dynamic_call, signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogAndFunction : FunctionImplementation = struct

    let name = "logand"
    let arity = 2
    let signature = binary_log_signature

    let dynamic_call values =
        let b1 = List.nth values 0 |> DiscreteValue.binary_word_value in
        let b2 = List.nth values 1 |> DiscreteValue.binary_word_value in
        Binary_word_value (BinaryWord.log_and b1 b2)

    let def = dynamic_call, signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogOrFunction : FunctionImplementation = struct

    let name = "logor"
    let arity = 2
    let signature = binary_log_signature

    let dynamic_call values =
        let b1 = List.nth values 0 |> DiscreteValue.binary_word_value in
        let b2 = List.nth values 1 |> DiscreteValue.binary_word_value in
        Binary_word_value (BinaryWord.log_or b1 b2)

    let def = dynamic_call, signature

end

(* binary(l) -> binary(l) -> binary(l) *)
module LogXorFunction : FunctionImplementation = struct

    let name = "logxor"
    let arity = 2
    let signature = binary_log_signature

    let dynamic_call values =
        let b1 = List.nth values 0 |> DiscreteValue.binary_word_value in
        let b2 = List.nth values 1 |> DiscreteValue.binary_word_value in
        Binary_word_value (BinaryWord.log_xor b1 b2)

    let def = dynamic_call, signature

end

(* binary(l) -> binary(l) *)
module LogNotFunction : FunctionImplementation = struct

    let name = "lognot"
    let arity = 1
    let signature = unary_log_signature

    let dynamic_call values =
        let b = List.nth values 0 |> DiscreteValue.binary_word_value in
        Binary_word_value (BinaryWord.log_not b)

    let def = dynamic_call, signature

end

(* 'a array(l1) -> 'a array(l2) -> 'a array(l1+l2) *)
module ArrayConcatFunction : FunctionImplementation = struct

    let name = "array_concat"
    let arity = 2

    let signature =
    [
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l1")));
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_scalar_constraint "l2")));
        Defined_type_constraint (Array_constraint (Type_name_constraint "a", Length_constraint_expression (Length_plus_constraint ("l1", Length_constraint_expression (Length_scalar_constraint "l2")))));
    ]

    let dynamic_call values =
        let a = List.nth values 0 |> DiscreteValue.array_value in
        let b = List.nth values 1 |> DiscreteValue.array_value in
        Array_value (Array.append a b)

    let def = dynamic_call, signature

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

    let dynamic_call values =
        let a = List.nth values 0 in
        let l = List.nth values 1 |> DiscreteValue.list_value in
        List_value (a :: l)

    let def = dynamic_call, signature
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

    let dynamic_call values =
        let l = List.nth values 0 |> DiscreteValue.list_value in
        List.hd l

    let def = dynamic_call, signature
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

    let dynamic_call values =
        let l = List.nth values 0 |> DiscreteValue.list_value in
        List_value (List.tl l)

    let def = dynamic_call, signature
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

    let dynamic_call values =
        let l = List.nth values 0 |> DiscreteValue.list_value in
        List_value (List.tl l)

    let def = dynamic_call, signature
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

    let dynamic_call values =
        let e = List.nth values 0 in
        let l = List.nth values 1 |> DiscreteValue.list_value in
        Bool_value (List.mem e l)

    let def = dynamic_call, signature
end

module FakeFunction : FunctionImplementation = struct

    let name = "fake"
    let arity = 1
    let signature =
    [
        Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
        Defined_type_constraint (List_constraint (Defined_type_constraint (List_constraint (Type_name_constraint "a"))));
    ]

    let dynamic_call values =
        List.nth values 0

    let def = dynamic_call, signature
end

(*
module Function (F : FunctionImplementation) = struct

end
*)


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
    | "array_concat" -> (module ArrayConcatFunction)
    | "list_cons" -> (module ListConsFunction)
    | "list_hd" -> (module ListHdFunction)
    | "list_tl" -> (module ListTlFunction)
    | "list_rev" -> (module ListRevFunction)
    | "list_mem" -> (module ListMemFunction)
    | "fake" -> (module FakeFunction)
    | function_name -> raise (UndefinedFunction function_name)

(* Get definition of a function given it's name *)
let function_definition_by_name function_name =
    let module M = (val function_module_by_name function_name : FunctionImplementation) in M.def

(* Get arity of a function given it's name *)
let arity_of_function function_name =
    let module M = (val function_module_by_name function_name : FunctionImplementation) in M.arity

(* Get implementation of a function given it's name *)
let dynamic_function_by_name name =
    let fun_impl, _ = function_definition_by_name name in fun_impl

(* Call a function given it's name (with dynamic typing, should be only use in init section) *)
let dynamic_function_call name values =
    dynamic_function_by_name name values

(* Get signature constraint of a function given it's name *)
let signature_constraint_of_function function_name =
    let _, fun_sig = function_definition_by_name function_name in fun_sig

(* String representation of the function signature constraint *)
let string_of_function_signature_constraint function_name =
     string_of_signature_constraint (signature_constraint_of_function function_name)