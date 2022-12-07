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

(* Utils modules *)
open Exceptions
open Constants

 (* Parsing structure modules *)
open ParsedValue
open DiscreteType

type abstract_number_value =
    | Abstract_rat_value of NumConst.t
    | Abstract_int_value of Int32.t

type abstract_scalar_value =
    | Abstract_number_value of abstract_number_value
    | Abstract_bool_value of bool
    | Abstract_binary_word_value of BinaryWord.t

type abstract_value =
    | Abstract_void_value
    | Abstract_scalar_value of abstract_scalar_value
    | Abstract_container_value of abstract_container_value

and abstract_container_value =
    | Abstract_array_value of abstract_value array
    | Abstract_list_value of abstract_value list
    | Abstract_stack_value of abstract_value Stack.t
    | Abstract_queue_value of abstract_value Queue.t

(* Strings *)

let customized_string_of_number_value customized_string = function
    | Abstract_rat_value v -> NumConst.to_string v
    | Abstract_int_value v -> Int32.to_string v

let customized_string_of_scalar_value customized_string = function
    | Abstract_number_value v -> customized_string_of_number_value customized_string v
    | Abstract_bool_value v -> if v then customized_string.boolean_string.true_string else customized_string.boolean_string.false_string
    | Abstract_binary_word_value v -> BinaryWord.string_of_binaryword v

let rec customized_string_of_value customized_string = function
    | Abstract_void_value -> ""
    | Abstract_scalar_value v -> customized_string_of_scalar_value customized_string v
    | Abstract_container_value v -> string_of_container_value customized_string v

and string_of_container_value customized_string = function
    | Abstract_array_value c ->
        let str_values = Array.map (customized_string_of_value customized_string) c in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        l_delimiter ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_values ^ r_delimiter

    | Abstract_list_value c ->
        let str_values = List.map (customized_string_of_value customized_string) c in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        "list(" ^ l_delimiter ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_values ^ r_delimiter ^ ")"

    | Abstract_stack_value c ->
        let str_values_with_extra_comma = Stack.fold (fun acc x -> acc ^ customized_string_of_value customized_string x ^ ", ") "" c in
        let str_values = if Stack.length c > 0 then String.sub str_values_with_extra_comma 0 ((String.length str_values_with_extra_comma) - 2) else str_values_with_extra_comma in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        if str_values = "" then "stack()" else "stack(" ^ l_delimiter ^ str_values ^ r_delimiter ^ ")"

    | Abstract_queue_value c ->
        let str_values_with_extra_comma = Queue.fold (fun acc x -> acc ^ customized_string_of_value customized_string x ^ ", ") "" c in
        let str_values = if Queue.length c > 0 then String.sub str_values_with_extra_comma 0 ((String.length str_values_with_extra_comma) - 2) else str_values_with_extra_comma in
        let l_delimiter, r_delimiter = customized_string.array_string.array_literal_delimiter in
        if str_values = "" then "queue()" else "queue(" ^ l_delimiter ^ str_values ^ r_delimiter ^ ")"

let string_of_value = customized_string_of_value global_default_string


let numconst_value = function
    | Abstract_scalar_value (Abstract_number_value (Abstract_rat_value v)) -> v
    | v -> raise (InternalError ("Unable to get rational value of non-rational value: " ^ string_of_value v))

let int_value = function
    | Abstract_scalar_value (Abstract_number_value (Abstract_int_value v)) -> v
    | v -> raise (InternalError ("Unable to get int value of non-int value: " ^ string_of_value v))

let bool_value = function
    | Abstract_scalar_value (Abstract_bool_value v) -> v
    | v -> raise (InternalError ("Unable to get bool value of non-bool value: " ^ string_of_value v))

let binary_word_value = function
    | Abstract_scalar_value (Abstract_binary_word_value v) -> v
    | v -> raise (InternalError ("Unable to get binary word value of non-binary word value: " ^ string_of_value v))

let array_value = function
    | Abstract_container_value (Abstract_array_value v) -> v
    | v -> raise (InternalError ("Unable to get array value of non-array value: " ^ string_of_value v))

let list_value = function
    | Abstract_container_value (Abstract_list_value v) -> v
    | v -> raise (InternalError ("Unable to get list value of non-list value: " ^ string_of_value v))

let stack_value = function
    | Abstract_container_value (Abstract_stack_value v) -> v
    | v -> raise (InternalError ("Unable to get stack value of non-stack value: " ^ string_of_value v))

let queue_value = function
    | Abstract_container_value (Abstract_queue_value v) -> v
    | v -> raise (InternalError ("Unable to get queue value of non-queue value: " ^ string_of_value v))

let of_int x = Abstract_scalar_value (Abstract_number_value (Abstract_int_value x))
let of_rat x = Abstract_scalar_value (Abstract_number_value (Abstract_rat_value x))

let hash_number_value = function
    | Abstract_rat_value x -> Gmp.Z.to_int (NumConst.get_num x)
    | Abstract_int_value x -> Int32.to_int x

let hash_scalar_value = function
    | Abstract_number_value v -> hash_number_value v
    | Abstract_bool_value v -> if v then 1 else 0
    | Abstract_binary_word_value v -> BinaryWord.hash v

let rec hash = function
    | Abstract_void_value -> 0
    | Abstract_scalar_value v -> hash_scalar_value v
    | Abstract_container_value v -> hash_container_value v

and hash_container_value = function
    (* Arbitrary *)
    | Abstract_array_value a -> Array.fold_left (fun acc x -> acc + (hash x)) 0 a
    | Abstract_list_value l -> List.fold_left (fun acc x -> acc + (hash x)) 0 l
    | Abstract_stack_value s -> Stack.fold (fun acc x -> acc + (hash x)) 0 s
    | Abstract_queue_value s -> Queue.fold (fun acc x -> acc + (hash x)) 0 s

let rational_zero = Abstract_scalar_value (Abstract_number_value (Abstract_rat_value NumConst.zero))

let to_float_number_value = function
    | Abstract_rat_value x -> (NumConst.to_float x)
    | Abstract_int_value x -> Int32.to_float x

let to_float_scalar_value = function
    | Abstract_number_value v -> to_float_number_value v
    | Abstract_bool_value x -> if x then 1.0 else 0.0
    | Abstract_binary_word_value x -> float_of_int (BinaryWord.hash x)

let to_float_value = function
    | Abstract_scalar_value v -> to_float_scalar_value v
    | _ -> raise (InternalError "Unable to convert container to float value")


(* Check if a value is equal to another value *)
let equal_number_value a b = match a, b with
    | Abstract_rat_value a, Abstract_rat_value b -> NumConst.equal a b
    | Abstract_int_value a, Abstract_int_value b -> Int32.equal a b
    | _ -> raise (InternalError "Unable to compare two different typed values.")

let equal_scalar_value a b = match a, b with
    | Abstract_number_value a, Abstract_number_value b -> equal_number_value a b
    | Abstract_bool_value a, Abstract_bool_value b -> a = b
    | Abstract_binary_word_value a, Abstract_binary_word_value b -> BinaryWord.equal a b
    | _ -> raise (InternalError "Unable to compare two different typed values.")

let equal_container_value a b = match a, b with
    | Abstract_array_value a, Abstract_array_value b -> a = b
    | Abstract_list_value a, Abstract_list_value b -> a = b
    | Abstract_stack_value a, Abstract_stack_value b -> a = b
    | Abstract_queue_value a, Abstract_queue_value b -> a = b
    | _ -> raise (InternalError "Unable to compare two different typed values.")

let equal a b = match a, b with
    | Abstract_void_value, Abstract_void_value -> true
    | Abstract_scalar_value a, Abstract_scalar_value b -> equal_scalar_value a b
    | Abstract_container_value a, Abstract_container_value b -> equal_container_value a b
    | _ -> raise (InternalError "Unable to compare two different typed values.")


(* Check if a value is not equal to another value *)
let neq a b =
    not (equal a b)



let default_number_value = function
    | Dt_rat -> Abstract_rat_value NumConst.zero
    | Dt_int -> Abstract_int_value Int32.zero
    | _ -> raise (InternalError "Unable to get number value of non number type.")

let rec default_value_of_discrete_type = function
    | Dt_void -> Abstract_void_value
    | Dt_number t -> Abstract_scalar_value (Abstract_number_value (default_number_value t))
    | Dt_bool -> Abstract_scalar_value (Abstract_bool_value false)
    | Dt_bin l -> Abstract_scalar_value (Abstract_binary_word_value (BinaryWord.zero l))
    | Dt_array (inner_type, length) -> Abstract_container_value (Abstract_array_value (Array.make length (default_value_of_discrete_type inner_type)))
    | Dt_list inner_type -> Abstract_container_value (Abstract_list_value [])
    | Dt_stack inner_type -> Abstract_container_value (Abstract_stack_value (Stack.create ()))
    | Dt_queue inner_type -> Abstract_container_value (Abstract_queue_value (Queue.create ()))
    | Dt_weak -> raise (InternalError "Unable to have default value of a weak typed variable.")

let default_value = function
    | Var_type_clock
    | Var_type_parameter -> Abstract_scalar_value (Abstract_number_value (Abstract_rat_value NumConst.zero))
    | Var_type_discrete discrete_type -> default_value_of_discrete_type discrete_type

let discrete_type_of_number_value = function
    | Abstract_rat_value _ -> Dt_number Dt_rat
    | Abstract_int_value _ -> Dt_number Dt_int

let discrete_type_of_scalar_value = function
    | Abstract_number_value v -> discrete_type_of_number_value v
    | Abstract_bool_value _ -> Dt_bool
    | Abstract_binary_word_value value -> Dt_bin (BinaryWord.length value)

let rec discrete_type_of_value = function
    | Abstract_void_value -> Dt_void
    | Abstract_scalar_value v -> discrete_type_of_scalar_value v
    | Abstract_container_value v -> discrete_type_of_container_value v

and discrete_type_of_container_value = function
    | Abstract_array_value a ->
        if Array.length a = 0 then
            Dt_array (Dt_weak, 0)
        else
            Dt_array (discrete_type_of_value (Array.get a 0), Array.length a)
    | Abstract_list_value l ->
        if List.length l = 0 then
            Dt_list Dt_weak
        else
            Dt_list (discrete_type_of_value (List.nth l 0))
    | Abstract_stack_value l ->
        if Stack.length l = 0 then
            Dt_stack Dt_weak
        else
            Dt_stack (discrete_type_of_value (Stack.top l))
    | Abstract_queue_value l ->
        if Queue.length l = 0 then
            Dt_queue Dt_weak
        else
            Dt_queue (discrete_type_of_value (Queue.peek l))


let rec deep_copy = function
    | Abstract_container_value v -> Abstract_container_value (deep_copy_container_value v)
    | v -> v

and deep_copy_container_value = function
    | Abstract_array_value values -> Abstract_array_value (Array.map deep_copy values)
    | Abstract_stack_value values ->
        let cpy = values |> Stack.to_seq |> List.of_seq |> List.map deep_copy |> List.rev |> List.to_seq |> Stack.of_seq in
        Abstract_stack_value cpy
    | Abstract_queue_value values ->
        let cpy = values |> Queue.to_seq |> List.of_seq |> List.map deep_copy |> List.rev |> List.to_seq |> Queue.of_seq in
        Abstract_queue_value cpy
    | Abstract_list_value _ as v -> v

(* Check whether the value is rational *)
let is_rational_value = function
    | Abstract_scalar_value (Abstract_number_value (Abstract_rat_value _)) -> true
    | _ -> false


(* Convert any discrete value to Int32 value, if possible *)
let to_int_value = function
    (* Warning !!!! conversion to int should be dependant of the platform ! *)
    | Abstract_scalar_value (Abstract_number_value Abstract_rat_value v) -> Int32.of_int (NumConst.to_int v)
    | Abstract_scalar_value (Abstract_number_value Abstract_int_value v) -> v
    | v -> raise (InternalError ("Unable to convert `" ^ string_of_value v ^ "` to Int32.t value"))

let to_rat_value = function
    | Abstract_scalar_value (Abstract_number_value Abstract_rat_value v) -> v
    | Abstract_scalar_value (Abstract_number_value Abstract_int_value v) -> NumConst.numconst_of_int (Int32.to_int v)
    | v -> raise (InternalError ("Unable to convert `" ^ string_of_value v ^ "` to NumConst.t value"))

let rec of_parsed_value = function
    | Weak_number_value v -> raise (InternalError "Unable to convert number value to abstract value")
    | Rational_value v -> Abstract_scalar_value (Abstract_number_value (Abstract_rat_value v))
    | Int_value v -> Abstract_scalar_value (Abstract_number_value (Abstract_int_value v))
    | Bool_value v -> Abstract_scalar_value (Abstract_bool_value v)
    | Binary_word_value v -> Abstract_scalar_value (Abstract_binary_word_value v)
    | Array_value values -> Abstract_container_value (Abstract_array_value (Array.map of_parsed_value values))
    | List_value values -> Abstract_container_value (Abstract_list_value (List.map of_parsed_value values))
    | Stack_value values ->
        let converted_values = values |> Stack.to_seq |> List.of_seq |> List.map of_parsed_value |> List.rev |> List.to_seq |> Stack.of_seq in
        Abstract_container_value (Abstract_stack_value converted_values)
    | Queue_value values ->
        let converted_values = values |> Queue.to_seq |> List.of_seq |> List.map of_parsed_value |> List.rev |> List.to_seq |> Queue.of_seq in
        Abstract_container_value (Abstract_queue_value converted_values)