(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Contain all types and functions for operations onto parsed discrete value
 *
 * File contributors : Benjamin L., Étienne André
 * Created           : 2021/03/01
 *
 ************************************************************)

(* Utils modules *)
open Constants
open Exceptions

(* Parsing structure modules *)
open DiscreteType

(* TODO benjamin CLEAN rename variants, rename module *)
type parsed_value =
    (* TODO benjamin rename a Weak_number_value *)
    | Number_value of NumConst.t
    | Rational_value of NumConst.t
    (* TODO benjamin make literal int to have the use of this variant *)
    | Int_value of Int32.t
    | Bool_value of bool
    | Binary_word_value of BinaryWord.t
    | Array_value of parsed_value array
    | List_value of parsed_value list
    | Stack_value of parsed_value Stack.t
    | Queue_value of parsed_value Queue.t

(* Get discrete var type of a discrete value *)
let rec discrete_type_of_value = function
    | Number_value _ -> Var_type_discrete_number Var_type_discrete_unknown_number
    | Rational_value _ -> Var_type_discrete_number Var_type_discrete_rat
    | Int_value _ -> Var_type_discrete_number Var_type_discrete_int
    | Bool_value _ -> Var_type_discrete_bool
    | Binary_word_value value -> Var_type_discrete_binary_word (BinaryWord.length value)
    | Array_value a ->
        if Array.length a = 0 then
            Var_type_discrete_array (Var_type_weak, 0)
        else
            Var_type_discrete_array (discrete_type_of_value (Array.get a 0), Array.length a)
    | List_value l ->
        if List.length l = 0 then
            Var_type_discrete_list Var_type_weak
        else
            Var_type_discrete_list (discrete_type_of_value (List.nth l 0))
    | Stack_value l ->
        if Stack.length l = 0 then
            Var_type_discrete_stack Var_type_weak
        else
            Var_type_discrete_stack (discrete_type_of_value (Stack.top l))
    | Queue_value l ->
        if Queue.length l = 0 then
            Var_type_discrete_queue Var_type_weak
        else
            Var_type_discrete_queue (discrete_type_of_value (Queue.peek l))

(************************************************************)
(** Value functions  *)
(************************************************************)

(** String of values  **)
let customized_string_of_value customized_string =

    (* Get delimiters of brackets and parenthesis *)
    let l_bra_del, r_bra_del = customized_string.array_string.array_literal_delimiter in
    let l_par_del, r_par_del = Constants.default_paren_delimiter in

    let rec customized_string_of_value_rec = function
        | Number_value x
        | Rational_value x -> NumConst.string_of_numconst x
        | Bool_value x -> if x then customized_string.boolean_string.true_string else customized_string.boolean_string.false_string
        | Int_value x -> Int32.to_string x
        | Binary_word_value b -> BinaryWord.string_of_binaryword b
        | Array_value a ->
            let str_values = Array.map (fun x -> customized_string_of_value_rec x) a in
            l_bra_del ^ OCamlUtilities.string_of_array_of_string_with_sep ", " str_values ^ r_bra_del

        | List_value l ->
            let str_values = List.map (fun x -> customized_string_of_value_rec x) l in
            Constants.list_string ^ l_par_del ^ l_bra_del ^ OCamlUtilities.string_of_list_of_string_with_sep ", " str_values ^ r_bra_del ^ r_par_del

        | Stack_value s ->
            let str_values_with_extra_comma = Stack.fold (fun acc x -> acc ^ customized_string_of_value_rec x ^ ", ") "" s in
            let str_values = if Stack.length s > 0 then String.sub str_values_with_extra_comma 0 ((String.length str_values_with_extra_comma) - 2) else str_values_with_extra_comma in
            if str_values = "" then Constants.stack_string ^ l_par_del ^ r_par_del else Constants.stack_string ^ l_par_del ^ l_bra_del ^ str_values ^ r_bra_del ^ r_par_del

        | Queue_value q ->
            let str_values_with_extra_comma = Queue.fold (fun acc x -> acc ^ customized_string_of_value_rec x ^ ", ") "" q in
            let str_values = if Queue.length q > 0 then String.sub str_values_with_extra_comma 0 ((String.length str_values_with_extra_comma) - 2) else str_values_with_extra_comma in
            if str_values = "" then Constants.queue_string ^ l_par_del ^ r_par_del  else Constants.queue_string ^ l_par_del ^ l_bra_del ^ str_values ^ r_bra_del ^ r_par_del
    in 
    customized_string_of_value_rec

let string_of_value = customized_string_of_value global_default_string


(** Default values  **)

(* Default discrete rational value *)
let default_rational = Rational_value NumConst.zero
(* Default discrete int value *)
let default_int = Int_value Int32.zero
(* Default discrete bool value *)
let default_bool = Bool_value false
(* Default discrete binary word value *)
let default_binary_word_value l = Binary_word_value (BinaryWord.zero l)
(* Default discrete list value *)
let default_list_value = List_value []
(* Default discrete stack value *)
let default_stack_value = Stack_value (Stack.create ())
(* Default discrete stack value *)
let default_queue_value = Queue_value (Queue.create ())


let is_zero = function
    | Number_value value
    | Rational_value value -> value = NumConst.zero
    | Int_value value -> value = Int32.zero
    | value -> false


(** Convert values  **)

(* Get NumConst.t value of rational discrete value *)
let numconst_value = function
    | Rational_value x -> x
    | v -> raise (InternalError ("Unable to get rational value of non-rational discrete value: " ^ string_of_value v))

(* Get Int32.t value of int32 discrete value *)
let int_value = function
    | Int_value x -> x
    | v -> raise (InternalError ("Unable to get int value of non-int discrete value: " ^ string_of_value v))

(* Get bool value of bool discrete value *)
let bool_value = function
    | Bool_value x -> x
    | v -> raise (InternalError ("Unable to get bool value of non-bool discrete value" ^ string_of_value v))

(* Get binary word value of discrete value *)
let binary_word_value = function
    | Binary_word_value x -> x
    | _ as value -> raise (InternalError ("Unable to get binary word value of non binary word `" ^ string_of_value value ^ "`"))

(* Get array value of discrete value *)
let array_value = function
    | Array_value x -> x
    | v -> raise (InternalError ("Unable to get array value of non-array discrete value: " ^ string_of_value v))

(* Get list value of discrete value *)
let list_value = function
    | List_value x -> x
    | v -> raise (InternalError ("Unable to get list value of non-list discrete value: " ^ string_of_value v))

(* Get stack value of discrete value *)
let stack_value = function
    | Stack_value x -> x
    | v -> raise (InternalError ("Unable to get stack value of non-stack discrete value: " ^ string_of_value v))

(* Get queue value of discrete value *)
let queue_value = function
    | Queue_value x -> x
    | v -> raise (InternalError ("Unable to get stack value of non-queue discrete value: " ^ string_of_value v))

(* Convert any discrete value to NumConst.t value, if possible *)
let to_numconst_value = function
    | Number_value x
    | Rational_value x -> x
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> NumConst.numconst_of_int (Int32.to_int x)
    | value -> raise (InternalError ("Unable to convert " ^ string_of_value value ^ " to rational NumConst.t value"))


(* Convert any discrete value to Int32 value, if possible *)
let to_int_value = function
    (* Warning !!!! conversion to int should be dependant of the platform ! *)
    | Number_value x
    | Rational_value x -> Int32.of_int (NumConst.to_int x)
    | Bool_value x -> if x then Int32.one else Int32.zero
    (* Warning, a bit is lost when converting on 32 bit platform !*)
    | Int_value x -> x
    | Binary_word_value x -> Int32.of_int (BinaryWord.hash x)
    | Array_value _ -> raise (InternalError "Unable to convert array to Int32.t value")
    | List_value _ -> raise (InternalError "Unable to convert list to Int32.t value")
    | Stack_value _ -> raise (InternalError "Unable to convert stack to Int32.t value")
    | Queue_value _ -> raise (InternalError "Unable to convert queue to Int32.t value")
