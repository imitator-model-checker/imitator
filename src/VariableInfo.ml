(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Utils functions that works on variable / constant
 *
 * File contributors : lbinria
 * Created           : 2022/05/10
 *
 ************************************************************)

open Exceptions
open ParsingStructure
open DiscreteType
open OCamlUtilities

(* Definition state of variable / constant *)
type variable_constant_defined_state =
    | Variable_defined
    | Constant_defined
    | Variable_removed
    | Not_declared

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind of int
    | Constant_kind of AbstractValue.abstract_value

(* Get variable name given a variable index  *)
let [@inline] variable_name_of_index variable_infos = List.nth variable_infos.variable_names

(* Get variable index given a variable name *)
let [@inline] index_of_variable_name variable_infos = Hashtbl.find variable_infos.index_of_variables

(* Get constant value given a constant name *)
let [@inline] value_of_constant_name variable_infos = Hashtbl.find variable_infos.constants

(* Check if variable is defined => declared and not removed  *)
let [@inline] is_variable_is_defined variable_infos = Hashtbl.mem variable_infos.index_of_variables

(* Check if variable was removed *)
let [@inline] is_variable_removed variable_infos variable_name = List.mem variable_name variable_infos.removed_variable_names

(* Check if variable was declared, even if removed *)
let [@inline] is_variable_declared variable_infos variable_name =
    is_variable_is_defined variable_infos variable_name
    || is_variable_removed variable_infos variable_name

(* Check if constant is defined => declared and removed or not *)
let [@inline] is_constant_is_defined variable_infos = Hashtbl.mem variable_infos.constants

(* Check if variable / constant is defined => declared and removed or not *)
let [@inline] is_variable_or_constant_defined variable_infos variable_name =
    is_variable_is_defined variable_infos variable_name || is_constant_is_defined variable_infos variable_name

let [@inline] is_variable_or_constant_declared variable_infos variable_name =
    is_variable_declared variable_infos variable_name || is_constant_is_defined variable_infos variable_name

let variable_constant_defined_state_of variable_infos variable_name =
    if is_variable_is_defined variable_infos variable_name then
        Variable_defined
    else if is_constant_is_defined variable_infos variable_name then
        Constant_defined
    else if is_variable_removed variable_infos variable_name then
        Variable_removed
    else
        Not_declared

let [@inline] var_type_of_variable_index variable_infos = variable_infos.type_of_variables

(* Get var type of a variable given it's name *)
let var_type_of_variable_name variable_infos variable_name =
    let variable_index = index_of_variable_name variable_infos variable_name in
    var_type_of_variable_index variable_infos variable_index

(* Get var type of a variable or a constant given it's name *)
let var_type_of_variable_or_constant variable_infos variable_name =
    let defined_state = variable_constant_defined_state_of variable_infos variable_name in
    match defined_state with
    | Variable_defined ->
        let variable_index = index_of_variable_name variable_infos variable_name in
        variable_infos.type_of_variables variable_index
    | Constant_defined ->
        let value = value_of_constant_name variable_infos variable_name in
        Var_type_discrete (AbstractValue.discrete_type_of_value value)
    | Variable_removed ->
        raise (InternalError ("Impossible to get the type of variable `" ^ variable_name ^ "` because it was removed."))
    | Not_declared ->
        (* Otherwise problem ! *)
        raise (InternalError ("Impossible to get the type of variable `" ^ variable_name ^ "` because it was not declared."))

(* Get some var type of a variable or a constant given it's name *)
(* it return None if constant or variable was not declared or removed *)
let var_type_of_variable_or_constant_opt variable_infos variable_name =
    try
        Some (var_type_of_variable_or_constant variable_infos variable_name)
    with _ ->
        None

(* Get discrete type of a variable or a constant given it's name *)
let discrete_type_of_variable_or_constant variable_infos variable_name =
    let var_type = var_type_of_variable_or_constant variable_infos variable_name in
    DiscreteType.discrete_type_of_var_type var_type

(* Get some discrete type of a variable or a constant given it's name *)
(* it return None if constant or variable was not declared or removed *)
let discrete_type_of_variable_or_constant_opt variable_infos variable_name =
    let var_type_opt = var_type_of_variable_or_constant_opt variable_infos variable_name in
    match var_type_opt with
    | Some var_type -> Some (DiscreteType.discrete_type_of_var_type var_type)
    | None -> None

(* Know if variable with a given name is a variable or a constant *)
let variable_kind_of_variable_name variable_infos variable_name =

    (* First check whether this is a constant *)
    if is_constant_is_defined variable_infos variable_name then (
        let value = value_of_constant_name variable_infos variable_name in
        Constant_kind value
    )
    (* Otherwise: a variable *)
    else
        Variable_kind (index_of_variable_name variable_infos variable_name)

(* Check if variable is a discrete variable given it's name *)
let is_discrete_variable variable_infos variable_name =
    (* Get defined state of variable *)
     let defined_state = variable_constant_defined_state_of variable_infos variable_name in

     match defined_state with
     | Variable_defined ->
        let variable_index = index_of_variable_name variable_infos variable_name in
        (* Keep if this is a discrete *)
        DiscreteType.is_discrete_type (variable_infos.type_of_variables variable_index)
    | Constant_defined -> false
    | _ ->
        raise (InternalError ("The variable `" ^ variable_name ^ "` mentioned in the init definition does not exist."));
