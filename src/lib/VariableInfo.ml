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

type variable_scope = Global | Local

(* Definition state of variable / constant *)
type variable_constant_defined_state =
    | Variable_defined (* of variable_index *)
    | Constant_defined
    | Variable_removed
    | Not_declared

(* Variable kind type represent a variable or a constant kind *)
type variable_kind =
    | Variable_kind
    | Constant_kind of AbstractValue.abstract_value

let [@inline] is_global (_, id) = id = 0

let [@inline] is_local variable_ref = not (is_global variable_ref)

let variable_scope_of variable_ref = if is_global variable_ref then Global else Local

(* Get variable name given a variable index  *)
let [@inline] variable_name_of_index variable_infos = List.nth variable_infos.variable_names

(* Get variable index given a variable name *)
let [@inline] index_of_variable_name variable_infos = Hashtbl.find variable_infos.index_of_variables

(* Get constant value given a constant name *)
let [@inline] value_of_constant_name variable_infos = Hashtbl.find variable_infos.constants

(* Check if variable is defined => declared and not removed  *)
let [@inline] is_variable_is_defined variable_infos = Hashtbl.mem variable_infos.variable_refs

(* Check if global variable is defined => declared and not removed  *)
let [@inline] is_global_variable_is_defined variable_infos variable_name =
    is_variable_is_defined variable_infos (variable_name, 0)

(* Check if global variable was removed *)
let [@inline] is_global_variable_removed variable_infos variable_name =
    List.mem variable_name variable_infos.removed_variable_names

(* Check if variable was removed *)
let [@inline] is_variable_removed variable_infos (variable_name, _) =
    is_global_variable_removed variable_infos variable_name

(* Check if variable was declared, even if removed *)
let [@inline] is_variable_declared variable_infos variable_ref =
    is_variable_is_defined variable_infos variable_ref
    || is_variable_removed variable_infos variable_ref

(* Check if constant is defined => declared and removed or not *)
let [@inline] is_constant_is_defined variable_infos = Hashtbl.mem variable_infos.constants

(* Check if variable / constant is defined => declared and removed or not *)
let [@inline] is_variable_or_constant_defined variable_infos ((variable_name, _) as variable_ref) =
    is_variable_is_defined variable_infos variable_ref || is_constant_is_defined variable_infos variable_name

let [@inline] is_variable_or_constant_declared variable_infos ((variable_name, _) as variable_ref) =
    is_variable_declared variable_infos variable_ref || is_constant_is_defined variable_infos variable_name

let [@inline] is_global_variable_or_constant_declared variable_infos variable_name =
    is_variable_or_constant_declared variable_infos (variable_name, 0)

let variable_constant_defined_state_of variable_infos ((variable_name, _) as variable_ref) =
    if is_variable_is_defined variable_infos variable_ref then
        Variable_defined
    else if is_constant_is_defined variable_infos variable_name then
        Constant_defined
    else if is_variable_removed variable_infos variable_ref then
        Variable_removed
    else
        Not_declared

let [@inline] var_type_of_variable_index variable_infos = variable_infos.type_of_variables

(* Get var type of a variable given it's name *)
let [@inline] var_type_of_variable_name variable_infos = Hashtbl.find variable_infos.variable_refs

(* Get var type of a global variable given it's name *)
let [@inline] var_type_of_global_variable_name variable_infos variable_name =
    var_type_of_variable_name variable_infos (variable_name, 0)

(* Get var type of a variable or a constant given it's name *)
let var_type_of_variable_or_constant variable_infos ((variable_name, _) as variable_ref) =
    let defined_state = variable_constant_defined_state_of variable_infos variable_ref in
    match defined_state with
    | Variable_defined ->
        Hashtbl.find variable_infos.variable_refs variable_ref
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
let var_type_of_variable_or_constant_opt variable_infos variable_ref =
    try
        Some (var_type_of_variable_or_constant variable_infos variable_ref)
    with _ ->
        None

(* Get some var type of a global variable or a constant given it's name *)
(* it return None if constant or global variable was not declared or removed *)
let var_type_of_global_variable_or_constant_opt variable_infos variable_name =
    var_type_of_variable_or_constant_opt variable_infos (variable_name, 0)

(* Get discrete type of a variable or a constant given it's name *)
let discrete_type_of_variable_or_constant variable_infos variable_ref =
    let var_type = var_type_of_variable_or_constant variable_infos variable_ref in
    DiscreteType.discrete_type_of_var_type var_type

(* Get discrete type of a global variable or a constant given it's name *)
let discrete_type_of_global_variable_or_constant variable_infos variable_name =
    discrete_type_of_variable_or_constant variable_infos (variable_name, 0)

(* Know if global variable with a given name is a variable or a constant *)
let variable_kind_of_global_variable_name variable_infos variable_name =

    (* First check whether this is a constant *)
    if is_constant_is_defined variable_infos variable_name then (
        let value = value_of_constant_name variable_infos variable_name in
        Constant_kind value
    )
    (* Otherwise: a variable *)
    else (
        Variable_kind
    )


(* Know if variable with a given name is a variable or a constant *)
let variable_kind_of_variable_name variable_infos (variable_name, _) =
    variable_kind_of_global_variable_name variable_infos variable_name

(* Know if variable with a given name is a variable or a constant, if variable not found, return None *)
let variable_kind_of_variable_name_opt variable_infos variable_ref =
    try
        Some (variable_kind_of_variable_name variable_infos variable_ref)
    with Not_found ->
        None

(* Check if variable is a discrete variable given it's name *)
let is_discrete_variable variable_infos ((variable_name, _) as variable_ref) =
    (* Get defined state of variable *)
     let defined_state = variable_constant_defined_state_of variable_infos variable_ref in

     match defined_state with
     | Variable_defined ->
        let var_type = Hashtbl.find variable_infos.variable_refs variable_ref in
        DiscreteType.is_discrete_type var_type
    | Constant_defined -> false
    | _ ->
        raise (InternalError ("The variable `" ^ variable_name ^ "` was undeclared or removed."))

(* Check if global variable is a discrete variable given it's name *)
let is_discrete_global_variable variable_infos variable_name =
    is_discrete_variable variable_infos (variable_name, 0)

(* Check whether variable is a clock *)
let is_clock variable_infos variable_ref =
    let var_type = var_type_of_variable_or_constant variable_infos variable_ref in
    let var_kind = variable_kind_of_variable_name variable_infos variable_ref in
    (* TODO benjamin IMPORTANT constants like (x = 1 : clock) should be set as rat before in ModelConverter ! *)
    match var_type, var_kind with
    (* Should be a clock and not constant ! *)
    | Var_type_clock, Variable_kind -> true
    | _ -> false

(* Check whether variable is a parameter *)
let is_param variable_infos variable_ref =
    let var_type = var_type_of_variable_or_constant variable_infos variable_ref in
    let var_kind = variable_kind_of_variable_name variable_infos variable_ref in
    (* TODO benjamin IMPORTANT parameters like (p = 1 : parameter) should be set as rat before in ModelConverter ! *)
    match var_type, var_kind with
    (* Should be a clock and not constant ! *)
    | Var_type_parameter, Variable_kind -> true
    | _ -> false

(* Check (if variable is defined) whether variable is a clock or a parameter *)
let [@inline] is_clock_or_param variable_infos variable_ref =
    is_clock variable_infos variable_ref || is_param variable_infos variable_ref

(* Get function meta given it's name, raise an error if the function doesn't exists *)
let function_metadata_by_name variable_infos function_name =
    let fun_definition_opt = Hashtbl.find_opt variable_infos.fun_meta function_name in
    match fun_definition_opt with
    | Some fun_definition -> fun_definition
    | None -> raise (UndefinedFunction function_name)