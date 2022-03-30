(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description:
 *
 * File contributors : Benjamin L.
 * Created           : 2021/11/20
 *
 ************************************************************)

open Exceptions
open DiscreteType
open FunctionSig
open ParsingStructureUtilities

type resolved_constraint =
    | Resolved_type_constraint of var_type_discrete
    | Resolved_length_constraint of int

(** -------------------- **)
(** Strings **)
(** -------------------- **)

(* String representation of a resolved constraint *)
let string_of_resolved_constraint = function
    | Resolved_type_constraint t -> string_of_var_type_discrete t
    | Resolved_length_constraint l -> string_of_int l

(* String representation of a tuple list of resolved constraints *)
let string_of_resolved_constraints resolved_constraints =
    let str_resolved_constraints = List.map (fun (constraint_name, resolved_constraint) -> constraint_name ^ ":" ^ string_of_resolved_constraint resolved_constraint) resolved_constraints in
    OCamlUtilities.string_of_list_of_string_with_sep ", " str_resolved_constraints

(** -------------------- **)
(** Constraints resolution **)
(** -------------------- **)

(* Given a discrete type, resolve constraint type / length of a signature type *)
let rec resolve_constraint defined_type_constraint discrete_type =
    match defined_type_constraint, discrete_type with
    | Number_constraint type_number_constraint, Var_type_discrete_number type_number ->
        resolve_type_number_constraint type_number type_number_constraint
    | Binary_constraint length_constraint, Var_type_discrete_binary_word length ->
        resolve_length_constraint length length_constraint
    | Array_constraint (type_constraint, length_constraint), Var_type_discrete_array (inner_type, length) ->
        resolve_type_constraint inner_type type_constraint @ resolve_length_constraint length length_constraint
    | List_constraint type_constraint, Var_type_discrete_list inner_type
    | Stack_constraint type_constraint, Var_type_discrete_stack inner_type
    | Queue_constraint type_constraint, Var_type_discrete_queue inner_type ->
        resolve_type_constraint inner_type type_constraint
    | _ -> []

and resolve_type_constraint discrete_type = function
    | Type_name_constraint constraint_name -> [constraint_name, Resolved_type_constraint discrete_type]
    | Defined_type_constraint defined_type_constraint -> resolve_constraint defined_type_constraint discrete_type

and resolve_defined_type_number_constraint = function
    | Rat_constraint
    | Int_constraint Int_type_constraint -> []
    | Int_constraint (Int_name_constraint constraint_name) -> []

and resolve_type_number_constraint type_number = function
    | Number_type_name_constraint constraint_name -> [constraint_name, Resolved_type_constraint (Var_type_discrete_number type_number)]
    | Defined_type_number_constraint type_number_constraint -> resolve_defined_type_number_constraint type_number_constraint

and resolve_length_constraint length = function
    | Length_constraint_expression length_constraint_expr -> resolve_length_constraint_expression length length_constraint_expr
    | Length_constraint _ -> []

and resolve_length_constraint_expression length = function
    | Length_scalar_constraint constraint_name -> [constraint_name, Resolved_length_constraint length]
    (* Difficult to resolve constraint here ! *)
    | Length_plus_constraint (constraint_name, length_constraint) -> []

(* Check whether two constraint resolutions are compatible *)
let is_resolved_constraints_compatibles constraint_a constraint_b =
    match constraint_a, constraint_b with
    | Resolved_type_constraint type_a, Resolved_type_constraint type_b ->
        DiscreteType.is_discrete_type_compatibles type_a type_b
    | Resolved_length_constraint length_a, Resolved_length_constraint length_b ->
        length_a = length_b
    | _ -> false

(*
(* Special type of constraint, dependent type are type dependent on value *)
(* for example fill_left : binary(l1) -> l:int -> binary(l1 + l) *)
(* The type of the returned binary word is dependent on the value of the second argument *)
(* As the type system of IMITATOR is static, all dependent type value must be a constant expression ! *)
let resolve_dependent_type_constraint variable_infos signature_constraint expressions =

    let signature_constraint_with_expressions = List.combine signature_constraint expressions in

    let dependent_type_constraints = OCamlUtilities.rev_filter_map (fun (type_constraint, expr) ->
        match type_constraint with
        | Defined_type_constraint (Number_constraint (Defined_type_number_constraint Int_constraint (Int_name_constraint constraint_name))) ->
            (* If expression is a type-dependent value, we must convert expression to an int expression *)


            if not (ParsingStructureUtilities.is_parsed_boolean_expression_constant variable_infos expr) then (
                raise (TypeError (""));
            )
            else (
                let value = ParsingStructureUtilities.try_reduce_parsed_boolean_expression variable_infos.constants expr in
                Some (constraint_name, Resolved_length_constraint (Int32.to_int (DiscreteValue.to_int_value value)))
            )
        | _ -> None

    ) signature_constraint_with_expressions
    in
    dependent_type_constraints
*)

(* Resolve signature constraints from passed argument discrete type *)
let resolve_constraints variable_infos signature discrete_types =

    (* Function that choose the best constraint resolution *)
    (* between many compatibles and well-formed resolutions *)
    let reduce_resolutions resolutions =

        let type_constraint_resolutions, length_constraint_resolutions = List.partition (function | Resolved_type_constraint _ -> true | _ -> false) resolutions in

        (* If constraint is a length constraint, return length contraint *)
        if List.length length_constraint_resolutions > 0 then
            List.hd length_constraint_resolutions
        (* Else if it's a type constraint, return the stronger *)
        else (
            let resolved_discrete_types = List.map (function | Resolved_type_constraint discrete_type -> discrete_type | Resolved_length_constraint _ -> Var_type_weak) type_constraint_resolutions in
            (* Reduce discrete types to get the stronger type !  *)
            let stronger_type = List.fold_left (fun acc discrete_type -> DiscreteType.stronger_discrete_type_of acc discrete_type) Var_type_weak resolved_discrete_types in
            Resolved_type_constraint stronger_type
        )

    in

    (* Zip lists: signature types / discrete types *)
    let type_constraint_discrete_type = List.combine signature discrete_types in

    (* Resolve classical constraint *)
    let resolved_classical_constraints = List.fold_left (fun acc (type_constraint, discrete_type) -> acc @ resolve_type_constraint discrete_type type_constraint) [] type_constraint_discrete_type in
    (* Resolve dependent type constraint *)
(*    let resolved_dependent_constraints = resolve_dependent_type_constraint variable_infos signature expressions in*)

    let resolved_constraints = resolved_classical_constraints (*@ resolved_dependent_constraints *) in

    (* Each constraint may have multiple resolutions *)
    (* examples: {'a : int, 'a : bool}; {'b : number; 'b : int }  *)
    (* We group each resolutions by constraint names *)
    let resolved_constraint_group_by_constraint_name = OCamlUtilities.group_by_and_map (fun (constraint_name, _) -> constraint_name) (fun (_, resolved_constraint) -> resolved_constraint) resolved_constraints in

    (* Now we have to partition list between well-formed constraints and mal-formed constraints *)
    let well_formed_constraint_resolutions, malformed_constraint_resolutions = List.partition (fun (constraint_name, resolutions) ->
        (* For a particular constraint name (for example 'a), check that resolutions are compatible *)
        (* If different resolutions for the same constraint are not compatible, then constraint is malformed *)
        OCamlUtilities.for_all_in_arrangement is_resolved_constraints_compatibles resolutions
    ) resolved_constraint_group_by_constraint_name
    in

    (* Reduce well formed constraints with multiple resolutions to one good resolution *)
    let well_formed_constraint_resolutions = List.map (fun (constraint_name, resolutions) ->
        constraint_name, reduce_resolutions resolutions
    ) well_formed_constraint_resolutions
    in

    (* Ungroup *)
    let malformed_constraint_resolutions = List.map (fun (constraint_name, resolutions) -> List.map (fun r -> constraint_name, r) resolutions) malformed_constraint_resolutions |> List.flatten in

    (* Return well-formed and malformed constraint resolutions as a tuple *)
    well_formed_constraint_resolutions, malformed_constraint_resolutions




(** -------------------- **)
(** Conversion **)
(** -------------------- **)

(* Get discrete type from a signature type according to the table of resolved constraints *)
let get_length_resolved_constraint resolved_constraints_table constraint_name =
    let resolved_constraint = Hashtbl.find resolved_constraints_table constraint_name in
    match resolved_constraint with
    | Resolved_length_constraint length -> length
    | _ -> raise (InternalError "")

let get_discrete_type_resolved_constraint resolved_constraints_table constraint_name =

    if not (Hashtbl.mem resolved_constraints_table constraint_name) then
        ImitatorUtilities.print_warning (constraint_name ^ " doesn't found");

    let resolved_constraint = Hashtbl.find resolved_constraints_table constraint_name in
    match resolved_constraint with
    | Resolved_type_constraint discrete_type -> discrete_type
    | _ -> raise (InternalError "")

let rec discrete_type_of_defined_type_constraint resolved_constraints_table = function
    | Number_constraint type_number_constraint ->
        discrete_type_of_type_number_constraint resolved_constraints_table type_number_constraint
    | Bool_constraint ->
        Var_type_discrete_bool
    | Binary_constraint length_constraint ->
        let length = discrete_type_of_length_constraint resolved_constraints_table length_constraint in
        Var_type_discrete_binary_word length
    | Array_constraint (type_constraint, length_constraint) ->
        Var_type_discrete_array (
            discrete_type_of_type_constraint_name resolved_constraints_table type_constraint,
            discrete_type_of_length_constraint resolved_constraints_table length_constraint
        )
    | List_constraint type_constraint ->
        Var_type_discrete_list (
            discrete_type_of_type_constraint_name resolved_constraints_table type_constraint
        )
    | Stack_constraint type_constraint ->
        Var_type_discrete_stack (
            discrete_type_of_type_constraint_name resolved_constraints_table type_constraint
        )
    | Queue_constraint type_constraint ->
        Var_type_discrete_queue (
            discrete_type_of_type_constraint_name resolved_constraints_table type_constraint
        )

and discrete_type_of_type_constraint_name resolved_constraints_table = function
    | Type_name_constraint constraint_name ->
        get_discrete_type_resolved_constraint resolved_constraints_table constraint_name
    | Defined_type_constraint defined_type_constraint -> discrete_type_of_defined_type_constraint resolved_constraints_table defined_type_constraint

and discrete_type_of_type_number_constraint resolved_constraints_table = function
    | Number_type_name_constraint constraint_name ->
        get_discrete_type_resolved_constraint resolved_constraints_table constraint_name
    | Defined_type_number_constraint number_type -> Var_type_discrete_number (discrete_type_of_defined_type_number_constraint resolved_constraints_table number_type)

and discrete_type_of_defined_type_number_constraint resolved_constraints_table = function
    | Rat_constraint -> Var_type_discrete_rational
    | Int_constraint _ -> Var_type_discrete_int

and discrete_type_of_length_constraint resolved_constraints_table = function
    | Length_constraint_expression length_constraint_expr -> discrete_type_of_length_constraint_expression resolved_constraints_table length_constraint_expr
    | Length_constraint length -> length

and discrete_type_of_length_constraint_expression resolved_constraints_table = function
    | Length_scalar_constraint constraint_name ->
        get_length_resolved_constraint resolved_constraints_table constraint_name

    | Length_plus_constraint (constraint_name, length_constraint) ->
        let length_0 = get_length_resolved_constraint resolved_constraints_table constraint_name in
        let length_1 = discrete_type_of_length_constraint resolved_constraints_table length_constraint in
        length_0 + length_1

(* Get signature from signature constraint given a resolved constraints table *)
let signature_of_signature_constraint resolved_constraints_table signature =
    List.map (discrete_type_of_type_constraint_name resolved_constraints_table) signature
