(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Syncronized Product of Parametric Timed Automatas
 *
 * File contributors : Daniel Torres
 * Created           : 2026/09/15
 *
 ************************************************************)

open AbstractModel
open DiscreteType
open Extrapolation
open LinearConstraint
open ModelConverter

module Ppl = Ppl_ocaml
open Ppl

let string_of_lu_status = function
  | PTA_notLU -> "PTA_notLU"
  | PTA_LU _ -> "PTA_LU"
  | PTA_L -> "PTA_L"
  | PTA_U -> "PTA_U"

let string_of_option = function
  |	None -> "-"
  | Some n -> string_of_int n

(* Opción A: usar Gmp directamente *)
let string_of_numconst_or_infinity = function
  | Finite n -> Gmp.Q.to_string n
  | Infinity -> "+inf"
  | Minus_infinity -> "-inf"

let string_of_p_bounds p_bounds =
  let (min_b, max_b) = get_p_bounds p_bounds in
    Printf.sprintf "(%s, %s)" (string_of_numconst_or_infinity min_b) (string_of_numconst_or_infinity max_b)

let string_of_var_type_discrete_number = function
  | Dt_rat -> "rat"
  | Dt_int -> "int"
  | Dt_weak_number -> "weak_number"

let rec string_of_var_type_discrete = function
  | Dt_weak -> "weak"
  | Dt_void -> "void"
  | Dt_bool -> "bool"
  | Dt_number n -> "number(" ^ string_of_var_type_discrete_number n ^ ")"
  | Dt_bin m  -> Printf.sprintf "bin(%d)" m
  | Dt_array (t, n) -> Printf.sprintf "array(%s, %d)" (string_of_var_type_discrete t) n
  | Dt_list t -> "list("  ^ string_of_var_type_discrete t ^ ")"
  | Dt_stack t -> "stack(" ^ string_of_var_type_discrete t ^ ")"
  | Dt_queue t -> "queue(" ^ string_of_var_type_discrete t ^ ")"

let string_of_var_type = function
  | Var_type_clock -> "clock"
  | Var_type_discrete d -> string_of_var_type_discrete d
  | Var_type_parameter -> "parameter"

let string_of_var_type_group (vt, names) =
  Printf.sprintf "%s -> [%s]" (string_of_var_type vt) (String.concat "; " names)

let string_of_action_type = function
	| Action_type_sync -> "Action_type_sync"
	| Action_type_nosync -> "Action_type_nosync"

let rec string_of_linear_term = function
  | IR_Var v -> string_of_int v
  | IR_Coef c -> Gmp.Q.to_string c
  | IR_Plus (a, b) -> "(" ^ string_of_linear_term a ^ " + " ^ string_of_linear_term b ^ ")"
  | IR_Minus (a, b) -> "(" ^ string_of_linear_term a ^ " - " ^ string_of_linear_term b ^ ")"
  | IR_Times (c, a) -> Gmp.Q.to_string c ^ " * " ^ string_of_linear_term a


let compute_PTA_syncronized_product (model : AbstractModel.abstract_model) : AbstractModel.abstract_model =
  print_endline "nb_automata";
  print_int model.nb_automata;
  print_newline ();
  
  print_endline "nb_actions";
  print_int model.nb_actions;
  print_newline ();

  print_endline "nb_clocks";
  print_int model.nb_clocks;
  print_newline ();

  print_endline "nb_discrete";
  print_int model.nb_discrete;
  print_newline ();

  print_endline "nb_rationals";
  print_int model.nb_rationals;
  print_newline ();

  print_endline "nb_parameters";
  print_int model.nb_parameters;
  print_newline ();

  print_endline "nb_variables";
  print_int model.nb_variables;
  print_newline ();

  print_endline "nb_ppl_variables";
  print_int model.nb_ppl_variables;
  print_newline ();

  print_endline "nb_locations";
  print_int model.nb_locations;
  print_newline ();

  print_endline "nb_transitions";
  print_int model.nb_transitions;
  print_newline ();

  print_endline "has_invariants";
  Printf.printf "%b\n" model.has_invariants;
	
  print_endline "has_non_1rate_clocks";
  Printf.printf "%b\n" model.has_non_1rate_clocks;

  print_endline "has_complex_updates";
  Printf.printf "%b\n" model.has_complex_updates;

  print_endline "lu_status";
  print_endline (string_of_lu_status model.lu_status);

  print_endline "strongly_deterministic";
  Printf.printf "%b\n" model.strongly_deterministic;

  print_endline "has_silent_actions";
  Printf.printf "%b\n" model.has_silent_actions;
	
  print_endline "bounded_parameters";
  Printf.printf "%b\n" model.bounded_parameters;

  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "parameters_bounds %d = %s\n" i (string_of_p_bounds (model.parameters_bounds i))
    done
  in

  print_endline "observer_pta";
  print_endline (string_of_option model.observer_pta);

  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "is_observer %d = %b\n" i (model.is_observer i);
    done
  in

  print_endline "clocks";
  List.iter (fun x -> print_endline (string_of_int x)) model.clocks;

  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "is_clock %d = %b\n" i (model.is_clock i);
    done
  in

  print_endline "special_reset_clock";
  print_endline (string_of_option model.special_reset_clock);

  print_endline "clocks_without_special_reset_clock";
  List.iter (fun x -> print_endline (string_of_int x)) model.clocks_without_special_reset_clock;

  print_endline "global_time_clock";
  print_endline (string_of_option model.global_time_clock);

  print_endline "discrete";
  List.iter (fun x -> print_endline (string_of_int x)) model.discrete;

  print_endline "discrete_rationals";
  List.iter (fun x -> print_endline (string_of_int x)) model.discrete_rationals;

  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "is_discrete %d = %b\n" i (model.is_discrete i);
    done
  in

  print_endline "parameters";
  List.iter (fun x -> print_endline (string_of_int x)) model.parameters;

  print_endline "clocks_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) model.clocks_and_discrete;

  print_endline "parameters_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) model.parameters_and_discrete;

  print_endline "parameters_and_clocks";
  List.iter (fun x -> print_endline (string_of_int x)) model.parameters_and_clocks;

  print_endline "variable_names";
  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "variable_names %d = %s\n" i (model.variable_names i);
    done
  in

  print_endline "discrete_names_by_type_group";
  List.iter (fun group -> print_endline ("  " ^ string_of_var_type_group group)) model.discrete_names_by_type_group;

  print_endline "type_of_variables";
  let () =
    for i = 0 to model.nb_parameters - 1 do
      Printf.printf "type_of_variables %d = %s\n" i (string_of_var_type (model.type_of_variables i));
    done
  in

  print_endline "automata";
  List.iter (fun x -> print_endline (string_of_int x)) model.automata;

  print_endline "automata_names";
  let () =
    for i = 0 to model.nb_automata - 1 do
      Printf.printf "automata_names %d = %s\n" i (model.automata_names i);
    done
  in

  print_endline "locations_per_automaton";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        Printf.printf "locations_per_automaton %d = [%s]\n" i (String.concat "; " (List.map string_of_int locs))
    done
  in

  print_endline "locations_names";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %s\n" i l (model.location_names i l)) locs
    done
  in

  print_endline "is_accepting";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (model.is_accepting i l)) locs
    done
  in

  print_endline "is_urgent";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (model.is_urgent i l)) locs
    done
  in

  print_endline "actions";
  List.iter (fun x -> print_endline (string_of_int x)) model.actions;

  print_endline "controllable_actions";
  List.iter (fun x -> print_endline (string_of_int x)) model.controllable_actions;

  print_endline "has_controllable_or_uncontrollable_actions";
  Printf.printf "%b\n" model.has_controllable_or_uncontrollable_actions;

  print_endline "action_names";
  List.iter (fun x -> print_endline (model.action_names x)) model.actions;

  print_endline "action_types";
  List.iter (fun x -> print_endline (string_of_action_type (model.action_types x))) model.actions;

  print_endline "actions_per_automaton";
  let () =
    for i = 0 to model.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (model.actions_per_automaton i);
    done
  in

  print_endline "automata_per_action";
  let () =
    for i = 0 to model.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (model.automata_per_action i);
    done
  in

  print_endline "actions_per_location";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map string_of_int (model.actions_per_location i l)))) locs
    done
  in

  print_endline "is_controllable_action";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
      List.iter (fun l ->
        let actions = model.actions_per_location i l in
          List.iter (fun a -> Printf.printf "  automaton %d, location %d, action %d = %b\n" i l a (model.is_controllable_action a)) actions
      ) locs
    done
  in

	print_endline "costs";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
      List.iter (fun l ->
        match model.costs i l with
        | None -> Printf.printf "  automaton %d, location %d = None\n" i l
        | Some t -> Printf.printf "  automaton %d, location %d = %s\n" i l (string_of_linear_term t)
      ) locs
    done
  in

(*
	model.invariants : automaton_index -> location_index -> invariant;
*)

  print_endline "transitions";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l ->
          let actions = model.actions_per_location i l in
            List.iter (fun a ->
              let trans = model.transitions i l a in
                Printf.printf "  automaton %d, location %d, action %d = [%s]\n" i l a (String.concat "; " (List.map string_of_int trans))
            ) actions
        ) locs
    done
  in

  print_endline "stopwatches";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map string_of_int (model.stopwatches i l)))
      ) locs
    done
  in

  print_endline "flow";
  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map (fun (c, v) -> Printf.sprintf "(%d, %s)" c (Gmp.Q.to_string v)) (model.flow i l)))
      ) locs
    done
  in

(*
	model.transitions_description : transition_index -> transition;
*)

  print_endline "automaton_of_transition";
  let () =
    for t = 0 to model.nb_transitions - 1 do
      Printf.printf "  transition %d = automaton %d\n" t (model.automaton_of_transition t)
    done
  in

  print_endline "functions_table";
(*
  model.functions_table : (variable_name, fun_definition) Hashtbl.t;
*)

  print_endline "local_variables_table";
(*
  model.local_variables_table : (variable_ref, AbstractValue.abstract_value) Hashtbl.t;
*)

  print_endline "px_clocks_non_negative";
(*
	model.px_clocks_non_negative: LinearConstraint.px_linear_constraint;
*)

  print_endline "initial_location";
(*
	model.initial_location : DiscreteState.global_location;
*)

  print_endline "initial_constraint";
(*
	model.initial_constraint : LinearConstraint.px_linear_constraint;
*)

  print_endline "initial_p_constraint";
(*
	model.initial_p_constraint : LinearConstraint.p_linear_constraint;
*)

  print_endline "px_clocks_non_negative_and_initial_p_constraint";
(*
	model.px_clocks_non_negative_and_initial_p_constraint: LinearConstraint.px_linear_constraint;
*)
  model
;;