(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Parametric Timed Automatas Syncronized Product
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
open ModelPrinter

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

let pta_sync_prod_locations_number = ref 0
let pta_sync_prod_automata_name = ref (Array.make 0 "")
let pta_sync_prod_location_names = ref (Array.make 0 "")
let pta_sync_prod_locations_per_automaton = ref (List.init 0 (fun i -> i))
let pta_sync_prod_actions_per_automaton = ref (List.init 0 (fun i -> i))

(*
  Function to compute the PTA syncronized product locations number
  The resulting automata has:
    length(A_{0}) * length(A_{1}) * ... * length(A_{n-2})  * length(A_{n-1})
  total locations
*)
let get_pta_sync_prod_locations_number (model : AbstractModel.abstract_model) : int =
  if !pta_sync_prod_locations_number = 0 then
    pta_sync_prod_locations_number :=
      List.fold_left (
        fun sum i -> sum * List.length (model.locations_per_automaton i)
      ) 1 (
        List.init model.nb_automata (fun i -> i)
      );
  !pta_sync_prod_locations_number

(*
  Function to initialize/get the PTA syncronized product automata name array
  To form name, it takes the name's first letter in each automata and concatenates them to form the name:
    "[lock, P1, P2] -> lPP"
  As the result is only one automata, the array is length 1
*)
let get_pta_sync_prod_automata_name (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_automata_name = 0 then
    pta_sync_prod_automata_name :=
      Array.make 1 (
        String.concat "" (
          List.map (fun name ->
            if name = "" then ""
            else String.make 1 name.[0]
          ) (List.init model.nb_automata model.automata_names)
        )
      );
  !pta_sync_prod_automata_name

(*
  Function to initialize/get the PTA syncronized product location names array
  The resulting array has:
    get_pta_sync_prod_locations_number model
  indices
*)
let get_pta_sync_prod_location_names (model : AbstractModel.abstract_model) =
  if Array.length !pta_sync_prod_location_names = 0 then
    pta_sync_prod_location_names :=
      Array.make (get_pta_sync_prod_locations_number model) "";
  !pta_sync_prod_location_names

(*
  Function to initialize/get the PTA syncronized product locations_per_automaton array
*)
let get_pta_sync_prod_locations_per_automaton (model : AbstractModel.abstract_model) =
  if List.length !pta_sync_prod_locations_per_automaton = 0 then
    pta_sync_prod_locations_per_automaton :=
      List.init (get_pta_sync_prod_locations_number model) (fun j -> j);
  !pta_sync_prod_locations_per_automaton

(*
  Function to initialize/get the PTA syncronized product action per automata array
*)
let get_pta_sync_prod_actions_per_automaton (model : AbstractModel.abstract_model) =
  if List.length !pta_sync_prod_actions_per_automaton = 0 then
    begin
      pta_sync_prod_actions_per_automaton := List.concat (List.init model.nb_automata model.actions_per_automaton)
      |> List.sort_uniq compare
    end;
  !pta_sync_prod_actions_per_automaton

(*
  Function to compute the digits to concatenate after the location name: [000, 001, 002, ..., 243, 244]
*)
let rec digits base number =
  match base with
  | [] -> []
  | [last] -> [number mod last]
  | _ :: rest ->
    let tr = List.fold_left ( * ) 1 rest in
      (number / tr) :: digits rest (number mod tr)

(*
  Function to compute all location name indices having a digit "dig" at position "pos" in the name:
  indices_with_digit_at 1 0 -> [0; 1; 2; 3; 4; 25; 26; 27; 28; 29; 50; 51; 52; 53; 54]
*)
let indices_with_digit_at (model : AbstractModel.abstract_model) (pos : int) (dig : int) =
  let automata_name = (get_pta_sync_prod_automata_name model).(0) in
  let loc_names = Array.to_list (get_pta_sync_prod_location_names model) in
  List.filter (fun li ->
    let loc_name = List.nth loc_names li in
    let l_loc_name = String.length loc_name in
    let l_automata_name = String.length automata_name in
    let actual_pos = l_automata_name + pos in
    if actual_pos >= l_loc_name then false
    else
      let c = loc_name.[actual_pos] in
      c = Char.chr (Char.code '0' + dig)
  ) (get_pta_sync_prod_locations_per_automaton model)

let compute_PTA_syncronized_product (model : AbstractModel.abstract_model) : AbstractModel.abstract_model =
  let sync_prod = {
    nb_actions                                      = model.nb_actions;
    nb_clocks                                       = model.nb_clocks;
    nb_discrete                                     = model.nb_discrete;
    nb_rationals                                    = model.nb_rationals;
    nb_parameters                                   = model.nb_parameters;
    nb_variables                                    = model.nb_variables;
    nb_ppl_variables                                = model.nb_ppl_variables;
    nb_transitions                                  = model.nb_transitions;
    has_invariants                                  = model.has_invariants;
    has_non_1rate_clocks                            = model.has_non_1rate_clocks;
    has_complex_updates                             = model.has_complex_updates;
    lu_status                                       = model.lu_status;
    strongly_deterministic                          = model.strongly_deterministic;
    has_silent_actions                              = model.has_silent_actions;

    bounded_parameters                              = model.bounded_parameters;
    parameters_bounds                               = model.parameters_bounds;

    observer_pta                                    = model.observer_pta;
    is_observer                                     = model.is_observer;

    clocks                                          = model.clocks;
    is_clock                                        = model.is_clock;
    special_reset_clock                             = model.special_reset_clock;
    global_time_clock                               = model.global_time_clock;
    clocks_without_special_reset_clock              = model.clocks_without_special_reset_clock;

    discrete                                        = model.discrete;
    discrete_rationals                              = model.discrete_rationals;
    is_discrete                                     = model.is_discrete;

    parameters                                      = model.parameters;
    clocks_and_discrete                             = model.clocks_and_discrete;
    parameters_and_discrete                         = model.parameters_and_discrete;
    parameters_and_clocks                           = model.parameters_and_clocks;

    variable_names                                  = model.variable_names;
    discrete_names_by_type_group                    = model.discrete_names_by_type_group;
    type_of_variables                               = model.type_of_variables;

    is_accepting                                    = model.is_accepting;
    is_urgent                                       = model.is_urgent;

    actions                                         = model.actions;
    controllable_actions                            = model.controllable_actions;
    has_controllable_or_uncontrollable_actions      = model.has_controllable_or_uncontrollable_actions;
    action_names                                    = model.action_names;
    action_types                                    = model.action_types;
    is_controllable_action                          = model.is_controllable_action;

    costs                                           = model.costs;
    invariants                                      = model.invariants;
    stopwatches                                     = model.stopwatches;
    flow                                            = model.flow;

    functions_table                                 = model.functions_table;
    local_variables_table                           = model.local_variables_table;

    px_clocks_non_negative                          = model.px_clocks_non_negative;

    (* PTA Sync Product has only 1 automata *)
    nb_automata                                     = 1; (* model.nb_automata; *)

    (* The resulting automata has: length(A_{0}) * length(A_{1}) * ... * length(A_{n-2})  * length(A_{n-1}) total locations *)
    nb_locations                                    = get_pta_sync_prod_locations_number model; (* model.nb_locations; *)
    
    (* The only automata *)
    automata                                        = OCamlUtilities.list_of_interval 0 0; (* model.automata; *)
    
    (* Takes the name's first letter in each automata and concatenates them to form the name: "[lock, P1, P2] -> lPP" *)
    automata_names                                  = (fun _ -> (get_pta_sync_prod_automata_name model).(0)); (* model.automata_names; *)

    (* Computes the number of locations and saves them as a list: [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; ...] *)
    locations_per_automaton                         = (fun _ -> get_pta_sync_prod_locations_per_automaton model); (* model.locations_per_automaton; *)
    
    (* Assigns a name to each location depending on the number of locations in each automata: [lock(3), P1(5), P2(5)] -> [lPP000, lPP001, lPP002, ..., lPP243, lPP244] *)
    location_names                                  = (fun _ li ->
                                                        let _ = get_pta_sync_prod_location_names model in
                                                          if (!pta_sync_prod_location_names).(li) = "" then
                                                            (!pta_sync_prod_location_names).(li) <- (
                                                              let locs_per_a = List.init model.nb_automata (fun i -> List.length (model.locations_per_automaton i)) in

                                                              (* Get digits and convert them into a string: ["000", "001", "002", ..., "243", "244"] *)
                                                              let name_digits = digits locs_per_a li in
                                                              let name = (get_pta_sync_prod_automata_name model).(0) in
                                                              let s_name_digits = String.concat "" (List.map string_of_int name_digits) in

                                                              (* Concatenate automata name with its digits: ["lPP000", "lPP001", "lPP002", ..., "lPP243", "lPP244"] *)
                                                              let width = List.length locs_per_a in
                                                              name ^ (
                                                                if String.length s_name_digits >= width then s_name_digits
                                                                else String.make (width - String.length s_name_digits) '0' ^ s_name_digits
                                                              )
                                                            );
                                                          (!pta_sync_prod_location_names).(li)
                                                      ); (* model.location_names; *)

    (* Get all unique actions and assign them to automata 0 *)
    actions_per_automaton                           = (fun _ -> get_pta_sync_prod_actions_per_automaton model); (* model.actions_per_automaton; *)
    
    (* Automata 0 has all actions *)
    automata_per_action                             = (fun i -> if i >= 0 && i < model.nb_actions then [0] else []); (* model.automata_per_action; *)

    
    actions_per_location                            = model.actions_per_location; (* *)

    (* Automata 0 has all transitions *)
    transitions                                     = model.transitions; (* model.transitions; *)
    transitions_description                         = model.transitions_description; (* *)
    automaton_of_transition                         = model.automaton_of_transition; (* *)
    initial_location                                = model.initial_location; (* *)
    initial_constraint                              = model.initial_constraint; (* *)
    initial_p_constraint                            = model.initial_p_constraint; (* *)
    px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint; (* *)
	} in
(*
  let variables = List.map model.variable_names variables in

	let title = "name[shape=none, style=bold, fontsize=24, label=\"" ^ options#model_local_file_name ^ "\"];" in
	let info_boxes = 
	"general_info[shape=record, style=filled, fillcolor=\"#f1e2cc\", label=\"" (*Model|{*)
	^ "{" ^ (string_of_int (List.length model.clocks_without_special_reset_clock)) ^ " clock" ^ (s_of_int (List.length model.clocks_without_special_reset_clock)) ^ "|" ^ (vertical_string_of_list_of_variables model.clocks_without_special_reset_clock) ^ "}"
		^ "|{" ^ (string_of_int (List.length model.parameters)) ^ " parameter" ^ (s_of_int (List.length model.parameters)) ^ "|" ^ (vertical_string_of_list_of_variables model.parameters) ^ "}"
	^ (if model.discrete <> [] then
		"|{" ^ (string_of_int (List.length model.discrete)) ^ " discrete|" ^ (vertical_string_of_list_of_variables model.discrete) ^ "}"
		else "")
	^ "|{Initial|" ^ (escape_string_for_dot (LinearConstraint.string_of_px_linear_constraint model.variable_names model.initial_constraint)) ^ "}"
	^ "\"];"

		(* Version and generation time infos *)
		^ "\ngeneration[rotation=90.0, style=filled, fillcolor=\"#f1e2cc\", shape=rectangle, fontsize=10, label=\"Generated by " ^ (OCamlUtilities.escape_string_for_dot (ImitatorUtilities.program_name_and_version_and_nickname)) ^ "
Build: " ^ ImitatorUtilities.git_branch_and_hash ^ "
Generation time: " ^ (now()) ^ "\"];" 
		(* To ensure the vertical ordering *)
	^ "\n name -> generation [color=white];"
	^ "\n generation -> general_info [color=white];" in

  (
			List.map (fun automaton_index -> string_of_automaton model automaton_index
		) model.automata)

	let inital_global_location  = model.initial_location in
	let initial_location = DiscreteState.get_location inital_global_location automaton_index in
	let t2 = "\n init" ^ (string_of_int automaton_index) ^ " -> " ^ (id_of_location automaton_index initial_location) ^ ";" in
	let t3 = "\n/* automaton " ^ (model.automata_names automaton_index) ^ " */" in

  (fun location_index ->
(* 		print_message Verbose_high "Entering string_of_locations…2.1"; *)
		print_message Verbose_high ("automaton_index: " ^ string_of_int automaton_index ^ " location_index: " ^ string_of_int location_index);
		string_of_location model automaton_index location_index;
		(* print_message Verbose_high "Entering string_of_locations…2.2"; *)
	)
	let is_accepting = model.is_accepting automaton_index location_index in
	let is_urgent = model.is_urgent automaton_index location_index in
	^ (id_of_location automaton_index location_index) ^ "["
	(* Color *)
	^ "fillcolor=" ^ location_color (*(color location_index)*) ^ ", style=filled, fontsize=16"
	(* LP: shape MRecord inhibits the peripheries display *)
	^ (if is_accepting then ", peripheries=2" else "")
	(* Label: start *)
	^ ", label=\""
	(* Label: accepting *)
	^ (if is_accepting then "acc |" else "")
	(* Label: urgency *)
	^ (if is_urgent then "U |" else "")
	(* Label: name *)
	^ (model.location_names automaton_index location_index)
	^ "|{" ^ (escape_string_for_dot (ModelPrinter.string_of_guard model.variable_names (model.invariants automaton_index location_index)))
	(* Label: stopwatches *)
	^ (if model.has_non_1rate_clocks then (
		let stopwatches = model.stopwatches automaton_index location_index in
		""
		^ (if stopwatches <> [] then "| stop " ^ string_of_list_of_variables model.variable_names stopwatches ^ "" else "")
		(*** TODO: better delimiter? ***)
		^ ""
		^ (if (model.flow automaton_index location_index) <> [] then "|" ^ (string_of_flow model automaton_index location_index) else "")
	)

List.map (fun action_index ->
		(* Get the list of transitions *)
		let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition model automaton_index location_index) transitions
			)
		) (model.actions_per_location automaton_index location_index)

	)
(if List.length (model.automata_per_action transition.action) > 1 then
			let color = color_of_action transition.action in
			"penwidth=3, color=" ^ color ^ ", "
		(* Check if this is a Action_type_nosync action: in which case dotted *)
		else
			match model.action_types transition.action with
			(* "Synchronized" action but with only 1 PTA involved: rather a non-synchronized named action *)
			| Action_type_sync -> ""
			(* Real silent action (no name, no synchronization) *)
			| Action_type_nosync -> "style=dotted, color=gray40, "
		)


		if transition.guard <> AbstractModel.True_guard then
			(*** HACK: also check that the result is not "True" ***)
			let guard_string = ModelPrinter.string_of_guard model.variable_names transition.guard in
			if guard_string = LinearConstraint.string_of_true then "" else
			(escape_string_for_dot guard_string) ^ "\\n"
		else ""

	^ (string_of_action_index model transition.action)
	(* Updates *)
	^ ModelPrinter.string_of_seq_code_bloc model 1 update_seq_code_bloc
*)

  print_endline "nb_automata";
  print_int sync_prod.nb_automata;
  print_newline ();

  print_endline "nb_locations";
  print_int sync_prod.nb_locations;
  print_newline ();

  print_endline "automata";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.automata;

  print_endline "automata_names";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      Printf.printf "automata_names %d = %s\n" i (sync_prod.automata_names i);
    done
  in

  print_endline "locations_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        Printf.printf "locations_per_automaton %d = [%s]\n" i (String.concat "; " (List.map string_of_int locs))
    done
  in

  print_endline "locations_names";
  let () =
    let locs = sync_prod.locations_per_automaton 0 in
      List.iter (fun l -> Printf.printf "  automaton %d, location %d = %s\n" 0 l (sync_prod.location_names 0 l)) locs
  in

  print_endline "actions_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (sync_prod.actions_per_automaton i);
    done
  in

  print_endline "automata_per_action";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      List.iter (fun x -> Printf.printf "%d\n" x) (sync_prod.automata_per_action i);
    done
  in

  print_endline "actions_per_location";

  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map string_of_int (model.actions_per_location i l)))) locs
    done
  in

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

  let () =
    for i = 0 to model.nb_automata - 1 do
      let locs = model.locations_per_automaton i in
      for j = 0 to (List.length locs) - 1 do
        let s_t = ModelPrinter.string_of_transitions model i j in
          Printf.printf "  automaton %d, location %d = [%s]\n" i j s_t
      done
    done
  in

  let () =
    let indices = indices_with_digit_at sync_prod 2 3 in
      List.iter (fun i ->
        Printf.printf "i[%d] = %s\n" i (sync_prod.location_names 0 i)
      ) indices
  in

  print_endline "transitions_description";
(*
	sync_prod.transitions_description : transition_index -> transition;
*)

  print_endline "automaton_of_transition";
  let () =
    for t = 0 to sync_prod.nb_transitions - 1 do
      Printf.printf "  transition %d = automaton %d\n" t (sync_prod.automaton_of_transition t)
    done
  in

  print_endline "initial_location";
(*
	sync_prod.initial_location : DiscreteState.global_location;
*)

  print_endline "initial_constraint";
(*
	sync_prod.initial_constraint : LinearConstraint.px_linear_constraint;
*)

  print_endline "initial_p_constraint";
(*
	sync_prod.initial_p_constraint : LinearConstraint.p_linear_constraint;
*)

  print_endline "px_clocks_non_negative_and_initial_p_constraint";
(*
	sync_prod.px_clocks_non_negative_and_initial_p_constraint: LinearConstraint.px_linear_constraint;
*)
(*
  print_endline "nb_actions";
  print_int sync_prod.nb_actions;
  print_newline ();

  print_endline "nb_clocks";
  print_int sync_prod.nb_clocks;
  print_newline ();

  print_endline "nb_discrete";
  print_int sync_prod.nb_discrete;
  print_newline ();

  print_endline "nb_rationals";
  print_int sync_prod.nb_rationals;
  print_newline ();

  print_endline "nb_parameters";
  print_int sync_prod.nb_parameters;
  print_newline ();

  print_endline "nb_variables";
  print_int sync_prod.nb_variables;
  print_newline ();

  print_endline "nb_ppl_variables";
  print_int sync_prod.nb_ppl_variables;
  print_newline ();

  print_endline "nb_transitions";
  print_int sync_prod.nb_transitions;
  print_newline ();

  print_endline "has_invariants";
  Printf.printf "%b\n" sync_prod.has_invariants;
	
  print_endline "has_non_1rate_clocks";
  Printf.printf "%b\n" sync_prod.has_non_1rate_clocks;

  print_endline "has_complex_updates";
  Printf.printf "%b\n" sync_prod.has_complex_updates;

  print_endline "lu_status";
  print_endline (string_of_lu_status sync_prod.lu_status);

  print_endline "strongly_deterministic";
  Printf.printf "%b\n" sync_prod.strongly_deterministic;

  print_endline "has_silent_actions";
  Printf.printf "%b\n" sync_prod.has_silent_actions;
	
  print_endline "bounded_parameters";
  Printf.printf "%b\n" sync_prod.bounded_parameters;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "parameters_bounds %d = %s\n" i (string_of_p_bounds (sync_prod.parameters_bounds i))
    done
  in

  print_endline "observer_pta";
  print_endline (string_of_option sync_prod.observer_pta);

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_observer %d = %b\n" i (sync_prod.is_observer i);
    done
  in

  print_endline "clocks";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_clock %d = %b\n" i (sync_prod.is_clock i);
    done
  in

  print_endline "special_reset_clock";
  print_endline (string_of_option sync_prod.special_reset_clock);

  print_endline "clocks_without_special_reset_clock";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks_without_special_reset_clock;

  print_endline "global_time_clock";
  print_endline (string_of_option sync_prod.global_time_clock);

  print_endline "discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.discrete;

  print_endline "discrete_rationals";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.discrete_rationals;

  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "is_discrete %d = %b\n" i (sync_prod.is_discrete i);
    done
  in

  print_endline "parameters";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters;

  print_endline "clocks_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.clocks_and_discrete;

  print_endline "parameters_and_discrete";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters_and_discrete;

  print_endline "parameters_and_clocks";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.parameters_and_clocks;

  print_endline "variable_names";
  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "variable_names %d = %s\n" i (sync_prod.variable_names i);
    done
  in

  print_endline "discrete_names_by_type_group";
  List.iter (fun group -> print_endline ("  " ^ string_of_var_type_group group)) sync_prod.discrete_names_by_type_group;

  print_endline "type_of_variables";
  let () =
    for i = 0 to sync_prod.nb_parameters - 1 do
      Printf.printf "type_of_variables %d = %s\n" i (string_of_var_type (sync_prod.type_of_variables i));
    done
  in

  print_endline "locations_per_automaton";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        Printf.printf "locations_per_automaton %d = [%s]\n" i (String.concat "; " (List.map string_of_int locs))
    done
  in

  print_endline "is_accepting";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (sync_prod.is_accepting i l)) locs
    done
  in

  print_endline "is_urgent";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
        List.iter (fun l -> Printf.printf "  automaton %d, location %d = %b\n" i l (sync_prod.is_urgent i l)) locs
    done
  in

  print_endline "actions";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.actions;

  print_endline "controllable_actions";
  List.iter (fun x -> print_endline (string_of_int x)) sync_prod.controllable_actions;

  print_endline "has_controllable_or_uncontrollable_actions";
  Printf.printf "%b\n" sync_prod.has_controllable_or_uncontrollable_actions;

  print_endline "action_names";
  List.iter (fun x -> print_endline (sync_prod.action_names x)) sync_prod.actions;

  print_endline "action_types";
  List.iter (fun x -> print_endline (string_of_action_type (sync_prod.action_types x))) sync_prod.actions;

  print_endline "is_controllable_action";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        let actions = sync_prod.actions_per_location i l in
          List.iter (fun a -> Printf.printf "  automaton %d, location %d, action %d = %b\n" i l a (sync_prod.is_controllable_action a)) actions
      ) locs
    done
  in

	print_endline "costs";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        match sync_prod.costs i l with
        | None -> Printf.printf "  automaton %d, location %d = None\n" i l
        | Some t -> Printf.printf "  automaton %d, location %d = %s\n" i l (string_of_linear_term t)
      ) locs
    done
  in

(*
	sync_prod.invariants : automaton_index -> location_index -> invariant;
*)

  print_endline "stopwatches";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map string_of_int (sync_prod.stopwatches i l)))
      ) locs
    done
  in

  print_endline "flow";
  let () =
    for i = 0 to sync_prod.nb_automata - 1 do
      let locs = sync_prod.locations_per_automaton i in
      List.iter (fun l ->
        Printf.printf "  automaton %d, location %d = [%s]\n" i l (String.concat "; " (List.map (fun (c, v) -> Printf.sprintf "(%d, %s)" c (Gmp.Q.to_string v)) (sync_prod.flow i l)))
      ) locs
    done
  in

  print_endline "functions_table";
(*
  sync_prod.functions_table : (variable_name, fun_definition) Hashtbl.t;
*)

  print_endline "local_variables_table";
(*
  sync_prod.local_variables_table : (variable_ref, AbstractValue.abstract_value) Hashtbl.t;
*)

  print_endline "px_clocks_non_negative";
(*
	sync_prod.px_clocks_non_negative: LinearConstraint.px_linear_constraint;
*)
*)
  sync_prod
;;
