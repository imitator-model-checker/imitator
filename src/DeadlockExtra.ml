(* JvdP: add some auxiliary code for deadlock checking, Paris July 2022 *)

type clock_updates = (Automaton.clock_index * LinearConstraint.pxd_linear_term) list

open LinearConstraint

(* note: we "undo" the effect of updates on zone z (by computing the weakest precondition) *)
let dl_inverse_update z updates = 
    let model = Input.get_model () in (* only for printing *)
    let constr = px_copy z in
    let equality update = (* create an equlity x-lt=0 *)
        let (x,lt) = update in
        let clock = make_pxd_linear_term [(NumConst.one,x)] NumConst.zero in
        let diff = sub_pxd_linear_terms clock lt in
        let eq = make_pxd_linear_inequality diff Op_eq in
        ImitatorUtilities.print_message Verbose_medium  
           ("JvdP: \027[32mNew inverse update:\027[0m = \n" ^ 
             LinearConstraint.string_of_pxd_linear_inequality model.variable_names eq);
        eq in
    let updates_eqs = List.map equality updates in
    let updates_pxd = make_pxd_constraint updates_eqs in
    let updates_px = pxd_hide_discrete_and_collapse updates_pxd in
        (* TODO: shouldn't we substitute the discrete variables from the "current" state? *)

    px_intersection_assign constr [updates_px];
    px_hide_assign (List.map fst updates) constr;
    constr

(* note: we don't add positive constraints for clocks here. *)
(* TODO: does this handle flows correctly? *)
let dl_inverse_time z =
    let constr = px_copy z in
    px_time_past_assign (clocks ()) (parameters ()) constr;
    constr

(* compute direct predecessor of z2 in z1, linked by (guard,updates) *)
(* copied the handling of pxd_constraint in guard, double check? *)
let dl_predecessor z1 guard updates z2 =
    let model = Input.get_model () in (* only for printing *)
    let constr = dl_inverse_update z2 updates in
    px_intersection_assign constr [z1];
    let constr_pxd = LinearConstraint.pxd_of_px_constraint constr in
    pxd_intersection_assign constr_pxd [guard];
    let result = LinearConstraint.pxd_hide_discrete_and_collapse constr_pxd in
    (* TODO: Why can we just hide the discrete variables??? *)

    ImitatorUtilities.print_message Verbose_medium  ("JvdP: \027[32mInverse_update\027[0m = \n" ^ LinearConstraint.string_of_px_linear_constraint model.variable_names constr);
    ImitatorUtilities.print_message Verbose_medium  ("JvdP: \027[32mWith guard\027[0m = \n" ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr_pxd);
    ImitatorUtilities.print_message Verbose_medium  ("JvdP: \027[32mAfter hiding\027[0m = \n" ^ LinearConstraint.string_of_px_linear_constraint model.variable_names result);
    result


open StateSpace

(* this extends get_resets: we return (x,0) for resets and (x,lt) for updates *)
let dl_get_clock_updates state_space combined_transition =
	(* Retrieve the model *)
	let model = Input.get_model () in

	(* For all transitions involved in the combined transition *)
	let resets = List.fold_left (fun current_resets transition_index ->
		(* Get the actual transition *)
		let transition = model.transitions_description transition_index in
        let zero =  make_pxd_linear_term [] NumConst.zero in
		(*** WARNING: we only accept clock resets (no arbitrary updates) ***)
		match transition.updates.clock with
			(* No update at all *)
			| No_update -> current_resets
			(* Reset to 0 only *)
			| Resets clock_resets -> List.rev_append (List.map (fun x -> (x,zero)) clock_resets) current_resets
			(* Reset to arbitrary value (including discrete, parameters and clocks) *)
			| Updates clock_updates -> List.rev_append clock_updates current_resets
	) [] combined_transition
	in

	(* Keep each update once *)
    (* TODO: check for inconsistent updates? *)
	OCamlUtilities.list_only_once resets

let dl_weakest_precondition state_space s1_index transition s2_index =
    let z1 = (get_state state_space s1_index).px_constraint in
    let z2 = (get_state state_space s2_index).px_constraint in
  

    let updates = dl_get_clock_updates state_space transition in
    (* TODO: are there other updates than resets??? *)
    let guard = get_guard state_space s1_index transition s2_index in
    (* TODO: We are hiding d from pxd to get the type correct. What is the effect? *)
 
    dl_predecessor z1 guard updates z2
