(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: "EF optimized" algorithm: minimization or minimization of a parameter valuation for which there exists a run leading to some states. Algorithm with a priority queue.
 * 
 * File contributors : Vincent Bloemen, Étienne André
 * Created           : 2018/0?/??
 * Last modified     : 2019/02/19
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open OCamlUtilities
open ImitatorUtilities
open Exceptions
open AbstractModel
open Result
open AlgoStateBased
open Statistics


(************************************************************)
(** Statistics *)
(************************************************************)


(*** NOTE: defined OUTSIDE the class, as many instances of this class can be created (for BC), and we want a single counter *)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFoptQueue =
	object (self) inherit algoStateBased as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Parameter valuations in all |P| dimensions for which the target is reached *)
	val mutable constraint_valuations : LinearConstraint.p_nnconvex_constraint option = None
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EF-opttime-PQ"

	(*------------------------------------------------------------*)
	(* Variables *)
	(*------------------------------------------------------------*)
	
    (* Epsilon value for "global_time > 0" and to subtract from best-worst-case time, since a small value gets added to floats *)
    (* NB: For best-worst-case, epsilon gets subtracted twice in the case global_time < 5 *)
    (* NB: Don't make epsilon too small, because that causes weird behavior... *)
	val epsilon = 0.0001
	
	(*------------------------------------------------------------*)
	(* Shortcuts *)
	(*------------------------------------------------------------*)
	

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)
	

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
		
	method private get_constraint_valuations =
		match constraint_valuations with
		| Some constr -> constr
		| None -> raise (InternalError "Variable 'constraint_valuations' not initialized in AlgoEFoptQueue although it should have been at this point")
	


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*                                                       *-*)
	(*-*                  Opt Time Reach code                  *-*)
	(*-*                                                       *-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Returns the variable index for global_time *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    method private get_global_time =
        let rec find_global_time clocks = match clocks with
            | [] ->  raise (InternalError ("Unable to locate global_time"));
            | head::body -> (
                if model.variable_names head = Constants.global_time_clock_name then head
                else find_global_time body
            )
        in
        find_global_time model.clocks


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Returns a list that contain all variable indices, except for the *)
    (* global_time index. This will be used by time_constr_to_val for *)
    (* filtering the constraints *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    method private variables_without_global_time =
        let rec filter_clocks clocks = match clocks with
            | [] ->  [];
            | head::body -> (
                if model.variable_names head = Constants.global_time_clock_name then filter_clocks body
                else List.append [head] (filter_clocks body)
            )
        in
        List.append model.parameters_and_discrete (filter_clocks model.clocks)


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Turns a global_time constraint into a minimum time value *)
	(* Note that all other parameters are filtered out, so the resulting *)
    (* value must be a float *)
    (* The method assumes that there is a single lower bound value in the *)
    (* constraint, though an upper bound is allowed *)
    (* e.g. "global_time >= 5 & 10 <= global_time" -> 5. *)
    (* *)
    (* New case: "& 2 >= 5*global_time" *)
    (* *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    method private time_constr_to_val time_constraint =
		LinearConstraint.pxd_hide_assign self#variables_without_global_time time_constraint;
		let time_str = LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint in
		(*print_message Verbose_standard("Constraint: #" ^ time_str ^ "#");*)
		let time_array = Str.split (Str.regexp "&\\|[ \t\n]+\\|\\*") time_str in
		(* temporary variables *)
		let min_time = ref max_float in (* lower time bound *)
        let is_float s =
            try ignore (float_of_string s); true
            with _ -> false in
    
        (*(* debug *)
		print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint);
        let rec print_constr_arr arr = match arr with
            | [] -> print_message Verbose_standard ("]\n");
            | a::body -> (
                print_message Verbose_standard ("(" ^ a ^ ")");
                print_constr_arr body;
            );
        in print_constr_arr time_array;*)

        (* Somehow, Constants.global_time_clock_name may not be used for *)
        (* matching expressions, so just do this hardcoded.. *)
	 	let rec parse_time_constraint time_list = match time_list with
            | [] -> ();
            | ""::body -> parse_time_constraint body;
            | "global_time"::comp::timeval::body -> ( (* "global_time >= 5" *)
                match comp with
                    | ">" | ">=" | "=" -> (
                        if is_float timeval then min_time := float_of_string timeval
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = ">" then min_time := !min_time +. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            | timeval::comp::"global_time"::body -> ( (* "5 <= global_time" *)
                match comp with
                    | "<" | "<=" | "=" -> (
                        if is_float timeval then min_time := float_of_string timeval
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = "<" then min_time := !min_time +. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            (* In case there is a multiplicative factor *)
            | factor::"global_time"::comp::timeval::body -> ( (* "2*global_time >= 5" *)
                match comp with
                    | ">" | ">=" | "=" -> (
                        if (is_float timeval) && (is_float factor) then min_time := (float_of_string timeval) /. (float_of_string factor)
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = ">" then min_time := !min_time +. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            | timeval::comp::factor::"global_time"::body -> ( (* "5 <= 2*global_time" *)
                match comp with
                    | "<" | "<=" | "=" -> (
                        if is_float timeval && is_float factor then min_time := (float_of_string timeval) /. (float_of_string factor)
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = "<" then min_time := !min_time +. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )

            | _::body -> (
		        print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint);
                raise (InternalError ("Unable to parse constraint d"));
            );
        in
        parse_time_constraint time_array;
        (*print_message Verbose_standard ("result: " ^ (string_of_float !min_time));*)
		!min_time


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Turns a global_time constraint into a maximum time value *)
    (* Similar to time_constr_to_val but now using the maximum bounds instead *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    method private time_constr_to_max_val time_constraint =
		LinearConstraint.pxd_hide_assign self#variables_without_global_time time_constraint;
		let time_str = LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint in
		(* print_message Verbose_standard("Constraint: #" ^ time_str ^ "#"); *)
		let time_array = Str.split (Str.regexp "&\\|[ \t\n]+\\|\\*") time_str in
		(* temporary variables *)
		let max_time = ref max_float in (* upper time bound *)
        let is_float s =
            try ignore (float_of_string s); true
            with _ -> false in
    
        (*(* debug *)
		print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint);
        let rec print_constr_arr arr = match arr with
            | [] -> print_message Verbose_standard ("]\n");
            | a::body -> (
                print_message Verbose_standard ("(" ^ a ^ ")");
                print_constr_arr body;
            );
        in print_constr_arr time_array;*)

        (* Somehow, Constants.global_time_clock_name may not be used for *)
        (* matching expressions, so just do this hardcoded.. *)
	 	let rec parse_time_constraint time_list = match time_list with
            | [] -> ();
            | ""::body -> parse_time_constraint body;
            | "global_time"::comp::timeval::body -> ( (* "global_time <= 5" *)
                match comp with
                    | "<" | "<=" | "=" -> (
                        if is_float timeval then max_time := float_of_string timeval
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = "<" then max_time := !max_time -. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            | timeval::comp::"global_time"::body -> ( (* "5 >= global_time" *)
                match comp with
                    | ">" | ">=" | "=" -> (
                        if is_float timeval then max_time := float_of_string timeval
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = ">" then max_time := !max_time -. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            (* In case there is a multiplicative factor *)
            | factor::"global_time"::comp::timeval::body -> ( (* "2*global_time <= 5" *)
                match comp with
                    | "<" | "<=" | "=" -> (
                        if (is_float timeval) && (is_float factor) then max_time := (float_of_string timeval) /. (float_of_string factor)
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = "<" then max_time := !max_time -. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )
            | timeval::comp::factor::"global_time"::body -> ( (* "5 >= 2*global_time" *)
                match comp with
                    | ">" | ">=" | "=" -> (
                        if is_float timeval && is_float factor then max_time := (float_of_string timeval) /. (float_of_string factor)
                        else raise (InternalError ("Unable to parse constraint: " ^ Constants.global_time_clock_name ^ comp ^ timeval));
                        if comp = ">" then max_time := !max_time -. epsilon
                    )
                    | _ -> ();
                ;
                parse_time_constraint body;
            )

            | _::body -> (
		        print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constraint);
                raise (InternalError ("Unable to parse constraint d"));
            );
        in
        parse_time_constraint time_array;
        (* print_message Verbose_standard ("result: " ^ (string_of_float !max_time)); *)
		!max_time



	(* Obtain the minimum time from a state index *)
	method private state_index_to_min_time state_index =
        let source_state = StateSpace.get_state state_space state_index in
        let _, source_constraint = source_state in
        let time_constraint = LinearConstraint.px_copy source_constraint in
        let pxd_constr = LinearConstraint.pxd_of_px_constraint time_constraint in
		self#time_constr_to_val pxd_constr

		
	(* Obtain the maximum time from a state index *)
	method private state_index_to_max_time state_index =
        let source_state = StateSpace.get_state state_space state_index in
        let _, source_constraint = source_state in
        let time_constraint = LinearConstraint.px_copy source_constraint in
        let pxd_constr = LinearConstraint.pxd_of_px_constraint time_constraint in
		self#time_constr_to_max_val pxd_constr


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Helper method to print state information *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
    method private print_state_info state_index =
        let source_state = StateSpace.get_state state_space state_index in
        let _, source_constraint = source_state in
        print_message Verbose_standard ("----------\nstate:" ^ (string_of_int state_index) ^ "\n");
        print_message Verbose_standard (ModelPrinter.string_of_state model source_state);
        let time_constraint = LinearConstraint.px_copy source_constraint in
        let pxd_constr = LinearConstraint.pxd_of_px_constraint time_constraint in
		let min_time = self#time_constr_to_val pxd_constr in
        print_message Verbose_standard ("\n[min time: " ^ (string_of_float min_time) ^ "]");
        print_message Verbose_standard ("----------\n");
        ()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Creates a constraint from a float, i.e., global_time_constraint_from_float 5. -> global_time = 5. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private global_time_constraint_from_float float_time =
		(*print_message Verbose_standard ("float time:   " ^ (string_of_float float_time) );
		print_message Verbose_standard ("float time ep:   " ^ (string_of_float ( float_time +. epsilon ) ) );*)
		let time_term = LinearConstraint.make_pxd_linear_term
			[(NumConst.minus_one, self#get_global_time) ] (NumConst.numconst_of_float (float_time +. epsilon)) in
		let time_ineq = LinearConstraint.make_pxd_linear_inequality time_term LinearConstraint.Op_ge in
		let time_constr = LinearConstraint.make_pxd_constraint [time_ineq] in
(*		print_message Verbose_standard ("term:   " ^ LinearConstraint.string_of_pxd_linear_term model.variable_names time_term);
		print_message Verbose_standard ("ineq:   " ^ LinearConstraint.string_of_pxd_linear_inequality model.variable_names time_ineq);
		print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constr);*)
		LinearConstraint.pxd_hide_discrete_and_collapse time_constr

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Creates a constraint from a float bounded from below, i.e., global_time_constraint_bounded_below_from_float 5. -> global_time >= 5. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private global_time_constraint_bounded_below_from_float float_time =
		let time_term = LinearConstraint.make_pxd_linear_term
			[(NumConst.minus_one, self#get_global_time) ] (NumConst.numconst_of_float (float_time -. epsilon)) in
		let time_ineq = LinearConstraint.make_pxd_linear_inequality time_term LinearConstraint.Op_le in
		let time_constr = LinearConstraint.make_pxd_constraint [time_ineq] in
(*		print_message Verbose_standard ("term:   " ^ LinearConstraint.string_of_pxd_linear_term model.variable_names time_term);
		print_message Verbose_standard ("ineq:   " ^ LinearConstraint.string_of_pxd_linear_inequality model.variable_names time_ineq);
		print_message Verbose_standard ("constr: " ^ LinearConstraint.string_of_pxd_linear_constraint model.variable_names time_constr);*)
		LinearConstraint.pxd_hide_discrete_and_collapse time_constr


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Returns whether the given state is a target state *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private is_target_state state_location =
        match model.correctness_condition with
            | None -> raise (InternalError("A correctness property must be defined to perform optTimeQueue"))
            | Some (Unreachable unreachable_global_locations) ->
                (* Check whether the current location matches one of the unreachable global locations *)
                if State.match_unreachable_global_locations unreachable_global_locations state_location then true
                else false;
            | _ -> raise (InternalError("We only allow (un)reachability properties in optTimeQueue"));
   






	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the minimal reachability algorithm [WORK IN PROGRESS] *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method explore_layer_bfs init_state_index =
	

		(* Statistics *)
		counter_explore_using_strategy#increment;
		counter_explore_using_strategy#start;

		let options = Input.get_options() in
		
		(* (temporarily?) Removed best-worst case algorithm *)
		(*
		if options#best_worst_case then print_message Verbose_standard("NOTE: some models with nonstandard target locations (e.g. a combination of multiple targets) may have to be updated for correctness sake.");
		*)

        (* Timing info for measuring algorithm performance *)
        let t_start = Unix.gettimeofday() in (* Start time for t_found, t_opt, t_prov, and t_all *)
        let t_found = ref max_float in (* Time to first solution found (i.e., reachability) for one parameter valuation *)
        let t_opt   = ref max_float in (* Time to find optimal time (akin to UPPAAL-CORA) for single parameter valuation *)
        let t_prov  = ref max_float in (* Time to ensure that found t_opt is optimal (akin to UPAAL-CORA) *)
        let t_all   = ref max_float in (* Time to find all parameter valuations that reach target in t_opt *)


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                     Priority Queue functions                      *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

        (* Maintain a list of tuples, with (PRIOR, STATE) and return (and remove) the smallest *)

        (* Printing function for the priority queue *)
        let pq_to_string pq =
            let rec r_pq_to_string pq = match pq with
                | [] -> "";
                | (p,s)::body  -> (
                    "(" ^ (string_of_float p) ^ "," ^ (string_of_int s) ^ ") " ^ (r_pq_to_string body)
                );
            in "[ " ^ r_pq_to_string pq ^ "]"
        in

        (* Checking for containment in the priority queue *)
        (* NOTE: _ to remove unused warning *)
        let rec _pq_contains pq state_id = match pq with
            | [] -> false; (* At the end of the queue, so we have not found the state *)
            | (_,s)::body  -> (
				(* State found in the PQ *)
                if s = state_id then true
				else _pq_contains body state_id
            );
        in

        (* Add state at first possible position, such that the list is *)
        (* and return the new queue *)
        (* Assumes that the list does not already contain the element! *)
        let rec pq_add_state pq prior state_id = match pq with
            | [] -> [(prior, state_id)]; (* Add it at the end, if prior is worse than the rest *)
            | (p,s)::body  -> (
				(* State already present in the PQ, just ignore it *)
                if s = state_id then (
		            print_message Verbose_standard("State " ^ (pq_to_string [(p,s)]) ^ " already present in the queue");
					List.append [(p,s)] body
                )
				(* Insert the state when all succeeding entries have a higher priority *)
				else if prior < p then (
					List.append [(prior,state_id); (p,s)] body
				)
				(* Continue iterating over the list otherwise *)
				else List.append [(p,s)] (pq_add_state body prior state_id)
            );
        in

        (* Returns True if the queue is empty *)
        let pq_is_empty pq = match pq with
            | [] -> true;
            | _ -> false;
        in

        (* Returns the first state (so with the lowest priority) from the PQ and also returns the rest of the PQ *)
		(* (without the first element) *)
        let pq_pick_state pq = match pq with
            | [] -> raise (InternalError ("Cannot pick a state from an empty queue"));
            | (p,s)::body  -> p,s,body;
        in


		let rec pq_list_of_states pq = match pq with
            | [] -> [];
            | (p,s)::body  -> List.append [s] (pq_list_of_states body);
        in


		let pq_merge pq = (
			(* Turn pq into a list of state IDs *)
			let list_pq = pq_list_of_states pq in
			(* Merge the list_pq states *)
			let eaten_states = StateSpace.merge state_space list_pq in
			let new_states_after_merging = list_diff list_pq eaten_states in

			let rec list_to_pq pq list_pq = match pq with
				| [] -> [];
				| (p,s)::body -> (
                    (* Add in the same order *)
                    if List.mem s list_pq then List.append [(p,s)] (list_to_pq body list_pq)
                    else list_to_pq body list_pq
				);
			in
			let result = list_to_pq pq new_states_after_merging in

			(*
			let rec print_intlist intlist = match intlist with
				| [] -> "";
				| intval::body -> (string_of_int intval) ^ " " ^ (print_intlist body);
			in

			print_message Verbose_standard("old PQ: " ^ (pq_to_string pq));
			print_message Verbose_standard("original size:   " ^ (string_of_int (List.length pq)));
			print_message Verbose_standard("eaten size:      " ^ (string_of_int (List.length eaten_states) ^ " : [" ^ (print_intlist eaten_states) ^ "]"));
			print_message Verbose_standard("new states size: " ^ (string_of_int (List.length new_states_after_merging)));
			print_message Verbose_standard("result size:     " ^ (string_of_int (List.length result)));

			print_message Verbose_standard("new PQ: " ^ (pq_to_string result));*)
			result
		) in

		(* Small tests
		let tpq = ref [(self#state_index_to_min_time init_state_index, init_state_index)] in
		print_message Verbose_standard("PQ: " ^ (pq_to_string !tpq));
        tpq := pq_add_state !tpq 2.3 5;
        print_message Verbose_standard("PQ: " ^ (pq_to_string !tpq));
        tpq := pq_add_state !tpq 7. 5;
        print_message Verbose_standard("PQ: " ^ (pq_to_string !tpq));
		let _, s, pqr = pq_pick_state !tpq in
		tpq := pqr;
		print_message Verbose_standard("PQ: " ^ (pq_to_string !tpq));
			(* output:
				PQ: [ (0.,0) ]
				PQ: [ (0.,0) (2.3,5) ]
				State [ (2.3,5) ] already present in the queue
				PQ: [ (0.,0) (2.3,5) ]
				PQ: [ (2.3,5) ]
			*)
		*)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                 End of Priority Queue functions                   *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                      Visited Set functions                        *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
 
        (* Printing function for the visited set *)
        let _vis_to_string vis =
            let rec r_vis_to_string vis = match vis with
                | [] -> "";
                | s::[]  -> (string_of_int s)
                | s::body  -> (string_of_int s) ^ ", " ^ (r_vis_to_string body);
            in "{ " ^ r_vis_to_string vis ^ " }"
        in

        (* Checking for containment in the visited set *)
        let rec vis_contains vis state_id = match vis with
            | [] -> false; (* At the end of the visited set, so we have not found the state *)
            | s::body  -> (
				(* State found in the PQ *)
                if s = state_id then true
				else vis_contains body state_id
            );
        in

        (* Add state in visited set *)
        let vis_add_state vis state_id =
			if not (vis_contains vis state_id) then List.append [state_id] vis
			else vis;
        in

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                  End of Visited Set functions                     *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                     State Space Exploration                       *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(* Add initial state to the queue and visited set with the corresponding time constraint *)
		let init_time = (self#state_index_to_min_time init_state_index) in
		let pq = ref [(init_time, init_state_index)] in (* Priority queue *)
		let vis = ref [init_state_index] in	(* Visited set *)
		limit_reached <- Keep_going; (* To check whether the time limit / state limit is reached *)
        let iteration = ref 1 in (* number of iterations in the algorithm (= number of states explored) *)
		(*let find_all_min_vals = true in (* false: stop at first time at target loc, otherwise collect all constraints with min time *)*)
		let best_time_bound = ref max_float in (* prevent exploring states that exceed minimum time *)
		let algorithm_keep_going = ref true in (* terminate when we found target loc *)
        let target_found = ref false in (* indicates whether we have already found the target state *)
        let explore_successors = ref true in (* indicates whether we should explore successors *)
        let constraint_list = ref [] in (* List of constraints that reach the target location (in minimal time) *)
		let can_merge = ref false in
		let pq_add = ref 1 in

		print_message Verbose_standard("---------------- Starting exploration ----------------");

		while limit_reached == Keep_going && not (pq_is_empty !pq) && !algorithm_keep_going do
        	(*print_message Verbose_standard("PQ:  " ^ (pq_to_string !pq));*)
          	(*print_message Verbose_standard("Vis: " ^ (vis_to_string !vis));*)
        	(*print_message Verbose_standard("|PQ| = " ^ (string_of_int (List.length !pq))
                ^ " \t |Vis| = " ^ (string_of_int (List.length !vis)));*)
			let time, source_id, pqr = pq_pick_state !pq in
			pq := pqr; (* there doesn't seem to be an (easy) direct way to update PQ *)

            (*
			print_message Verbose_standard("Time: " ^ (string_of_float time)   ^ " of state " ^  (string_of_int source_id));
        	self#print_state_info source_id;*)


			(*print_message Verbose_standard("Exploring " ^ (string_of_int source_id)); *)

			(* Check time constraint and stop when we reached the limit *)
			if time > !best_time_bound then (
				print_message Verbose_standard("All states visited up to time bound");
                algorithm_keep_going := false;
                termination_status <- Some (Result.Regular_termination);
			)
            else (
                (* Check if this is the target location *)
                let source_location, source_constraint = StateSpace.get_state state_space source_id in
                if self#is_target_state source_location then (
                    (* Target state found ! (NB: assert time = upper_bound) *)
                    (* NB: We update best_time_bound in the successor part, so we should never see time < best_time_bound *)
                    		(* (temporarily?) Removed best-worst case algorithm *)
		(*

                    if options#best_worst_case then (
                        let worst_time = self#state_index_to_max_time source_id in
                        if !best_time_bound > worst_time then raise (InternalError ("Should not find better best_time_bound while "
                            ^ " exploring the source state (assuming init is not target), time: " ^ (string_of_float worst_time)
                            ^ " best_time: " ^ (string_of_float !best_time_bound )));

                        if not !target_found then (
                            (*print_message Verbose_standard("Iteration " ^ (string_of_int !iteration)
                                ^ ": Target reached in time: " ^ (string_of_float worst_time));*)
                            (* self#print_state_info source_id; *)
                            target_found := true;
                        );
                        explore_successors := false;

                        if !best_time_bound = worst_time then (
                            (* Intersect constraint with minimum time  *)
                            let time_constr = self#global_time_constraint_bounded_below_from_float !best_time_bound in
                            let target_constraint = LinearConstraint.px_intersection (time_constr::[source_constraint]) in
                            let p_constraint  = LinearConstraint.px_hide_nonparameters_and_collapse target_constraint in
                            constraint_list := p_constraint::!constraint_list
(*                            print_message Verbose_standard ("source constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names source_constraint);
                            print_message Verbose_standard ("time constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names time_constr);
                            print_message Verbose_standard ("target constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names target_constraint);
                            print_message Verbose_standard ("p constr: " ^ LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint)*)
                        )
                    )
                    else ( *)
                        if !best_time_bound <> time then raise (InternalError ("Should not find better best_time_bound while "
                            ^ " exploring the source state (assuming init is not target), time: " ^ (string_of_float time)
                            ^ " best_time: " ^ (string_of_float !best_time_bound )));

                        if not !target_found then (
                            print_message Verbose_standard("Iteration " ^ (string_of_int !iteration)
                                ^ ": Target reached in time: " ^ (string_of_float time));
                            (* self#print_state_info source_id; *)
                            target_found := true;

                            (* If target state is at the head of the PQ, we can ensure that it is the optimal one *)
                            if !t_prov == max_float then t_prov := time_from t_start;
                            print_message Verbose_standard ("t_opt:   " ^ (string_of_seconds !t_opt));
                            print_message Verbose_standard ("t_prov:  " ^ (string_of_seconds !t_prov));
                        );
                        explore_successors := false;

                        (* Intersect constraint with minimum time *)
                        let time_constr = self#global_time_constraint_from_float !best_time_bound in
                        let target_constraint = LinearConstraint.px_intersection (time_constr::[source_constraint]) in
                        let p_constraint  = LinearConstraint.px_hide_nonparameters_and_collapse target_constraint in
                        constraint_list := p_constraint::!constraint_list
(*		                print_message Verbose_standard ("source constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names source_constraint);
		                print_message Verbose_standard ("time constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names time_constr);
		                print_message Verbose_standard ("target constr: " ^ LinearConstraint.string_of_px_linear_constraint model.variable_names target_constraint);
		                print_message Verbose_standard ("p constr: " ^ LinearConstraint.string_of_p_linear_constraint model.variable_names p_constraint)*)
                    (*);*)
                );
    

                (* Don't compute successors when target is found *)
                if not !explore_successors then (
                    (* Possibly terminate when target state is found *)
                    if (options#early_terminate) then (
                        print_message Verbose_standard("Found target!");
                        algorithm_keep_going := false;
                        termination_status <- Some (Result.Regular_termination);
                    );
                    explore_successors := true; (* do explore successors in future iterations *)
                )
                (* Otherwise, compute successors *)
                else (
                    let successors = self#post_from_one_state source_id in

                    (*print_message Verbose_standard("Iteration " ^ (string_of_int !iteration) ^ ":\t State "^ (string_of_int source_id) ^
                        " has " ^ (string_of_int (List.length successors)) ^ " successors"); *)
                    
					if options#merge then can_merge := false;

                    let rec process_sucs suclist = match suclist with
                        |  [] ->  ();
                        | suc_id::body -> (
                            if not (vis_contains !vis suc_id) then (
                                let suc_time = self#state_index_to_min_time suc_id in
                                let goal_suc_time = (* only used for best_worst_case *)
                                    		(* (temporarily?) Removed best-worst case algorithm *)
		(*
if options#best_worst_case then (self#state_index_to_max_time suc_id) else
	*) suc_time in

                                (* Only add states if the time to reach does not exceed the minimum time *)
                                if suc_time <= !best_time_bound then (
                                    (* Check if the suc state is the target location, and possibly update minimum time *)
                                    let suc_location, _ = StateSpace.get_state state_space suc_id in
                                    if self#is_target_state suc_location then (
										
										if options#merge && (options#merge_heuristic = Merge_targetseen) then can_merge := true;

                                        if !t_found = max_float then (
											t_found := time_from t_start;
											print_message Verbose_standard ("t_found: " ^ (string_of_seconds !t_found));
										);
                                        if goal_suc_time < !best_time_bound then (
											print_message Verbose_standard ("Best time to target location: " ^ (string_of_float goal_suc_time));
                                            best_time_bound := goal_suc_time;
                                            t_opt := time_from t_start; (* might update several times *)
                                            constraint_list := []; (* Empty the constraint list *)
                                        );
                                        (* We ensure optimal time if suc time <= PQ.hd *)
                                        		(* (temporarily?) Removed best-worst case algorithm *)
		(*

                                        if (not options#best_worst_case) && !t_prov == max_float && suc_time <= time then
                                            t_prov := time_from t_start;
                                            *)
                                    );
                                    (* Add the suc state to the queue *)
                                    pq := pq_add_state !pq suc_time suc_id;
                                    pq_add := !pq_add + 1;
                                    vis := vis_add_state !vis suc_id;
                                );
                            )
                            else print_message Verbose_standard("Already visited state " ^ (string_of_int suc_id));
                            process_sucs body
                        );
                    in
                    process_sucs successors;
					(* Only call merging after processing all successors *)

					if options#merge then (
						can_merge := match options#merge_heuristic with
						| Merge_always -> true;
						| Merge_targetseen -> !can_merge; (* already set *)
						| Merge_pq10 -> ((!pq_add mod 10) = 0);
						| Merge_pq100 -> ((!pq_add mod 100) = 0);
						| Merge_iter10 -> ((!iteration mod 10) = 0);
						| Merge_iter100 -> ((!iteration mod 100) = 0);
					);

					if options#merge && !can_merge then (
                        let old_pq_size = List.length !pq in
						pq := pq_merge !pq;
                        print_message Verbose_standard("Merging, iteration: " ^ (string_of_int !iteration) ^ ", |PQ|: " ^ (string_of_int old_pq_size) ^ " -> " ^ (string_of_int (List.length !pq)));
					);
                );
            );

            (* Update termination condition (NOTE: Is this used correctly?) *)
            (match limit_reached with
                (* No limit: regular termination *)
                (*** NOTE: check None, as it may have been edited from outside, in which case it should not be Regular_termination ***)
                | Keep_going when termination_status = None -> termination_status <- Some (Result.Regular_termination)
                (*** NOTE: obliged to comment out the condition below, otherwise there is a compiling warning… ***)
                | Keep_going (*when termination_status <> None*) -> ()
                (* Termination due to time limit reached *)
                | Time_limit_reached -> termination_status <- Some (Result.Time_limit (List.length !pq)) (* TODO: what to give here? *)
                (* Termination due to state space depth limit reached *)
                | Depth_limit_reached -> termination_status <- Some (Result.Depth_limit (List.length !pq))
                (* Termination due to a number of explored states reached *)
                | States_limit_reached -> termination_status <- Some (Result.States_limit (List.length !pq))
            );

            iteration := !iteration + 1;
		done; (* END WHILE *)

        (* Algorithm done, so all valuations found *)
        t_all := time_from t_start;

		print_message Verbose_standard("---------------- Ending exploration ------------------");

        print_message Verbose_standard("Completed after " ^ (string_of_int !iteration) ^ " iterations.");
        print_message Verbose_standard("States remaining in priority queue: " ^ (string_of_int (List.length !pq)));

		(* Combine constraints that reach the final state with the best_time_bound *)
		print_message Verbose_standard ("We found " ^ (string_of_int (List.length !constraint_list))
			^ " constraint(s) that reach the target in min time " ^ (string_of_float !best_time_bound));
		
        print_message Verbose_standard ("");
(*        print_message Verbose_standard ("t_found: " ^ (string_of_seconds !t_found)); *)
(*        print_message Verbose_standard ("t_opt:   " ^ (string_of_seconds !t_opt)); *)
(*        print_message Verbose_standard ("t_prov:  " ^ (string_of_seconds !t_prov));*)
        print_message Verbose_standard ("t_all:   " ^ (string_of_seconds !t_all));
(*
        print_message Verbose_standard("The resulting parameter valuations is given by the union of the following constraint(s)");
		let rec output_target_constraints constr_list = match constr_list with
			| [] -> ();
			| head::body -> (
				print_message Verbose_standard ("\n"
					^ LinearConstraint.string_of_p_linear_constraint model.variable_names head);
				output_target_constraints body;
			);
		in output_target_constraints !constraint_list;
*)

        (* combine the linear constraints *)
		constraint_valuations <- Some (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraints !constraint_list);
		
		print_message Verbose_standard("---------------- Ending algorithm --------------------");
		
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                 End of State Space Exploration                    *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(* Statistics *)
		counter_explore_using_strategy#stop;
	
		(* The end *)
		()
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(* Nothing to do *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add_a_new_state source_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		(* Build the state *)
		let new_state = location, final_constraint in

		
		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()
		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			(*** NOTE: in fact not necessary for this algorithm ***)
			self#update_statespace_nature new_state;
			
			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			new_states_indexes := new_state_index :: !new_states_indexes;
			
		end; (* end if new state *)
		
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Update the transitions *)
		self#add_transition_to_state_space (source_state_index, action_index, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;
	
		(* The state is necessarily kept by the algorithm *)
		true
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state _ =
		(* Always keep the initial state *)
		true

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"State space exploration completed " ^ (after_seconds ()) ^ "."
		);

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in EFsynth.compute_result")
			| Some status -> status
		in

		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		let constr_result = match constraint_valuations with
				| None -> LinearConstraint.false_p_nnconvex_constraint()
				| Some constr -> constr
		in

		(* Return result *)
		Single_synthesis_result
		{
			result = Good_constraint (constr_result, soundness);
			(*result = Good_constraint (LinearConstraint.false_p_nnconvex_constraint(), soundness);*)

			(* Explored state space *)
			state_space			= state_space;
			
			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;
			
			(* Termination *)
			termination			= termination_status;
		}
	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
