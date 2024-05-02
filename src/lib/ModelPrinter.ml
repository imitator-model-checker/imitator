(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Convert an abstract model to the input syntax of IMITATOR
 *
 * File contributors : Étienne André, Jaime Arias, Laure Petrucci
 * Created           : 2009/12/02
 *
 ************************************************************)

open OCamlUtilities
open Exceptions
open DiscreteExpressions
open AbstractModel
open AbstractProperty
open ImitatorUtilities
open State
open StateSpace


(************************************************************)
(** Getting the flows of a location *)
(************************************************************)

(*** BADPROG: very, very bad programming: this function should be in AlgoStateBased BUT ModelPrinter doesn't have access to AlgoStateBased (but the other way is possible); and it is called from both modules, so defined here (ÉA, 2021/11/02) ***)

(*------------------------------------------------------------*)
(* Compute a hashtable clock => flow in a DiscreteState.global_location *)
(* Also returns a Boolean being true iff there is any non-1 flow *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_gen (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : (((Automaton.clock_index, NumConst.t) Hashtbl.t) * bool) =
	(* Hashtbl clock_id --> flow *)
	let flows_hashtable = Hashtbl.create (List.length model.clocks) in
	
	(* Maintain a Boolean to see if any clock has a rate different from 1 *)
	let flow_mode = ref false in
	
	(* Update hash table *)
	List.iter (fun automaton_index ->
		(* Get the current location *)
		let location_index = DiscreteState.get_location location automaton_index in
		
		(* 1. Manage the list of stopped clocks *)
		let stopped = model.stopwatches automaton_index location_index in
		(* If list non null: we have flows <> 1 *)
		if stopped <> [] then flow_mode := true;
		(* Add each clock *)
		List.iter (fun stopwatch_id ->
			Hashtbl.replace flows_hashtable stopwatch_id NumConst.zero
		) stopped;
		
		(* 2. Manage the explicit flows *)
		let flows = model.flow automaton_index location_index in
		(* Add each clock *)
		List.iter (fun (clock_id, flow_value) ->
			(* If flow <> 1, update Boolean *)
			if NumConst.neq flow_value NumConst.one then flow_mode := true;

			(* Compare with previous value *)
			try(
				(* Get former value *)
				let former_flow_value = Hashtbl.find flows_hashtable clock_id in
				(* Compare *)
				if NumConst.neq former_flow_value flow_value then(
					
					(*** TODO: a flag should be raised somewhere so that the result is said to be perhaps wrong! (or unspecified) ***)
					
					print_warning ("Clock `" ^ (model.variable_names clock_id) ^ "` is assigned to two different flow values at the same time (`" ^ (NumConst.string_of_numconst flow_value) ^ "` in location `" ^ (model.location_names automaton_index location_index) ^ "`, as well as `" ^ (NumConst.string_of_numconst former_flow_value) ^ "`). The behavior becomes unspecified!");
				);
				(* Do not add *)
				()
			) with Not_found ->(
			(* Not found: not yet defined => add *)
				Hashtbl.add flows_hashtable clock_id flow_value
			);
			
		) flows;
		
	) model.automata;
	
	(* Return the hash table and the Boolean *)
	flows_hashtable , !flow_mode


(*------------------------------------------------------------*)
(* Compute the list of clocks with their flow in a DiscreteState.global_location *)
(* Returns a list of pairs (clock_index, flow)                *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_list (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : ((Automaton.clock_index * NumConst.t) list) =
	(* Call generic function *)
	let flows_hashtable, non_1_flow = compute_flows_gen model location in

	(* If there are no explicit flows then just return the set of clocks with flow 1 *)
	if (not non_1_flow) then (List.map (fun clock_id -> clock_id, NumConst.one) model.clocks) else (
		(* Computing the list of clocks with their flow *)
		List.map (fun clock_id ->
			(* Try to get the clock explicit flow *)
			try(
				(* Get value *)
				let flow_value = Hashtbl.find flows_hashtable clock_id in
				(* Return *)
				clock_id, flow_value
			) with Not_found ->
				(* Absent: flow is 1 *)
				clock_id, NumConst.one
		) model.clocks
	) (* if no explicit flow for this location *)



(*------------------------------------------------------------*)
(* Compute the functional flow function in a DiscreteState.global_location *)
(* Returns a function clock_index -> flow                     *)
(* Raises a warning whenever a clock is assigned to TWO different flows *)
(*------------------------------------------------------------*)
let compute_flows_fun (model : AbstractModel.abstract_model) (location : DiscreteState.global_location) : (Automaton.clock_index -> NumConst.t) =
	(* Call generic function *)
	let flows_hashtable, non_1_flow = compute_flows_gen model location in

	(* If there are no explicit flows then just return the set of clocks with flow 1 *)
	if (not non_1_flow) then (fun _ -> NumConst.one) else (
		(* Return the functional view *)
		(fun clock_id ->
			(* Try to get the clock explicit flow *)
			try(
				(* Try to get value *)
				Hashtbl.find flows_hashtable clock_id
			) with Not_found ->
				(* Absent: flow is 1 *)
				NumConst.one
		)
	) (* if no explicit flow for this location *)


(************************************************************)
(** Constants *)
(************************************************************)
let string_of_true		= "True"
let string_of_false		= "False"
let string_of_accepting	= "accepting"



(************************************************************)
(** JSON *)
(************************************************************)
let json_TRUE	= "True"
let json_FALSE	= "False"

let json_escape_ampersand str =
	(Str.global_replace (Str.regexp "\n") (" ")
		(Str.global_replace (Str.regexp "&") ("AND") str)
	)

(************************************************************)
(* Parameter valuation (PVal.pval) *)
(************************************************************)

(** Convert a parameter valuation (PVal.pval) into a string *)
let string_of_pval model pval =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(model.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pval#get_value parameter))
		) model.parameters
	)
	)

(** Convert a parameter valuation (PVal.pval) into a JSON-like string *)
let json_of_pval model pval =
	(* Hack for empty model *)
	if model.nb_parameters = 0 then
	    JsonFormatter.Json_null
	else
	    JsonFormatter.Json_struct (
            List.map (fun parameter ->
                model.variable_names parameter, JsonFormatter.Json_string (NumConst.string_of_numconst (pval#get_value parameter))
            ) model.parameters
	    )

(************************************************************)
(** V0 *)
(************************************************************)
(* Convert a V0 into a string *)
let string_of_v0 model v0 =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(model.variable_names parameter)
			^ " = "
			^ (
(* 				let min_bound, max_bound = v0.(parameter) in *)
				let min_bound = v0#get_min parameter in
				let max_bound = v0#get_max parameter in
				if min_bound = max_bound
					then NumConst.string_of_numconst min_bound
					else (NumConst.string_of_numconst min_bound) ^ ".." ^ (NumConst.string_of_numconst max_bound)
			)
		) model.parameters
	)
	)



(************************************************************)
(** Header *)
(************************************************************)

let common_header =
	          "(************************************************************"
	^ "\n" ^ " * File automatically generated by " ^ Constants.program_name ^ ""
	^ "\n" ^ " * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n" ^ " * Git      : " ^ (ImitatorUtilities.git_branch_and_hash)



(* Add a header to the model *)
let model_header() =
	let options = Input.get_options () in
	common_header
	^ "\n" ^ " * Model    : '" ^ options#model_file_name ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************)"

(* Add a header to the property *)
let property_header() =
	let options = Input.get_options () in
	common_header
	^ "\n" ^ " * Model    : '" ^ (match options#property_file_name with Some property_file_name -> property_file_name | None -> raise (InternalError "Property file name not found in `ModelPrinter.property_header()`")) ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************)"
	^ "\n"



(************************************************************)
(** Footer *)
(************************************************************)

(** End of the file *)
let footer = "\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* The end *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "end"
	^ "\n" ^ ""

(************************************************************)
(** Controllable actions *)
(************************************************************)

(* Controllable actions *)
let string_of_controllable_actions (model : AbstractModel.abstract_model) : string =
	if model.controllable_actions = [] then "" else(
		"\ncontrollable actions: " ^ (string_of_list_of_string_with_sep ", " (List.map model.action_names model.controllable_actions)) ^ ";\n"
	)


(************************************************************)
(** Variable declarations *)
(************************************************************)

(* Convert a var_type_discrete into a string *)
(* let string_of_var_type_discrete = DiscreteType.string_of_var_type_discrete *)

(* Convert a var_type into a string *)
let string_of_var_type = DiscreteType.string_of_var_type

(* Convert discrete variable declarations group (by type) into a string *)
let string_of_discrete_variables_by_type var_type variable_names =
	if List.length variable_names > 0 then (
	    (* Get string of all variable names of the group *)
	    let string_of_variable_names = string_of_list_of_string_with_sep ", " variable_names in
	    (* Get string of declaration group *)
		("\n\t" ^ string_of_variable_names ^ "\n\t\t: " ^ (string_of_var_type var_type) ^ ";\n")
    )
    else ""

(* Convert discrete variable declarations into a string *)
let string_of_discrete_variables model =
    (* Get string of each variable groups (by type) *)
    let str = List.map (fun (var_type, variable_names) -> string_of_discrete_variables_by_type var_type variable_names) model.discrete_names_by_type_group in
    (* Join all strings *)
    string_of_list_of_string str

(* Convert the initial variable declarations into a string *)
let string_of_declarations model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_declarations`…"; *)

	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map model.variable_names list_of_variables) in

		"var "
	^
	(if model.nb_clocks > 0 then
		("\n\t" ^ (string_of_variables model.clocks_without_special_reset_clock) ^ "\n\t\t: clock;\n") else "")
	^
	(string_of_discrete_variables model)
	^
	(if model.nb_parameters > 0 then
		("\n\t" ^ (string_of_variables model.parameters) ^ "\n\t\t: parameter;\n") else "")

(* Count the number of instruction in a code bloc *)
let count_instructions (* seq_code_bloc *) =
    let rec count_instructions code_bloc =
        List.fold_left (fun acc x -> acc + count_instruction x) 0 code_bloc

    and count_instruction = function
        | For_loop (_, _, _, _, inner_bloc)
        | While_loop (_, inner_bloc) ->
            2 + count_instructions inner_bloc
        | If (_, then_bloc, else_bloc_opt) ->
            1 + count_instructions then_bloc
            + (match else_bloc_opt with None -> 0 | Some else_bloc -> 1 + count_instructions else_bloc)
        | Local_decl _
        | Assignment _
        | Instruction _
        | Clock_assignment _ -> 1
    in
    count_instructions (* seq_code_bloc *)

(* Convert loop direction into a string *)
let string_of_loop_dir = function Loop_up -> " to " | Loop_down -> " downto "

(* Convert a sequential code bloc into a string *)
let string_of_seq_code_bloc model level ?(sep=" ") (* seq_code_bloc *) =

    let rec string_of_seq_code_bloc level code_bloc =
        let str_instructions = List.map (string_of_instruction level) code_bloc in
        OCamlUtilities.string_of_list_of_string_with_sep sep str_instructions

    and string_of_instruction level instruction =

        (* Create tabs according to level *)
        let tabs, _  = OCamlUtilities.string_n_times level "  ", OCamlUtilities.string_n_times (level + 1) "  " in

        match instruction with
        | Local_decl ((variable_name, _), discrete_type, init_expr) ->
            tabs
            ^ "var " ^ variable_name ^ " : "
            ^ DiscreteType.string_of_var_type_discrete discrete_type
            ^ " = "
            ^ DiscreteExpressions.string_of_global_expression model.variable_names init_expr
            ^ ";"

        | For_loop ((variable_name, _), from_expr, to_expr, loop_dir, inner_bloc) ->
            tabs ^ "for " ^ variable_name ^ " from "
            ^ DiscreteExpressions.string_of_int_arithmetic_expression model.variable_names from_expr
            ^ string_of_loop_dir loop_dir
            ^ DiscreteExpressions.string_of_int_arithmetic_expression model.variable_names to_expr
            ^ " do\n"
            ^ string_of_seq_code_bloc (level + 1) inner_bloc ^ "\n"
            ^ tabs ^ "done\n"

        | While_loop (condition_expr, inner_bloc) ->
            tabs ^ "while "
            ^ DiscreteExpressions.string_of_boolean_expression model.variable_names condition_expr
            ^ " do\n"
            ^ string_of_seq_code_bloc (level + 1) inner_bloc ^ "\n"
            ^ tabs ^ "done\n"

        | If (condition_expr, then_bloc, else_bloc_opt) ->
            (* Get string of else bloc if defined *)
            let str_else_bloc =
                match else_bloc_opt with
                | Some _ ->
                    tabs ^ "else\n" ^ string_of_seq_code_bloc (level + 1) then_bloc ^ "\n"
                | None -> ""
            in

            tabs ^ "if "
            ^ DiscreteExpressions.string_of_boolean_expression model.variable_names condition_expr
            ^ " then\n"
            ^ string_of_seq_code_bloc (level + 1) then_bloc ^ "\n"
            ^ str_else_bloc
            ^ tabs ^ "end\n"

        | Assignment discrete_update ->
            tabs ^ DiscreteExpressions.string_of_discrete_update model.variable_names discrete_update ^ ";"

        | Instruction expr ->
            tabs ^ DiscreteExpressions.string_of_global_expression model.variable_names expr ^ ";"

        | Clock_assignment (clock_index, expr) ->
            let clock_name = model.variable_names clock_index in
            tabs
            ^ clock_name
            ^ " := "
            ^ DiscreteExpressions.string_of_rational_arithmetic_expression model.variable_names expr
            ^ ";"
    in

    string_of_seq_code_bloc level (* seq_code_bloc *)





(* Convert the function definitions into a string *)
let string_of_fun_definitions model =

    (* Convert a function definition into a string *)
    let string_of_fun_definition fun_def =

        (* Convert function into a string *)
        let string_of_fun_type = function
            | Fun_builtin _ -> "" (* Don't print builtin functions *)
            | Fun_user (code_bloc, return_expr_opt) ->
                let parameters_signature, return_type_constraint = FunctionSig.split_signature fun_def.signature_constraint in
                let parameter_names = List.map first_of_tuple fun_def.parameter_refs in
                let parameter_names_with_constraints = List.combine parameter_names parameters_signature in
                (* Convert parameters into a string *)
                let str_param_list = List.map (fun (param_name, type_constraint) -> param_name ^ " : " ^ FunctionSig.string_of_type_constraint type_constraint) parameter_names_with_constraints in
                let str_params = OCamlUtilities.string_of_list_of_string_with_sep ", " str_param_list in

                (* Convert code bloc into a string *)
                let str_code_bloc = string_of_seq_code_bloc model 1 ~sep:"\n" code_bloc in
                (* Convert return expr into a string *)
                let str_return_expr =
                    match return_expr_opt with
                    | Some return_expr -> "\n  return " ^ DiscreteExpressions.string_of_global_expression model.variable_names return_expr
                    | None -> ""
                in

                (* Get whole string body *)
                let str_body = str_code_bloc ^ str_return_expr in

                "function " ^ fun_def.name ^ "(" ^ str_params ^ ") : " ^ FunctionSig.string_of_type_constraint return_type_constraint ^ " begin\n"
                ^ str_body
                ^ "\nend"
        in



        (* Format function definition *)
        string_of_fun_type fun_def.body

    in

    (* Convert hashtbl values to list *)
    let fun_definition_list = model.functions_table |> Hashtbl.to_seq_values |> List.of_seq |> List.rev in
    (* Map each definition to it's string representation *)
    let str_fun_definitions_list = List.map string_of_fun_definition fun_definition_list in
    (* Join all strings *)
    OCamlUtilities.string_of_list_of_string_with_sep_without_empty_strings "\n\n" str_fun_definitions_list

(* Get string of a global expression *)
let string_of_global_expression = DiscreteExpressions.string_of_global_expression
(* Get string of an arithmetic expression *)
let string_of_arithmetic_expression = DiscreteExpressions.string_of_arithmetic_expression
(* Get string of a boolean expression *)
let string_of_boolean_expression = DiscreteExpressions.string_of_boolean_expression
(* Get string of a discrete boolean expression *)
let string_of_discrete_boolean_expression = DiscreteExpressions.string_of_discrete_boolean_expression


(************************************************************)
(** Guard *)
(************************************************************)

(*** NOTE: special handling as we have a discrete and a continuous guard that must be handled homogeneously ***)

let customized_string_of_guard customized_boolean_string variable_names = function
	| True_guard -> LinearConstraint.string_of_true
	| False_guard -> LinearConstraint.string_of_false
	| Discrete_guard discrete_guard ->
	    DiscreteExpressions.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_guard
	| Continuous_guard continuous_guard ->
	    LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard ->
		DiscreteExpressions.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_continuous_guard.discrete_guard
		^ LinearConstraint.string_of_and
		^ LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard

(** Convert a guard into a string *)
let string_of_guard = customized_string_of_guard Constants.global_default_string

(** Convert a guard into a JSON-style string *)
let json_of_guard variable_names guard =
	let customized_boolean_string = Constants.global_default_string in
	match guard with
	| True_guard -> json_TRUE
	| False_guard -> json_FALSE
	| Discrete_guard discrete_guard -> json_escape_ampersand (DiscreteExpressions.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_guard)
	| Continuous_guard continuous_guard -> json_escape_ampersand (LinearConstraint.string_of_pxd_linear_constraint variable_names continuous_guard)
	| Discrete_continuous_guard discrete_continuous_guard ->
		(*** HACK for now (2021/12/09): just replace the " & " with some suited string ***)
		(json_escape_ampersand (DiscreteExpressions.customized_string_of_nonlinear_constraint customized_boolean_string variable_names discrete_continuous_guard.discrete_guard))
		^ " AND " ^
		(json_escape_ampersand (LinearConstraint.string_of_pxd_linear_constraint variable_names discrete_continuous_guard.continuous_guard))


(************************************************************)
(** Automata *)
(************************************************************)

(* Convert the actions declaration of an automaton into a string *)
let string_of_actions_declaration model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_actions_declaration(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	"actions: "
	^ (let synclabs, _ = (List.fold_left (fun (synclabs, first) action_index ->
		match model.action_types action_index with
		(* Case sync: declare *)
		| Action_type_sync ->
			synclabs
			^ (if not first then ", " else "")
			^ (model.action_names action_index)
			(* Not the first one anymore *)
			, false
		(* Case nosync: do not declare *)
		| Action_type_nosync -> (synclabs, first)
	) ("", true) (model.actions_per_automaton automaton_index)) in synclabs)
	^ ";"


(** Convert the invariant and stopwatches and flows of a location into a string *)
let string_of_invariant_and_flows model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_invariant_and_flows(" ^ (model.automata_names automaton_index) ^ ", " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)
	
	(* Invariant *)
	"invariant "
	^ (string_of_guard model.variable_names (model.invariants automaton_index location_index))

	(* Handle stopwatches *)
	^
	(let stopped = model.stopwatches automaton_index location_index in
	(* Case 1: no stopwatches *)
	if stopped = [] then ""
	(* Case 2: some clocks stopped *)
	else(
	let stopped_str = string_of_list_of_string_with_sep "," (List.map model.variable_names stopped) in
	" stop{" ^ stopped_str ^ "}"
	)
	)

	(* Handle flow *)
	^
	(
	let flow = model.flow automaton_index location_index in
	(* Case 1: no explicit flow *)
	if flow = [] then ""
	(* Case 2: some flow *)
	else(
	let flow_str = string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, flow_value) -> (model.variable_names variable_index) ^ "' = " ^ (NumConst.string_of_numconst flow_value) ) flow) in
	" flow{" ^ flow_str ^ "}"
	)
	)


(* Convert an action into a string *)
let string_of_action model (action_index : Automaton.action_index) =
	match model.action_types action_index with
	| Action_type_sync -> " sync " ^ (model.action_names action_index)
	| Action_type_nosync -> " (* sync " ^ (model.action_names action_index) ^ "*) "

(* Convert an action into a JSON-like string *)
let json_of_action model (action_index : Automaton.action_index) =
	match model.action_types action_index with
	| Action_type_sync -> (model.action_names action_index)
	| Action_type_nosync -> "(silent)"


(*(** generic template for converting clock updates into string *)
let string_of_clock_updates_template _(*variable_names*) clock_updates wrap_reset wrap_expr sep =
	match clock_updates with
		| No_update -> ""
		| Resets list_of_clocks ->
			string_of_list_of_string_with_sep sep (List.map (fun variable_index ->
				wrap_reset variable_index
			) list_of_clocks)
		| Updates list_of_clocks_lt ->
			string_of_list_of_string_with_sep sep (List.map (fun (variable_index, linear_term) ->
				wrap_expr variable_index linear_term
			) list_of_clocks_lt)*)

(*(** Convert a clock update into a string *)
let string_of_clock_updates variable_names clock_updates =
	let sep = ", " in
	let wrap_reset variable_index =  (variable_names variable_index) ^ " := 0" in
	let wrap_expr variable_index linear_term = (variable_names variable_index)
			^ " := "
			^ (LinearConstraint.string_of_pxd_linear_term variable_names linear_term) in
	string_of_clock_updates_template variable_names clock_updates wrap_reset wrap_expr sep*)

let customized_string_of_scalar_or_index_update_type customized_string variable_names scalar_or_index_update_type =
    DiscreteExpressions.customized_string_of_scalar_or_index_update_type customized_string variable_names scalar_or_index_update_type

let string_of_scalar_or_index_update_type variable_names scalar_or_index_update_type =
    DiscreteExpressions.string_of_scalar_or_index_update_type variable_names scalar_or_index_update_type

let json_of_seq_code_bloc variable_names seq_code_bloc =

    let rec json_of_seq_code_bloc seq_code_bloc =
        List.map json_of_instruction seq_code_bloc

    and json_of_instruction = function
        | Assignment (scalar_or_index_update_type, expr) ->
            DiscreteExpressions.string_of_scalar_or_index_update_type variable_names scalar_or_index_update_type, JsonFormatter.Json_string (DiscreteExpressions.string_of_global_expression variable_names expr)

        | Clock_assignment (clock_index, expr) ->
            variable_names clock_index, JsonFormatter.Json_string (DiscreteExpressions.string_of_rational_arithmetic_expression variable_names expr)

        | Local_decl ((variable_name, _), discrete_type, expr) ->
            "declaration", JsonFormatter.Json_struct [
                "name", JsonFormatter.Json_string variable_name;
                "type", JsonFormatter.Json_string (DiscreteType.string_of_var_type_discrete discrete_type);
                "value", JsonFormatter.Json_string (DiscreteExpressions.string_of_global_expression variable_names expr)
            ]

        | Instruction expr ->
            "expr", JsonFormatter.Json_string (DiscreteExpressions.string_of_global_expression variable_names expr)

        | For_loop ((variable_name, _), from_expr, to_expr, loop_dir, inner_bloc) ->
            "for", JsonFormatter.Json_struct [
                "name", JsonFormatter.Json_string variable_name;
                "from", JsonFormatter.Json_string (DiscreteExpressions.string_of_int_arithmetic_expression variable_names from_expr);
                "to", JsonFormatter.Json_string (DiscreteExpressions.string_of_int_arithmetic_expression variable_names to_expr);
                "dir", JsonFormatter.Json_string (string_of_loop_dir loop_dir);
                "bloc", JsonFormatter.Json_struct (json_of_seq_code_bloc inner_bloc)
            ]

        | While_loop (condition_expr, inner_bloc) ->
            "while", JsonFormatter.Json_struct [
                "condition", JsonFormatter.Json_string (DiscreteExpressions.string_of_boolean_expression variable_names condition_expr);
                "bloc", JsonFormatter.Json_struct (json_of_seq_code_bloc inner_bloc)
            ]

        | If (condition_expr, then_bloc, else_bloc_opt) ->
            let json_if = [
                "condition", JsonFormatter.Json_string (DiscreteExpressions.string_of_boolean_expression variable_names condition_expr);
                "then", JsonFormatter.Json_struct (json_of_seq_code_bloc then_bloc);
            ] in

            let json_else =
                match else_bloc_opt with
                Some else_bloc -> ["else", JsonFormatter.Json_struct (json_of_seq_code_bloc else_bloc)]
                | None -> []
            in

            "if", JsonFormatter.Json_struct (json_if @ json_else)
    in
    JsonFormatter.Json_struct (List.map json_of_instruction seq_code_bloc)


(*(** Return if there is no clock updates *)
let no_clock_updates clock_updates =
	clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []*)

(* Convert a transition into a string *)
let string_of_transition model automaton_index (transition : transition) =
	(* Print some information *)
    (* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transition(" ^ (model.automata_names automaton_index) ^ ")` with target `" ^ (model.location_names automaton_index transition.target) ^ "` via action `" ^ (string_of_action model transition.action) ^ "`…"); *)

    (* Get update code bloc*)
	let _, seq_code_bloc_updates = transition.updates in
	(* Convert the updates *)
    let str_do =
        let nb_instructions = count_instructions seq_code_bloc_updates in
        let tab_level, str_do, str_do_end, sep =
            if nb_instructions < 4 then
                0, " do {", "}", " "
            else
                3, " do {\n", "\n\t}", "\n"
        in

        let str_updates = string_of_seq_code_bloc model tab_level ~sep:sep seq_code_bloc_updates in
        str_do ^ str_updates ^ str_do_end

    in
	"\n\t" ^ "when "
	(* Convert the guard *)
	^ (string_of_guard model.variable_names transition.guard)
    (* Convert do *)
	^ str_do
	(* Convert the sync *)
	^ (string_of_action model transition.action)
	(* Convert the target location *)
	^ " goto " ^ (model.location_names automaton_index transition.target)
	^ ";"


(* Convert a transition into a string: version for runs *)
let string_of_transition_for_runs model automaton_index (transition : transition) =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transition(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

    let _, seq_code_bloc_updates = transition.updates in

	"[PTA " ^ (model.automata_names automaton_index) ^ ": guard{"
	(* Convert the guard *)
	^ (string_of_guard model.variable_names transition.guard)

	(* Convert the updates *)
	^ "} updates{"
	(* Sequential updates *)
	^ (string_of_seq_code_bloc model 0 seq_code_bloc_updates)
	^ "} "

	(* Convert the sync *)
	^ (string_of_action model transition.action)
	(* Convert the target location *)
	^ " Target " ^ (model.location_names automaton_index transition.target)
	^ "] "

(* Convert a transition into a JSON-like string *)
let json_of_transition model automaton_index (transition : transition) =

    let _, seq_code_bloc_updates = transition.updates in

    JsonFormatter.Json_struct [
        "transition", JsonFormatter.Json_struct [
        	(* PTA name *)
            "PTA", JsonFormatter.Json_string (model.automata_names automaton_index);
        	(* Guard *)
            "guard", JsonFormatter.Json_string (json_of_guard model.variable_names transition.guard);
            (* Updates *)
            "updates",  json_of_seq_code_bloc model.variable_names seq_code_bloc_updates;
        ]
    ]


(* Convert the transitions of a location into a string *)
let string_of_transitions model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_transitions(" ^ (model.automata_names automaton_index) ^ ": " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)

	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index ->
		(* Print some information *)
(* 		print_message Verbose_total ("Retrieving transitions via `" ^ (string_of_action model action_index) ^ "`…"); *)

		(* Get the list of transitions *)
		let transitions = model.transitions automaton_index location_index action_index in
		
		(* Print some information *)
(* 		print_message Verbose_total ("Transitions retrieved."); *)

		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition model automaton_index) (List.map model.transitions_description transitions)
			)
		) (model.actions_per_location automaton_index location_index)
	)


(** Convert a location of an automaton into a string *)
let string_of_location model automaton_index location_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_location(" ^ (model.automata_names automaton_index) ^ ": " ^ (model.location_names automaton_index location_index) ^ ")`…"); *)

	"\n"
	^ (if model.is_urgent automaton_index location_index then "urgent " else "")
	^ (if model.is_accepting automaton_index location_index then "accepting " else "")
	^ "loc "
	^ (model.location_names automaton_index location_index)
	 ^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost -> "[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "]"
	)
	^ ": "
	^ (string_of_invariant_and_flows model automaton_index location_index) (* bug here! *)
	^ (string_of_transitions model automaton_index location_index)


(* Convert the locations of an automaton into a string *)
let string_of_locations model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_locations(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		(* Print some information *)
(* 		print_message Verbose_total ("Location_index : " ^ (string_of_int location_index)); *)
		
		string_of_location model automaton_index location_index
	) (model.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton model automaton_index =
	(* Print some information *)
(* 	print_message Verbose_total ("Entering `ModelPrinter.string_of_automaton(" ^ (model.automata_names automaton_index) ^ ")`…"); *)

	"\n(************************************************************)"
	^ "\n automaton " ^ (model.automata_names automaton_index)
	^ "\n(************************************************************)"
	^ "\n " ^ (string_of_actions_declaration model automaton_index)
	^ "\n " ^ (string_of_locations model automaton_index)
	^ "\n end (* " ^ (model.automata_names automaton_index) ^ " *)"
	^ "\n(************************************************************)"


(* Convert the automata into a string *)
let string_of_automata model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_automata`…"; *)

	(*	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in*)

	(* Print all (other) PTA *)
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model automaton_index
	) model.automata)

(* Convert initial state of locations to string *)
let string_of_new_initial_locations ?indent_level:(i=1) model =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in

	(* Handle all (other) PTA *)
	let inital_global_location  = model.initial_location in
	let initial_automata = List.map
	(fun automaton_index ->
		(* Finding the initial location for this automaton *)
		let initial_location = DiscreteState.get_location inital_global_location automaton_index in
		(* '& loc[pta] = location' *)
		let tabulations = string_n_times i "\t" in
		tabulations ^ "loc[" ^ (model.automata_names automaton_index) ^ "] := " ^ (model.location_names automaton_index initial_location)
	) pta_without_obs
	in string_of_list_of_string_with_sep ", \n" initial_automata

(* Convert initial state of discrete variables to string *)
let string_of_new_initial_discretes ?indent_level:(i=1) model =
	let initial_discrete = List.map
	(fun discrete_index ->
		(* Finding the initial value for this discrete *)
		let initial_value = DiscreteState.get_discrete_value model.initial_location discrete_index in
		(* '& var = val' *)
		let tabulations = string_n_times i "\t" in
		tabulations ^ (model.variable_names discrete_index) ^ " := " ^ (AbstractValue.string_of_value initial_value)
	) model.discrete
	in string_of_list_of_string_with_sep ", \n" initial_discrete

(************************************************************)
(** Initial state *)
(************************************************************)
let string_of_initial_state model =
	(* Header of initial state *)
	"\n"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ "(* Initial state *)"
	^ "\n" ^ "(************************************************************)"
	^ "\n" ^ ""
	^ "\n" ^ "init := {"
    ^ "\n"
	(* Discrete zone *)
    ^ "\n" ^ "\tdiscrete = "
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n" ^ "\t\t(* Initial location *)"
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n"
    ^ (string_of_new_initial_locations ~indent_level:2 model) ^ ","
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n" ^ "\t\t(* Initial discrete variables assignments *)"
	^ "\n" ^ "\t\t(*------------------------------------------------------------*)"
    ^ "\n"
    ^ (string_of_new_initial_discretes ~indent_level:2 model)
    ^ "\n" ^ "\t;"
    ^ "\n"
	(* Continuous zone *)
	^ "\n" ^ "\t(*------------------------------------------------------------*)"
	^ "\n" ^ "\t(* Initial continuous constraint *)"
	^ "\n" ^ "\t(*------------------------------------------------------------*)"

    ^ "\n" ^ "\tcontinuous = "
    ^ "\n\t\t& " ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names model.initial_constraint)
    ^ "\n" ^ "\t;"
    ^ "\n"
	^ "\n" ^ "}"

(************************************************************)
(** Property *)
(************************************************************)

(** Convert a timed_interval to a string *)
let string_of_timed_interval (model : AbstractModel.abstract_model) = function
	| Zero_closed_interval duration ->
		"[0, " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration) ^ "]"

	| Zero_open_interval duration ->
		"[0, " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration) ^ ")"

	| Closed_closed_interval (duration_1, duration_2) ->
		"[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_1) ^ ", " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_2) ^ "]"

	| Closed_open_interval (duration_1, duration_2) ->
		"[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_1) ^ ", " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_2) ^ ")"

	| Open_closed_interval (duration_1, duration_2) ->
		"(" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_1) ^ ", " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_2) ^ "]"

	| Open_open_interval (duration_1, duration_2) ->
		"(" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_1) ^ ", " ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration_2) ^ ")"

	| Closed_infinity_interval duration ->
		"[" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration) ^ ", infinity]"

	| Open_infinity_interval duration ->
		"(" ^ (LinearConstraint.string_of_p_linear_term model.variable_names duration) ^ ", infinity]"


(** Convert a state_predicate to a string *)

let string_of_loc_predicate (model : AbstractModel.abstract_model) = function
	| Loc_predicate_EQ (automaton_index , location_index) ->
		"loc[" ^ (model.automata_names automaton_index) ^ "] = " ^ (model.location_names automaton_index location_index)
	| Loc_predicate_NEQ (automaton_index , location_index) ->
		"loc[" ^ (model.automata_names automaton_index) ^ "] =/= " ^ (model.location_names automaton_index location_index)


let string_of_simple_predicate (model : AbstractModel.abstract_model) = function
	| State_predicate_discrete_boolean_expression discrete_boolean_expression ->
		string_of_discrete_boolean_expression model.variable_names discrete_boolean_expression

	| Loc_predicate loc_predicate ->
		string_of_loc_predicate model loc_predicate

	| State_predicate_true -> string_of_true
	
	| State_predicate_false -> string_of_false
	
	| State_predicate_accepting -> string_of_accepting

	
let rec string_of_state_predicate_factor model = function
	| State_predicate_factor_NOT state_predicate_factor ->
		"not(" ^ (string_of_state_predicate_factor model state_predicate_factor) ^ ")"
	| Simple_predicate simple_predicate ->
		string_of_simple_predicate model simple_predicate
	| State_predicate state_predicate ->
		string_of_state_predicate model state_predicate

and string_of_state_predicate_term model = function
	| State_predicate_term_AND (state_predicate_term_1 , state_predicate_term_2) ->
		(string_of_state_predicate_term model state_predicate_term_1)
		^
		" && "
		^
		(string_of_state_predicate_term model state_predicate_term_2)
	| State_predicate_factor state_predicate_factor ->
		string_of_state_predicate_factor model state_predicate_factor


and string_of_state_predicate (model : AbstractModel.abstract_model) = function
	| State_predicate_OR (state_predicate_1 , state_predicate_2) ->
		(string_of_state_predicate model state_predicate_1)
		^
		" || "
		^
		(string_of_state_predicate model state_predicate_2)
		
	| State_predicate_term state_predicate_term ->
		string_of_state_predicate_term model state_predicate_term
		



(** Convert the projection to a string, if any *)
let string_of_projection (model : AbstractModel.abstract_model) property =
	match property.projection with
	| None -> ""
	| Some parameter_index_list ->
		"\nprojectresult(" ^ (string_of_list_of_string_with_sep ", " (List.map model.variable_names parameter_index_list)) ^ ");"


(** Convert a property to a string *)
let string_of_abstract_property (model : AbstractModel.abstract_model) property =
	let header = property_header() in
	let prefix = "property := " in
	
	header
	^
	prefix
	
	^
	
	(* Handle synthesis_type *)
	(
	match property.synthesis_type with
	| Exemplification	-> "#exemplify"
	| Synthesis			-> "#synth"
	| Witness			-> "#exhibit"
	)
	
	^
	
	(
	(* Handle the actual property *)
	match property.property with
		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| Valid -> "Valid"

		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
		(* Reachability *)
		| EF state_predicate -> "EF(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		(* Safety *)
		| AGnot state_predicate ->
			(* Print some information *)
(* 			print_message Verbose_high "Converting an AGnot property to a string…"; *)
			
			(* Convert the state predicate *)
			let state_predicate_str = string_of_state_predicate model state_predicate in
			
			(* Print some information *)
(* 			print_message Verbose_total "State predicate converted"; *)

			"AGnot(" ^ state_predicate_str ^ ")"

		(* Global invariant *)
		| AG state_predicate -> "AG(" ^ (string_of_state_predicate model state_predicate) ^ ")"

		(* Exists globally *)
		| EG state_predicate -> "EG(" ^ (string_of_state_predicate model state_predicate) ^ ")"

		(* Exists release *)
		| ER (state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")R(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Exists until *)
		| EU (state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")U(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Exists weak until *)
		| EW (state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")W(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Unavoidability *)
		| AF state_predicate -> "AF(" ^ (string_of_state_predicate model state_predicate) ^ ")"

		(* Always release *)
		| AR (state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")R(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Always until *)
		| AU (state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")U(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Always weak until *)
		| AW (state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")W(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(*------------------------------------------------------------*)
		(* Non-nested CTL: timed version *)
		(*------------------------------------------------------------*)
		(* Reachability with timing constraint *)
		| EF_timed (timed_interval, state_predicate) -> "EF_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate) ^ ")"

		(* Exists until with timing constraint *)
		| EU_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")U_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Exists release with timing constraint *)
		| ER_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")R_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Exists weak until with timing constraint *)
		| EW_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "E(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")W_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Unavoidability with timing constraint *)
		| AF_timed (timed_interval, state_predicate) -> "AF_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate) ^ ")"

		(* Always release with timing constraint *)
		| AR_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")R_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Always until with timing constraint *)
		| AU_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")U_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"

		(* Always weak until with timing constraint *)
		| AW_timed (timed_interval, state_predicate_phi, state_predicate_psi) -> "A(" ^ (string_of_state_predicate model state_predicate_phi) ^ ")W_" ^ (string_of_timed_interval model timed_interval) ^ "(" ^ (string_of_state_predicate model state_predicate_psi) ^ ")"


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
		
		(* Reachability with minimization of a parameter valuation *)
		| EFpmin (state_predicate , parameter_index) ->
			"EFpmin(" ^ (string_of_state_predicate model state_predicate) ^ ", " ^ (model.variable_names parameter_index) ^ ")"
		
		(* Reachability with maximization of a parameter valuation *)
		| EFpmax (state_predicate , parameter_index) ->
			"EFpmax(" ^ (string_of_state_predicate model state_predicate) ^ ", " ^ (model.variable_names parameter_index) ^ ")"
		
		(* Reachability with minimal-time *)
		| EFtmin state_predicate ->
			"EFpmin(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		

		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
		(* Accepting infinite-run (cycle) through a state predicate *)
		| Cycle_through state_predicate ->
			if state_predicate = (State_predicate_term (State_predicate_factor (Simple_predicate State_predicate_true))) then "Cycle"
			else "CycleThrough(" ^ (string_of_state_predicate model state_predicate) ^ ")"
		
		(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
		| Cycle_through_generalized state_predicate_list ->
			"CycleThrough(" ^ (string_of_list_of_string_with_sep " , " (List.map (string_of_state_predicate model) state_predicate_list)) ^ ")"
		
		(* Infinite-run (cycle) with non-Zeno assumption *)
		| NZ_Cycle -> "NZCycle"
		
		
		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)
		
		(* Deadlock-free synthesis *)
		| Deadlock_Freeness -> "DeadlockFree"
		
		
		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
		(* Inverse method with complete, non-convex result *)
		| IM pval -> "TracePreservation(" ^ (string_of_pval model pval) ^ ")"

		(* Non-complete, non-deterministic inverse method with convex result *)
		| ConvexIM pval -> "IMconvex(" ^ (string_of_pval model pval) ^ ")"

		| PRP (state_predicate, pval) -> "PRP(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_pval model pval) ^ ")"

		| IMK pval -> "IMK(" ^ (string_of_pval model pval) ^ ")"

		| IMunion pval -> "IMunion(" ^ (string_of_pval model pval) ^ ")"

		
		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
		
		(* Cartography *)
		| Cover_cartography (hyper_rectangle, step) ->
			"BCcover(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"

		(* Cover the whole cartography using learning-based abstractions *)
		| Learning_cartography (state_predicate, hyper_rectangle, step) ->
			"BClearn(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
		| Shuffle_cartography (hyper_rectangle, step) ->
			"BCshuffle(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(* Look for the border using the cartography*)
		| Border_cartography (hyper_rectangle, step) ->
			"BCborder(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(* Randomly pick up values for a given number of iterations *)
		| Random_cartography (hyper_rectangle, nb, step) ->
			"BCrandom(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (string_of_int nb)  ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
		| RandomSeq_cartography (hyper_rectangle, nb, step) ->
			"BCrandomseq(" ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (string_of_int nb)  ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"

		(* Parametric reachability preservation *)
		| PRPC (state_predicate , hyper_rectangle, step) ->
			"PRPC(" ^ (string_of_state_predicate model state_predicate) ^ " , " ^ (string_of_v0 model hyper_rectangle) ^ ", " ^ (NumConst.string_of_numconst step)  ^ ")"
		
		(*------------------------------------------------------------*)
		(* Games *)
		(*------------------------------------------------------------*)

		(* Parametric timed game: reachability condition *)
		| Win state_predicate -> "Win(" ^ (string_of_state_predicate model state_predicate) ^ ")"


		(*** TODO ***)
(* 		| _ -> raise (NotImplemented "ModelPrinter.string_of_property for any other algorithm") *)
	)
	
	^
	
	(* Final (optional) semi-colon *)
	";"
	
	^
	
	(* Add the projection, if any *)
	(string_of_projection model property)


(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* Print some information *)
(* 	print_message Verbose_total "Entering `ModelPrinter.string_of_model`…"; *)
	
	(* The header *)
	model_header ()
	(* Controllable actions *)
	^  "\n" ^ string_of_controllable_actions model
	(* The variable declarations *)
	^  "\n" ^ string_of_declarations model
	(* The function definitions *)
	^  "\n" ^ string_of_fun_definitions model
	(* All automata *)
	^  "\n" ^ string_of_automata model
	(* The initial state *)
	^ "\n" ^ string_of_initial_state model
	(* The footer *)
	^  "\n" ^ footer



(************************************************************)
(** PX-valuation *)
(************************************************************)
(* Convert a valuation into a string *)
let string_of_valuation variables variable_names valuation =
	string_of_list_of_string_with_sep " & " (
		List.map (fun variable ->
			(variable_names variable)
			^ " = "
			^ (NumConst.string_of_numconst (valuation variable))
		) variables
	)

(* Convert a valuation into a JSON-like string *)
let json_of_valuation variables variable_names valuation =
    JsonFormatter.Json_struct (
		List.map (fun variable ->
		    variable_names variable, JsonFormatter.Json_string (NumConst.string_of_numconst (valuation variable))
		) variables
	)

(* Convert a px-valuation into a string *)
let string_of_px_valuation model = string_of_valuation model.parameters_and_clocks model.variable_names

(* Convert an x-valuation into a string *)
let string_of_x_valuation model = string_of_valuation model.clocks model.variable_names

(* Convert a px-valuation into a JSON-like string *)
let json_of_px_valuation model = json_of_valuation model.parameters_and_clocks model.variable_names






(************************************************************)
(** States *)
(************************************************************)

(* Convert a state into a string *)
let string_of_state model (state : state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	"" ^ (DiscreteState.string_of_location model.automata_names model.location_names model.variable_names (if options#output_float then DiscreteState.Float_display else DiscreteState.Exact_display) state.global_location) ^ " ==> \n&" ^ (LinearConstraint.string_of_px_linear_constraint model.variable_names state.px_constraint) ^ ""


(* Convert a concrete state (locations, discrete variables valuations, continuous variables valuations, current flows for continuous variables) *)
let string_of_concrete_state model (state : State.concrete_state) =
	(* Retrieve the input options *)
	let options = Input.get_options () in

	""
	(* Convert location *)
	^ (DiscreteState.string_of_location model.automata_names model.location_names model.variable_names (if options#output_float then DiscreteState.Float_display else DiscreteState.Exact_display) state.global_location)
	(* Convert variables valuations *)
	^ " ==> \n" ^ (string_of_px_valuation model state.px_valuation)
	(* Convert rates *)
	^ " flows["
	^ (
		let global_location : DiscreteState.global_location = state.global_location in
		let flows : (Automaton.clock_index * NumConst.t) list = compute_flows_list model global_location in
		(* Iterate *)
		string_of_list_of_string_with_sep ", " (
			List.map (fun (variable_index, flow) -> (model.variable_names variable_index ) ^ "' = "  ^ (NumConst.string_of_numconst flow) ) flows
		)
	)
	^ "]"

(* Convert a global location into JSON-style string (locations, NO discrete variables valuations) *)
let json_of_global_location model (global_location : DiscreteState.global_location) =
    JsonFormatter.Json_struct (
		List.map (fun automaton_index ->
			(* Retrieve location for `automaton_index` *)
			let location_index = DiscreteState.get_location global_location automaton_index in

			(* Get names *)
			let automaton_name = model.automata_names automaton_index in
			let location_name = model.location_names automaton_index location_index in

			(* Convert *)
			automaton_name, JsonFormatter.Json_string location_name
		) model.automata
	)

(* Convert the values of the discrete variables in a global location into JSON-style string *)
let json_of_discrete_values model (global_location : DiscreteState.global_location) =
    JsonFormatter.Json_struct (
		List.map (fun discrete_index ->
			(* Retrieve valuation for `discrete_index` *)
			let variable_value = DiscreteState.get_discrete_value global_location discrete_index in

			(* Convert to strings *)
			let variable_name = model.variable_names discrete_index in
			let variable_valuation = AbstractValue.string_of_value variable_value in

			(* Convert *)
			variable_name, JsonFormatter.Json_string variable_valuation
		) model.discrete
	)

(* Convert a concrete state into JSON-style string (locations, discrete variables valuations, continuous variables valuations, current flows for continuous variables) *)
let json_of_concrete_state model (state : State.concrete_state) =
    let global_location = state.global_location in
    let flows = compute_flows_list model global_location in
    "state", JsonFormatter.Json_struct [
        "location", json_of_global_location model state.global_location;
        "discrete_variables", json_of_discrete_values model state.global_location;
        "continuous_variables", json_of_px_valuation model state.px_valuation;
        "flows", JsonFormatter.Json_struct (
            List.map (fun (variable_index, flow) ->
                model.variable_names variable_index, JsonFormatter.Json_string (NumConst.string_of_numconst flow)
            ) flows
        )
    ]



(************************************************************)
(** Runs conversion to strings *)
(************************************************************)


(* Function to pretty-print combined transitions *)
let debug_string_of_combined_transition model combined_transition = string_of_list_of_string_with_sep ", " (
	List.map (fun transition_index ->
		(* Get automaton index *)
		let automaton_index = model.automaton_of_transition transition_index in
		(* Get actual transition *)
		let transition = model.transitions_description transition_index in
		(* Convert *)
		string_of_transition_for_runs model automaton_index transition
	) combined_transition
)

(** Convert a symbolic run to a string (for debug-purpose) *)
let debug_string_of_symbolic_run model (state_space : StateSpace.stateSpace) (symbolic_run : StateSpace.symbolic_run) =
	(* Iterate *)
	let steps_string = string_of_list_of_string_with_sep "\n" (List.map (fun (symbolic_step : StateSpace.symbolic_step)  ->
		(* Get actual state *)
		let state = state_space#get_state symbolic_step.source in
	
		  (" " ^ (string_of_state model state))
		^ ("\n | ")
		^ ("\n | via combined transition " ^ (debug_string_of_combined_transition model symbolic_step.transition))
		^ ("\n | ")
		^ ("\n v ")
	) symbolic_run.symbolic_steps) in
	
	(* Get the state *)
	let target_state = state_space#get_state symbolic_run.final_state in
	
	(* Add target and return *)
	steps_string ^ (" " ^ (string_of_state model target_state))




let debug_string_of_concrete_steps model concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep "\n" (List.map (fun (concrete_step : StateSpace.concrete_step)  ->
		  ("\n | ")
		^ ("\n | via d = " ^ (NumConst.string_of_numconst concrete_step.time))
		^ ("\n | followed by combined transition " ^ (debug_string_of_combined_transition model concrete_step.transition))
		^ ("\n | ")
		^ ("\n v ")
		^ (" " ^ (string_of_concrete_state model concrete_step.target))
	) concrete_steps))


let debug_string_of_arbitrary_or_impossible_concrete_step_gen (step_description : string) model (impossible_concrete_step : StateSpace.impossible_concrete_step) =
		("\n | ")
	^ ("\n | via d = " ^ (NumConst.string_of_numconst impossible_concrete_step.time))
	^ ("\n | followed by " ^ step_description ^ " transition labeled with " ^ (model.action_names impossible_concrete_step.action))
	^ ("\n | ")
	^ ("\n v ")
	^ (" " ^ (string_of_concrete_state model impossible_concrete_step.target))

let debug_string_of_impossible_concrete_step = debug_string_of_arbitrary_or_impossible_concrete_step_gen "impossible"

let debug_string_of_arbitrary_concrete_steps model impossible_concrete_steps =
	(* Iterate on following steps *)
	(string_of_list_of_string_with_sep ",\n" (List.map (fun (impossible_concrete_step : StateSpace.impossible_concrete_step)  ->
		debug_string_of_arbitrary_or_impossible_concrete_step_gen "arbitrary" model impossible_concrete_step
	) impossible_concrete_steps))



(** Convert a concrete run to a string (for debug-purpose) *)
let debug_string_of_concrete_run model (concrete_run : StateSpace.concrete_run) =
	(* First recall the parameter valuation *)
	"Concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then convert the initial state *)
	^ "\n" ^ (string_of_concrete_state model concrete_run.initial_state)
	
	(* Iterate on following steps *)
	^ (debug_string_of_concrete_steps model concrete_run.steps)



(** Convert an impossible_concrete_run to a string (for debug-purpose) *)
let debug_string_of_impossible_concrete_run model (impossible_concrete_run : StateSpace.impossible_concrete_run) =
	(* First recall the parameter valuation *)
	"Impossible concrete run for parameter valuation:"
	^ "\n" ^ (string_of_pval model impossible_concrete_run.p_valuation)
	
	^ "\n"
	
	(* Then print the initial state *)
	^ "\n" ^ (string_of_concrete_state model impossible_concrete_run.initial_state)
	
	(* Iterate on following concrete steps *)
	^ (debug_string_of_concrete_steps model impossible_concrete_run.steps)
	
	(*** NOTE: only the first step is impossible; others are "arbitrary" ***)
	^ (match impossible_concrete_run.impossible_steps with
	| [] -> ""
	| first_step :: following_steps ->
		(* Convert the first impossible step *)
		(debug_string_of_impossible_concrete_step model first_step)

		(* Iterate on following impossible steps *)
		^ (debug_string_of_arbitrary_concrete_steps model following_steps)
	)


(************************************************************)
(** Runs conversion to JSON *)
(************************************************************)

(* Function to pretty-print combined transitions *)
let json_of_combined_transition model combined_transition =
    JsonFormatter.Json_array (
        List.map (fun transition_index ->
            (* Get automaton index *)
            let automaton_index = model.automaton_of_transition transition_index in

            (* Get actual transition *)
            let transition = model.transitions_description transition_index in

            (* Convert *)
            json_of_transition model automaton_index transition
        ) combined_transition
    )


let json_of_model_transition model (concrete_step : StateSpace.concrete_step) =
    (* Get the action: a little tricky, as it is stored in *each* of transition of the combined transition *)
    (*** NOTE: we get it arbitrarily from the first transition of the combined transition ***)
    let first_transition_index : AbstractModel.transition_index = (List.hd (concrete_step.transition)) in
    let first_transition : AbstractModel.transition = model.transitions_description first_transition_index in
    let action_index : Automaton.action_index = first_transition.action in
    (* Convert action to string *)
    let action_name = json_of_action model action_index in
    "transition", JsonFormatter.Json_struct [
        "nature", JsonFormatter.Json_string "concrete";
        "duration", JsonFormatter.Json_string (NumConst.string_of_numconst concrete_step.time);
        "action", JsonFormatter.Json_string action_name;
        "transitions", json_of_combined_transition model concrete_step.transition
    ]

let json_of_concrete_steps model concrete_steps =
    List.map (fun (concrete_step : StateSpace.concrete_step)  ->
        JsonFormatter.Json_struct [json_of_model_transition model concrete_step] ::
        [JsonFormatter.Json_struct [json_of_concrete_state model concrete_step.target]]
    ) concrete_steps |> List.flatten


let json_of_arbitrary_or_impossible_concrete_step_gen (step_description : string) model (impossible_concrete_step : StateSpace.impossible_concrete_step) =
	(* Convert action to string *)
	let action_name = json_of_action model impossible_concrete_step.action in

	(* Begin transition *)
	[
        JsonFormatter.Json_struct [
            "transition", JsonFormatter.Json_struct [
                "nature", JsonFormatter.Json_string step_description;
                "duration", JsonFormatter.Json_string (NumConst.string_of_numconst impossible_concrete_step.time);
                "action", JsonFormatter.Json_string action_name;
            ]
        ];
        JsonFormatter.Json_struct [
            (* Target state *)
            json_of_concrete_state model impossible_concrete_step.target
        ]
    ]

let json_of_impossible_concrete_step = json_of_arbitrary_or_impossible_concrete_step_gen "impossible"

let json_of_arbitrary_concrete_steps model impossible_concrete_steps =
	(* Iterate on following steps *)
	List.map (fun (impossible_concrete_step : StateSpace.impossible_concrete_step)  ->
		json_of_arbitrary_or_impossible_concrete_step_gen "arbitrary" model impossible_concrete_step
	) impossible_concrete_steps |> List.flatten


(** Convert a concrete run to a JSON-style string *)
let json_of_concrete_run model (concrete_run : StateSpace.concrete_run) =

    (* Convert states and transition to JSON array *)
    let json_init_state = JsonFormatter.Json_struct [json_of_concrete_state model concrete_run.initial_state] in
    let json_concrete_steps = json_of_concrete_steps model concrete_run.steps in
    let json_steps = JsonFormatter.Json_array (json_init_state :: json_concrete_steps) in

    let json_concrete_run =
        JsonFormatter.Json_struct [
            "run", JsonFormatter.Json_struct [
                "nature", JsonFormatter.Json_string "concrete";
                "valuation", json_of_pval model concrete_run.p_valuation;
                "steps", json_steps;
            ]
        ]
    in
    JsonFormatter.to_string ~pretty:true json_concrete_run

(** Convert an impossible_concrete_run to a JSON-style string *)
let json_of_impossible_concrete_run model (impossible_concrete_run : StateSpace.impossible_concrete_run) =

    (* Convert states and transition to JSON array *)
    let json_init_state = JsonFormatter.Json_struct [json_of_concrete_state model impossible_concrete_run.initial_state] in
    let json_concrete_steps = json_of_concrete_steps model impossible_concrete_run.steps in
    let concrete_steps = json_init_state :: json_concrete_steps in

	(*** NOTE: only the first step is impossible; others are "arbitrary" ***)
	let impossible_steps =
        match impossible_concrete_run.impossible_steps with
        | [] -> []
        | first_step :: following_steps ->
            (* Convert the first impossible step *)
            (* And get following impossible steps *)
            json_of_impossible_concrete_step model first_step @
            json_of_arbitrary_concrete_steps model following_steps
	in

	let json_steps = JsonFormatter.Json_array (concrete_steps @ impossible_steps) in

	(* First recall the parameter valuation *)
	let json_run = JsonFormatter.Json_struct [
	    "run", JsonFormatter.Json_struct [
	        "nature", JsonFormatter.Json_string "negative";
	        "valuation", json_of_pval model impossible_concrete_run.p_valuation;
	        "steps", json_steps;
	    ]
	]
	in
    JsonFormatter.to_string ~pretty:true json_run