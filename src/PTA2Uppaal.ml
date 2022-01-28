(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Translater to Uppaal
 *
 * File contributors : Étienne André
 * Created           : 2019/03/01
 * Last modified     : 2021/10/07
 *
 ************************************************************)

open Exceptions
open Constants
open OCamlUtilities
open ImitatorUtilities
open LinearConstraint
open DiscreteExpressions
open AbstractModel
open Result


(************************************************************)
(** Customized values for constraint conversion *)
(************************************************************)

let uppaal_boolean_strings : customized_boolean_string = {
	true_string     = "true";
	false_string    = "false";
	and_operator    = " &amp;&amp; ";
	or_operator     = " or "; (* useless *)
	l_operator      = " &lt; ";
	le_operator     = " &lt;= ";
	eq_operator     = " == ";
	neq_operator    = " != ";
	ge_operator     = " &gt;= ";
	g_operator      = " &gt; ";
	not_operator    = "!";
	in_operator     = " in ";
}

let uppaal_array_strings = { Constants.default_array_string with array_literal_delimiter = "{", "}" }

let all_uppaal_strings : customized_string = {
    arithmetic_string = default_arithmetic_string;
    boolean_string = uppaal_boolean_strings;
    array_string = uppaal_array_strings;
    binary_word_representation = Binary_word_representation_int;
}

let uppaal_update_separator = ", "

let uppaal_assignment = " = "


(* Positioning *)
let scaling_factor = 200

(* Customized string of discrete number type *)
let string_of_var_type_discrete_number = function
    | DiscreteType.Var_type_discrete_unknown_number
    | DiscreteType.Var_type_discrete_rational -> "int"
    | DiscreteType.Var_type_discrete_int -> "int"

(* Customized string of discrete var type *)
let rec string_of_var_type_discrete = function
    | DiscreteType.Var_type_discrete_number x -> string_of_var_type_discrete_number x
    | DiscreteType.Var_type_discrete_bool -> "bool"
    | DiscreteType.Var_type_discrete_binary_word length ->
        let warning_in_comment = if length > 31 then ", WARNING: length > 31 can lead to overflow !" else "" in
        let comment = "/* binary(" ^ string_of_int length ^ ")" ^ warning_in_comment ^ " */" in
        "int " ^ comment
    | DiscreteType.Var_type_discrete_array (inner_type, _)
    | DiscreteType.Var_type_discrete_list inner_type ->
        string_of_var_type_discrete inner_type
    | DiscreteType.Var_type_weak ->
        raise (InternalError "An expression should have a determined type. Maybe something has failed before.")


(* Customized string of var_type *)
let string_of_var_type = function
	| DiscreteType.Var_type_clock -> "clock"
	| DiscreteType.Var_type_discrete var_type_discrete -> string_of_var_type_discrete var_type_discrete
	| DiscreteType.Var_type_parameter -> "parameter"

(* Get the UPPAAL string representation of a value according to it's IMITATOR type *)
(* For example a literal array is translated from `[1,2,..,n]` to `{1,2,..,n}` *)
let rec string_of_value = function
    | DiscreteValue.Number_value x
    | DiscreteValue.Rational_value x -> NumConst.string_of_numconst x
    | DiscreteValue.Bool_value x -> if x then uppaal_boolean_strings.true_string else uppaal_boolean_strings.false_string
    | DiscreteValue.Int_value x -> Int32.to_string x
    | DiscreteValue.Binary_word_value value ->
        let length = BinaryWord.length value in

        if length > 31 then
            ImitatorUtilities.print_warning ("Encoding a binary word of length `" ^ string_of_int length ^ "` on an integer can leads to an overflow.");

        string_of_int (BinaryWord.to_int value)

    | DiscreteValue.Array_value a ->
        let string_array = Array.map (fun x -> string_of_value x) a in
        "{" ^ OCamlUtilities.string_of_array_of_string_with_sep ", " string_array ^ "}"
    | DiscreteValue.List_value l ->
        let string_list = List.map (fun x -> string_of_value x) l in
        "{" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " string_list ^ "}"


(* Get the UPPAAL string representation of a variable name according to it's IMITATOR var type *)
(* For example a variable name `x` is translated to `x[l]` if the given type is an array of length l *)
let rec string_of_discrete_name_from_var_type discrete_name = function
    | DiscreteType.Var_type_discrete discrete_type -> string_of_discrete_name_from_var_type_discrete discrete_name discrete_type
    | _ -> discrete_name

(* Get the UPPAAL string representation of a variable name according to it's IMITATOR var type *)
(* For example a variable name `x` is translated to `x[l]` if the given type is an array of length l *)
and string_of_discrete_name_from_var_type_discrete discrete_name = function
    | DiscreteType.Var_type_discrete_array (inner_type, length) ->
        string_of_discrete_name_from_var_type_discrete discrete_name inner_type
        ^ "["
        ^ string_of_int length
        ^ "]"
    | _ -> discrete_name


(************************************************************)
(** Header *)
(************************************************************)

(* Add a header to the model *)
let string_of_header model =
	"<nta>"



(************************************************************)
(** Footer *)
(************************************************************)

(* End of the file *)
let footer = ""
(*	^ "\n" ^ "/*************************************************************"
	^ "\n" ^ " * The end *"
	^ "\n" ^ " ************************************************************/"*)
	^ "</nta>"
	^ "\n" ^ ""


(************************************************************)
(** Variable declarations *)
(************************************************************)


(* Naming the discrete variables checking for strong broadcast *)
let string_of_nb_strongbroadcast model action_index =
	(* Get name *)
	let action_name = model.action_names action_index in
	(* Name it *)
	"nb__" ^ action_name


(* Create initial discrete declarations to encode strong broadcast *)
let string_of_discrete_nb_strongbroadcast model actions_and_nb_automata =
	(* Do not generate anything for empty list *)
	if actions_and_nb_automata = [] then ""
	else
		(* Comment *)
		"\n\n/* Discrete variable declarations needed to encode IMITATOR's strong broadcast into Uppaal */\n"
		^
		(* Initialize *)
		(string_of_list_of_string_with_sep "\n" (List.map (fun (action_index , nb_automata) ->
			let discrete_name = string_of_nb_strongbroadcast model action_index in
			"int " ^ discrete_name ^ " = " ^ (string_of_int nb_automata) ^ ";"
		) actions_and_nb_automata))



(* Convert the initial clocks declarations into a string *)
let string_of_clocks model =
	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map model.variable_names list_of_variables)
	in
	if model.nb_clocks > 0 then
		("\n/* Clocks declarations */\nclock " ^ (string_of_variables model.clocks_without_special_reset_clock) ^ ";")
		else ""

(* Convert the initial discrete declarations into a string *)
let string_of_discrete model =
	(* Some comment *)
	if model.nb_discrete > 0 then(
		"\n\n/* Discrete variables declarations (WARNING: these variables can be rational-valued in IMITATOR, but they become integer-valued in Uppaal) */"
		^
		(string_of_list_of_string_with_sep "\n"
			(List.map (fun discrete_index ->
				(* Get the name *)
				let discrete_name = model.variable_names discrete_index in
                let discrete_type = model.type_of_variables discrete_index in
				(* Get the initial value *)
				let initial_global_location  = model.initial_location in
				let initial_value = Location.get_discrete_value initial_global_location discrete_index in

                let str_initial_value = string_of_value initial_value in
                let str_type = string_of_var_type discrete_type in
                (* case of arrays: format name with length of array *)
                let format_discrete_name = string_of_discrete_name_from_var_type discrete_name discrete_type in

				(* Assign *)
				"\n" ^ str_type ^ " " ^ format_discrete_name ^ " = " ^ str_initial_value ^ ";"
			) model.discrete
			)
		)

	) else ""


(* Convert the parameter declarations into a string *)
let string_of_parameters model =
	(* Some comment *)
	if model.nb_parameters > 0 then(
		"\n\n/* Parameters declarations */\n"
		^
		(string_of_list_of_string_with_sep "\n"
			(List.map (fun parameter_index ->
				(* Get the name *)
				let parameter_name = model.variable_names parameter_index in

				(* Assign *)
				(*** NOTE: assign arbitrarily to 0 to allow Uppaal compiling ***)
				"const int " ^ parameter_name ^ " = 0 /* TODO: add your favorite value here */;"
			) model.parameters
			)
		)

	) else ""

(* Convert the declared actions into a string *)
let string_of_declared_actions model =
	"\n\n/* Action declarations */\n"
	^
	(string_of_list_of_string_with_sep "\n"
		(List.map (fun action_index ->
			(* Do not declare silent actions *)
			match model.action_types action_index with
			| Action_type_nosync -> ""
			| Action_type_sync ->
			(* Get name *)
			let action_name = model.action_names action_index in

			(* Get number of automata *)
			let nb_automata = List.length (model.automata_per_action action_index) in

			(* Case action unused: drop *)
			if nb_automata = 0 then "/* action " ^ action_name ^ " is unused in the model */"

			(* For action in a single automaton, we use Uppaal "broadcast chan" system *)
			else if nb_automata = 1 then "broadcast chan " ^ action_name ^ ";"

			(* For action in exactly two automata, we use Uppaal standard "chan" system *)
			else if nb_automata = 2 then "chan " ^ action_name ^ ";"

			(* For action in exactly > 2 automata, we use broadcast with a special encoding *)
			else (
(*				(* Issue a warning *)
				print_warning ("Action '" ^ action_name ^ "' is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the behavior may differ!");*)
				"broadcast chan " ^ action_name ^ "; /* This action is used in " ^ (string_of_int nb_automata) ^ " automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the correctness is ensured thanks to variable '" ^ (string_of_nb_strongbroadcast model action_index) ^ "' */"
			)

		) model.actions
		)
	)

(* `shift_left` function string in Uppaal *)
let string_of_shift_left_function =
    "int shift_left(int b, int k, int l)\n"
    ^ "{\n"
    ^ "    /* Simulate shift_left of IMITATOR by truncating binary word at length l */\n"
    ^ "    return (b &lt;&lt; k) - ((b &gt;&gt; l - k) &lt;&lt; l);\n"
    ^ "}\n\n"

(* `shift_right` function string in Uppaal *)
let string_of_shift_right_function =
    "int shift_right(int b, int k)\n"
    ^ "{\n"
    ^ "    return b &gt;&gt; k;\n"
    ^ "}\n\n"

(* `fill_left` function string in Uppaal *)
let string_of_fill_left_function =
    "int fill_left(int b, int k)\n"
    ^ "{\n"
    ^ "    return b &lt;&lt; k;\n"
    ^ "}\n\n"

(* `fill_right` function string in Uppaal *)
let string_of_fill_right_function =
    "int fill_right(int b, int k)\n"
    ^ "{\n"
    ^ "    return b &gt;&gt; k;\n"
    ^ "}\n\n"

(* `logand` function string in Uppaal *)
let string_of_logand_function =
    "int logand(int a, int b)\n"
    ^ "{\n"
    ^ "    return a &amp; b;\n"
    ^ "}\n\n"

(* `logor` function string in Uppaal *)
let string_of_logor_function =
    "int logor(int a, int b)\n"
    ^ "{\n"
    ^ "    return a | b;\n"
    ^ "}\n\n"

(* `logxor` function string in Uppaal *)
let string_of_logxor_function =
    "int logxor(int a, int b)\n"
    ^ "{\n"
    ^ "    return a ^ b;\n"
    ^ "}\n\n"

(* `lognot` function string in Uppaal *)
let string_of_lognot_function =
    (* Simulate lognot on int using following formula: *)
    (* b - f, with: *)
    (* b an arbitrary binary word of length l *)
    (* f a binary word of length l holding max value (full of ones), eg: f = 0b1111 for a binary word of length l=4  *)
    "int lognot(int b, int l)\n"
    ^ "{\n"
    ^ "    /* Simulate not bitwise operator */\n"
    ^ "    /* by generating binary word of length l with all bit at one, eg: 0b1111 */\n"
    ^ "    /* Then inverse by subtracting b */\n"
    ^ "    /* Power of two is simulated using 1 &lt;&lt; exponent */\n"
    ^ "    return ((1 &lt;&lt; l) - 1)  /* pow(2, l) - 1 */ - b; \n"
    ^ "}\n\n"

(* List of function declarations in Uppaal *)
let string_of_builtin_functions model =
    "/* Functions declarations */\n\n"
    ^ string_of_shift_left_function
    ^ string_of_shift_right_function
    ^ string_of_fill_left_function
    ^ string_of_fill_right_function
    ^ string_of_logand_function
    ^ string_of_logor_function
    ^ string_of_logxor_function
    ^ string_of_lognot_function


(* Convert the initial variable declarations into a string *)
let string_of_declarations model actions_and_nb_automata =
	(* Header *)
	"<declaration>"

	^
	(* Some comments *)
	let options = Input.get_options () in
	          "\n/************************************************************"
	^ "\n" ^ " * File automatically generated by " ^ Constants.program_name ^ ""
	^ "\n" ^ " * Version  : " ^ (ImitatorUtilities.program_name_and_version_and_nickname_and_build)
	^ "\n" ^ " * Model    : '" ^ options#model_file_name ^ "'"
	^ "\n" ^ " * Generated: " ^ (now()) ^ ""
	^ "\n" ^ " ************************************************************/"

    (* Declare built-in functions *)

    ^ string_of_builtin_functions model

	(* Declare clocks *)
	^ (string_of_clocks model)

	(* Declare discrete *)
	^ (string_of_discrete model)

	(* Declare discrete needed to encode strong broadcast *)
	^ (string_of_discrete_nb_strongbroadcast model actions_and_nb_automata)

	(* Declare parameters *)
	^ (string_of_parameters model)

	(* Declare actions *)
	^ (string_of_declared_actions model)

	(*** TODO: get the initial value of clocks from the initial constraint and, if not 0, then issue a warning ***)


	(* The initial constraint (in comment only) *)
	^ "\n" ^ ""
	^ "\n" ^ "\t/*------------------------------------------------------------*/"
	^ "\n" ^ "\t/* Initial constraint (not interpreted by Uppaal)             */"
	^ "\n" ^ "\t/*------------------------------------------------------------*/"
	^ "\n\t /* " ^ (LinearConstraint.customized_string_of_px_linear_constraint uppaal_boolean_strings model.variable_names model.initial_constraint) ^ " */"


	(* Footer *)
	^ "\n</declaration>"



(************************************************************)
(** Guard / Invariant *)
(************************************************************)

(** General function to get string of label XML tag for UPPAAL **)
let get_uppaal_label_tag_string kind x_coord_str y_coord_str content =
    "<label kind=\"" ^ kind ^ "\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">" ^ content ^ "</label>"

(*** NOTE: special handling as we have a discrete and a continuous guard that must be handled homogeneously ***)

(** Convert a guard or an invariant (according to kind) into a string *)
let string_of_guard_or_invariant kind actions_and_nb_automata variable_names x_coord_str y_coord_str = function
	(* True guard = no guard *)
	| True_guard -> ""

	(* False *)
	| False_guard ->
	    "false"

	(*** TODO: use the proper Uppaal syntax here ***)

	| Discrete_guard discrete_guard ->

        let str_discrete_guard = NonlinearConstraint.customized_string_of_nonlinear_constraint all_uppaal_strings variable_names discrete_guard in
        let str_discrete_guard_without_true = if kind = "invariant" && str_discrete_guard = "true" then "" else str_discrete_guard in
        str_discrete_guard_without_true

	| Continuous_guard continuous_guard ->
		(* Remove true guard *)
		if LinearConstraint.pxd_is_true continuous_guard then "" else
		(LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_boolean_strings variable_names continuous_guard)

	| Discrete_continuous_guard discrete_continuous_guard ->
	    let content = (
            (NonlinearConstraint.customized_string_of_nonlinear_constraint all_uppaal_strings variable_names discrete_continuous_guard.discrete_guard)
            ^
            (
                (* Remove true guard *)
                if LinearConstraint.pxd_is_true discrete_continuous_guard.continuous_guard then ""
                else uppaal_boolean_strings.and_operator ^ (LinearConstraint.customized_string_of_pxd_linear_constraint uppaal_boolean_strings variable_names discrete_continuous_guard.continuous_guard)
            )
        ) in
        content

(** Convert a guard into a string *)
let string_of_guard =
    string_of_guard_or_invariant "guard"

(** Convert an invariant into a string *)
let string_of_invariant =
    string_of_guard_or_invariant "invariant"

(** Convert a guard or an invariant into a XML string for Uppaal, ex : <label kind=\""guard\"">content</label> *)
let uppaal_string_of_guard_or_invariant kind actions_and_nb_automata variable_names x_coord_str y_coord_str guard =
    get_uppaal_label_tag_string kind x_coord_str y_coord_str (string_of_guard_or_invariant kind actions_and_nb_automata variable_names x_coord_str y_coord_str guard)

(** Convert a guard into a XML string for Uppaal, ex : <label kind=\""guard\"">content</label> *)
let uppaal_string_of_guard =
    uppaal_string_of_guard_or_invariant "guard"

(** Convert a guard into a XML string for Uppaal, ex : <label kind=\""invariant\"">content</label> *)
let uppaal_string_of_invariant =
    uppaal_string_of_guard_or_invariant "invariant"




(************************************************************)
(** Automata *)
(************************************************************)


(* Creates a unique id for each location *)
let id_of_location model automaton_index location_index =
	(* Return an id of the form id_pta0_loc0 *)
	"id_pta" ^ (string_of_int automaton_index) ^ "_loc" ^ (string_of_int location_index ^ "")


(* Convert the invariant of a location into a string *)
let string_of_invariant model actions_and_nb_automata automaton_index location_index =
	(* In addition to the "real" invariant, we also constrain the variables corresponding to the strong broadcast actions to be equal to the number of automata that declare them *)
	(*** NOTE: of course, it would be better to only add these invariants to the locations target of such an action ***)
	(*** TODO: simplify some day ***)
	let strong_broadcast_invariant =
		(string_of_list_of_string_with_sep uppaal_boolean_strings.and_operator (List.map (fun (action_index , nb_automata) ->
			let discrete_name = string_of_nb_strongbroadcast model action_index in
			discrete_name ^ uppaal_boolean_strings.eq_operator ^ (string_of_int nb_automata)
		) actions_and_nb_automata))
	in

    (* Compute coordinates *)
    (*** NOTE: arbitrary positioning (location_id * scaling_factor, +20%) ***)
    let x_coord_str, y_coord_str =
        string_of_int (location_index * scaling_factor),
        string_of_int (scaling_factor / 5)
    in
	(*** TODO: check well formed with constraints x <= … as requested by Uppaal, and issue a warning otherwise ***)

    let invariant = string_of_invariant actions_and_nb_automata model.variable_names x_coord_str y_coord_str (model.invariants automaton_index location_index) in

	(* Avoid "true and …" *)
	let invariant_and_strong_broadcast_invariant =
		if invariant = "" then strong_broadcast_invariant
		else if actions_and_nb_automata = [] then invariant
		else invariant ^ uppaal_boolean_strings.and_operator ^ strong_broadcast_invariant
	in
	(* Invariant *)
	"\n\t" ^ get_uppaal_label_tag_string "invariant" x_coord_str y_coord_str invariant_and_strong_broadcast_invariant





let string_of_clock_updates model = function
	| No_update -> ""
	| Resets list_of_clocks ->
		string_of_list_of_string_with_sep ", " (List.map (fun variable_index ->
			(model.variable_names variable_index)
			^ " = 0"
		) list_of_clocks)
	| Updates list_of_clocks_lt ->
		string_of_list_of_string_with_sep ", " (List.map (fun (variable_index, linear_term) ->
			(model.variable_names variable_index)
			^ " = "
			^ (LinearConstraint.string_of_pxd_linear_term model.variable_names linear_term)
		) list_of_clocks_lt)

(* Convert a list of updates into a string *)
let string_of_discrete_updates model updates =
	string_of_list_of_string_with_sep uppaal_update_separator (List.map (fun (variable_access, global_expression) ->
        (* Convert the variable access to string *)
		ModelPrinter.string_of_variable_access model variable_access
		^ uppaal_assignment
		(* Convert the arithmetic_expression *)
		^ DiscreteExpressions.customized_string_of_global_expression all_uppaal_strings model.variable_names global_expression
	) updates)


let string_of_updates model automaton_index action_index x_coord_str y_coord_str clock_updates discrete_updates =

	(* First add the update for the strong broadcast encoding *)

	(* Get number of automata *)
	let automata_for_this_action = model.automata_per_action action_index in
	let nb_automata = List.length automata_for_this_action in
	(* If strong broadcast encoding (i.e. >= 3) *)
	let update_strong_broadcast =
	if nb_automata >= 3 then
		(* Get discrete name *)
		let discrete_name = string_of_nb_strongbroadcast model action_index in
			discrete_name
			^
			(* Arbitrarily, the first automaton index in the list is "!" and therefore responsible for resetting the number, and the others are "?", and therefore they increment *)
			if automaton_index = List.nth automata_for_this_action 0
				(* := 1 *)
				then uppaal_assignment ^ (string_of_int 1)
				(* ++ *)
				else uppaal_assignment ^ discrete_name ^ " + 1"
	(* Otherwise, no update *)
	else ""
	in

	(* Check for emptiness of some updates *)
	let no_clock_updates =
		clock_updates = No_update || clock_updates = Resets [] || clock_updates = Updates []
	in
	let no_discrete_updates = discrete_updates = [] in

	(* If no update at all: empty string *)
	if no_clock_updates && no_discrete_updates && update_strong_broadcast = "" then ""

	else(
		(* Manage separator between clock updates and discrete updates *)
		let separator_clock_discrete =
			if no_clock_updates || no_discrete_updates then "" else uppaal_update_separator
		in

		(* Manage separator between the normal updates (clocks and discrete), and the strong broadcast updates *)
		let separator_clockdiscrete_strongbroadcast =
			if no_clock_updates && no_discrete_updates || update_strong_broadcast = "" then "" else uppaal_update_separator
		in

		"<label kind=\"assignment\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ y_coord_str ^ "\">"
		(* Clock updates *)
		^ (string_of_clock_updates model clock_updates)
		(* Add a separator in case of both clocks and discrete *)
		^ separator_clock_discrete
		(* Discrete updates *)
		^ (string_of_discrete_updates model discrete_updates)
		(* Add separator *)
		^ separator_clockdiscrete_strongbroadcast
		(* Special strong broadcast update *)
		^ update_strong_broadcast
		^ "</label>"
	)


(* Convert an action_index in automaton_index into a string; the automaton_index is important for "!" / "?" issues *)
let string_of_sync model automaton_index action_index =
	let action_name = model.action_names action_index in

	(* Different models of synchronization depending on the number of synchronizations *)

	(* Get number of automata *)
	let automata_for_this_action = model.automata_per_action action_index in
	let nb_automata = List.length automata_for_this_action in

	(* Case action unused: should not happen (but keep just in case) *)
	if nb_automata = 0 then(
		print_warning ("Action '" ^ action_name ^ "' seems to be unused in the model, so this transition is strange. This may be an error of the translator, in which case you may want to contact us.");
		(* Arbitrarily use it sending *)
		action_name ^ "!"
	)

	(* For action in a single automaton, we use Uppaal "broadcast chan" system => action "!" *)
	else if nb_automata = 1 then action_name ^ "!"

	(*** NOTE: duplicate code below; but since it is a bit a different framework, let us keep so, so far ***)

	(* For action in exactly two automata, we use Uppaal standard "chan" system *)
	else if nb_automata = 2 then(
		action_name
		^
		(* Arbitrarily, the first automaton index in the list is "!", and the other one is "?" *)
		if automaton_index = List.nth automata_for_this_action 0 then "!" else "?"
	)

	(* For action in >= 3 automata, we again use Uppaal "broadcast chan" system => action "!", with additional variables on updates/guard to ensure good behavior *)
	else
		action_name
		^
		(* Again, arbitrarily, the first automaton index in the list is "!", and the others are "?" *)
		if automaton_index = List.nth automata_for_this_action 0 then "!" else "?"



(* Convert a transition of a location into a string *)
(** TODO: Add conditions to the translation *)
let string_of_transition model actions_and_nb_automata automaton_index source_location transition =
	let clock_updates = transition.updates.clock in
	let discrete_updates = transition.updates.discrete in
	(* Arbitrary positioning: x = between source_location and target_location *)
	(*** NOTE: integer division here, so first multiplication, then division (otherwise result can be 0) ***)
	let x_coord_str = (string_of_int ((source_location + transition.target) * scaling_factor / 2)) in

	(* Header *)
	"\n\t<transition>"

	(* Source *)
	^ "\n\t\t<source ref=\"" ^ (id_of_location model automaton_index source_location) ^ "\"/>"

	(* Target *)
	^ "\n\t\t<target ref=\"" ^ (id_of_location model automaton_index transition.target) ^ "\"/>"

	(* Synchronisation label *)
	^ (
		match model.action_types transition.action with
			| Action_type_sync -> "\n\t\t<label kind=\"synchronisation\" x=\"" ^ x_coord_str ^ "\" y=\"" ^ (string_of_int (scaling_factor * 2 / 5)) ^ "\">" ^ (string_of_sync model automaton_index transition.action) ^ "</label>"
			| Action_type_nosync -> ""
	)

	(* Guard *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (scaling_factor / 5)) in
		"\n\t\t" ^ (uppaal_string_of_guard actions_and_nb_automata model.variable_names x_coord_str y_coord_str transition.guard)
	)

	(* Updates *)
	^ (
		(* Quite arbitrary positioning *)
		let y_coord_str = (string_of_int (- scaling_factor / 5)) in
		"\n\t\t" ^ (string_of_updates model automaton_index transition.action x_coord_str y_coord_str clock_updates discrete_updates)
	)

	(* Footer *)
	^ "\n\t</transition>"



(* Convert the transitions of an automaton into a string *)
let string_of_transitions model actions_and_nb_automata automaton_index =
	string_of_list_of_string (
	(* For each location *)
	List.map (fun location_index ->
		string_of_list_of_string (
		(* For each action *)
		List.map (fun action_index ->
			(* Get the list of transitions *)
			let transitions = List.map model.transitions_description (model.transitions automaton_index location_index action_index) in
			(* Convert to string *)
			string_of_list_of_string (
				(* For each transition *)
				List.map (string_of_transition model actions_and_nb_automata automaton_index location_index) transitions
				)
			) (model.actions_per_location automaton_index location_index)
		)
	) (model.locations_per_automaton automaton_index))



(* Convert a location of an automaton into a string *)
let string_of_location model actions_and_nb_automata automaton_index location_index =
	"\n"

	(* Header *)
	^ "<location id=\"" ^ (id_of_location model automaton_index location_index) ^ "\" "
	(*** NOTE: arbitrary positioning at (location_id * scaling_factor, 0) ***)
	^ "x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"0\""
	(* Add yellow color if urgent :-) *)
	^ (if model.is_urgent automaton_index location_index then " color=\"#ffff00\"" else "")
	^ ">"

	(* Name *)
	(*** NOTE: arbitrary positioning at (location_id * scaling_factor, -20%) ***)
	^ "\n\t<name x=\"" ^ (string_of_int (location_index * scaling_factor)) ^ "\" y=\"" ^ (string_of_int (- scaling_factor / 5)) ^ "\">" ^ (model.location_names automaton_index location_index) ^ "</name>"

	(* Invariant *)
	^ (string_of_invariant model actions_and_nb_automata automaton_index location_index)

	(* Urgency *)
	^ (if model.is_urgent automaton_index location_index then "<urgent/>" else "")

	(* Stopwatches *)
	(*** TODO ***)

	(* Costs: dropped for now *)
	(*** TODO ***)
	 ^ (match model.costs automaton_index location_index with
		| None -> ""
		| Some cost ->
			(* Issue warning *)
			print_warning ("Cost '" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "' is not supported in the translation to Uppaal.");
			"\n/* Cost '" ^ (LinearConstraint.string_of_p_linear_term model.variable_names cost) ^ "' is not supported in the translation to Uppaal. */\n"
	)

	(* Footer *)
	^ "</location>"


(* Convert the locations of an automaton into a string *)
let string_of_locations model actions_and_nb_automata automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location model actions_and_nb_automata automaton_index location_index
	) (model.locations_per_automaton automaton_index))

(* Convert the initial location of an automaton *)
let string_of_initial_location model automaton_index =
	(* Get initial location *)
	let initial_global_location  = model.initial_location in
	let initial_location = Location.get_location initial_global_location automaton_index in
	"<init ref=\"" ^ (id_of_location model automaton_index initial_location) ^ "\"/>"

(* Convert an automaton into a string *)
let string_of_automaton model actions_and_nb_automata automaton_index =
		(*** NOTE: arbitrary positioning at (automaton_index, automaton_index) ***)
(* 	"\n/************************************************************/" *)
	(*^*) "\n<template><name x=\"" ^ (string_of_int automaton_index) ^ "\" y=\"" ^ (string_of_int automaton_index) ^ "\">" ^ (model.automata_names automaton_index) ^ "</name><declaration>// No local declaration for automaton '" ^ (model.automata_names automaton_index) ^ "'
</declaration>"
	^ "\n " ^ (string_of_locations model actions_and_nb_automata automaton_index)
	^ "\n " ^ (string_of_initial_location model automaton_index)
	^ "\n " ^ (string_of_transitions model actions_and_nb_automata automaton_index)
	^ "\n </template>" (*/ * end " ^ (model.automata_names automaton_index) ^ " */ *)
(* 	^ "\n/************************************************************/" *)


(* Convert the automata into a string *)
let string_of_automata model actions_and_nb_automata =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in

	(* Print all (other) PTA *)
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton model actions_and_nb_automata automaton_index
	) pta_without_obs)



(************************************************************)
(** System *)
(************************************************************)
(* Create the system definition *)
let string_of_system model =
	(*** WARNING: Do not print the observer ***)
	let pta_without_obs = List.filter (fun automaton_index -> not (model.is_observer automaton_index)) model.automata
	in
	(* Open *)
	"<system>"
	(* Comment *)
	^ "\n// List one or more processes to be composed into a system."
	(* System definition *)
	^ "\n\nsystem " ^ (string_of_list_of_string_with_sep ", " (List.map model.automata_names pta_without_obs)) ^ ";"
	(* Close *)
	^ "\n</system>"



(*** TODO: add properties, projection… as comments ***)



(************************************************************)
(** Model *)
(************************************************************)

(* Convert the model into a string *)
let string_of_model model =
	(* Create a list (action_index , nb_automata for this action), needed for strong broadcast encoding *)
	let actions_and_nb_automata = List.map (fun action_index ->
		(* Get number of automata *)
		let nb_automata = List.length (model.automata_per_action action_index) in
		(* Make it a pair *)
		action_index , nb_automata
	) model.actions
	in
	(* Strong broadcast is encoded to this scheme if the number of automata is >= 3, so filter *)
	let encoding_needed = List.filter (fun (_ , nb_automata) -> nb_automata >= 3) actions_and_nb_automata in

	(* The header *)
	string_of_header model

	(* The variable declarations *)
	^  "\n" ^ (string_of_declarations model encoding_needed)

	(* All automata *)
	^  "\n" ^ (string_of_automata model encoding_needed)

	(* The system definition *)
	^  "\n" ^ (string_of_system model)

	(*** TODO ***)
	(* The property *)
(*	^ property_header
	^  "\n" ^ string_of_property model model.user_property*)

	(*** TODO ***)
	(* The projection *)
(* 	^  "\n" ^ string_of_projection model *)

	(*** TODO ***)
	(* The optimization *)
(* 	^  "\n" ^ string_of_optimization model *)

	(* The footer *)
	^  footer
