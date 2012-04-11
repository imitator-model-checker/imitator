(************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2011/11/22
 * Last modified: 2012/04/10
 *
 ************************************************************)


open Global
open AbstractImitatorFile


(************************************************************
 Global variable
************************************************************)
let id_transition = ref 0



(************************************************************
 Functions
************************************************************)

(* Convert a var_type into a string *)
let string_of_var_type = function
	| Var_type_clock -> "clock"
	| Var_type_discrete -> "discrete"
	| Var_type_parameter -> "parameter"


(* Add a header to the program *)
let string_of_header program =
	          "<!-- ************************************************************"
	^ "\n" ^" * Program " ^ program.options#file
	^ "\n" ^" * Converted by " ^ program_name ^ " " ^ version_string
(* 	^ "\n" ^" * Generated at time " ^ time? *)
	^ "\n" ^" ************************************************************ -->"
	^ "\n"
	^ "\n" ^ "<!-- ************************************************************"
	^ "\n" ^" * !! Experimental translation !!"
	^ "\n" ^" *   - All automata are defined into one file (but in independent GML structures)"
	^ "\n" ^" *   - All variables are declared in all automata."
	^ "\n" ^" *   - Initial constraint (on all variables) is added to each automaton."
	^ "\n" ^" *   - We suppose that automata synchronize on variables and actions sharing the same names (common behavior)."
	^ "\n" ^" * This translation will be improved by the definition of synchronization rules conform with FML. Work in progress."
	^ "\n" ^" ************************************************************ -->"
	^ "\n"

	
	
	
	
	(** TO DO: ADD INITIAL CONSTRAINT !!!!! *)
	
	
	

(* Convert the initial variable declarations into a string *)
let string_of_declarations program =
	let string_of_variables type_string list_of_variables =
		string_of_list_of_string (List.map (fun variable_index ->
			  "\n\t\t\t<attribute name=\"" ^ type_string ^ "\">"
			^ "\n\t\t\t\t<attribute name=\"name\">" ^ (program.variable_names variable_index) ^ "</attribute>"
			^ "\n\t\t\t</attribute>"
		) list_of_variables)
	in
	(* VARIABLES *)
	"\n\n\t" ^ " <attribute name=\"variables\">"
	^
	(if program.nb_clocks > 0 then
		("\n\t\t" ^ " <attribute name=\"clocks\">"
			^ (string_of_variables "clock" program.clocks)
			^ "\n\t\t" ^ "</attribute>"
	) else "")
	^
	(if program.nb_discrete > 0 then
		("\n\t\t" ^ " <attribute name=\"discretes\">"
			^ (string_of_variables "discrete" program.discrete)
			^ "\n\t\t" ^ "</attribute>"
	) else "")

	^ "\n\t" ^ "</attribute>"
	
	(* CONSTANTS AND PARAMETERS *)
	^ "\n\n\t" ^ " <attribute name=\"constants\">"
	^
	(if program.nb_parameters > 0 then
		("\n\t\t" ^ " <attribute name=\"parameters\">"
			^ (string_of_variables "parameter" program.parameters)
			^ "\n\t\t" ^ "</attribute>"
	) else "")

	^ "\n\t" ^ "</attribute>"


(* Convert a sync into a string *)
let string_of_sync program label =
	match program.action_types label with
	| Action_type_sync -> "\n\t\t<!-- Nosync " ^ (program.action_names label) ^ " -->"
	| Action_type_nosync -> "\n\t\t<attribute name=\"label\">" ^ (program.action_names label) ^ "</attribute>"
	



let string_of_clock_updates program = function
	| No_update -> ""
	| Resets list_of_clocks -> 
			string_of_list_of_string (List.map (fun variable_index ->
		"\n\t\t\t<attribute name=\"update\">"
		^ "\n\t\t\t\t<attribute name=\"name\">" ^ (program.variable_names variable_index) ^ "</attribute>"
		^ "\n\t\t\t\t<attribute name=\"expr\">"
		^ "\n\t\t\t\t\t<attribute name=\"const\">0</attribute>"
		^ "\n\t\t\t\t</attribute>"
		^ "\n\t\t\t</attribute>"
	) list_of_clocks)
	| Updates list_of_clocks_lt ->
			string_of_list_of_string (List.map (fun (variable_index, linear_term) ->
		"\n\t\t\t<attribute name=\"update\">"
		^ "\n\t\t\t\t<attribute name=\"name\">" ^ (program.variable_names variable_index) ^ "</attribute>"
		^ "\n\t\t\t\t<attribute name=\"expr\">"
		^ (LinearConstraint.gml_of_linear_term program.variable_names 5 linear_term)
		^ "\n\t\t\t\t</attribute>"
		^ "\n\t\t\t</attribute>"
	) list_of_clocks_lt)


(* Convert a list of updates into a string *)
let string_of_updates program updates =
	string_of_list_of_string (List.map (fun (variable_index, linear_term) ->
		"\n\t\t\t<attribute name=\"update\">"
		^ "\n\t\t\t\t<attribute name=\"name\">" ^ (program.variable_names variable_index) ^ "</attribute>"
		^ "\n\t\t\t\t<attribute name=\"expr\">"
		^ (LinearConstraint.gml_of_linear_term program.variable_names 5 linear_term)
		^ "\n\t\t\t\t</attribute>"
		^ "\n\t\t\t</attribute>"
	) updates)


  (*  <arc id="5" arcType="transition" source="1" target="2">
        <attribute name="label">a</attribute>
        <attribute name="guard">
            <attribute name="boolExpr">
                <attribute name="less">
                    <attribute name="expr">
                        <attribute name="name">y</attribute>
                    </attribute>
                    <attribute name="expr">
                        <attribute name="const">4</attribute>
                    </attribute>
                </attribute>
            </attribute>
        </attribute>
        <attribute name="updates">
            <attribute name="update">
                <attribute name="name">x</attribute>
                <attribute name="expr">
                    <attribute name="const">0</attribute>
                </attribute>
            </attribute>
        </attribute>
    </arc>*)
    
    
(* Convert a transition of a location into a string *)
let string_of_transition program automaton_index action_index location_index (guard, clock_updates, discrete_updates, destination_location) =
	(* Increment the counter *)
	id_transition := !id_transition + 1;
	
	(* Convert source and dest *)
	"\n\t<arc id=\"" ^ (string_of_int !id_transition) ^ "\" arcType=\"transition\" source=\"" ^ (string_of_int location_index) ^ "\" target=\"" ^ (string_of_int destination_location) ^ "\">"
	
	(* Convert the action *)
	^ (string_of_sync program action_index)
	^
	(* Convert the guard if any *)
	(if not (LinearConstraint.is_true guard) then (
		"\n\t\t<attribute name=\"guard\">"
		^ "\n\t\t\t<attribute name=\"boolExpr\">"
		^ (LinearConstraint.gml_of_linear_constraint program.variable_names 4 guard)
		^ "\n\t\t\t</attribute>"
		^ "\n\t\t</attribute>"
	) else "")
	^
	(* Convert the updates if any*)
	(if clock_updates != No_update || List.length discrete_updates > 0 then (
			"\n\t\t<attribute name=\"updates\">"
			^ (string_of_clock_updates program clock_updates)
			^ (string_of_updates program discrete_updates)
			^ "\n\t\t</attribute>"
		) else "")
	^ "\n\t</arc>"



(* Convert the transitions of a location into a string *)
let string_of_transitions_for_one_location program automaton_index location_index =
	string_of_list_of_string (
	(* For each action *)
	List.map (fun action_index -> 
		(* Get the list of transitions *)
		let transitions = program.transitions automaton_index location_index action_index in
		(* Convert to string *)
		string_of_list_of_string (
			(* For each transition *)
			List.map (string_of_transition program automaton_index action_index location_index) transitions
			)
		) (program.actions_per_location automaton_index location_index)
	)

(* Convert the transitions into a string *)
let string_of_transitions program automaton_index =
	string_of_list_of_string_with_sep "\n" (List.map (fun location_index ->
		string_of_transitions_for_one_location program automaton_index location_index
	) (program.locations_per_automaton automaton_index))


(* Convert a location of an automaton into a string *)
let string_of_location program automaton_index location_index =
	let inital_global_location  = program.initial_location in
	let initial_location = Automaton.get_location inital_global_location automaton_index in
	
	(* ID and name *)
	"\n\t<node id=\"" ^ (string_of_int location_index) ^ "\" nodeType=\"state\">"
	^ "\n\t\t<attribute name=\"name\">" ^ (program.location_names automaton_index location_index) ^ "</attribute>"
	
	(* Init ? *)
	^ (if initial_location = location_index then (
		"\n\t\t<attribute name=\"type\">"
		^ "\n\t\t\t<attribute name=\"initialState\"/>"
        ^ "\n\t\t</attribute>"
	) else "")
	
	^
	(* Invariant only if not true *)
	let invariant = program.invariants automaton_index location_index in
	(if (not (LinearConstraint.is_true invariant)) then (
		  "\n\t\t<attribute name=\"invariant\">"
		^ "\n\t\t\t<attribute name=\"boolExpr\">"
		^ (LinearConstraint.gml_of_linear_constraint program.variable_names 4 invariant)
		^ "\n\t\t\t</attribute>"
		^ "\n\t\t</attribute>"
	) else "")
	
	^ "\n\t</node>"


(* Convert the locations of an automaton into a string *)
let string_of_locations program automaton_index =
	string_of_list_of_string_with_sep "\n " (List.map (fun location_index ->
		string_of_location program automaton_index location_index
	) (program.locations_per_automaton automaton_index))


(* Convert an automaton into a string *)
let string_of_automaton program declarations_string automaton_index =
	         "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
	^ "\n" ^ "<model formalismUrl=\"http://alligator.lip6.fr/parametric-timed-automaton.fml\""
	^ "\n" ^ "    xmlns=\"http://gml.lip6.fr/model\">"

	^ "\n<!-- ************************************************************"
	^ "\n automaton " ^ (program.automata_names automaton_index)
	^ "\n ************************************************************ -->"
	(* Declarations *)
	^ declarations_string
	(* Description of states *)
	^ "\n " ^ (string_of_locations program automaton_index)
	(* Description of transitions *)
 	^ "\n " ^ (string_of_transitions program automaton_index)
 	(* The end *)
	^ "\n" ^ "</model>"


(* Convert the automata into a string *)
let string_of_automata program declarations_string =
	id_transition := 0;
	string_of_list_of_string_with_sep "\n\n" (
		List.map (fun automaton_index -> string_of_automaton program declarations_string automaton_index
	) program.automata)


(* Convert an automaton into a string *)
let string_of_program program =
	(* Compute the declarations *)
	let declarations_string = string_of_declarations program in
	string_of_header program
	^ "\n" ^ string_of_automata program declarations_string


(**************************************************)
(** States *)
(**************************************************)

(* Convert a state into a string *)
let string_of_state program (global_location, linear_constraint) =
	"" ^ (Automaton.string_of_location program.automata_names program.location_names program.variable_names global_location) ^ " ==> \n&" ^ (LinearConstraint.gml_of_linear_constraint program.variable_names 0 linear_constraint) ^ "" 


(**************************************************)
(** Pi0 *)
(**************************************************)
(* Convert a pi0 into a string *)
let string_of_pi0 program pi0 =
	"  " ^ (
	string_of_list_of_string_with_sep "\n& " (
		List.map (fun parameter ->
			(program.variable_names parameter)
			^ " = "
			^ (NumConst.string_of_numconst (pi0 parameter))
		) program.parameters
	)
	)
