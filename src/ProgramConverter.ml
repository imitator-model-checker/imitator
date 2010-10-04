(*****************************************************************
 *
 *                     IMITATOR II
 *
 * Convert a parsing structure into an abstract program
 *
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/09
 * Last modified: 2010/03/29
 *
 ****************************************************************)


(**************************************************)
(* OPTIMISATIONS A FAIRE POUR LA CONVERSION

- eviter deux parcours des listes de parametres, horloges, etc., dans ProgramConverter.ml
- ne pas chercher automatiquement les actions synchronisees si le programme ne considere que les actions manuellement declarees (comme HyTech)

**************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open Global
open ParsingStructure
open AbstractImitatorFile
open ImitatorPrinter


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InvalidProgram
exception InvalidPi0



(****************************************************************)
(** Constraint conversion *)
(****************************************************************)
exception False_exception


(****************************************************************)
(** Global variables for conversion *)
(****************************************************************)

let index_of_variables = ref (Hashtbl.create 16)
let glob_variable_names = ref (Array.make 0 "")
let glob_nb_variables = ref 0
let glob_prime_offset = ref 0

let prime_of_variable v = v + !glob_prime_offset
let unprime_variable v = v - !glob_prime_offset
let is_primed_variable v = v >= !glob_nb_variables

(*--------------------------------------------------*)
(* Convert a ParsingStructure.linear_expression into an array of coef and constant *)
(*--------------------------------------------------*)
let array_of_coef_of_linear_expression linear_expression =
	(* Create an array of coef *)
	let array_of_coef = Array.make (!glob_prime_offset + !glob_nb_variables) NumConst.zero in
	(* Create a zero constant *)
	let constant = ref NumConst.zero in
	
	(* Internal function to update the array for a linear term *)
	let update_array_linear_term mul_coef = function
		(* Case constant -> update the constant with the coef *)
		| Constant c -> constant := NumConst.add !constant (NumConst.mul c mul_coef);
		(* Case variables -> update the array with the coef  *)
		| Variable (coef, variable_name) -> ( 
			try(
				(* Find the variable_index *)
				let variable_index = Hashtbl.find !index_of_variables variable_name in
				(* Update the variable with its coef *)
				array_of_coef.(variable_index) <- NumConst.add array_of_coef.(variable_index) (NumConst.mul coef mul_coef);
			) with Not_found -> raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although it was checked before.")))
		| PrimedVariable (coef, variable_name) -> (
			try(
				(* Find the variable_index *)
				let variable_index = Hashtbl.find !index_of_variables variable_name in
				let primed_index = prime_of_variable variable_index in
				(* Update the variable with its coef *)
				array_of_coef.(primed_index) <- NumConst.add array_of_coef.(primed_index) (NumConst.mul coef mul_coef);
			) with Not_found -> raise (InternalError ("Impossible to find the index of variable '" ^ variable_name ^ "' although it was checked before.")))
			
	in

	(* Internal function to update the array for a linear expression *)
	let rec update_array_linear_expression = function
		| Linear_term lt -> update_array_linear_term NumConst.one lt
		| Linear_plus_expression (le, lt) ->
			(* Fill the array with le *)
			update_array_linear_expression le;
			(* Fill the array with lt *)
			update_array_linear_term NumConst.one lt;
		| Linear_minus_expression (le, lt) ->
			(* Fill the array with le *)
			update_array_linear_expression le;
			(* Fill the array with lt *)
			update_array_linear_term NumConst.minus_one lt;
	in
	(* Call the recursive function *)
	update_array_linear_expression linear_expression;
	(* Return the array of coef and the constant *)
	array_of_coef, !constant


(*--------------------------------------------------*)
(* Convert an array of variable coef into a linear term *)
(*--------------------------------------------------*)
let linear_term_of_array (array_of_coef, constant) =
	(* Create an empty list of members *)
	let members = ref [] in
	(* Iterate on the coef *)
	Array.iteri (fun variable_index coef ->
		if NumConst.neq coef NumConst.zero then (
			(* Add the member *)
			members := (coef, variable_index) :: !members;
		);
	) array_of_coef;
	(* Create the linear term *)
	LinearConstraint.make_linear_term !members constant
	

(*--------------------------------------------------*)
(* Direct conversion of a ParsingStructure.linear_expression into a Linear_term.linear_term *)
(*--------------------------------------------------*)
let linear_term_of_linear_expression linear_expression =
	let array_of_coef, constant = array_of_coef_of_linear_expression linear_expression in
	linear_term_of_array (array_of_coef, constant)


(*--------------------------------------------------*)
(* Perform the substraction of 2 NumConst array of same size *)
(*--------------------------------------------------*)
let sub_array array1 array2 =
	(* Create the result *)
	let result = Array.make (Array.length array1) NumConst.zero in
	(* Iterate on both arrays *)
	for i = 0 to (Array.length array1) - 1 do
		(* Perform array1 - array2 *)
		result.(i) <- NumConst.sub array1.(i) array2.(i);
	done;
	(* Return the result *)
	result


(*--------------------------------------------------*)
(* Convert a ParsingStructure.linear_constraint into a Constraint.linear_inequality *)
(*--------------------------------------------------*)
let linear_inequality_of_linear_constraint (le1, relop, le2) =
	(* Get the array of variables and constant associated to the linear terms *)
	let array1, constant1 = array_of_coef_of_linear_expression le1 in
	let array2, constant2 = array_of_coef_of_linear_expression le2 in
	(* Consider the operator *)
	match relop with
	(* a < b <=> b - a > 0 *)
	| OP_L ->
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_g
(* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_g *)

	(* a <= b <=> b - a >= 0 *)
	| OP_LEQ ->
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_ge
(* 	(Constraint.substract_linear_terms lt2 lt1), Constraint.Op_ge *)

(* a = b <=> b - a = 0 *)
	| OP_EQ -> 
		(* Create the array *)
		let array12 = sub_array array2 array1 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant2 constant1 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_eq
	
(* 	(Constraint.substract_linear_terms lt1 lt2), Constraint.Op_eq *)

	(* a >= b <=> a - b >= 0 *)
	| OP_GEQ ->
		(* Create the array *)
		let array12 = sub_array array1 array2 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant1 constant2 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_ge
(* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_ge *)

	(* a > b <=> a - b > 0 *)
	| OP_G ->
		(* Create the array *)
		let array12 = sub_array array1 array2 in
		(* Create the constant *)
		let constant12 = NumConst.sub constant1 constant2 in
		(* Create the linear_term *)
		let linear_term = linear_term_of_array (array12, constant12) in
		(* Return the linear_inequality *)
		LinearConstraint.make_linear_inequality linear_term LinearConstraint.Op_g
(* (Constraint.substract_linear_terms lt1 lt2), Constraint.Op_g *)


(*--------------------------------------------------*)
(* Convert a ParsingStructure.convex_predicate into a Constraint.linear_constraint *)
(*--------------------------------------------------*)


(* let i = ref 0 *)


let linear_constraint_of_convex_predicate convex_predicate =
	try(
	
	
(*	print_int (!i);
	print_newline();
	i := !i + 1;	
	if (!i > 100) then (raise False_exception);*)
	
	(* Compute a list of inequalities *)
	let linear_inequalities = List.fold_left
		(fun linear_inequalities linear_inequality -> 
		match linear_inequality with
		| True_constraint -> linear_inequalities
		| False_constraint -> raise False_exception
		| Linear_constraint (le1, relop, le2) -> (linear_inequality_of_linear_constraint (le1, relop, le2)) :: linear_inequalities
	) [] convex_predicate
	in LinearConstraint.make linear_inequalities
	(* Stop if any false constraint is found *)
	) with False_exception -> LinearConstraint.false_constraint ()


(****************************************************************)
(** Functions to get the variable names from the parsing structure *)
(****************************************************************)

(*--------------------------------------------------*)
(* Get all (possibly identical) names of variables in the header *)
(*--------------------------------------------------*)
let get_declared_variable_names variable_declarations =
	(* Get all (possibly identical) names of variables in one variable declaration and add it to the computed triple (clocks, discrete, parameters) *)
	let get_variable_from_declaration (analogs,clocks, discrete, parameters) (var_type, var_name, _) =
		match var_type with
		| ParsingStructure.Var_type_analog    -> (var_name::analogs, clocks, discrete, parameters)
		| ParsingStructure.Var_type_clock     -> (analogs, var_name::clocks, discrete, parameters)
		| ParsingStructure.Var_type_discrete  -> (analogs, clocks, var_name::discrete, parameters)
		| ParsingStructure.Var_type_parameter -> (analogs, clocks, discrete, var_name::parameters)
	in
	let (analogs, clocks, discrete, parameters) = List.fold_left get_variable_from_declaration ([], [], [], []) variable_declarations in
	(* Reverse lists *)
	(List.rev analogs, List.rev clocks, List.rev discrete, List.rev parameters)


(*--------------------------------------------------*)
(* Get all continuous variables per automaton index *)
(*--------------------------------------------------*)
let get_variables_per_automaton index_of_automata variable_declarations =
	(* create empty index for variables *)
	let vars_per_automaton = Hashtbl.create 0 in
	(* build function to insert a variable into the index *)
	let make_index = fun (var_type, var_name, var_scope) -> (
		match var_scope with
		| Global -> (
		  (* only discrete and parameters are admitted global *)
			match var_type with
				| ParsingStructure.Var_type_analog
				| ParsingStructure.Var_type_clock -> (
						print_error ("No global analog and clock variables permitted (variable " ^ var_name ^ ")");
						raise InvalidProgram
					)
				| _ -> ()
		  )
		| Local aut_name -> (		
			(* get index for automaton *)
			let aut_index = Hashtbl.find index_of_automata aut_name in
			(* get index for variable *)
			let var_index = Hashtbl.find !index_of_variables var_name in
			(* only register continuous variables here *)
			match var_type with
				| ParsingStructure.Var_type_analog 
				| ParsingStructure.Var_type_clock -> (
						print_message Debug_total ("register local cont. variable " ^ var_name ^ " for automaton " ^ aut_name);
						(* find previously registered variables for this automaton *)
						let var_set = try (Hashtbl.find vars_per_automaton aut_index) with Not_found -> VariableSet.empty in
						(* store variable in table *)
						Hashtbl.replace vars_per_automaton aut_index (VariableSet.add var_index var_set)				
					)
				| _ -> () (* do not register discrete and parameters as local variables *)
			)				
	) in
	(* create index from declarations *)
	List.iter make_index variable_declarations;
	(* return functional representation *)
	fun aut_index -> (
		try (Hashtbl.find vars_per_automaton aut_index) with Not_found -> VariableSet.empty
	)

(*--------------------------------------------------*)
(* Get all (possibly identical) names of automata *)
(*--------------------------------------------------*)
let get_declared_automata_names =
	List.map (fun (automaton_name, _, _) -> automaton_name)

(*--------------------------------------------------*)
(* Get all (all different) names of synclabs *)
(*--------------------------------------------------*)
let get_declared_synclabs_names =
	List.fold_left (fun synclabs_names (_, synclabs, _) -> list_union synclabs_names synclabs) []


(****************************************************************)
(** Verification functions *)
(****************************************************************)

(*--------------------------------------------------*)
(* Check that variable names are all different, return false otherwise; warns if a variable is defined twice as the same type *)
(*--------------------------------------------------*)
let check_variable_names analog_names clock_names discrete_names parameters_names =
	(* Warn if a variable is defined twice as the same type *)
	let warn_for_multiply_defined_variables list_of_variables =
		(* Compute the multiply defined variables *)
		let multiply_defined_variables = elements_existing_several_times list_of_variables in
		(* Print a warning for each of them *)
		List.iter (fun variable_name -> print_warning ("Multiply-declared variable '" ^ variable_name ^"'")) multiply_defined_variables;
	in
	warn_for_multiply_defined_variables analog_names;
	warn_for_multiply_defined_variables clock_names;
	warn_for_multiply_defined_variables discrete_names;
	warn_for_multiply_defined_variables parameters_names;
	(* Error for variables defined as different types *)
	let error_for_multiply_defined_variables l1 l2 =
		let inter = list_inter l1 l2 in
		match inter with
			| [] -> true
			| _ -> List.iter (fun variable_name -> print_error ("The variable '" ^ variable_name ^ "' is defined twice as two different types.")) inter; false
	in
	let check1 = error_for_multiply_defined_variables clock_names discrete_names in
	let check2 = error_for_multiply_defined_variables clock_names parameters_names in
	let check3 = error_for_multiply_defined_variables discrete_names parameters_names in
	let check4 = error_for_multiply_defined_variables analog_names clock_names in
	let check5 = error_for_multiply_defined_variables analog_names discrete_names in
	let check6 = error_for_multiply_defined_variables analog_names parameters_names in
	check1 && check2 && check3 && check4 && check5 && check6


(*--------------------------------------------------*)
(* Check that the names of automata are all different; return false otherwise *)
(*--------------------------------------------------*)
let check_declared_automata_names automata_names =
	(* Compute the multiply defined variables *)
	let multiply_defined_names = elements_existing_several_times automata_names in
	(* Print an error for each of them *)
	match multiply_defined_names with
		| [] -> true
		| _ -> List.iter (fun variable_name -> print_error ("Several automata have name " ^ variable_name ^ ".")) multiply_defined_names; false


(*--------------------------------------------------*)
(* Check that all locations of a given automaton are different *)
(*--------------------------------------------------*)
let all_locations_different = 
	(* Check for every automaton *)
	List.fold_left
	(fun all_different (automaton_name, _, locations) ->
		(* Get all the location names *)
		let locations =
			List.map (fun (location_name, _, _, _) -> location_name) locations in
		(* Look for multiply declared locations *)
		let multiply_declared_locations = elements_existing_several_times locations in
			List.iter (fun location_name -> print_error ("Several locations have name '" ^ location_name ^ "' in automaton '" ^ automaton_name ^ "'.")) multiply_declared_locations;
			if multiply_declared_locations = [] then all_different else false
	)
	true


(*--------------------------------------------------*)
(* Check that all variables are defined in a linear_term *)
(*--------------------------------------------------*)
let check_linear_term variable_names = function
	| Constant _ -> true
	| Variable (_, variable_name) -> if not (List.mem variable_name variable_names) then(
		print_error ("The variable '" ^ variable_name ^ "' used in the program was not declared."); false
		) else true
	| PrimedVariable (_, variable_name) -> if not (List.mem variable_name variable_names) then(
		print_error ("The variable '" ^ variable_name ^ "' used in the program was not declared."); false
		) else true

(*--------------------------------------------------*)
(* Check that all variables are defined in a linear_expression *)
(*--------------------------------------------------*)
let rec check_linear_expression variable_names = function
	| Linear_term linear_term -> check_linear_term variable_names linear_term
	| Linear_plus_expression (linear_expression, linear_term)
		-> evaluate_and (check_linear_expression variable_names linear_expression) (check_linear_term variable_names linear_term)
	| Linear_minus_expression (linear_expression, linear_term)
		-> evaluate_and (check_linear_expression variable_names linear_expression) (check_linear_term variable_names linear_term)


(*--------------------------------------------------*)
(* Check that all variables are defined in a linear_constraint *)
(*--------------------------------------------------*)
let check_linear_constraint variable_names = function
	| True_constraint -> true
	| False_constraint -> true
	| Linear_constraint (linear_expression1, relop, linear_expression2) ->
		evaluate_and (check_linear_expression variable_names linear_expression1)
		(check_linear_expression variable_names linear_expression2)


(*--------------------------------------------------*)
(* Check that all variables are defined in a convex predicate *)
(*--------------------------------------------------*)
let check_convex_predicate variable_names =
	List.fold_left
	(fun all_defined linear_constraint ->
		evaluate_and all_defined (check_linear_constraint variable_names linear_constraint)
	)
	true

(*--------------------------------------------------*)
(* Check that a linear expression contains only discrete variables and constants *)
(*--------------------------------------------------*)
let only_discrete_in_linear_term type_of_variables = function
	| Constant _ -> true
	| Variable (_, variable_name) ->
		let variable_index = Hashtbl.find !index_of_variables variable_name in
		type_of_variables variable_index = Var_type_discrete
	| PrimedVariable (_, _) -> false

let rec only_discrete_in_linear_expression type_of_variables = function
	| Linear_term linear_term ->
		only_discrete_in_linear_term type_of_variables  linear_term
	| Linear_plus_expression (linear_expression, linear_term) ->
		only_discrete_in_linear_expression type_of_variables linear_expression
		&& only_discrete_in_linear_term type_of_variables linear_term
	| Linear_minus_expression (linear_expression, linear_term) ->
	only_discrete_in_linear_expression type_of_variables linear_expression
	&& only_discrete_in_linear_term type_of_variables linear_term

(*--------------------------------------------------*)
(* Check that an update is well formed *)
(*--------------------------------------------------*)
(*let check_update type_of_variables variable_names automaton_name (variable_name, linear_expression) =                                                                                                                        *)
(*	(* Get the index of the variable *)                                                                                                                                                                                        *)
(*	let index, declared = try (Hashtbl.find !index_of_variables variable_name, true)                                                                                                                                           *)
(*		with Not_found -> (                                                                                                                                                                                                      *)
(*			print_error ("The variable '" ^ variable_name ^ "' used in an update in automaton '" ^ automaton_name ^ "' was not declared."); 0, false                                                                               *)
(*		)                                                                                                                                                                                                                        *)
(*	in                                                                                                                                                                                                                         *)
(*	if not declared then false else(                                                                                                                                                                                           *)
(*		(* Get the type of the variable *)                                                                                                                                                                                       *)
(*		let type_of_variable = try (type_of_variables index)                                                                                                                                                                     *)
(*			with Invalid_argument comment -> (                                                                                                                                                                                     *)
(*			raise (InternalError ("The variable '" ^ variable_name ^ "' was not found in '" ^ automaton_name ^ "', although this was checked before. OCaml says: " ^ comment ^ "."))                                               *)
(*		) in                                                                                                                                                                                                                     *)
(*		match type_of_variable with                                                                                                                                                                                              *)
(*		(* Case of a clock: allow only 0 as an update *)                                                                                                                                                                         *)
(*		| AbstractImitatorFile.Var_type_clock ->                                                                                                                                                                                 *)
(*			let result =                                                                                                                                                                                                           *)
(*			match linear_expression with                                                                                                                                                                                           *)
(*			| Linear_term (Constant constant) ->                                                                                                                                                                                   *)
(*				if NumConst.equal constant NumConst.zero then true                                                                                                                                                                   *)
(*				else (print_error ("The variable '" ^ variable_name ^ "' is a clock and can only be reset to 0 in automaton '" ^ automaton_name ^ "'."); false)                                                                      *)
(*			| _ -> print_error ("The variable '" ^ variable_name ^ "' is a clock and can only be reset to 0 in automaton '" ^ automaton_name ^ "'."); false                                                                        *)
(*			in result                                                                                                                                                                                                              *)
(*		(* Case of a discrete var.: allow only a linear combinations of constants and discrete *)                                                                                                                                *)
(*		| AbstractImitatorFile.Var_type_discrete -> let result = only_discrete_in_linear_expression type_of_variables linear_expression in                                                                                       *)
(*		if not result then (print_error ("The variable '" ^ variable_name ^ "' is a discrete and its update can only be a linear combination of constants and discrete variables in automaton '" ^ automaton_name ^ "'."); false)*)
(*		else true                                                                                                                                                                                                                *)
(*		(* Case of a parameter: forbidden! *)                                                                                                                                                                                    *)
(*		| AbstractImitatorFile.Var_type_parameter -> print_error ("The variable '" ^ variable_name ^ "' is a parameter and can not be updated in automaton '" ^ automaton_name ^ "'."); false                                    *)
(*		| AbstractImitatorFile.Var_type_analog -> print_error ("The variable '" ^ variable_name ^ "' is an analog and can not be updated in automaton '" ^ automaton_name ^ "'."); false                                         *)
(*	)                                                                                                                                                                                                                          *)

(*--------------------------------------------------*)
(* Check that a flow is well formed *)
(*--------------------------------------------------*)
(*let check_flow type_of_variables flow =                                                                        *)
(*  List.for_all (fun (variable_name, rate) ->                                                                   *)
(*		(* Get the index of the variable *)                                                                        *)
(*		let index, declared = try (Hashtbl.find !index_of_variables variable_name, true)                           *)
(*		with Not_found -> (                                                                                        *)
(*			print_error ("The variable '" ^ variable_name ^ "' used in a rate condition was not declared."); 0, false*)
(*		)	in                                                                                                      *)
(*		if not declared then false else (                                                                          *)
(*			(* Get the type of the variable *)                                                                       *)
(*			let type_of_variable = type_of_variables index in                                                        *)
(*			if not (type_of_variable = AbstractImitatorFile.Var_type_analog) &&                                      *)
(*				 not (type_of_variable = AbstractImitatorFile.Var_type_clock) then (                                   *)
(*				print_error ("The variable '" ^ variable_name ^ "' used in a rate condition is not analog.");          *)
(*				false                                                                                                  *)
(*			) else true                                                                                              *)
(*		)                                                                                                          *)
(*	) flow                                                                                                       *)

(*--------------------------------------------------*)
(* Check that a sync is well formed *)
(*--------------------------------------------------*)
let check_sync sync_name_list automaton_name = function
	| Sync sync_name ->  if not (List.mem sync_name sync_name_list) then (
		print_error ("The sync label '" ^ sync_name ^ "' used in automaton '" ^ automaton_name ^ "' was not declared for this automaton."); false)
		else true
	| NoSync -> true

(*--------------------------------------------------*)
(* Check that a sync is used in all the automata where it is declared *)
(*--------------------------------------------------*)
let synclab_used_everywhere automata synclab_name =
	(* Try to find the synclab in all the automaton where it is declared *)
	try(
		(* Check each automaton *)
		List.iter (fun (_, sync_name_list, locations) ->
			(* Only check if the synclab is declared here *)
			if List.mem synclab_name sync_name_list then(
				(* Check that at least one location contains the synclab *)
				if not (List.exists (fun (_, _, _, transitions) ->
					(* Check that at least one transition contains the synclab *)
					List.exists (fun (_, _, sync, _) -> sync = (Sync synclab_name)) transitions
				) locations ) then (
					(* No location contains the synclab *)
					raise Not_found;
				);
			);
		) automata;
		(* The synclab was found everywhere: true *)
		true
	(* At least one automata does not use the synclab : false *)
	) with Not_found -> false


(*--------------------------------------------------*)
(* Check that the automata are well-formed *)
(*--------------------------------------------------*)
let check_automata type_of_variables variable_names index_of_automata locations_per_automaton automata =
	let well_formed = ref true in

	(* Check each automaton *)
	List.iter (fun (automaton_name, sync_name_list, locations) ->
		(* Get the index of the automaton *)
		let index = try (Hashtbl.find index_of_automata automaton_name) with
			Not_found -> raise (InternalError ("Impossible to find the index of automaton '" ^ automaton_name ^ "'."))
		in
		(* Check each location *)
		List.iter (fun (location_name, convex_predicate, flow, transitions) -> (***** FIXME: check flows *)
			(* Check that the location_name exists (which is obvious) *)
			if not (in_array location_name locations_per_automaton.(index)) then(
				print_error ("The location '" ^ location_name ^ "' declared in automaton '" ^ automaton_name ^ "' does not exist.");
				well_formed := false);

			(* Check the convex predicate *)
			(* TODO: preciser quel automate et quelle location en cas d'erreur *)
			if not (check_convex_predicate variable_names convex_predicate) then well_formed := false;
			(* Check the rate condition *)
(*			if not (check_flow type_of_variables flow) then well_formed := false;*)
			(* Check transitions *)
			List.iter (fun (convex_predicate, updates, sync, dest_location_name) ->
				(* Check the convex predicate *)
				if not (check_convex_predicate variable_names convex_predicate) then well_formed := false;
				(* Check the updates *)
				(***** FIXME : replaced list of simple updates by a convex_predicate *)
(*				List.iter (fun update -> if not (check_update index_of_variables type_of_variables variable_names automaton_name update) then well_formed := false) updates;*)
				(* Check the sync *)
				if not (check_sync sync_name_list automaton_name sync) then well_formed := false;
				(* Check that the destination location exists for this automaton *)
				if not (in_array dest_location_name locations_per_automaton.(index)) then(
					print_error ("The destination location '" ^ dest_location_name ^ "' used in automaton '" ^ automaton_name ^ "' does not exist.");
					well_formed := false);
			) transitions;
		) locations;
	) automata;

	(* Return whether the automata passed the tests *)
	!well_formed


(*--------------------------------------------------*)
(* Check that the init_definition are well-formed *)
(*--------------------------------------------------*)
let check_init discrete variable_names type_of_variables automata automata_names index_of_automata locations_per_automaton init_definition =
	let well_formed = ref true in
	(* Check that (automaton / location / variable) names exist in each predicate *)
	List.iter (function
		| Loc_assignment (automaton_name, location_name) ->
			(* Check that the automaton_name exists *)
			let index, exists = try (Hashtbl.find index_of_automata automaton_name, true) with
			Not_found -> (print_error ("The automaton '" ^ automaton_name ^ "' mentioned in the init definition does not exist."); well_formed := false; 0, false) in
			(* Check that the location_name exists (only if the automaton_name exists) *)
			if exists && not (in_array location_name locations_per_automaton.(index)) then (
				print_error ("The location '" ^ location_name ^ "' mentioned in the init definition does not exist in automaton '" ^ automaton_name ^ "'."); well_formed := false
			)
		| Linear_predicate linear_constraint ->
			if not (check_linear_constraint variable_names linear_constraint) then well_formed := false
	) init_definition;

	(* Get all the Loc_assignment *)
	let loc_assignments, init_inequalities = List.partition (function
		| Loc_assignment _ -> true
		| Linear_predicate _ -> false
	) init_definition in
	(* Make couples (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Loc_assignment was found in a Loc_assignment list")
	) loc_assignments in

	(* Check that every automaton is given at most one initial location *)
	let init_locations_for_automata = Hashtbl.create (List.length automata) in
	List.iter (fun (automaton_name, location_name) ->
		(* Check if this automaton was already given an initial location before *)
		if Hashtbl.mem init_locations_for_automata automaton_name then(
			(* Get the initial location already declared previously *)
			let previous_location = Hashtbl.find init_locations_for_automata automaton_name in
			(* If identical : only warns *)
			if location_name = previous_location then (
				print_warning ("The automaton '" ^ automaton_name ^ "' is assigned twice the initial location '" ^ location_name ^ "' in the init definition.");
			(* If different : error *)
			) else (
				print_error ("The automaton '" ^ automaton_name ^ "' is assigned several different locations in the init definition.");
				well_formed := false;
			);
		(* If not already given : add it *)
		) else (
			Hashtbl.add init_locations_for_automata automaton_name location_name;
		);
	) initial_locations;
	(* Check that every automaton is given at least one initial location *)
	List.iter (fun automaton_index -> 
		(* Get the name *)
		let automaton_name = automata_names automaton_index in
		(* Look for it in the hash table *)
		if not (Hashtbl.mem init_locations_for_automata automaton_name) then (
			(* Error *)
			print_error ("The automaton '" ^ automaton_name ^ "' is not given any initial location in the init definition.");
			well_formed := false;
		);
	) automata;
	
	(*
	(* Only keep the automaton_names *)
	let automata_with_initial_locations, _ = List.split initial_locations in
	List.iter (fun automaton_index -> 
		(* Find the name *)
		let automaton_name = automata_names automaton_index in
		(* Filter the initial locations for automaton_name *)
		(**** TO OPTIMIZE !!!! : here exponential instead of linear ****)
		let initial_locations_for_this_automaton =
			List.filter (fun name -> name = automaton_name) automata_with_initial_locations
		in
		let nb_locations = List.length initial_locations_for_this_automaton in
		if nb_locations = 0 then (
			print_error ("The automaton '" ^ automaton_name ^ "' is not given any initial location in the init definition."); well_formed := false
		)else (if nb_locations >= 2 then (
			print_error ("The automaton '" ^ automaton_name ^ "' must be given only one initial location in the init definition; here, " ^ (string_of_int nb_locations) ^ " are given."); well_formed := false
		))
	) automata;*)

	(* Partition the init inequalities between the discrete init assignments, and other inequalities *)
	let discrete_init, other_inequalities = List.partition (function
		(* Check if the left part is only a variable name *)
		| Linear_predicate (Linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			let is_discrete =
			try (
				(* Get the variable index *)
				let variable_index =  Hashtbl.find !index_of_variables variable_name in
				(* Keep if this is a discrete *)
				type_of_variables variable_index = Var_type_discrete
			(* If not existing : false *)
			) with
			Not_found -> (
				print_error ("The variable '" ^ variable_name ^ "' mentioned in the init definition does not exist.");
				false)
			in is_discrete
		(* Otherwise false *)
		| _ -> false
	) init_inequalities in
	
	(* Check that every discrete variable is given only one (rational) initial value *)
	let init_values_for_discrete = Hashtbl.create (List.length discrete) in
	List.iter (fun lp -> 
		match lp with
			| Linear_predicate (Linear_constraint (Linear_term (Variable (coeff, discrete_name)), op , expression)) ->
				if NumConst.neq coeff NumConst.one then (
					print_error ("The discrete variable '" ^ discrete_name ^ "' must have a coeff 1 in the init definition.");
					well_formed := false;
				);
				(* Check if the assignment is well formed, and keep the discrete value *)
				let discrete_value =
				match (op, expression) with
					| (OP_EQ, Linear_term (Constant c)) -> c
					| _ -> print_error ("The initial value for discrete variable '" ^ discrete_name ^ "' must be under the form of an equality with a constant.");
					well_formed := false;
					NumConst.zero
				in
				(* Get the variable index *)
				let discret_index =  Hashtbl.find !index_of_variables discrete_name in
				(* Check if it was already declared *)
				if Hashtbl.mem init_values_for_discrete discret_index then(
					print_error ("The discrete variable '" ^ discrete_name ^ "' is given an initial value several times in the init definition.");
					well_formed := false;
				) else (
					(* Else add it *)
					Hashtbl.add init_values_for_discrete discret_index discrete_value;
				);
			| _ -> raise (InternalError ("Must have this form since it was checked before."))
	) discrete_init;
	
	(* Check that every discrete variable is given at least one (rational) initial value (if not: warns) *)
	List.iter (fun discrete_index ->
		if not (Hashtbl.mem init_values_for_discrete discrete_index) then(
			print_warning ("The discrete variable '" ^ (List.nth variable_names discrete_index) ^ "' was not given an initial value in the init definition: it will be assigned to 0.");
			Hashtbl.add init_values_for_discrete discrete_index NumConst.zero
		);
	) discrete;

	(* Convert the Hashtbl to couples (discrete_index, init_value) *)
	let discrete_values_couples =
		List.map (fun discrete_index ->
			discrete_index, Hashtbl.find init_values_for_discrete discrete_index
		) discrete
	in

	(* Check that no discrete variable is used in other inequalities (warns if yes) *)
	(**** TO DO ****) (*use 'other_inequalities' *)

	(* Return whether the init declaration passed the tests *)
	discrete_values_couples, !well_formed


(*--------------------------------------------------*)
(* Check the pi0 w.r.t. the program parameters *)
(*--------------------------------------------------*)
let check_pi0 pi0 parameters_names =
	(* Compute the list of variable names *)
	(**** TO OPTIMIZE: not tail recursvie ****)
	let list_of_variables, _ = List.split pi0 in

	(* Compute the multiply defined variables *)
	let multiply_defined_variables = elements_existing_several_times list_of_variables in
	(* Print an error for each of them *)
	List.iter (fun variable_name -> print_error ("The parameter '" ^ variable_name ^ "' was assigned several times a valuation in pi0.")) multiply_defined_variables;
	(* TODO: only warns if it is always defined to the same value *)

	(* Check if the variables are all defined *)
	let all_defined = List.fold_left
		(fun all_defined variable_name ->
			if List.mem variable_name list_of_variables then all_defined
			else (
				print_error ("The parameter '" ^ variable_name ^ "' was not assigned a valuation in pi0.");
				false
			)
		)
		true
		parameters_names
	in

	(* Check if some defined variables are not parameters (and warn) *)
	List.iter
		(fun variable_name ->
			if not (List.mem variable_name parameters_names) then (
				print_warning ("'" ^ variable_name ^ "', which is assigned a valuation in pi0, is not a valid parameter name.")
			)
		)
		list_of_variables
	;

	(* If something went wrong: launch an error *)
(*	if multiply_defined_variables != [] || not all_defined
	then false else true*)
	multiply_defined_variables = [] && all_defined



(*--------------------------------------------------*)
(* Check the pi0 cube w.r.t. the program parameters *)
(*--------------------------------------------------*)
let check_pi0cube pi0cube parameters_names =
	(* Compute the list of variable names *)
	let list_of_variables = List.map (fun (v, _, _, _) -> v) pi0cube in

	(* Compute the multiply defined variables *)
	let multiply_defined_variables = elements_existing_several_times list_of_variables in
	(* Print an error for each of them *)
	List.iter (fun variable_name -> print_error ("The parameter '" ^ variable_name ^ "' was assigned several times a valuation in pi0cube.")) multiply_defined_variables;
	(* TODO: only warns if it is always defined to the same value *)

	(* Check if the variables are all defined *)
	let all_defined = List.fold_left
		(fun all_defined variable_name ->
			if List.mem variable_name list_of_variables then all_defined
			else (
				print_error ("The parameter '" ^ variable_name ^ "' was not assigned a valuation in pi0cube.");
				false
			)
		)
		true
		parameters_names
	in

	(* Check that the intervals are not null *)
	let all_intervals_ok = List.fold_left
		(fun all_intervals_ok (variable_name, a, b, _) ->
			if NumConst.le a b then all_intervals_ok
			else (
				print_error ("The interval [" ^ (NumConst.string_of_numconst a) ^ ", " ^ (NumConst.string_of_numconst b) ^ "] is null for parameter '" ^ variable_name ^ "' in pi0cube.");
				false
			)
		)
		true
		pi0cube
	in

	(* Check if some defined variables are not parameters (and warn) *)
	List.iter
		(fun variable_name ->
			if not (List.mem variable_name parameters_names) then (
				print_warning ("'" ^ variable_name ^ "', which is assigned a valuation in pi0cube, is not a valid parameter name.")
			)
		)
		list_of_variables
	;

	(* If something went wrong: launch an error *)
	multiply_defined_variables = [] && all_defined && all_intervals_ok



(****************************************************************)
(** Program conversion *)
(****************************************************************)

(*--------------------------------------------------*)
(* Get all the declared actions for every automaton *)
(*--------------------------------------------------*)
let make_actions_per_automaton index_of_labels index_of_automata automata =
	(* Create an empty array for every automaton *)
	let actions_per_automaton = Array.make (List.length automata) [] in
	(* Fill it *)
	List.iter (fun (automaton_name, sync_name_list, _) ->
		(* Get the index of the automaton *)
		let automaton_index = Hashtbl.find index_of_automata automaton_name in
		(* Update the array *)
		actions_per_automaton.(automaton_index) <-
			List.map (fun sync_name ->
			(* Get the index of the label *)
			let label_index = Hashtbl.find index_of_labels sync_name in
			(* Return the label index *)
			label_index
			) sync_name_list;
	) automata;
	(* Return the array *)
	actions_per_automaton


(*--------------------------------------------------*)
(* Get all the locations for every automaton *)
(*--------------------------------------------------*)
let make_locations_per_automaton index_of_automata automata =
	(* Create an empty array for every automaton *)
	let locations_per_automaton = Array.make (Hashtbl.length index_of_automata) (Array.make 0 "") in
	(* For each automaton: *)
	List.iter
		(fun (automaton_name, _, transitions) ->
			(* Get the index of the automaton *)
			let index = Hashtbl.find index_of_automata automaton_name in
			(* Get the location names *)
			let location_names = List.map (fun (location_name, _, _, _) -> location_name) transitions in
			(* Update the array *)
			locations_per_automaton.(index) <- Array.of_list location_names
		)
		automata;
	(* Return a functional view *)
	locations_per_automaton


(*--------------------------------------------------*)
(* Get all the possible actions for every location of every automaton *)
(*--------------------------------------------------*)
let make_automata index_of_automata index_of_locations labels index_of_labels removed_synclab_names automata =
	(* Create an empty array for the actions of every automaton *)
	let actions_per_automaton = Array.make (Hashtbl.length index_of_automata) [] in
	(* Create an empty array for the actions of every location of every automaton *)
	let actions_per_location = Array.make (Hashtbl.length index_of_automata) (Array.make 0 []) in
  (* Create an empty array for the rate conditions *)
	let flows = Array.make (Hashtbl.length index_of_automata) (Array.make 0 []) in
	(* Create an empty array for the transitions *)
	let transitions = Array.make (Hashtbl.length index_of_automata) (Array.make 0 []) in
	(* Create an empty array for the invariants *)
	let invariants = Array.make (Hashtbl.length index_of_automata) (Array.make 0 []) in
	(* Maintain the index of no_sync *)
	let no_sync_index = ref (Array.length labels) in
	(* For each automaton: *)
	List.iter
	(fun (automaton_name, _, locations) ->
		(* Get the index of the automaton *)
		print_message Debug_total ("    - Building automaton " ^ automaton_name);
		let automaton_index = try (Hashtbl.find index_of_automata automaton_name) with Not_found -> raise (InternalError ("Impossible to find the index of automaton '" ^ automaton_name ^ "'.")) in
		(* Get the number of locations *)
		let nb_locations = List.length locations in 
		(* Create the array of lists of actions for this automaton *)
		actions_per_location.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of list of transitions for this automaton *)
		transitions.(automaton_index) <- Array.make nb_locations [];
		(* Create the array of invariants for this automaton *)
		invariants.(automaton_index) <- Array.make nb_locations [];
    (* Create the array of rate conditions for this automaton *)
		flows.(automaton_index) <- Array.make nb_locations [];
		(* For each location: *)
		List.iter
		(fun (location_name, invariant, flow, parsed_transitions) -> 
			(* Get the index of the location *)
			let location_index = try (Hashtbl.find index_of_locations.(automaton_index) location_name) with Not_found -> raise (InternalError ("Impossible to find the index of location '" ^ location_name ^ "'.")) in
			(* Create the list of actions for this location *)
			let list_of_actions, list_of_transitions =  List.fold_left (fun (current_list_of_actions, current_list_of_transitions) (guard, updates, sync, dest_location_name) ->
				(* Get the index of the dest location *)
				let dest_location_index = try (Hashtbl.find index_of_locations.(automaton_index) dest_location_name) with Not_found -> raise (InternalError ("Impossible to find the index of location '" ^ dest_location_name ^ "'.")) in
				(* Depend on the action type *)
				match sync with
				| ParsingStructure.Sync action_name ->
					(* If the 'sync' is within the removed actions, do nothing *)
					if List.mem action_name removed_synclab_names then (
						current_list_of_actions, current_list_of_transitions
					(* Else : *)
					) else (
						(* Get the action index *)
						let action_index =
							try (Hashtbl.find index_of_labels action_name) with Not_found -> raise (InternalError ("Impossible to find the index of action '" ^ action_name ^ "'."))
						in
						(* Compute the list of actions *)
						(action_index :: current_list_of_actions)
						,
						(* Compute the list of transitions *)
						((action_index, guard, updates, dest_location_index) :: current_list_of_transitions)
					)
				| ParsingStructure.NoSync ->
					(* Get the action index *)
					let action_index = !no_sync_index in
					(* Increment the number of nosync indexes *)
					no_sync_index := !no_sync_index + 1;
					(* Compute the list of actions *)
					(action_index :: current_list_of_actions)
					,
					(* Compute the list of transitions *)
					((action_index, guard, updates, dest_location_index) :: current_list_of_transitions)
			) ([], []) parsed_transitions in
			(* Update the array of actions per location *)
			actions_per_location.(automaton_index).(location_index) <- (List.rev (list_only_once list_of_actions));
			(* Update the array of transitions per location *)
			transitions.(automaton_index).(location_index) <- (List.rev list_of_transitions);
			(* Update the array of invariants *)
			invariants.(automaton_index).(location_index) <- invariant;
			(* Update the array of (raw) rate conditions *)
			flows.(automaton_index).(location_index) <- flow;
		) locations;
		(* Update the array of actions per automaton *)
		let all_actions_for_this_automaton = Array.fold_left (fun list_of_all_actions list_of_actions ->
			list_union list_of_all_actions list_of_actions
		) [] actions_per_location.(automaton_index) in
		actions_per_automaton.(automaton_index) <- all_actions_for_this_automaton
	) automata;

	(* Create the array of action names *)
	let nb_actions = !no_sync_index in
	let array_of_action_names = Array.make nb_actions "" in
	(* Create the array of action types (sync / no_sync) *)
	let array_of_action_types = Array.make nb_actions Action_type_sync in
	(* Fill the sync actions *)
	for i = 0 to (Array.length labels) - 1 do
		array_of_action_names.(i) <- labels.(i);
	done;
	(* Fill the no sync actions *)
	for i = Array.length labels to nb_actions - 1 do
		array_of_action_names.(i) <- ("nosync_" ^ (string_of_int (i - (Array.length labels) + 1)));
		array_of_action_types.(i) <- Action_type_nosync;

	done;
	(* Create the action list *)
	let actions = list_of_interval 0 (nb_actions - 1) in
	(* Create the functional representation for action names *)
	let action_names = fun action_index ->
		try (array_of_action_names.(action_index))
		with _ -> raise (InternalError ("Action index " ^ (string_of_int action_index) ^ " does not exist in the program."))
	in
	(* Create the functional representation for action types *)
	let action_types = fun action_index -> array_of_action_types.(action_index) in
	(* Create the functional representation for the actions of every automaton *)
	let actions_per_automaton = fun automaton_index -> actions_per_automaton.(automaton_index) in
	(* Create an empty array for the actions of every location of every automaton *)
	let actions_per_location = fun automaton_index location_index -> actions_per_location.(automaton_index).(location_index) in

	(* Return all the structures in a functional representation *)
	actions, action_names, action_types, actions_per_automaton, actions_per_location, invariants, flows, transitions


(*--------------------------------------------------*)
(* Get the automata for every action *)
(*--------------------------------------------------*)
let make_automata_per_action actions_per_automaton nb_automata nb_actions =
	(* Create an empty array for actions *)
	let automata_per_action = Array.make nb_actions [] in
	(* For all automaton *)
	for automaton_index = 0 to nb_automata - 1 do
		(* For all action *)
		List.iter (fun action_index -> 
			(* Add the automaton_index to the action_index *)
			automata_per_action.(action_index) <- automaton_index :: automata_per_action.(action_index);
		) (actions_per_automaton automaton_index);
	done;
	(* Reverse it (not so important...) and return it *)
	let automata_per_action = Array.map List.rev automata_per_action in
	(* Return a functional representation *)
	fun automaton_index -> automata_per_action.(automaton_index)
	

(*--------------------------------------------------*)
(* Convert the invariants *)
(*--------------------------------------------------*)
(* Convert the structure: 'automaton_index -> location_index -> ParsingStructure.convex_predicate' into a structure: 'automaton_index -> location_index -> Constraint.linear_constraint' *)
let convert_invariants index_of_variables invariants =
	(* Convert for each automaton *)
	let invariants = Array.map (
		(* Convert for each location *)
		Array.map (linear_constraint_of_convex_predicate)
	) invariants in
	(* Functional representation *)
	fun automaton_index location_index -> invariants.(automaton_index).(location_index)


(*--------------------------------------------------*)
(* Convert the rate conditions *)
(*--------------------------------------------------*)

(* construct the standard flow for paramters, discrete and clocks *)
let construct_standard_flow parameters discrete clocks =
	let clock_deriv = LinearConstraint.make_set_all_variables clocks NumConst.one in
	let stable_deriv = LinearConstraint.make_set_all_variables (List.rev_append discrete parameters) NumConst.zero in
	LinearConstraint.intersection [clock_deriv; stable_deriv]


(* check and convert a linear constraint to a rate condition *)
let convert_flow type_of_variables constr =
	let ineq = linear_inequality_of_linear_constraint constr in
	let support = LinearConstraint.inequality_support ineq in
	(* check that only primed analogs are in the support *)
	VariableSet.iter (fun v -> 
		let x = unprime_variable v in
	  if x < 0 || (type_of_variables x) <> Var_type_analog then(
			print_error "Variable in rate condition is not analog";
			abort_program ()
		)
	) support;
	(* replace primed variables by unprimed variables *)
	let unprime = fun v -> Ppl_ocaml.Variable (unprime_variable v) in
	let unprimed_ineq = LinearConstraint.substitute_variables unprime ineq in
	unprimed_ineq


(* convert the rate conditions *)
let convert_flows type_of_variables raw_flows =
	let convert_location = (fun predicate -> 
		if predicate = [] then
			(* no flow condition in this location *)
			None
	  else (
			(* iterate over linear constraints *)
			let flows = ref [] in
			List.iter (fun lin_constr -> 
				match lin_constr with
					| True_constraint -> ()
					| False_constraint -> raise False_exception  
					| Linear_constraint (lexpr, op, rexpr) -> 
							let ineq = convert_flow type_of_variables (lexpr, op, rexpr) in
							flows := ineq :: !flows
			) predicate;
			(* construct linear constraint from collected inequalities *)
			let flow = LinearConstraint.make !flows in			
			if debug_mode_greater Debug_total then (
				print_message Debug_total ("converted flow:" ^ (LinearConstraint.string_of_linear_constraint (fun i -> !glob_variable_names.(i)) flow))
			);
			Some flow
	  )
	) in	
	let convert_automaton = Array.map convert_location in
	let flows = Array.map convert_automaton raw_flows in
	flows
	

(*--------------------------------------------------*)
(* Convert an update *)
(*--------------------------------------------------*)

(* construct a discrete update (discrete_index, linear_term) from a linear_constraint *)
let try_discrete_update type_of_variables linear_constr =
	(* construct a dummy value if the constraint is not a discrete update *)
	let no_return_value = ((0, LinearConstraint.make_linear_term [] NumConst.zero), false) in
	match linear_constr with
		| Linear_constraint (Linear_term (PrimedVariable (c, v)), OP_EQ, expr) -> (
				let var_index = Hashtbl.find !index_of_variables v in
				if not ((type_of_variables var_index) = Var_type_discrete) ||
				   not (NumConst.equal c NumConst.one) then
					no_return_value
				else
					((var_index, linear_term_of_linear_expression expr), true)
			)
		| _ -> no_return_value


(* Convert the structure: ParsingStructure.convex_predicate to (continuous_update, discrete_update list) *) 
let convert_update type_of_variables local_variables update =
	let discrete_updates = ref [] in
	let continuous_updates = ref [] in
	(* for each linear_constraint in the update convex predicate *)
	List.iter (fun lin_constr -> 
		let (discrete_update, ok) = try_discrete_update type_of_variables lin_constr in
		if ok then
			(* was a discrete update -> put into list *)
		  discrete_updates := discrete_update :: !discrete_updates
		else (
			(* convert to a linear constraint *)
			match lin_constr with
				| True_constraint -> ()
				| False_constraint -> raise False_exception  
				| Linear_constraint (lexpr, op, rexpr) -> 
						let ineq = linear_inequality_of_linear_constraint (lexpr, op, rexpr) in
						continuous_updates := ineq :: !continuous_updates 	
		) 			
  ) update;
	(* fuse all inequalities to one constraint for continuous updates *)
	let update_constraint = LinearConstraint.make !continuous_updates in
	(* get support variables for creating the update set *)
	let support = LinearConstraint.support update_constraint in
	(* consider only primed (= updated) variables *)
	let primes = VariableSet.filter (is_primed_variable) support in
	(* convert to normal variables *)
	let updated = VariableSet.fold (fun pv varset -> 
		VariableSet.add (unprime_variable pv) varset
	) primes VariableSet.empty in
	(* check variables in support *)
	VariableSet.iter (fun var -> 
		match type_of_variables var with
			| Var_type_discrete -> print_error "illegal discrete update"
			| Var_type_parameter -> print_error "illegal parameter update"
			| _ -> ()
	) updated;
	if (VariableSet.is_empty updated) then 
		(* keep clocks stable, analogs are free *)
		(Clocks_stable, !discrete_updates)
	else
		(* add stable constraints for non-updated local clocks *)
		let local_clocks = VariableSet.filter (fun v -> 
			match type_of_variables v with
				| Var_type_clock -> true
				| _ -> false
		) local_variables in
		let non_updated_clocks = VariableSet.elements (VariableSet.diff local_clocks updated) in
		let stable_clock_pairs = List.map (fun v -> (v, v + !glob_prime_offset)) non_updated_clocks in
		let stable_constr = LinearConstraint.make_equalities stable_clock_pairs in
		LinearConstraint.intersection_assign update_constraint [stable_constr];
	  (* return the converted data structures *)
	  (Update update_constraint, !discrete_updates)
	
(*--------------------------------------------------*)
(* Convert the transitions *)
(*--------------------------------------------------*)
(* Convert the structure: 'automaton_index -> location_index -> list of (action_index, guard, resets, dest_state)' into a structure: 'automaton_index -> location_index -> action_index -> list of (guard, resets, dest_state)' *)
let convert_transitions nb_actions type_of_variables continuous_vars transitions =
	(* Create the empty array *)
	let array_of_transitions = Array.make (Array.length transitions) (Array.make 0 (Array.make 0 [])) in
	(* Iterate on automata *)
	Array.iteri (fun automaton_index transitions_for_this_automaton ->
		let nb_locations = Array.length transitions_for_this_automaton in 
		(* Set the array for this automaton *)
		array_of_transitions.(automaton_index) <- Array.make nb_locations (Array.make 0 []);
		(* Iterate on locations *)
		Array.iteri (fun location_index transitions_for_this_location ->
			(* Set the array for this location *)
			array_of_transitions.(automaton_index).(location_index) <- Array.make nb_actions [];
			(* Iterate on transitions *)
			List.iter (fun (action_index, guard, update, dest_location_index) ->
				(* Convert the guard *)
				let converted_guard = linear_constraint_of_convex_predicate guard in
				(* Convert the updates *)
				let local_vars = continuous_vars automaton_index in
				let converted_updates = convert_update type_of_variables local_vars update in
				(* Split between the continuous and discrete updates *)
				let continuous_updates, discrete_updates = converted_updates in
				(* Update the transition *)
				array_of_transitions.(automaton_index).(location_index).(action_index) <- (converted_guard, continuous_updates, discrete_updates, dest_location_index) :: array_of_transitions.(automaton_index).(location_index).(action_index);

			) transitions_for_this_location;
		) transitions_for_this_automaton;
	) transitions;
	(* Return a functional representation *)
	fun automaton_index location_index action_index -> array_of_transitions.(automaton_index).(location_index).(action_index)


(*--------------------------------------------------*)
(* Create the initial state *)
(*--------------------------------------------------*)
let make_initial_state index_of_automata locations_per_automaton index_of_locations type_of_variables init_discrete init_definition =
	(* Get the location initialisations and the constraint *)
	let loc_assignments, linear_predicates = List.partition (function
		| Loc_assignment _ -> true
		| _ -> false
	) init_definition in
	(* Make couples (automaton_name, location_name) *)
	let initial_locations = List.map (function
		| Loc_assignment (automaton_name, location_name) -> (automaton_name, location_name)
		| _ -> raise (InternalError "Something else than a Loc_assignment was found in a Loc_assignment list")
	) loc_assignments in
	(* Convert the couples to automaton_index, location_index *)
	let locations = List.map (fun (automaton_name, location_name) ->
		(* Find the automaton index *)
		let automaton_index = Hashtbl.find index_of_automata automaton_name in
		(* Find the location index *)
		automaton_index,
 		Hashtbl.find index_of_locations.(automaton_index) location_name
	) initial_locations in
	(* Construct the initial location *)
	let initial_location = Automaton.make_location locations init_discrete in
	(* Remove the init definitions for discrete variables *)
	let other_inequalities = List.filter (function
		(* Check if the left part is only a variable name *)
		| Linear_predicate (Linear_constraint (Linear_term (Variable (_, variable_name)), _ , _)) ->
			let is_discrete =
			try (
				(* Get the variable index *)
				let variable_index =  Hashtbl.find !index_of_variables variable_name in
				(* Keep if this is a discrete *)
				type_of_variables variable_index = Var_type_discrete
			(* If not existing : false *)
			) with
			Not_found -> (
				raise (InternalError ("The variable '" ^ variable_name ^ "' mentioned in the init definition does not exist, although this was checked before.")))
			in not is_discrete
		| _ -> true
	) linear_predicates in
	(* Convert the inequalities *)
	let convex_predicate = List.map (function 
		| Linear_predicate lp -> lp
		| _ -> raise (InternalError "Something else than a Linear_predicate was found in a Linear_predicate list.")
	) other_inequalities in
	let initial_constraint = linear_constraint_of_convex_predicate convex_predicate in
	(* Return the initial state *)
	initial_location, initial_constraint


(*--------------------------------------------------*)
(* Convert the parsed pi0 into a valid pi0 *)
(*--------------------------------------------------*)
let make_pi0 parsed_pi0 variables nb_parameters =
	let pi0 = Array.make nb_parameters NumConst.zero in
	for i = 0 to nb_parameters - 1 do
		let parameter_name = variables.(i) in
		let value = try(
			List.assoc parameter_name parsed_pi0
			) with Not_found ->
			raise (InternalError ("The parameter name '" ^ parameter_name ^ "' was not found in pi0 although checks should have been performed before."))
		in
		pi0.(i) <- value
	done;
	pi0

let make_pi0cube parsed_pi0cube nb_parameters =
	let pi0cube = Array.make nb_parameters (NumConst.zero, NumConst.zero, NumConst.one) in
	List.iter (fun (variable_name, a, b, c) ->
		let variable_index = try Hashtbl.find !index_of_variables variable_name
			with Not_found ->
			raise (InternalError ("The variable name '" ^ variable_name ^ "' was not found in the list of variables although checks should have been performed before."))
		in
		pi0cube.(variable_index) <- (a, b, c)
	) parsed_pi0cube;
	pi0cube


(*--------------------------------------------------*)
(* Convert the parsing structure into an abstract program *)
(*--------------------------------------------------*)
let abstract_program_of_parsing_structure (parsed_variable_declarations, parsed_automata, parsed_init_definition) parsed_pi0 parsed_pi0cube ~acyclic:acyclic ~sync_auto_detection:sync_auto_detection ~inclusion_mode:inclusion_mode ~union_mode:union_mode ~no_random:no_random ~with_parametric_log:with_parametric_log imitator_mode program_name =
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug functions *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug print function for arrays *)
	let debug_print_array =
		Array.iteri (fun i e ->
			print_message Debug_high ((string_of_int i) ^ " -> " ^ e)
		)
	in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get names *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get the declared variable names *)
	let analog_names, clock_names, discrete_names, parameters_names = get_declared_variable_names parsed_variable_declarations in
	(* Get the declared automata names *)
	let declared_automata_names = get_declared_automata_names parsed_automata in
	(* Get the declared synclabs names *)
	let synclabs_names = get_declared_synclabs_names parsed_automata in

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the synclabs declarations *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let synclabs_names, removed_synclab_names = if sync_auto_detection then synclabs_names, [] else (
		(* Keep only the synclabs which are used in ALL the automata where they are declared *)
		List.partition (fun synclab_name -> if synclab_used_everywhere parsed_automata synclab_name then
			(* If it is used everywhere: keep *)
			true
			(* If there exists an automaton where it is not used : warns and remove *)
			else (print_warning ("The synclab " ^ synclab_name ^ " is not used in some of the automata where it is declared: it will thus be removed."); false)
		) synclabs_names
	) in
	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the variable_declarations *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all variable names are different (and print warnings for multiply-defined variables if same type) *)
	let all_variables_different = check_variable_names analog_names clock_names discrete_names parameters_names in
	(* Check that all automata names are different *)
	let all_automata_different = check_declared_automata_names declared_automata_names in

	(***** FIXME: For now, merge analogs into clocks *)
			
	(* Keep every element only once in those 4 lists *)	
	let clock_names = list_only_once clock_names in
	let analog_names = list_only_once analog_names in
	let discrete_names = list_only_once discrete_names in
	let parameters_names = list_only_once parameters_names in
	
	(* Make only one list for all variables *)
	let variable_names = list_append (list_append (list_append parameters_names analog_names) clock_names) discrete_names in
	
	(* Numbers *)
	let nb_automata = List.length declared_automata_names in
	let nb_labels = List.length synclabs_names in
	let nb_analogs = List.length analog_names in
	let nb_clocks = List.length clock_names in
	let nb_discrete = List.length discrete_names in
	let nb_parameters = List.length parameters_names in
	let nb_variables = List.length variable_names in
	
	(* Check that at least one automaton is defined *)
	let at_least_one_automaton =
		if nb_automata = 0 then (
			print_error ("At least one automaton should be declared."); false
		) else true in
	
	(* Perform intersection and may raise exception *)
 	if not (all_variables_different && all_automata_different && at_least_one_automaton) then raise InvalidProgram;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the LinearConstraint manager *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let nb_integer_variables = 0 in
	(* 'nb_variables' represent the total number of variables, including renamings (see below) *)
	let nb_real_variables = 2 * (nb_analogs + nb_clocks) + nb_discrete + nb_parameters in
	LinearConstraint.set_manager nb_integer_variables nb_real_variables;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the arrays *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	
	(* The array of variables names ; index -> variable name *)
	let variables = Array.of_list variable_names in
	(* A (constant) hash table 'variable name -> index' *)
	index_of_variables := Hashtbl.create nb_variables;
	for i = 0 to nb_variables - 1 do
		Hashtbl.add !index_of_variables variables.(i) i;
	done;
	
	let first_parameter_index = 0 in
	let first_analog_index    = first_parameter_index + nb_parameters in
	let first_clock_index     = first_analog_index + nb_analogs in
	let first_discrete_index  = first_clock_index + nb_clocks in
	
	(* An array 'variable index -> AbstractImitatorFile.var_type' *)
	let type_of_variables = Array.make nb_variables AbstractImitatorFile.Var_type_parameter in	
	for i = first_analog_index to first_clock_index - 1 do
		type_of_variables.(i) <- AbstractImitatorFile.Var_type_analog;
	done;
	for i = first_clock_index to first_discrete_index - 1 do
		type_of_variables.(i) <- AbstractImitatorFile.Var_type_clock;
	done;
	for i = first_discrete_index to nb_variables - 1 do
		type_of_variables.(i) <- AbstractImitatorFile.Var_type_discrete;
	done;
	(* Functional representation *)
	let type_of_variables = fun variable_index -> type_of_variables.(variable_index) in

	(* Create the lists of different variables *)
	let parameters = list_of_interval first_parameter_index (first_analog_index - 1) in
	let analogs    = list_of_interval first_analog_index (first_clock_index - 1) in
	let clocks     = list_of_interval first_clock_index (first_discrete_index - 1) in
	let discrete   = list_of_interval first_discrete_index (nb_variables - 1) in

	(* merged list of analog and clock variables *)
	let analogs_and_clocks = list_append analogs clocks in
	
	(* create a set of all continuous variables *)
	let continuous = List.fold_left (fun set var -> 
		VariableSet.add var set
(*	) VariableSet.empty clocks in*)
	) VariableSet.empty analogs_and_clocks in

	(* Create the type check functions *)
	let is_analog = (fun variable_index -> try (type_of_variables variable_index = Var_type_analog) with Invalid_argument _ ->  false) in
	let is_clock = (fun variable_index -> try (type_of_variables variable_index = Var_type_clock) with Invalid_argument _ ->  false) in
	let is_discrete = (fun variable_index -> try (type_of_variables variable_index = Var_type_discrete) with Invalid_argument _ ->  false) in

	(* Add more variables to allow renaming *) 

	print_message Debug_total ("*** Building renamed variables...");
	(* The renamed variable indexes are set just after the current variables *)	
	let offset = nb_analogs + nb_clocks + nb_discrete in
	let prime_of_variable variable_index = variable_index + offset in
	let variable_of_prime variable_index = variable_index - offset in

	(* set global variables for use in other functions *)
	glob_nb_variables := nb_variables;
	glob_prime_offset := offset;

	(* Clocks: add the new indexes *)
	let renamed_clocks = list_of_interval nb_variables (nb_variables + nb_analogs + nb_clocks - 1) in

	(* Create the function is_renamed_clock *)
	let is_renamed_clock  = (fun variable_index -> variable_index >= nb_variables && 
																								 variable_index < nb_variables + nb_analogs + nb_clocks) in

	(* Create an array for all variable names with renamings *)
	let array_of_variable_names = Array.make (nb_variables + nb_analogs + nb_clocks + 1) "" in
	(* Add normal names *)
	for variable_index = 0 to nb_variables - 1 do
		array_of_variable_names.(variable_index) <- variables.(variable_index);
	done;
	(* Add renamed clocks and discrete *)
	for variable_index = nb_variables to nb_variables + nb_analogs + nb_clocks - 1 do
		array_of_variable_names.(variable_index) <- variables.(variable_of_prime variable_index) ^ "_PRIME";
	done;
	(* Add 'd' *)
	(**** TO DO: should avoid the declaration of 'd' as a variable name (to do later, and only important in debug mode...) *)
(*	array_of_variable_names.(d) <- "d";*)

	(* save the list for later use *)
	let variable_name_list = variable_names in

	(* Create the functional representation *)
	let variable_names = fun i -> array_of_variable_names.(i) in
	
	(* set gloabl reference *)
	glob_variable_names := array_of_variable_names;

	(* Couples (x, x') for renamings *)
	let renamed_clocks_couples = List.map (fun index -> index, prime_of_variable index) analogs_and_clocks in
	
	(* Couples (x', x) for 'un'-renamings *)	
	let unrenamed_clocks_couples = List.map (fun index -> prime_of_variable index, index) analogs_and_clocks in	

	(* Variables *)
	print_message Debug_high ("\n*** Variables with renamings:");
	for i = 0 to nb_variables + nb_analogs + nb_clocks do
		print_message Debug_high ("  "
			^ (string_of_int i) ^ " : " ^ (variable_names i)
			^ (if is_renamed_clock i then " (renamed clock)" else "")
		);
	done;

		(* The list of automata *)
	let automata = list_of_interval 0 (nb_automata - 1) in
	
	(* The array of automata names ; index -> automaton name *)
	let array_of_automata_names = Array.of_list declared_automata_names in
	(* A (constant) hash table 'automaton name -> index' *)
	let index_of_automata = Hashtbl.create nb_automata in
	for i = 0 to nb_automata - 1 do
		Hashtbl.add index_of_automata array_of_automata_names.(i) i;
	done;
	
	(* Functional version *)
	let automata_names = fun automaton_index -> array_of_automata_names.(automaton_index) in
	
	(* create a table for local continuous variables per automaton *)
	let continuous_per_automaton = get_variables_per_automaton index_of_automata parsed_variable_declarations in
	
	(* The array of labels ; index -> label name *)
	let labels = Array.of_list synclabs_names in
	(* A (constant) hash table 'label name -> index' *)
	let index_of_labels = Hashtbl.create nb_labels in
	for i = 0 to nb_labels - 1 do
		Hashtbl.add index_of_labels labels.(i) i;
	done;	
						
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *)
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Numbers *)
	print_message Debug_low (
		(string_of_int nb_automata) ^ " automata, "
		^ (string_of_int nb_labels) ^ " declared label" ^ (s_of_int nb_labels) ^ ", "
		^ (string_of_int nb_analogs) ^ " analog variable" ^ (s_of_int nb_analogs) ^ ", "
		^ (string_of_int nb_clocks) ^ " clock variable" ^ (s_of_int nb_clocks) ^ ", "
		^ (string_of_int nb_discrete) ^ " discrete variable" ^ (s_of_int nb_discrete) ^ ", "
		^ (string_of_int nb_parameters) ^ " parameter" ^ (s_of_int nb_parameters) ^ ", "
		^ (string_of_int nb_variables) ^ " variable" ^ (s_of_int nb_variables) ^ "."
	);
	
	(* Automata *)
	print_message Debug_high ("\n*** Array of automata names:");
	debug_print_array array_of_automata_names;

	(* Labels *)
	print_message Debug_high ("\n*** Array of declared label names:");
	debug_print_array labels;

	(* Variables *)
	print_message Debug_high ("\n*** Variables:");
	Array.iteri (fun i e ->
		print_message Debug_high ((string_of_int i) ^ " -> " ^ e ^ " : " ^ (string_of_var_type (type_of_variables i)))
	) variables;
	

	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Get all the locations *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check that all the location names of an automaton are different *)
	if not (all_locations_different parsed_automata) then raise InvalidProgram;
	
	(* Get all the locations for each automaton: automaton_index -> location_index -> location_name *)
	let array_of_location_names = make_locations_per_automaton index_of_automata parsed_automata in
	(* A (constant) array of hash tables 'automaton_index -> location_name -> location_index' *)
	let index_of_locations = Array.create nb_automata (Hashtbl.create 0) in
	for automaton_index = 0 to nb_automata - 1 do
		let nb_locations = Array.length (array_of_location_names.(automaton_index)) in
		let location_index_table = Hashtbl.create nb_locations in
		for location_index = 0 to nb_locations - 1 do
			Hashtbl.add location_index_table array_of_location_names.(automaton_index).(location_index) location_index;
		done;
		index_of_locations.(automaton_index) <- location_index_table;
	done;
	(* Create the access function returning a list of locations*)
	let array_of_locations_per_automaton = Array.make nb_automata [] in
	for automaton_index = 0 to nb_automata - 1 do
		array_of_locations_per_automaton.(automaton_index) <-
		Array.to_list (Array.mapi (fun location_index _ -> location_index) array_of_location_names.(automaton_index));
	done;
	let locations_per_automaton = fun automaton_index -> array_of_locations_per_automaton.(automaton_index) in
	(* Create the access function returning a location name *)
	let location_names = fun automaton_index location_index -> array_of_location_names.(automaton_index).(location_index) in



	(* Debug print *)
	print_message Debug_high ("\n*** Locations per automaton:");
	List.iter (fun automaton_index ->
		print_message Debug_high ((automata_names automaton_index) ^ " : ");
		List.iter (fun location_index ->
			print_message Debug_high ("    " ^ (string_of_int location_index) ^ " -> " ^ (location_names automaton_index location_index) ^ "");
		)
		(locations_per_automaton automaton_index);
	) automata;

	
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the automata *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Debug_total ("*** Checking automata...");
	if not (check_automata type_of_variables variable_name_list index_of_automata array_of_location_names parsed_automata) then raise InvalidProgram;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Check the init_definition *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Debug_total ("*** Checking init definition...");
	(* Get couples for the initialisation of the discrete variables, and check the init definition *)
	let init_discrete_couples, well_formed_init =
		check_init discrete variable_name_list type_of_variables automata automata_names index_of_automata array_of_location_names parsed_init_definition in
	if not well_formed_init then raise InvalidProgram;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Constuct the pi0 *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	let pi0, pi0cube =
		match imitator_mode with
		| Reachability_analysis -> 
			(* Return blank values *)
			Array.make 0 NumConst.zero, Array.make 0 (NumConst.zero, NumConst.zero, NumConst.one)
		| Inverse_method -> 
			print_message Debug_total ("*** Building reference valuation...");
			(* Verification of the pi_0 *)
			if not (check_pi0 parsed_pi0 parameters_names) then raise InvalidPi0;
			(* Construction of the pi_0 *)
			let pi0 = make_pi0 parsed_pi0 variables nb_parameters in
			(* Return the pair *)
			pi0, Array.make 0 (NumConst.zero, NumConst.zero, NumConst.one)
		| _ -> 
			print_message Debug_total ("*** Building reference rectangle...");
			(* Verification of the pi_0 *)
			if not (check_pi0cube parsed_pi0cube parameters_names) then raise InvalidPi0;
			(* Construction of the pi_0 *)
			let pi0cube = make_pi0cube parsed_pi0cube nb_parameters in
			(* Return the pair *)
			Array.make 0 NumConst.zero, pi0cube
	in
	
	(* Make a functional version of the pi0 *)
	let pi0 = fun index -> pi0.(index) in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the automata *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Debug_total ("*** Building automata...");
	(* Get all the possible actions for every location of every automaton *)
	let actions, action_names, action_types, actions_per_automaton, actions_per_location, invariants, flows, transitions =
		make_automata index_of_automata index_of_locations labels index_of_labels removed_synclab_names parsed_automata in
	let nb_actions = List.length actions in

	(* List of automata for every action *)
	print_message Debug_total ("*** Building automata per action...");
	let automata_per_action = make_automata_per_action actions_per_automaton nb_automata nb_actions in

	(* Convert the invariants *)
	print_message Debug_total ("*** Building invariants...");
	let invariants = convert_invariants index_of_variables invariants in
  	
  print_message Debug_total ("*** Building flow conditions...");		
	(* Convert the flow conditions *)
	let standard_flow = construct_standard_flow parameters discrete clocks in
	let analog_flows = 
		if nb_analogs = 0 then(
			 (* Return empty flow for all automata and locations *)
			 fun _ _ -> None
		) else (
			 (* Convert the annotated rate conditions *)			 
			 let flow_table = convert_flows type_of_variables flows in
			 fun aut_index loc_index -> flow_table.(aut_index).(loc_index) 
		) in 
	
	(* Convert the transitions *)
	print_message Debug_total ("*** Building transitions...");
	let transitions = convert_transitions nb_actions type_of_variables continuous_per_automaton transitions in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Set the number of discrete variables *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	let min_discrete_index = first_discrete_index in
	let max_discrete_index = nb_variables - 1 in
	Automaton.initialize nb_automata min_discrete_index max_discrete_index;


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Construct the initial state *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	print_message Debug_total ("*** Building initial state...");
	let initial_state = make_initial_state index_of_automata array_of_location_names index_of_locations type_of_variables init_discrete_couples parsed_init_definition in


	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Debug prints *) 
	(**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* All action names *)
	print_message Debug_total ("\n*** All action names:");
	(* For each action *)
	List.iter (fun action_index -> 
		print_message Debug_total ((string_of_int action_index) ^ " -> " ^ (action_names action_index));
	) actions;

	(* Debug print: actions per automaton *)
	print_message Debug_total ("\n*** Actions per automaton:");
	(* For each automaton *)
	List.iter (fun automaton_index ->
		(* Get the actions *)
		let actions = actions_per_automaton automaton_index in
		(* Print it *)
		let actions_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
		print_message Debug_total ((automata_names automaton_index) ^ " : " ^ actions_string)
	) automata;

	(* Debug print: automata per action *)
	print_message Debug_total ("\n*** Automata per action:");
	(* For each action *)
	List.iter (fun action_index ->
		(* Get the automata *)
		let automata = automata_per_action action_index in
		(* Print it *)
		let automata_string = string_of_list_of_string_with_sep ", " (List.map automata_names automata) in
		print_message Debug_total ((action_names action_index) ^ " : " ^ automata_string)
	) actions;

	(* Possible actions per location *)
	print_message Debug_total ("\n*** Possible actions per location:");
	(* For each automaton *)
	List.iter (fun automaton_index ->
		(* Print the automaton name *)
		print_message Debug_total ("" ^ (automata_names automaton_index) ^ " :");
		(* For each location *)
		List.iter (fun location_index ->
			(* Get the actions *)
			let actions = actions_per_location automaton_index location_index in
			(* Print it *)
			let my_string = string_of_list_of_string_with_sep ", " (List.map action_names actions) in
			print_message Debug_total (" - " ^ (location_names automaton_index location_index) ^ " :" ^ my_string);
		) (locations_per_automaton automaton_index);
	) automata;

	(* Debut print: Pi0 *)
	let _ =
	match imitator_mode with
	| Reachability_analysis -> ()
	| Inverse_method -> 
		print_message Debug_medium ("\n*** Reference valuation pi0:");
		List.iter (fun parameter ->
			print_message Debug_medium (
				variables.(parameter) ^ " : " ^ (NumConst.string_of_numconst (pi0 parameter))
			)
		) parameters;
	| _ -> 
		print_message Debug_medium ("\n*** Reference rectangle V0:");
		Array.iteri (fun i (a, b, c) ->
			print_message Debug_medium (
				variables.(i) ^ " : [" ^ (NumConst.string_of_numconst a) ^ ", " ^ (NumConst.string_of_numconst b) ^ "] step " ^ (NumConst.string_of_numconst c)
			)
		) pi0cube
	in


	(* Make the structure *)
	{
	(* Cardinality *)
	nb_automata = nb_automata;
	nb_actions = nb_actions;
	(* nb_analogs = nb_analogs; *)
	nb_clocks = nb_clocks + nb_analogs;
	nb_discrete = nb_discrete;
	nb_parameters = nb_parameters;
	nb_variables = nb_variables;

	continuous = continuous;
	continuous_per_automaton = continuous_per_automaton;

	(* The list of clock indexes *)
	clocks = analogs_and_clocks;
	(* True for analogs, false otherwise *)
	is_analog = is_analog;	
	(* True for clocks, false otherwise *)
	is_clock = is_clock;
	(* The list of discrete indexes *)
	discrete = discrete;
	(* True for discrete, false otherwise *)
	is_discrete = is_discrete;
	(* The list of parameter indexes *)
	parameters = parameters;
	(* The non parameters (clocks and discrete) *)
	clocks_and_discrete = list_append analogs_and_clocks discrete;
	(* The function : variable_index -> variable name *)
	variable_names = variable_names;
	(* The type of variables *)
	type_of_variables = type_of_variables;

	(* Renamed clocks *)
	renamed_clocks = renamed_clocks;
	(* True for renamed clocks, false otherwise *)
	is_renamed_clock = is_renamed_clock;
	(* Get the 'prime' equivalent of a variable *)
	prime_of_variable = prime_of_variable;
	(* Get the normal equivalent of a 'prime' variable *)
	variable_of_prime = variable_of_prime;
	(* Couples (x, x') for clock renamings *)
	renamed_clocks_couples = renamed_clocks_couples;
	(* Couples (x', x) for clock 'un'-renamings *)
	unrenamed_clocks_couples = unrenamed_clocks_couples;

	(* The automata *)
	automata = automata;
	(* The automata names *)
	automata_names = automata_names;
	
	(* The locations for each automaton *)
	locations_per_automaton = locations_per_automaton;
	(* The location names for each automaton *)
	location_names = location_names;

	(* All action indexes *)
	actions = actions;
	(* Action names *)
	action_names = action_names;
	(* The type of actions *)
	action_types = action_types;
	(* The list of actions for each automaton *)
	actions_per_automaton = actions_per_automaton;
	(* The list of automatons for each action *)
	automata_per_action = automata_per_action;
	(* The list of actions for each automaton for each location *)
	actions_per_location = actions_per_location;

	(* The invariant for each automaton and each location *)
	invariants = invariants;
	(* The standard flow for parameters, discrete and clocks *)
	standard_flow = standard_flow;
	(* The rate conditions for each automaton and each location *)
	analog_flows = analog_flows;
	(* The transitions for each automaton and each location and each action *)
	transitions = transitions;

	(* Init : the initial state *)
	init = initial_state;

	(* Acyclic mode *)
	acyclic = acyclic;
	(* Inclusion for the post operation *)
	inclusion = inclusion_mode;
	(* Return union of last states *)
	union = union_mode;
	(* Random selection of the pi0-incompatible inequality *)
	random = not no_random;
	(* Mode for IMITATOR *)
	imitator_mode = imitator_mode;
	(* Mode with parametric constraints (clock elimination) in the log file *)
	with_parametric_log = with_parametric_log;
	(* The name of the program *)
	program_name = program_name;
	}

	,
	(* Also return the pi0 *)
	pi0
	,
	(* Also return the pi0cube *)
	pi0cube
