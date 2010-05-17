(*****************************************************************
 *
 *                     IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/12/17
 * Last modified: 2010/03/04
 *
 ****************************************************************)



(**************************************************)

(** Modules *)
(**************************************************)

open Global
open Constraint
open AbstractImitatorFile
open Apron
open Lincons0


(**************************************************)
(** Global variables *)
(**************************************************)

let manager = Polka.manager_alloc_strict ()


(**************************************************)
(** Function for conversion to and from Apron *)
(**************************************************)

(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(* To Apron *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(* Convert a Constraint.op into a Apron Lincons0.typ *)
let apron_typ_of_op = function
	| Op_g -> Lincons0.SUP
	| Op_ge -> Lincons0.SUPEQ
	| Op_eq -> Lincons0.EQ


(* Convert a variable into a Apron dim *)
let apron_dim_of_variable variable =
	variable

(* Convert a constant into a Apron coeff *)
let apron_coeff_of_constant constant =
	Coeff.s_of_mpq (NumConst.mpq_of_numconst constant)


(* Convert a constant into a Apron coeff option *)
let apron_coeff_option_of_constant constant =
	if NumConst.equal constant NumConst.zero then None
	else Some (apron_coeff_of_constant constant)


(* Convert a constant into a Apron coeff option *)
let apron_coeff_dim_of_member (coef, variable) =
	apron_coeff_of_constant coef, apron_dim_of_variable variable


(* Convert a linear term into a Apron linear expression *)
let apron_linexpr0_of_linear_term (member_list, constant) =
	Linexpr0.of_list
		None
		(List.map apron_coeff_dim_of_member member_list)
		(apron_coeff_option_of_constant constant)


(* Convert a linear inequality into a Apron linear constraint *)
let apron_lincons0_of_linear_inequality (linear_term, op) =
	Lincons0.make
		(apron_linexpr0_of_linear_term linear_term)
		(apron_typ_of_op op)

(* Convert a list of inequalities into a Apron Abstract0.'a t *)
let apron_abstract0_of_linear_inequalities program list_of_inequalities =
	let list_of_lincons = List.map apron_lincons0_of_linear_inequality list_of_inequalities in
	let my_abstract = Abstract0.of_lincons_array
		manager
		program.nb_variables
		program.nb_variables
		(Array.of_list list_of_lincons)
	in
	my_abstract


(* Convert a linear constraint into a Apron Abstract0.'a t *)
let apron_abstract0_of_constraint program = function
	| LC_false -> Abstract0.bottom manager program.nb_variables program.nb_variables
	| LC_maybe inequalities -> apron_abstract0_of_linear_inequalities program inequalities


(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
(* To Apron *)
(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)

(* Convert an Apron Lincons0.typ into a Constraint.op *)
let op_of_apron_typ = function
	| Lincons0.SUP -> Op_g
	| Lincons0.SUPEQ -> Op_ge
	| Lincons0.EQ -> Op_eq
	| _ -> raise (InternalError "Non standard operator in 'op_of_apron_typ' function.")


(* Convert a Apron dim into a variable  *)
let variable_of_apron_dim dim =
	dim


(* Convert a Apron coeff into a constant *)
let constant_of_apron_coeff coeff =
	match coeff with
	| Coeff.Scalar s -> let constant =
		match s with
			Scalar.Mpqf m -> NumConst.numconst_of_mpq (Mpqf.mpq m)
			| _ -> raise (InternalError "Expecting Apron to use only Mpqf in Scalar, in 'constant_of_apron_coeff'  function.")
			in constant
	| _ -> raise (InternalError "Expecting Apron to use only Scalar coeff in 'constant_of_apron_coeff' function.")


(* Convert an Apron linear constraint into a linear inequality *)
let linear_term_of_apron_linexpr linexpr =
	(* Convert the constant *)
	let constant = constant_of_apron_coeff (Linexpr0.get_cst linexpr) in
	(* Convert the members *)
	let members = ref [] in
	Linexpr0.iter (fun coeff dim ->
		(* Add the member only if not null *)
		let coef = constant_of_apron_coeff coeff in
		if NumConst.neq coef NumConst.zero then (
		members := (coef, variable_of_apron_dim dim) :: !members;);
	) linexpr;
	(* Return the linear term *)
	!members, constant


(* Convert an Apron linear constraint into a linear inequality *)
let inequality_of_apron_lcons lcons =
	let linear_term = linear_term_of_apron_linexpr lcons.linexpr0 in
	let op = op_of_apron_typ lcons.typ in

	let string_of_var = fun i -> "x" ^ (string_of_int i) in
	linear_term, op


(* Convert an Apron abstract0 into a linear constraint *)
let constraint_of_apron_abstract0 abstract =
	if Abstract0.is_bottom manager abstract then LC_false
	else(
		let lcons_list = Array.to_list (Abstract0.to_lincons_array manager abstract) in
			LC_maybe (List.map inequality_of_apron_lcons lcons_list)
	)
	


(**************************************************)
(** String functions: use HyTech *)
(**************************************************)


let hytech_tag       = "---INTERESTING ZONE DELIMITER---"
let hytech_tag_true  = "---TRUE :-)---"
let hytech_tag_false = "---FALSE :-)---"

(* Convert a variable into a string *)
let hytech_of_variable variable =
	"var" ^ (string_of_int variable)
	
(* Convert a 'coef' into a string *)
let hytech_of_coef = NumConst.string_of_numconst

(* Convert a 'constant' into a string *)
let hytech_of_constant = NumConst.string_of_numconst

(* Convert an 'op' into a string *)
let hytech_of_op = function
	| Op_g -> ">"
	| Op_ge -> ">="
	| Op_eq -> "="

(* Convert a member into a string *)
let hytech_of_member (coef, variable) =
	(* Case coef 1 *)
	if NumConst.equal coef NumConst.one then (hytech_of_variable variable)
	(* Case coef -1 *)
	else if NumConst.equal coef (NumConst.numconst_of_int (-1)) then ("- " ^ (hytech_of_variable variable))
	(* Other case *)
	else ((hytech_of_coef coef) ^ " * " ^ (hytech_of_variable variable))

(* Convert various constants into a string *)
let hytech_of_zero = "0"
let hytech_of_true = "True"
let hytech_of_false = "False"



(* Convert a linear term into a string *)
let hytech_of_term (list_of_members, constant) =
	(* TODO (ou pas) : remove the members whose coef is equal to 0 *)
	match list_of_members with
	(* Case: empty list *)
	| [] -> hytech_of_constant constant
	(* Case: non-empty list *)
	| (first_coef, first_variable) :: rest ->
		(* Convert the first *)
		(
			(* HyTech needs a "0" before a negative number *)
			(if NumConst.l first_coef NumConst.zero then "0 " else "")
			(* Convert normally the first *)
			^ (hytech_of_member (first_coef, first_variable))
		)
		(* Convert the rest *)
		^ (List.fold_left (fun the_string (coef, variable) -> 
			the_string
			(* Add the +/- *)
			 ^ (if NumConst.g coef NumConst.zero then " + " else " ")
			(* Convert the member *)
			 ^ (hytech_of_member (coef, variable))
		) "" rest)
		(* Convert the constant *)
		^ (if NumConst.neq constant NumConst.zero then(
			(* Add the +/- *)
			 (if NumConst.g constant NumConst.zero then " + " else " ")
			^ (hytech_of_constant constant)
			) else ""
		)

(* Convert an inequality into a string *)
let hytech_of_inequality (linear_term, op) =
	(hytech_of_term linear_term)
	^ " " ^ (hytech_of_op op)
	^ " " ^ hytech_of_zero


(* Convert a constraint into a string *)
let hytech_of_constraint = function
	(* Case false *)
	| LC_false -> hytech_of_false
	(* Other case: *)
	| LC_maybe linear_constraint -> begin match linear_constraint with 
		(* Case empty list: true *)
		| [] -> hytech_of_true
		(* Case non-empty list: iterate *)
		| first :: rest ->
			string_of_list_of_string_with_sep " & "
			(List.map (fun inequality -> hytech_of_inequality inequality) (first :: rest))
		end


(** Make a header for a HyTech program *)
let hytech_header program =
	"-- START OF TEMPORARY HYTECH FILE" ^ "\n"
	^
	"\n" ^ "var "
	^
	(* Variable declarations *)
	let string_of_variables list_of_variables =
		string_of_list_of_string_with_sep ", " (List.map (fun v -> hytech_of_variable v) list_of_variables) in
	(if program.nb_clocks > 0 then
		("\n\t" ^ (string_of_variables (List.rev_append program.clocks program.renamed_clocks)) ^ "\n\t\t: clock;") else "")
	^
	(if program.nb_discrete > 0 then
		("\n\t" ^ (string_of_variables (List.rev_append program.discrete program.renamed_discrete)) ^ "\n\t\t: discrete;") else "")
	^
	("\n\t" ^ (string_of_variables (program.d :: program.parameters)) ^ "\n\t\t: parameter;")
	(* Sample stupid automaton *)
	^ "\n" ^ "automaton aa"
	^ "\n" ^ "synclabs: ;"
	^ "\n" ^ "initially A1;"
	^ "\n" ^ "loc A1: while True wait {}"
	^ "\n" ^ "  when True do {} goto A1;"
	^ "\n" ^ "end -- aa"
	(* Declaration of the region *)
	^ "\n" ^ "var region_toto1, region_toto2 : region;"
	(* A special string to be found later *)
	^ "\n" ^ "prints \"" ^ hytech_tag ^ "\";"


(** Make a footer for a HyTech program *)
let hytech_footer program =
	(* A special string to be found later *)
	"\n" ^ "prints \"" ^ hytech_tag ^ "\";"
	^ "\n" ^ "-- END OF TEMPORARY HYTECH FILE"


(* Call hytech on hytech_program and return the adress of the log file *)
let hytech program hytech_program =
	(* Time counter *)
	let start_time = Unix.gettimeofday() in
	(* Various file names *)
	let hytech_file = program.program_name ^ ".tmp.hy" in
	let log_file = program.program_name ^ ".tmp.log" in
	(* Create the temporary HyTech file *)
	let oc = open_out hytech_file in
	output_string oc hytech_program;
	close_out oc; (* flush and close the channel *)
	print_message Debug_total ("Temporary HyTech file '" ^ hytech_file ^ "' successfully created.");
	(* Execute HyTech *)
	let command_result = Sys.command ("hytech " ^ hytech_file ^ " 1> " ^ log_file ^ " 2>> " ^ log_file ^ "") in
(* 	let command_result = Sys.command ("hytech " ^ hytech_file ^ " 2>\&1 " ^ log_file ^ "") in *)
	print_message Debug_total ("Result of the HyTech command: " ^ (string_of_int command_result));
	(* Increment the counter *)
	program.nb_hytech_calls := !(program.nb_hytech_calls) + 1;
	(* Remove the temporary HyTech file *)
	Sys.remove hytech_file;
	print_message Debug_total ("Temporary HyTech file '" ^ hytech_file ^ "' successfully removed.");
	(* Read the log file *)
	let ic = open_in log_file in
	let tmp_log_file_content = ref "" in
	let log_file_content =
	try(
	while true do
		let new_line = input_line ic in
		tmp_log_file_content := (!tmp_log_file_content) ^ new_line ^ "\n";
	done; "") with End_of_file -> !tmp_log_file_content
	in
	close_in ic;
(*	print_message Debug_total "\nLog generated by HyTech:";
	print_message Debug_total log_file_content;*)
	(* Remove the temporary log file *)
	Sys.remove log_file;
	print_message Debug_total ("Temporary log file '" ^ log_file ^ "' successfully removed.");
	(* Only keep the interesting part within the tags *)
	let split = Str.split (Str.regexp_string hytech_tag) log_file_content in
	let interesting_part = try (List.nth split 1) with Failure _ ->
		print_error ("Error in the following HyTech log file: \n" ^ log_file_content);
		raise (InternalError "Something went wrong when parsing a HyTech file. It seems that the tags were not found.") in
	print_message Debug_total "\nInteresting part of the log:";
	print_message Debug_total interesting_part;
	(* Time counter *)
	let end_time = Unix.gettimeofday() in
	(**** BAD PROG: this should be defined somewhere else ****)
	let time_diff = ((float_of_int) (int_of_float ((end_time -. start_time) *. 1000.0))) /. 1000.0 in
	print_message Debug_total ("*** Call to HyTech finished after " ^ (string_of_seconds time_diff) ^ ".\n");
	(* Return the content of the log file *)
	interesting_part


(* Parse a hytech log file and return a constraint *)
let hytech_parser program log_file =
	(* Parse the log file *)
	let lexbuf = try(
		Lexing.from_string log_file
	) with
		| Sys_error e -> print_error ("The HyTech log file could not be opened.\n" ^ e); abort_program (); exit(0)
		| Failure f -> print_error ("Lexing error in HyTech log file: " ^ f); abort_program (); exit(0)
	in
	let resulting_constraint = try(
		HytechParser.main HytechLexer.token lexbuf
	) with
		| Parsing.Parse_error -> print_error ("Parsing error in HyTech log file."); abort_program (); exit 0
		| Failure f -> print_error ("Parsing error in HyTech log file."); abort_program (); exit 0
	in
	resulting_constraint



(**************************************************)

(** Real constraints functions: no use of HyTech *)
(**************************************************)


(* 'rename_variables program renaming_function c' renames all variable according to the renaming_function *)
let rename_variables program renaming_function = function
	(* Case false constraint: return false *)
	| LC_false -> LC_false
	(* Case real constraint: *)
	| LC_maybe inequalities ->
	(* Debug message *)
	print_message Debug_total "\n***** RENAMING VARIABLES *****";
	(* Add it to all the variables in 'variables' *)
	let renamed_inequalities =
		(* For all inequality *)
		List.map (fun ((member_list, constant), op) -> 
			(* For all member *)
			(
				List.map (fun (coef, variable) -> coef, renaming_function variable) member_list
				, constant
			), op
		) inequalities
	in
	(* Create the constraint *)
	let resulting_constraint = LC_maybe renamed_inequalities in
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total "Result of renaming";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);
	);
	(* Return the constraint *)
	resulting_constraint


(* 'add_d_generic program is_selected coef_d c' adds a constant 'd' multiplied by 'coef_d' to any variable variable_index s.t. is_selected variable_index = true *)
let add_d_generic program is_selected coef_d = function
	(* Case false constraint: return false *)
	| LC_false -> LC_false
	(* Case real constraint: *)
	| LC_maybe inequalities ->
	(* Get the index for variable 'd' *)
	let d = program.d in
	(* Add it to all the variables in 'variables' *)
	let inequalities_with_d =
		(* For all inequality *)
		List.map (fun ((member_list, constant), op) -> 
			(* For all member *)
			(**** TO OPTIMIZE: not tail recursive ****)
			(List.flatten (List.map (fun (coef, variable) ->
				(* Add coef_d * d *)
				if is_selected variable then [(NumConst.mul coef coef_d, d); (coef, variable)]
				else [coef, variable])
				member_list
				), constant), op
		) inequalities
	in
	(* Create the constraint *)
	let resulting_constraint = LC_maybe inequalities_with_d in
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		let adding = if NumConst.ge coef_d NumConst.zero then "adding" else "substracting" in
		let coef = if NumConst.equal coef_d NumConst.one || NumConst.equal coef_d (NumConst.numconst_of_int (-1)) then "" else NumConst.string_of_numconst (NumConst.abs coef_d) in
		print_message Debug_total ("Result of " ^ adding ^ " '" ^ coef ^ "d':");
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);
	);
	(* Return the constraint *)
	resulting_constraint


(* 'add_d program is_selected c' adds a constant 'd' to any variable variable_index s.t. is_selected variable_index = true *)
let add_d program is_selected c =
	(* Debug message *)
	print_message Debug_total "\n***** ADDING 'D' *****";
	(* Call the generic function *)
	add_d_generic program is_selected one c


(* 'sub_d program is_selected c' substracts a constant 'd' to any variable variable_index s.t. is_selected variable_index = true *)
let sub_d program is_selected c =
	(* Debug message *)
	print_message Debug_total "\n***** SUBSTRACTING 'D' *****";
	(* Call the generic function *)
	add_d_generic program is_selected minus_one c


(**************************************************)
(** pi0-compatibility: no use of HyTech *)
(**************************************************)


(* Evaluate a member w.r.t. pi0. (Raise InternalError if the value for the variable is not defined in pi0) *)
let evaluate_member pi0 (coef, variable) =
	let real_value =
		try pi0.(variable)
		with Invalid_argument message ->
			raise (InternalError ("Variable " ^ (string_of_int variable) ^ " is certainly not a parameter, while trying to evaluate a member with pi0. OCaml says: '" ^ message ^ "'"));
	in
	NumConst.mul coef real_value


(* Evaluate a term w.r.t. pi0. *)
let evaluate_term pi0 (members, constant) =
	(* Evaluate the members *)
	NumConst.add
	(List.fold_left (fun current_value member -> NumConst.add current_value (evaluate_member pi0 member)) NumConst.zero members)
	(* Add the constant *)
	constant


(* Check if an inequality is pi0-compatible *)
let is_inequality_pi0_compatible pi0 (term, op) =
	(* Evaluate the members *)
	let value = evaluate_term pi0 term in
	(* Check the equality *)
	match op with
		| Op_g -> NumConst.g value NumConst.zero
		| Op_ge -> NumConst.ge value NumConst.zero
		| Op_eq -> NumConst.equal value NumConst.zero


(* Check if a linear constraint is pi0-compatible *)
let is_pi0_compatible pi0 = function
	| LC_false -> false
	| LC_maybe c -> List.for_all (is_inequality_pi0_compatible pi0) c


(* Compute the pi0-compatible and pi0-incompatible inequalities within a constraint *)
let partition_pi0_compatible pi0 = function
	| LC_false -> raise (InternalError "Trying to partition the pi0-compatible and pi0-incompatible inequalities within a false constraint.")
	| LC_maybe inequalities -> List.partition (is_inequality_pi0_compatible pi0) inequalities


(* Negate an inequality; for an equality, perform the pi0-compatible negation *)
let negate_inequality pi0 (linear_term, op) =
	let negate = fun (member_list, constant) ->
		List.map (fun (coef, variable) -> NumConst.neg coef, variable) member_list,
		NumConst.neg constant
	in
	(* Consider the operators *)
	match op with
		(* Normal ops: easy *)
		| Op_ge -> negate linear_term, Op_g
		| Op_g -> negate linear_term, Op_ge
		(* Equality: consider two cases *)
		| Op_eq -> let value = evaluate_term pi0 linear_term in
			(* Case value > 0 *)
			if NumConst.g value NumConst.zero then linear_term, Op_g
			(* Case value < 0 *)
			else if NumConst.l value NumConst.zero then negate linear_term, Op_g
			else (
(* 				print_error (Constraint.string_of_inequality (linear_term, op)); *)
				raise (InternalError "Trying to negate an equality already true w.r.t. pi0")
			)
			


(**************************************************)
(** Real constraints functions: use Apron *)
(**************************************************)


(* Perform the intersection of a list of constraints *)
let and_constraints program list_of_constraints =
	(* Compute only one list of inequalities *)
	let list_of_inequalities = List.fold_left
		(fun list_of_inequalities linear_constraint ->
			match linear_constraint with
			(* This is not guaranteed ! If happens, should add a test before *)
			| LC_false -> raise (InternalError "Expecting to have only non-false constraints for intersection.")
			| LC_maybe list_of_i ->
				List.rev_append list_of_i list_of_inequalities)
		[]
		list_of_constraints
	in
	
	(* Convert to Abstract0 *)
	let my_abstract = apron_abstract0_of_linear_inequalities program list_of_inequalities in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nBefore intersection:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter my_abstract;
	);

	(* Perform the intersection *)
	let intersection = Abstract0.meet_array manager [|my_abstract|] in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nResult of intersection:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter intersection;
	);
	
	(* Convert the constraint to the old Imitator format *)
	let resulting_constraint = constraint_of_apron_abstract0 intersection in
		print_message Debug_total "Result of intersection in IMITATOR format:";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);

	resulting_constraint



(*
	(* Small optimization: if one of the constraints is false *)
	if List.exists (fun c -> 
	c = LC_false) list_of_constraints then LC_false else (	
	(* Debug message *)
	print_message Debug_total "\n***** PERFORMING INTERSECTION OF CONSTRAINTS *****";
	(* Convert the list of constraints into a string *)
	let constraint_index = ref 0 in
	let constraints = string_of_list_of_string_with_sep "\n" (List.map (fun c ->
		constraint_index := !constraint_index + 1;
		   "\n" ^ "& \n -- Constraint #" ^ (string_of_int !constraint_index)
		^ "\n" ^ (hytech_of_constraint c)
	) list_of_constraints) in

	(* Create the HyTech program *)
	let hytech_program =
		(hytech_header program)
	^ "\n" ^ "region_toto1 := True"
	^ constraints
(*	^ "\n" ^ "& \n -- Second constraint"
	^ "\n" ^ (hytech_of_constraint c2)*)
	^ "\n" ^ ";"
	^ "\n" ^ "print region_toto1;"
	^ (hytech_footer program) in
	print_message Debug_total "\nGenerated HyTech program:";
	print_message Debug_total hytech_program;
	(* Execute HyTech and get the log *)
	let log_file_content = hytech program hytech_program in
	(* Parse the log *)
	let resulting_constraint = hytech_parser program log_file_content in
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total "Result of intersection:";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);
	);
	(* Debug message *)
	print_message Debug_total "\n***** END OF INTERSECTION *****";
	(* Return the constraint *)
	resulting_constraint
	)*)
	
(* 	; raise (InternalError "blublu") *)


(* Hide variables in a constraint *)
let hide_variables program variables = function
	(* Optimization: if the constraint is false, return false *)
	| LC_false -> LC_false
	| LC_maybe inequalities ->

	(* Convert to Abstract0 *)
	let my_abstract = apron_abstract0_of_linear_inequalities program inequalities in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nBefore hiding variables:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter my_abstract;
	);

	(* Perform the hiding *)
	let hiding = Abstract0.forget_array
		manager
		my_abstract
		(Array.of_list (List.map apron_dim_of_variable variables))
		false
	in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nResult of hiding:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter hiding;
	);
	
	(* Convert the constraint to the old Imitator format *)
	let resulting_constraint = constraint_of_apron_abstract0 hiding in
		print_message Debug_total "Result of hiding in IMITATOR format:";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);

	resulting_constraint

(*	(* Optimization: if the constraint is false, return false *)
	if c = LC_false then LC_false else (
	(* Debug message *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total "\n***** PERFORMING HIDING *****";
		print_message Debug_total (string_of_constraint program.variable_names c);
	);
	(* Create the HyTech program *)
	let variables_to_hide = string_of_list_of_string_with_sep ", \n\t" (List.map hytech_of_variable variables) in
	let hytech_program =
		(hytech_header program)
	^ "\n" ^ "region_toto1 := True"
	^ "\n" ^ "& \n -- Constraint"
	^ "\n" ^ (hytech_of_constraint c)
	^ "\n" ^ ";"
	^ "\n" ^ "print (hide \n\t" ^ variables_to_hide ^ "\n\tin region_toto1 endhide\n);"
	^ (hytech_footer program) in
	print_message Debug_total "\nGenerated HyTech program:";
	print_message Debug_total hytech_program;
	(* Execute HyTech and get the log *)
	let log_file_content = hytech program hytech_program in
	(* Parse the log *)
	let resulting_constraint = hytech_parser program log_file_content in
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total "Result of hiding:";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);
	);
	(* Debug message *)
	print_message Debug_total "\n***** END OF HIDING *****";
	(* Return the constraint *)
	resulting_constraint
	)*)


(* Hide non-parameters in a constraint *)
let hide_non_parameters program =
	hide_variables program (List.rev_append program.clocks program.discrete)
(*	(* Optimization: if the constraint is false, return false *)
	if c = LC_false then LC_false else (
	(* Debug message *)
	print_message Debug_total "\n***** PERFORMING HIDING OF NON-PARAMETERS *****";
	(* Create the HyTech program *)
	let hytech_program =
		(hytech_header program)
	^ "\n" ^ "region_toto1 := True"
	^ "\n" ^ "& \n -- Constraint"
	^ "\n" ^ (hytech_of_constraint c)
	^ "\n" ^ ";"
	^ "\n" ^ "print (hide non_parameters in region_toto1 endhide\n);"
	^ (hytech_footer program) in
	print_message Debug_total "\nGenerated HyTech program:";
	print_message Debug_total hytech_program;
	(* Execute HyTech and get the log *)
	let log_file_content = hytech program hytech_program in
	(* Parse the log *)
	let resulting_constraint = hytech_parser program log_file_content in
	(* Debug print *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total "Result of hiding:";
		print_message Debug_total (string_of_constraint program.variable_names resulting_constraint);
	);
	(* Debug message *)
	print_message Debug_total "\n***** END OF HIDING *****";
	(* Return the constraint *)
	resulting_constraint
	)*)


(* Check if a constraint is satisfiable, i.e., has valuations *)
let is_satisfiable program = function
	(* Optimization: if the constraint is false, return false *)
	| LC_false -> false
	| LC_maybe inequalities ->

	(* Convert to Abstract0 *)
	let my_abstract = apron_abstract0_of_linear_inequalities program inequalities in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nTest of satisfiability for the following constraint:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter my_abstract;
	);

	(* Perform the satisfiability: non-emptyness test *)
	let satisfiable = not (Abstract0.is_bottom manager my_abstract) in

	(* Debug *)
	print_message Debug_total ("\nResult of satisfiability: " ^ (string_of_bool satisfiable));
	
	satisfiable

	(*(* Optimization: if the constraint is false, return false *)
	if c = LC_false then false else
	(* Optimization: if the constraint is true, return true *)
	if c = LC_maybe [] then true else (
	(* Debug message *)
	print_message Debug_total "\n***** CHECKING SATISFIABILITY OF CONSTRAINT *****";
	(* Create the HyTech program *)
	let hytech_program =
		(hytech_header program)
	^ "\n" ^ "region_toto1 := True"
	^ "\n" ^ "& \n -- Constraint"
	^ "\n" ^ (hytech_of_constraint c)
	^ "\n" ^ ";"
	^ "\n" ^ "print region_toto1;"
	^ (hytech_footer program) in
	print_message Debug_total "\nGenerated HyTech program:";
	print_message Debug_total hytech_program;
	(* Execute HyTech and get the log *)
	let log_file_content = hytech program hytech_program in
	(* Find a set of blank characters *)
	let satisfiable = not (Str.string_match (Str.regexp "^[ \n\t]+$") log_file_content 0) in
	(* Debug print *)
	print_message Debug_total ("Result of satisfiability: " ^ (string_of_bool satisfiable));
	(* Debug message *)
	print_message Debug_total "\n***** END OF SATISFIABILITY *****";
	(* Return the result *)
	satisfiable
	)*)


(* Compare 2 constraints *)
let comp_constraint program c1 c2 op_comp =
	(* Convert to Abstract0 *)
	let abstract1 = apron_abstract0_of_constraint program c1 in
	let abstract2 = apron_abstract0_of_constraint program c2 in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nComparison of 2 constraints:");
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter abstract1;
		Abstract0.print (fun i -> program.variable_names.(i)) Format.std_formatter abstract2;
	);

	(* Perform the comparison *)
	let result = op_comp abstract1 abstract2 in

	(* Debug *)
	if debug_mode_greater Debug_total then (
		print_message Debug_total ("\nResult of comparison: " ^ (string_of_bool result));
	);
	
	result


	(*(* Create the HyTech program *)
	let hytech_program =
		(hytech_header program)
	^ "\n" ^ "region_toto1 := True"
	^ "\n" ^ "& \n -- Constraint 1"
	^ "\n" ^ (hytech_of_constraint c1)
	^ "\n" ^ ";"
	^ "\n" ^ "region_toto2 := True"
	^ "\n" ^ "& \n -- Constraint 2"
	^ "\n" ^ (hytech_of_constraint c2)
	^ "\n" ^ ";"
	^ "\n" ^ "if region_toto1 " ^ op_comp ^ " region_toto2 then"
	^ "\n" ^ "prints \"" ^ hytech_tag_true ^ "\";"
	^ "\n" ^ "else prints \"" ^ hytech_tag_false ^ "\"; endif;"
	^ (hytech_footer program) in
	print_message Debug_total "\nGenerated HyTech program:";
	print_message Debug_total hytech_program;
	(* Execute HyTech and get the log *)
	let log_file_content = hytech program hytech_program in
	(* Look at the log *)
	let blanks = "[ \n\t]+" in
	let equality = 
		(* Search for true *)
		if (Str.string_match (Str.regexp(*_string*) (blanks ^ hytech_tag_true ^ blanks)) log_file_content 0) then true
		(* Search for false *)
		else if (Str.string_match (Str.regexp(*_string*) (blanks ^ hytech_tag_false ^ blanks)) log_file_content 0) then false
		(* Else: error *)
		else (print_error hytech_program;
			raise (InternalError "Something went wrong in the previous HyTech program when performing an inequality: no 'True' or 'False' tag could be found.");)
	in
	(* Return the result *)
	equality*)

(* Check if 2 constraints are equal *)
let is_equal program c1 c2 =
	(* Debug message *)
	print_message Debug_total "\n***** CHECKING EQUALITY OF 2 CONSTRAINTS *****";
	(* Real comparison *)
	comp_constraint program c1 c2 (Abstract0.is_eq manager)

(*	(* Call the generic function *)
	let equality = comp_constraint program c1 c2 "=" in
	(* Debug print *)
	print_message Debug_total ("Result of equality: " ^ (string_of_bool equality));
	(* Debug message *)
	print_message Debug_total "\n***** END OF EQUALITY *****";
	(* Return the result *)
	equality*)

(* Check if a constraint is included or equal to another *)
let is_le program c1 c2 =
	(* Debug message *)
	print_message Debug_total "\n***** CHECKING THAT A CONSTRAINT IS LOWER OR EQUAL TO ANOTHER ONE *****";
	(* Real comparison *)
	comp_constraint program c1 c2 (Abstract0.is_leq manager)
(*	(* Call the generic function *)
	let equality = comp_constraint program c1 c2 "<=" in
	(* Debug print *)
	print_message Debug_total ("Result of comparison: " ^ (string_of_bool equality));
	(* Debug message *)
	print_message Debug_total "\n***** END OF COMPARISON *****";
	(* Return the result *)
	equality*)
