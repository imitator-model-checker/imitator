(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Checks whether a PTA is a CUB-PTA
 * 
 * File contributors : Nguyen Hoang Gia, Étienne André
 * Created           : 2016/04/13
 * Last modified     : 2017/11/27
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
open Statistics



(************************************************************)
(************************************************************)
(* Useful global functions *)
(************************************************************)
(************************************************************)

(*** NOTE/HACK: duplicate function in ModelConverter and StateSpace ***)
(*** NOTE2: NOT efficient, as we create a lot of such pxd_true_constraint(), which are expensive! ***)
let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
	| True_guard -> LinearConstraint.pxd_true_constraint()
	| False_guard -> LinearConstraint.pxd_false_constraint()
	| Discrete_guard discrete_guard -> LinearConstraint.pxd_true_constraint()
	| Continuous_guard continuous_guard -> continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard -> discrete_continuous_guard.continuous_guard

(* Alert 26-09-2017: All operators are negated, the new one "convert_inequality_list_2_tuple_list" created to adapt to the new current code *)
(* 
Simple funtion to covert from list of inequalities to list of tuple (clock; operator; linear expression) 
Note that: if there are True constraints, it will return back a list of clock greater than Zero
This only uses to indicate clock smaller than INFINITE, not for lower-bound
*)

let convert_inequality_list_2_tuple_list model inequalities =
	let list_s0 = ref [] in 
	match inequalities with 
	(*True constraints -> list of clocks >= 0*)
		[] ->  List.iter 	(fun clock_index -> 
						list_s0 := !list_s0@[(clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)] 
						) model.clocks_without_special_reset_clock; 
			!list_s0

	| _ ->	print_message Verbose_low (" Covert inequalities -> list(clock; operator; linear expression) Start:"); 
			List.iter 	(fun inequality -> (
						print_message Verbose_low (" Inequality: " ^ (LinearConstraint.string_of_pxd_linear_inequality model.variable_names inequality ) );
						
						let (clock_index_2, operator, parametric_linear_term) = LinearConstraint.clock_guard_of_linear_inequality inequality in
						
						list_s0 := !list_s0@[(clock_index_2, operator, parametric_linear_term)]; 
						print_message Verbose_low (" inequality: " ^ (model.variable_names clock_index_2) 
														^ " " ^ (LinearConstraint.operator2string operator) 
														^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
														^ " added!"); 
							) 
						) inequalities; 
			print_message Verbose_low (" Covert inequalities -> list(clock; operator; linear expression) End!");
			print_message Verbose_low ("\n"); 
			!list_s0



(* ONE UPPER-BOUNDS OF THE SAME CLOCK *)
let filter_upperbound_by_clock clock_index tuple_inequalities_s0 =	
	(* print_message Verbose_low (" 	filtering upper-bound of clock (" ^ (model.variable_names clock_index) ^ ") in this list of tuple inequalities:"); *)
	match tuple_inequalities_s0 with
	| [] ->  raise (InternalError("Detected empty list, check again the input inequalities or it might be True constraint "))
	| _  -> 
	let length = List.length tuple_inequalities_s0 in
	(*index variable is to mark the position of upper-bound in the list*)
	let index = ref 0 in
	(*count is the number of upperbounds*)
	let count = ref 0 in

	for i = 0 to length -1
	do 
		let (clock_index_2, operator, parametric_linear_term) = (List.nth tuple_inequalities_s0 i) in 

		if ( (clock_index = clock_index_2) && ( operator = LinearConstraint.Op_l || operator = LinearConstraint.Op_le || operator = LinearConstraint.Op_eq ) )
		then 
			(
			(* print_message Verbose_low ("	Upper-bounded found: " ^ (model.variable_names clock_index_2) 
											^ " " ^ (LinearConstraint.operator2string operator) 
											^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
											^ " !"); *)
			index := i; 
			count := (!count + 1);
			if !count > 1 then(
				(* Retrieve the model *)
				let model = Input.get_model() in
				raise (InternalError("Detected more than 1 different upper-bound of the same clock '" ^ (model.variable_names clock_index) ^ "' in a same constraint!"))
			);
		);
	done;
	(*just for printing, start*)
	(* if !count = 0
	then 
		print_message Verbose_low ("	Upper-bound not found:!, return clock  " ^ (model.variable_names clock_index) ^">= 0" );  *)
		(*just for printing, end*)
		(* if there is no upper-bound -> reutrn (clock >= 0) *)

	if !count = 0
	then
		( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero ) (* make_linear_term [] NumConst.zero) *)
	else
		(* result *)
		let (clock_index_2, operator, parametric_linear_term) = List.nth tuple_inequalities_s0 !index in
		let tup = ref (clock_index_2, operator, parametric_linear_term) in
		if ( operator = LinearConstraint.Op_eq )
		then  tup := (clock_index_2, LinearConstraint.Op_le, parametric_linear_term);
		!tup




(* 
THIS IS USED FOR FILTERING TUPLES [CUB-PTA TRANSFORMATION], IF THERE IS EMPTY MUST RETURN THE CLOCK >= 0, 
IN CASE THE EMPTY LIST RETURNED, THE RESULT WILL BE WRONG! (EX: LIST.ITER), 
*)
(* let filter_upperbound_by_clock_2 clock_index tuple_inequalities_s0 =
	let ls = ref [] in
	match tuple_inequalities_s0 with
	| [] -> ls := !ls@[( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero )]	
	| _  -> 
			(
			List.find_all (fun (index, op,_) -> index = clock_index && (op = LinearConstraint.Op_le ||op = LinearConstraint.Op_l || op = LinearConstraint.Op_eq) ) tuple_inequalities_s0);
			);
	!ls	 *)



(* 
THE NEW UPDATED FUNCTION OF "filter_upperbound_by_clock_2", THE EQUAL "=" WILL BE FILTERED AS "<=" (NOTE: "=" <=> "<=" AND ">=")
 *)

 
let filter_upperbound_by_clock_3 clock_index tuple_inequalities_s0 =
	let ls = ref [] in
	( 
	match tuple_inequalities_s0 with
	| [] -> raise (InternalError(" Input list is empty!!! "))
	| _  -> 
			(	
			List.iter ( fun (index, op, linear_term)-> 
				if index == clock_index
				then
					(
						match op with
						| LinearConstraint.Op_le -> ls := !ls@[( clock_index, LinearConstraint.Op_le, linear_term )]
						| LinearConstraint.Op_l  -> ls := !ls@[( clock_index, LinearConstraint.Op_l, linear_term )]
						| LinearConstraint.Op_eq -> ls := !ls@[( clock_index, LinearConstraint.Op_le, linear_term )]
						| _ -> ();
					);
			) tuple_inequalities_s0;
			);
	);
	if !ls = []
	then ls := !ls@[( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero )];
	!ls												



(* THIS WILL RETURN (NEGATIVE OPERATOR AND, CUB INEQUALITY) *)
let make_CUB_inequality (op1, linear_term1) (op2, linear_term2) = 	
	let linear_term = LinearConstraint.sub_p_linear_terms linear_term1 linear_term2 in
	match op1, op2 with
	(*linear_term1 < linear_term2*)
	| LinearConstraint.Op_le, LinearConstraint.Op_l -> (LinearConstraint.Op_ge, (LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_l))
	(*linear_term1 <= linear_term2*)
	| _, _ -> (LinearConstraint.Op_g, (LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_le))



let make_CUB_constraint = LinearConstraint.make_p_constraint




(*generated_constraints: we describe Disjunction by a list of conjunction constraints*)
let disjunction_constraints param_constraints =
	let generated_constraints = ref [LinearConstraint.p_true_constraint ()] in
	DynArray.iter ( fun (is_and, constraint_list) ->
		if(is_and = true)
		then
			(
				if List.length constraint_list <> 1
				then raise (InternalError(" Conjunction constraint_list error! "));
				generated_constraints := List.map (fun con -> 
					LinearConstraint.p_intersection ([con]@constraint_list);
				) !generated_constraints;
			)
		else 
			(
				if List.length constraint_list = 0
				then raise (InternalError(" Disjunction constraint_list error! "));
				let temp = ref [] in
				List.iter (fun cons1 -> 
					List.iter (fun cons2 ->
						let con3 = LinearConstraint.p_intersection [cons1; cons2] in 
						temp := !temp@[con3];
					) !generated_constraints
				) constraint_list;
				generated_constraints := !temp;
			);
	) param_constraints;
	!generated_constraints

(*generated_constraints: we describe Disjunction by a list of conjunction constraints - end*)



(*check parameters_constraints all false*)
let is_parameters_constraints_false p_cons =
	let ls = disjunction_constraints p_cons in
	let resut = ref true in
	List.iter (fun con ->
		if (LinearConstraint.p_is_satisfiable con) 
		then resut := false;
	) ls;
	!resut
(*check parameters_constraints all false - end*)



(*Check cub_contraint conflict with parameters_constraints*)
let isContraintConflictsParametersConstraints con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref false in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.p_intersection [con; con1] in 
		(* print_message Verbose_low ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ) ; *)
		if LinearConstraint.p_is_false con_intersection
		then
			(
			(* print_message Verbose_low ("\n Conflict!!! "); *)
			check := true;
			);
	) disjunction_cons;
	!check

(*Check cub_contraint conflict with parameters_constraints - end*)


(*Check cub_contraint conflict with parameters_constraints*)
let isContraintConflictsParametersConstraints2 con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref false in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.pxd_intersection [con; LinearConstraint.pxd_of_p_constraint con1] in 
		(* print_message Verbose_low ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ) ; *)
		if LinearConstraint.pxd_is_false con_intersection
		then
			(
			(* print_message Verbose_low ("\n Conflict!!! "); *)
			check := true;
			);
	) disjunction_cons;
	!check

(*Check cub_contraint conflict with parameters_constraints - end*)



(*Check cub_contraint all conflict with parameters_constraints - use for p and p*)
let isContraintAllConflictsParametersConstraints con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref true in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.p_intersection [con; con1] in 
		(*
		if verbose_mode_greater Verbose_low then(
			(* Get the model *)
			(*** NOTE: should be safe (this is for pretty-printing anyway) but note that we are modifying the model ***)
			let model = Input.get_model() in
			
			(*
			print_message Verbose_low ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
			^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
			*)

		);
		*)
		if not (LinearConstraint.p_is_false con_intersection)
		then
			(
			print_message Verbose_low ("\n Conflict!!! ");
			check := false;
			);
	) disjunction_cons;
	!check

(*Check cub_contraint conflict with parameters_constraints - end*)



(*Check cub_contraint all conflict with parameters_constraints - use for pxd and p*)
let isContraintAllConflictsParametersConstraints2 con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref true in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.pxd_intersection [con; (LinearConstraint.pxd_of_p_constraint con1)] in 

		(*
		if verbose_mode_greater Verbose_low then(
			(* Get the model *)
			(*** NOTE: should be safe (this is for pretty-printing anyway) but note that we are modifying the model ***)
			let model = Input.get_model() in
			
			(*
			print_message Verbose_low ("\n Constraint1: \n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
			*)

		);
		*)

		if not (LinearConstraint.pxd_is_false con_intersection)
		then
			(
			print_message Verbose_low ("\n Conflict!!! ");
			check := false;
			);
	) disjunction_cons;
	!check





(* DynArray *)
(* CHECK WHETHER A CLOCK CONSTRAINT CONATAINED IN CLOCKS CONSTRAINTS *)
let isConstraintContainedInClocksConstraints loc_index con c_cons =
	let check = ref false in
	DynArray.iter (fun (location_index, con1) -> 
		if location_index = loc_index
		then (
				if LinearConstraint.pxd_is_equal con1 con
				then (
						check := true;
					);
			);
	) c_cons;
	!check





(*CHECK WHETHER A CONSTRAINT CONATAINED IN PARAMETERS RELATION*)
let isConstraintContainedInParametersConstraints con p_cons =
	let check = ref false in
	DynArray.iter (fun (b, cons) ->
		if (b = true) 
		then  
			(
			let con1 = List.nth cons 0 in
			if (LinearConstraint.p_is_equal con1 con)
			then check := true; 
			);
	) p_cons;
	!check



(*
let getInfoCurrentModel model submodel =
	let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in
	print_message Verbose_low ("\n ------------------------SUBMODEL INFO------------------------------- ");
	print_message Verbose_low ("\n NUMBER OF LOCATIONS: " ^ string_of_int (Hashtbl.length locations) );
	print_message Verbose_low ("\n NUMBER OF TRANSITIONS: " ^ string_of_int (DynArray.length transitions) );
	(*for checking the first parameters constraints*)
	print_message Verbose_low ("\n CURRENT PARAMETERS RELATION!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_low ("\n IS CONJUNCTION: " ^ string_of_bool is_and ^ ": ");
		
		List.iter (fun cons -> 
			print_message Verbose_low (" 	CONSTRAINTS: \n" 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons) );
		) constraint_list;

	) parameters_constraints;
	(*for checking the first parameters constraints - end*)
	(*clocks constraitns*)
	print_message Verbose_low ("\n CURRENT CLOCKS CONSTRAINS!!!!: ");
	(
	DynArray.iter (fun (index, cons) -> 
		print_message Verbose_low ("\n LOCATION: " ^ index );
		
		print_message Verbose_low ("  CONSTRAINTS: \n" 
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) );
	) clocks_constraints;
	);
	(*clocks constraitns*)
	print_message Verbose_low ("\n ------------------------SUBMODEL INFO END--------------------------- ");
	()
*)



(************************************************************)
(************************************************************)
(* Main function for CUB checking *)
(************************************************************)
(************************************************************)

(*attention: multi inequalities of the same clock in model*)

(*let check_cub_condition *)

(*check CUB condition function -> return boolean value*)

let cub_check_2 model invariant_s0 guard_t invariant_s1 clock_updates = 
	(*ppl*)
	(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
	let inequalities = ref [] in
	print_message Verbose_low (" CUB check, Start:");
	print_message Verbose_low ("\n");

	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

	print_message Verbose_low (" **Beginning state/location** :");
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
	print_message Verbose_low (" **Transition** :");
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
	print_message Verbose_low (" **Destination state/location** :");
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in
	
	let isCUB_PTA = ref true in

	List.iter (	fun clock_index -> 
		let inequalities_need_to_solve = ref [] in
		print_message Verbose_low ("   Checking CUB conditions at clock (" ^ (model.variable_names clock_index) ^ "):");

		print_message Verbose_low ("\n 	**Beginning state/location** :");
		let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
		print_message Verbose_low ("\n 	**Transition** :");
		let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
		print_message Verbose_low ("\n 	**Destination state/location** :");
		let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in


		(*

		print_message Verbose_low ("\n");
		print_message Verbose_low ("Comparing: ");

		let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
		let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
		let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in

		print_message Verbose_low (" 	 get upper-bound s0: " ^ s0_upperbound_str );
		print_message Verbose_low (" 	 get upper-bound t: " ^ t_upperbound_str );
		print_message Verbose_low (" 	 get upper-bound s1: " ^ s1_upperbound_str );
		
		print_message Verbose_low (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");

		*)

		(* if List.mem clock_index reset_clocks = true 
		then lower_inequality := linear_term_t; *)

		(* let result = ref true in *)
		let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

			| (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_low (" 	 Case 1 " );
																									true;

			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_low (" 	 Case 2 " );
																									false;

			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
				print_message Verbose_low (" 	 Case 3 " );
				if List.mem clock_index clock_updates = true
				then
					(

					(*
					let _ = print_message Verbose_low (" 	 Detected " 
													^ (model.variable_names clock_index) 
													^ " was a reset clock!\n 	 skipping the process: (" 
													^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
					in
					*)

					true
					)
				else
					(
					false;
					);

		|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
																									false; 

		|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_low (" 	 Case 4 " );
																									true;

		|_							, _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_low (" 	 Case 5 " );
				let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
				
				(*
				print_message Verbose_low ("INEQUALITY S0 =< T: \n" 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
											^ "!!!\n"); 
				*)

				let constr = make_CUB_constraint [ineq] in
				
				(*
				print_message Verbose_low ("CUB_CONSTRAINT: \n" 
											^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr
											^ "!!!\n"); 
				*)

				if LinearConstraint.p_is_true constr
				then true
				else
					(
					if LinearConstraint.p_is_false constr
					then false
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
						(* false; *)
						true
						);
					);



		|_							, (LinearConstraint.Op_ge, _), _							->	print_message Verbose_low (" 	 Case 5 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(

				(*
				let _ = print_message Verbose_low (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
				in
				*)

				true
				)
			else
				(
				let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
				(*
				print_message Verbose_low (" INEQUALITY S0 <= S1: \n" 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
											^ "!!!\n");
				*)
				
				let constr = make_CUB_constraint [ineq] in
				
				(*
				print_message Verbose_low ("CUB_CONSTRAINT: \n" 
											^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr
											^ "!!!\n"); 
				*)

				if LinearConstraint.p_is_true constr
				then 
					true
				else
					(
					if LinearConstraint.p_is_false constr
					then 
						false
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
						(* false; *)
						true
						);
					);
				);
																										

		| _							, _							 , _							-> 	print_message Verbose_low (" 	 Case 6 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(

				(*
				print_message Verbose_low (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
				*)

				let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
				
				(*
				print_message Verbose_low ("INEQUALITY S0 =< T: \n" 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
											^ "!!!\n"); 
				*)

				let constr = make_CUB_constraint [ineq] in
				
				(*
				print_message Verbose_low ("CUB_CONSTRAINT: \n" 
											^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr
											^ "!!!\n"); 
				*)

				if LinearConstraint.p_is_true constr
				then 
					true
					else
					(
					if LinearConstraint.p_is_false constr
					then 
						false
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
						(* false; *)
						true
						);
					);
				)
			else
				(
				let (_, ineq1) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
				
				(*
				print_message Verbose_low ("INEQUALITY S0 =< T: \n" 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 
											^ "!!!\n"); 

				*)

				let (_, ineq2) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
				
				(*
				print_message Verbose_low (" INEQUALITY S0 <= S1: \n" 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 
											^ "!!!\n");
				*)

				let constr = make_CUB_constraint [ineq1;ineq2] in
				
				(*
				print_message Verbose_low ("CUB_CONSTRAINT: \n" 
											^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr
											^ "!!!\n"); 
				*)

				if LinearConstraint.p_is_true constr
				then 
					true
					else
					(
					if LinearConstraint.p_is_false constr
					then 
						(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 						let clock_linear_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in *)
						false
						(* true *)
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq1;ineq2];
						(* false; *)
						true;
						);
					);
				);


		in

		if (result = false (* && !inequalities_need_to_solve = [] *) )
		then 	(
				isCUB_PTA := false;
				(*comment this line below for the CUB-PTA transformation*)
				(* raise (InternalError("   The model is impossible CUB-PTA! ")); *) 
				);
		
		inequalities := !inequalities@(!inequalities_need_to_solve);

		if (result = false)
		then

			print_message Verbose_low (" This is not satisfied CUB-PTA! ")
		else 
			print_message Verbose_low (" This can be satisfied CUB-PTA! ");



	print_message Verbose_low ("\n");
	) model.clocks_without_special_reset_clock; (* end List.iter *)

	print_message Verbose_low ("\n");
	print_message Verbose_low (" CUB check, End!");
	print_message Verbose_low ("\n");
	(!isCUB_PTA, !inequalities)


(* Fixed 20-11-17 - Need to rewrite the function - Wrong operator here!!!!!! BUG!!!!! -- LinearConstraint.pxd_linear_constraint_of_clock_and_parameters *)
let tuple2pxd_constraint (clock_index, op, linear_term) = 
	(* Call dedicated function from LinearConstraint; "true" means the operator direction is not reversed *)
	LinearConstraint.pxd_linear_constraint_of_clock_and_parameters clock_index (LinearConstraint.reverse_op op) linear_term true
(*	let clock_term = LinearConstraint.make_p_linear_term [NumConst.one, clock_index] NumConst.zero in
	let linear_inequality = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term) op in
	let constr = LinearConstraint.make_p_constraint [linear_inequality] in
	let constr = LinearConstraint.pxd_of_p_constraint constr in
	constr*)
	

let create_x_ge_zero clock_index = (clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)


let get_all_clocks_ge_zero_comstraint model = 
	let ls = ref [] in
	List.iter (fun clock_index ->
		ls := !ls@[tuple2pxd_constraint (create_x_ge_zero clock_index)];
	) model.clocks_without_special_reset_clock;
	let cons = LinearConstraint.pxd_intersection !ls in
	cons


let get_all_clocks_ge_zero_comstraint2 clock_index model = 
	let ls = ref [] in
	List.iter (fun clock_indx ->
		if (clock_index != clock_indx)
		then
			(
			ls := !ls@[tuple2pxd_constraint (create_x_ge_zero clock_indx)];
			);
	) model.clocks_without_special_reset_clock;
	let cons = LinearConstraint.pxd_intersection !ls in
	cons






(** Takes an abstract model as input, and infers a constraint on the parameters (possibly false) under which this PTA is a CUB-PTA *)
let check_cub model =
	(* raise (InternalError "not implemented") *)
	
	(* Create and start counter *)
	let cub_detection_counter = create_time_counter_and_register "CUB detection" Algorithm_counter Verbose_standard in
	cub_detection_counter#start;

	let isCUB_PTA = ref true in
	let inequalities_need_to_solve = ref [] in
	(*main function for CUB-PTA*)
	List.iter (fun automaton_index -> print_message Verbose_low ("Automaton: " ^ (model.automata_names automaton_index) );

			(*Checking bounded clocked in invariant (Location)*)
	        List.iter (fun location_index -> print_message Verbose_low ("----------------Begin checking at " ^ (model.location_names automaton_index location_index) ^ "-------------------");

	        		print_message Verbose_low ("\n");

	        		print_message Verbose_low (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ;

	        		let invariant1 = model.invariants automaton_index location_index in
	        
	                (* print_message Verbose_low ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ; *)
							

							print_message Verbose_low ("\n");

	                	(*Checking bounded clocked in guards (Transition)*)
	                	List.iter (fun action_index -> print_message Verbose_low (" Transition/Action: " ^ (model.action_names action_index) );
	            
	                    	List.iter (fun (guard, clock_updates, _, target_location_index) 
	                    		-> 
	                    		(*print_message Verbose_low ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));	*)	

	                        	(** WORK HERE **)

	                        	let invariant2 = model.invariants automaton_index target_location_index in

	                        	(*
								print_message Verbose_low ("\n");
	                			print_message Verbose_low (" State/Location(D): " ^ (model.location_names automaton_index target_location_index) ) ;
	                			print_message Verbose_low ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
	                			print_message Verbose_low ("	  ----Map:(" 
	                											^ (model.location_names automaton_index location_index) 
	                											^ ")--" ^ (model.action_names action_index) ^ "-->(" 
	                											^ (model.location_names automaton_index target_location_index) 
	                											^ ") ----" );
	                			print_message Verbose_low ("\n");
	                			*)

	                        	let clock_updates = match clock_updates with
	                        						  No_update -> []
													| Resets clock_update -> clock_update
													| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); in


	                			let (result, inequalities) = cub_check_2 model invariant1 (continuous_part_of_guard guard) invariant2 clock_updates in
	                			
	                			inequalities_need_to_solve := !inequalities_need_to_solve@inequalities;
	                			if result = false
								then
	    							isCUB_PTA := false;
	                			()

	                    	) (model.transitions automaton_index location_index action_index); 

	                	) (model.actions_per_location automaton_index location_index); 

	            		 print_message Verbose_low ("----------------End checking " ^ (model.location_names automaton_index location_index) ^ "---------------------");

	            		 print_message Verbose_low ("\n");

	        ) (model.locations_per_automaton automaton_index);

	        print_message Verbose_low ("\n");

	) model.automata;


	let constraint_for_cub = ref (LinearConstraint.p_true_constraint ()) in
	(
	if (!isCUB_PTA ) = true 
	then
		(
			if (!inequalities_need_to_solve = []) = true 
			then
			(
	    		print_message Verbose_low ("   The model is CUB-PTA! ");
	    	)
			else
			(	
				constraint_for_cub := (LinearConstraint.make_p_constraint (!inequalities_need_to_solve) );
	   			
	   			print_message Verbose_low ("   The model is possible CUB-PTA! \nbut you need to solve the inequalities below!!: "); 
	   			(* print_message Verbose_low ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names !constraint_for_cub)); *)
			)
		)
	else 
		(
			print_message Verbose_low (" This is not satisfied CUB-PTA! ");
			constraint_for_cub := (LinearConstraint.p_false_constraint ()) ;
	    );
	 );
	 
	 
	(* Stop counter *)
	cub_detection_counter#stop;
	
	(* Return result *)
	!constraint_for_cub







(************************************************************)
(************************************************************)
(* Local function for CUB transforming *)
(************************************************************)
(************************************************************)

(* [CUB-PTA TRANSFORMATION] THIS FUNCTION USED FOR REMOVING PROBLEMATIC TRANSITIONS *)
let check_problematic_transition model (invariant_s0, guard_t, invariant_s1, clock_updates, parameters_constraints) = 	
	print_message Verbose_low ("\nCHECKING FOR REMOVING PROBLEMATIC TRANSITIONS!" ); 
	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities (continuous_part_of_guard guard_t) in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in

	(* let isCUB = ref true in *)
	let result = ref true in
	
	(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 	let inequalities_need_to_solve = ref [] in *)

	List.iter (	fun clock_index -> 
		let isCUB = ref false in
		let ls_tup_ineq_s0 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_s0) in
		let ls_tup_ineq_t 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_t) in
		let ls_tup_ineq_s1 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_s1) in
		List.iter (fun (clock_index_s0, op_s0, linear_term_s0) -> 
			List.iter (fun (clock_index_t, op_t, linear_term_t) -> 
				List.iter (fun (clock_index_s1, op_s1, linear_term_s1) -> 
					match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
						(*Case 1*)
						print_message Verbose_low ("\nCASE 1 " );
						print_message Verbose_low ("SATISFIED CUB! " );
						isCUB := true;
						(* (); *)
						(*Case 1 - end*)
					|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 2*)
						print_message Verbose_low ("\nCASE 2 " );
						print_message Verbose_low ("NOT SATISFIED CUB! " );
						(* none reset zone *)
						(* isCUB := false; *)
						();
						(* none reset zone - end *)
						(*Case 2 - end*)
					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
						(*Case 3*)
						(*reset*)
						print_message Verbose_low ("\nCASE 3 " );
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							print_message Verbose_low ("DETECTED RESET CLOCK! " );
							print_message Verbose_low ("SATISFIED CUB! " );
							isCUB := true;
							(* (); *)
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)
							print_message Verbose_low ("NOT SATISFIED CUB! " );
							(* isCUB := false; *)
							();
							(* none reset zone - end *)
							);
						(*Case 3 - end*)
					|(LinearConstraint.Op_ge, _), _							 , _							->
						(*Case 4*)
						(*reset but useless*)
						print_message Verbose_low ("\nCASE 4 " );
						print_message Verbose_low ("NOT SATISFIED CUB! " );
						(* isCUB := false; *)
						();
						(*Case 4 - end*)
					|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
						(*Case 5*)
						print_message Verbose_low ("\nCASE 5 " );
						print_message Verbose_low ("SATISFIED CUB! " );
						isCUB := true;
						(* (); *)
						(*Case 5 - end*)
					|_							, _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 6*)
						(* none reset zone *)
						print_message Verbose_low ("\nCASE 6 " );
						let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						
						(*
						print_message Verbose_low ("FORMING INEQUALITY: " 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
														^ "!!!\n");
						*)

						let constrCUB = make_CUB_constraint [ineq] in
						let constr = LinearConstraint.pxd_intersection [(LinearConstraint.pxd_of_p_constraint constrCUB); invariant_s0; continuous_part_of_guard guard_t] in
			
						if LinearConstraint.pxd_is_true constr
						then
							(
							print_message Verbose_low ("TRUE, COMPARABLE! "); 
							print_message Verbose_low ("SATISFIED CUB! " );
							isCUB := true;
							(* (); *)
							)
						else
							(
							if LinearConstraint.pxd_is_false constr
							then 
								(
								print_message Verbose_low ("FALSE, COMPARABLE! ");
								print_message Verbose_low ("NOT SATISFIED CUB! " );
								(* isCUB := false; *)
								();
								)
							else
								(
								print_message Verbose_low ("NOT DETERMINED! ");
								if isContraintConflictsParametersConstraints2 constr parameters_constraints
								then
									(
									print_message Verbose_low ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
									print_message Verbose_low ("NOT SATISFIED CUB! " ); 
									(* isCUB := false; *)
									();
									)
								else
									(
									print_message Verbose_low ("NOT CONFLICTED! " );
									print_message Verbose_low ("SATISFIED CUB! " );
									isCUB := true;
									(* (); *)
									);
								);
							);
						(* none reset zone - end*)
						(*Case 6 - end*)
					|_							, (LinearConstraint.Op_ge, _), _							->	
						(*Case 7*)
						print_message Verbose_low ("\nCASE 7 " );
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							print_message Verbose_low ("DETECTED RESET CLOCK! " );
							print_message Verbose_low ("SATISFIED CUB! " );
							isCUB := true;
							(* (); *)
							(* reset zone - end*)
							)
						else
							(
							(* none reset zone *)
							let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							
							(*
							print_message Verbose_low ("FORMING INEQUALITY: " 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
															^ "!!!\n");
							*)

							let constrCUB = make_CUB_constraint [ineq] in
							let constr = LinearConstraint.pxd_intersection [(LinearConstraint.pxd_of_p_constraint constrCUB); invariant_s0; invariant_s1] in
							if LinearConstraint.pxd_is_true constr
							then 
								(
								print_message Verbose_low ("TRUE, COMPARABLE! ");
								print_message Verbose_low ("SATISFIED CUB! " );
								isCUB := true;
								(* (); *)
								)
							else
								(
								if LinearConstraint.pxd_is_false constr
								then 
									(
									print_message Verbose_low ("FALSE, COMPARABLE! ");
									print_message Verbose_low ("NOT SATISFIED CUB! " );
									(* isCUB := false; *)
									();
									)
								else
									(
									print_message Verbose_low ("FALSE, NOT DETERMINED! ");
									if isContraintConflictsParametersConstraints2 constr parameters_constraints
									then
										(
										print_message Verbose_low ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
										print_message Verbose_low ("NOT SATISFIED CUB! " ); 
										(* isCUB := false; *)
										();
										)
									else
										(
										print_message Verbose_low ("NOT CONFLICTED! " );
										print_message Verbose_low ("SATISFIED CUB! " );
										isCUB := true;
										(* (); *)
										);
									);
								);
							(* none reset zone *)
							);
						(*Case 7 - end*)																						
				| _							, _							 , _							-> 	
					(*Case 8*)
					print_message Verbose_low ("\nCASE 8 " );
					(*reset*)
					if List.mem clock_index clock_updates = true
					then
						(
						(* reset zone *)
						print_message Verbose_low ("DETECTED RESET CLOCK! " );
						let (_, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						
						(*
						print_message Verbose_low ("FORMING INEQUALITY: " 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
														^ "!!!\n"); 
						*)

						let constrCUB = make_CUB_constraint [ineq] in
						let constr = LinearConstraint.pxd_intersection [(LinearConstraint.pxd_of_p_constraint constrCUB); invariant_s0; continuous_part_of_guard guard_t] in
						if LinearConstraint.pxd_is_true constr
						then 
							(
							print_message Verbose_low ("TRUE, COMPARABLE! ");
							print_message Verbose_low ("SATISFIED CUB! " );
							isCUB := true;
							(* (); *)
							)
						else
							(

							if LinearConstraint.pxd_is_false constr
							then 
								(
								print_message Verbose_low ("FALSE, COMPARABLE! ");
								print_message Verbose_low ("NOT SATISFIED CUB! " ); 
								(* isCUB := false; *)
								();
								)
							else
								(
								print_message Verbose_low ("FALSE, NOT DETERMINED ");
								if isContraintConflictsParametersConstraints2 constr parameters_constraints
								then
									(
									print_message Verbose_low ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
									print_message Verbose_low ("NOT SATISFIED CUB! " ); 
									(* isCUB := false; *)
									();
									)
								else
									(
									print_message Verbose_low ("NOT CONFLICTED! " );
									print_message Verbose_low ("SATISFIED CUB! " ); 
									isCUB := true;
									(* (); *)
									);
								);
							);
						(* reset zone - end *)
						)
					else
						(
						(* none reset zone *)
						let (_, ineq1) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						
						(*
						print_message Verbose_low ("INEQUALITY S0 =< T: \n" 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 
														^ "!!!\n"); 
						*)

						let (_, ineq2) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
						
						(*
						print_message Verbose_low ("INEQUALITY S0 =< S1: \n" 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 
														^ "!!!\n"); 
						*)

						let constrCUB = make_CUB_constraint [ineq1; ineq2] in
						let constr = LinearConstraint.pxd_intersection [(LinearConstraint.pxd_of_p_constraint constrCUB); invariant_s0; continuous_part_of_guard guard_t; invariant_s1] in
						if LinearConstraint.pxd_is_true constr
						then 
							(
							print_message Verbose_low ("TRUE, COMPARABLE! ");
							print_message Verbose_low ("SATISFIED CUB! " ); 
							isCUB := true;
							(* (); *)
							)
						else
							(
							if LinearConstraint.pxd_is_false constr
							then 
								(
								print_message Verbose_low ("FALSE, COMPARABLE! ");
								print_message Verbose_low ("NOT SATISFIED CUB! " );
								(* isCUB := false; *)
								();
								)
							else
								(
								print_message Verbose_low ("FALSE, NOT DETERMINED! ");
								if isContraintConflictsParametersConstraints2 constr parameters_constraints
								then
									(
									print_message Verbose_low ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
									print_message Verbose_low ("NOT SATISFIED CUB! " );
									(* isCUB := false; *)
									();
									)
								else
									(
									print_message Verbose_low ("NOT CONFLICTED! " );
									print_message Verbose_low ("SATISFIED CUB! " ); 
									isCUB := true;
									(* (); *)
									);
								);
							);
						(* none reset zone - end*)
						);
					(*Case 8 - end*)
				) ls_tup_ineq_s1;
			) ls_tup_ineq_t;
		) ls_tup_ineq_s0;

		result := (!result && !isCUB);

	) model.clocks_without_special_reset_clock; 
	print_message Verbose_low ("CHECKING FOR REMOVING PROBLEMATIC TRANSITIONS - END!!" ); 
	!result



(* [CUB-PTA TRANSFORMATION] THIS IS A FUNCTION USED FOR CUB-PTA TRANSITIONS *)
let cub_tran model submodels count_m
			locations transitions
			location_index clock_index clock_updates 
			clocks_constraints parameters_constraints
			(clock_index_s0 , op_s0, linear_term_s0) (clock_index_t, op_t, linear_term_t) (clock_index_s1, op_s1, linear_term_s1) 
			(*for printing - not important*)
			submodel = 

	print_message Verbose_total ("Entering cub_tran; converting constraints…" ); 
	
	(*convert back to constraint for each inequality*)
	(* print_message Verbose_low ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term));  *)
	print_message Verbose_total ("Converting s0…" );
	let constraint_s0 = tuple2pxd_constraint (clock_index_s0 , op_s0, linear_term_s0) in
	 print_message Verbose_low ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0));  
	print_message Verbose_total ("Converting t…" );
	let constraint_t = tuple2pxd_constraint (clock_index_t, op_t, linear_term_t) in
	 print_message Verbose_low ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t)); 
	print_message Verbose_total ("Converting s1…" );
	let constraint_s1 = tuple2pxd_constraint (clock_index_s1, op_s1, linear_term_s1) in
	 print_message Verbose_low ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));   

	print_message Verbose_total ("Starting matching cases…" );

	(*
	print_message Verbose_low ("\n");
	print_message Verbose_low ("Comparing: ");
	(*just for printing*)
	let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " 
							^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
	let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " 
							^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
	let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " 
							^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
	print_message Verbose_low (" 	 get upper-bound s0: " ^ s0_upperbound_str );
	print_message Verbose_low (" 	 get upper-bound t: " ^ t_upperbound_str );
	print_message Verbose_low (" 	 get upper-bound s1: " ^ s1_upperbound_str );
	print_message Verbose_low (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
	(*just for printing - end*)

	*)
	
	(
	match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
		|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
		 	(*Case 1*)
		 	print_message Verbose_low (" 	 Case 1 " );
		 	()
		 	(*Case 1 - end*)
		|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
			(*Case 2*)
			print_message Verbose_low (" 	 Case 2 " );
			(* none reset zone *)
			let clock_cons = (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
			(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
			if check2 = false 
			then *)
				(
				DynArray.add clocks_constraints (location_index, clock_cons);
				);
			()
			(* none reset zone - end *)
			(*Case 2 - end*)
		|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
			(*Case 3*)
			(*reset*)
			print_message Verbose_low (" 	 Case 3 " );
			if List.mem clock_index clock_updates = true
			then
				(
				(* reset zone *)

				(* )
				let _ = print_message Verbose_low (" 	 Detected " 
														^ (model.variable_names clock_index) 
														^ " was a reset clock!\n 	 skipping the process: (" 
														^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str 
														^ ")!" ) 
				in
				*)

				()
				(* reset zone - end *)
				)
			else
				(
				(* none reset zone *)
				let clock_cons = (LinearConstraint.pxd_intersection [constraint_s1; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
				(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
				if check2 = false 
				then *)
					(
					DynArray.add clocks_constraints (location_index, clock_cons);
					);
				()
				(* none reset zone - end *)
				);
			(*Case 3 - end*)
		|(LinearConstraint.Op_ge, _), _							 , _							->
			(*Case 4*)
			(*reset but useless*)
			print_message Verbose_low (" 	 Case 4 " );
			(*reset*)
			let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
			(* reset zone *)
			if (List.mem clock_index clock_updates) = true
			then
				(
				clock_cons := (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint2 clock_index model)]);
				);
			(* reset zone - end*)

			(* let check2 = isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints in
			if check2 = false 
			then *)
				(
				DynArray.add clocks_constraints (location_index, !clock_cons);
				
				print_message Verbose_low (" Added constraints: " 
															^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names !clock_cons)  
															^ "\n at state: " 
															^ location_index );
				
				);
			()
			(*Case 4 - end*)
		|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
			(*Case 5*)
			print_message Verbose_low (" 	 Case 5 " );
			()
			(*Case 5 - end*)
		|_							, _							 , (LinearConstraint.Op_ge, _)	->	
			(*Case 6*)
			(* none reset zone *)
			print_message Verbose_low (" 	 Case 6 " );
			let (nop, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
			
			print_message Verbose_low (" Forming inequality: " 
											^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
											^ "!!!\n");
			
			let constr = make_CUB_constraint [ineq] in

			if LinearConstraint.p_is_true constr
			then
				(
				print_message Verbose_low (" true, comparable "); 
				()
				)
			else
				(
				let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
				(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in *)
				if LinearConstraint.p_is_false constr
				then 
					(
					print_message Verbose_low (" false, comparable ");

					(* if check2 = false 
					then *)
						(
						DynArray.add clocks_constraints (location_index, clock_cons);
						);
					()

					)
				else
					(
					print_message Verbose_low (" false, not determined ");
					(*submodel info*)
					(* getInfoCurrentModel model submodel; *)
					let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
					let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
					print_message Verbose_low ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
													(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
													^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

					if check1 = true
					then
						(
						(* if check2 = false
						then *)
							(
							print_message Verbose_low ("\n Cub constraints conflicted with parameters constraints!!! " );
							print_message Verbose_low (" Adding new clocks constraints" );
							DynArray.add clocks_constraints (location_index, clock_cons);
							
							print_message Verbose_low (" Added constraints: " 
															^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
															^ "\n at state: " 
															^ location_index );
							
							);
						)
					else
						(
						if check3 = false
						then
							(
							print_message Verbose_low ("\n cub constraints did not conflict with parameters constraints!!! " );
							(*Add First parameter constraints in the current submodel*)
							let new_parameters_constraints = DynArray.copy parameters_constraints in

							
							print_message Verbose_low ("\n Add constraint " 
															^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
															^ " into submodel ((parameters_constraints)) " 
															^ string_of_int (!count_m)  );
							

							DynArray.add parameters_constraints (true, [constr]);
							(*Create new submodel with Second constraint*)
							let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
							(* let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in *)
							let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 nop in
							let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in

							
							print_message Verbose_low ("\n Add constraint " 
															^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
															^ " into submodel (parameters_constraints) " 
															^ string_of_int ((DynArray.length submodels) +1) );
							

							DynArray.add new_parameters_constraints (false, [constr1]);
							if is_parameters_constraints_false new_parameters_constraints
							then 
								(
								print_message Verbose_low ("\n New parameters relations all False!!!, not created new submodel!!");
								)
							else
								(
								let new_clocks_constraints = DynArray.make 0 in
								(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
								(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
								DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
								);
							);
						);
					()
					);
				);
			(* none reset zone - end*)
			(*Case 6 - end*)
		|_							, (LinearConstraint.Op_ge, _), _							->	
			(*Case 7*)
			print_message Verbose_low (" 	 Case 7 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(
				(* reset zone *)
				(* )
				let _ = print_message Verbose_low (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
				in
				*)

				()
				(* reset zone - end*)
				)
			else
				(
				(* none reset zone *)
				let (nop, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in

				(*
				print_message Verbose_low (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
				*)

				let constr = make_CUB_constraint [ineq] in
				if LinearConstraint.p_is_true constr
				then 
					(
					print_message Verbose_low (" true, comparable ");
					()
					)
				else
					(
					let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_s1; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
					(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in *)
					if LinearConstraint.p_is_false constr
					then 
						(
						print_message Verbose_low (" false, comparable ");

						(* if check2 = false 
						then *)
							(
							DynArray.add clocks_constraints (location_index, clock_cons);
							);
						()

						)
					else
						(
						print_message Verbose_low (" false, not determined ");
						(*submodel info*)
						(* getInfoCurrentModel model submodel; *)
						let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
						let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
						print_message Verbose_low ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
														(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
														^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
						if check1 = true
						then
							(
							(* if check2 = false
							then *)
								(
								print_message Verbose_low ("\n Cub constraints conflicted with parameters constraints!!! " );
								print_message Verbose_low (" Adding new clocks constraints" );
								DynArray.add clocks_constraints (location_index, clock_cons);

								
								print_message Verbose_low (" Added constraints: " 
																^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																^ "\n at state: " 
																^ location_index );
								

								);
							)
						else
							(
							if check3 = false
							then
								(
								print_message Verbose_low ("\n cub constraints did not conflict with parameters constraints!!! " );
								(*Add First parameter constraints in the current submodel*)
								let new_parameters_constraints = DynArray.copy parameters_constraints in

								
								print_message Verbose_low ("\n Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																^ " into submodel ((parameters_constraints)) " 
																^ string_of_int (!count_m)  );
								

								DynArray.add parameters_constraints (true, [constr]);

								(*Create new submodel with Second constraint*)
								let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
								(* let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in *)
								let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 nop in
								let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in

								
								print_message Verbose_low (" Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																^ " into submodel (parameters_constraints) " 
																^ string_of_int ((DynArray.length submodels) +1) );
								

								DynArray.add new_parameters_constraints (false, [constr2]);

								if is_parameters_constraints_false new_parameters_constraints
								then 
									(
									print_message Verbose_low ("\n New parameters relations all False!!!, not created new submodel!!");
									)
								else
									(
									let new_clocks_constraints = DynArray.make 0 in
									(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
									(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
									DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
									);
								);
							);
						()
						);
					);
				(* none reset zone *)
				);
			(*Case 7 - end*)																						
		| _							, _							 , _							-> 	
			(*Case 8*)
			print_message Verbose_low (" 	 Case 8 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(
				(* reset zone *)
				(* )
				print_message Verbose_low (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
				*)
				

				let (nop, ineq) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in

				
				print_message Verbose_low (" Forming inequality: " 
												^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
				

				let constr = make_CUB_constraint [ineq] in

				if LinearConstraint.p_is_true constr
				then 
					(
					print_message Verbose_low (" true, comparable ");
					()
					)
				else
					(
					let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
					(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in *)
					if LinearConstraint.p_is_false constr
					then 
						(
						print_message Verbose_low (" false, comparable ");

						(* if check2 = false 
						then *)
						DynArray.add clocks_constraints (location_index, clock_cons);
						()

						)
					else
						(
						print_message Verbose_low (" false, not determined ");

						(*submodel info*)
						(* getInfoCurrentModel model submodel; *)
						let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
						let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
						print_message Verbose_low ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
														(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
														^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
						if check1 = true
						then
							(
							(* if check2 = false
							then *)
								(
								print_message Verbose_low ("\n Cub constraints conflicted with parameters constraints!!! " );
								print_message Verbose_low (" Adding new clocks constraints" );
								DynArray.add clocks_constraints (location_index, clock_cons);
								
								print_message Verbose_low (" Added constraints: " 
																^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																^ "\n at state: " 
																^ location_index );
								

								);
							)
						else
							(
							if check3 = false
							then
								(
								print_message Verbose_low ("\n cub constraints did not conflict with parameters constraints!!! " );
								(*Add First parameter constraints in the current submodel*)
								let new_parameters_constraints = DynArray.copy parameters_constraints in

								
								print_message Verbose_low ("\n Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																^ " into submodel ((parameters_constraints)) " 
																^ string_of_int (!count_m)  );
								

								DynArray.add parameters_constraints (true, [constr]);
								(*Create new submodel with Second constraint*)
								let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
								(* let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in *)
								let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 nop in
								let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in

								
								print_message Verbose_low ("\n Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																^ " into submodel (parameters_constraints) " 
																^ string_of_int ((DynArray.length submodels) +1) );
								

								DynArray.add new_parameters_constraints (false, [constr1]);
								if is_parameters_constraints_false new_parameters_constraints
								then 
									(
									print_message Verbose_low ("\n New parameters relations all False!!!, not created new submodel!!");
									)
								else
									(
									let new_clocks_constraints = DynArray.make 0 in
									(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
									(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
									DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
									);
								);
							);
						()
						);
					);
				(* reset zone - end *)
				)
			else
				(
				(* none reset zone *)
				let (nop1, ineq1) = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in

				(*
				print_message Verbose_low (" Inequality s0 <= t: \n" 
												^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");

				*)
				
				let (nop2, ineq2) = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
				
				(*
				print_message Verbose_low (" Inequality s0 <= s1: \n" 
												^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
				*)

				let constr = make_CUB_constraint [ineq1;ineq2] in
				


				if LinearConstraint.p_is_true constr
				then 
					(
					print_message Verbose_low (" true, comparable ");
					()
					)
				else
					(
					let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint2 clock_index model)]) in
					(* let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in *)
					if LinearConstraint.p_is_false constr
					then 
						(
						print_message Verbose_low (" false, comparable ");

						(* if check2 = false 
						then *)
							(
							DynArray.add clocks_constraints (location_index, clock_cons);
							);
						()
						)
					else
						(
						print_message Verbose_low (" false, not determined ");
						(*submodel info*)
						(* getInfoCurrentModel model submodel; *)
						let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
						let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
						(* Print some information *)
						if verbose_mode_greater Verbose_low then(
							print_message Verbose_low ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
														(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
														^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
						);
						if check1 = true
						then
							(
							(* if check2 = false
							then *)
								(
								print_message Verbose_low ("\n Cub constraints conflicted with parameters constraints!!! " );
								print_message Verbose_low (" Adding new clocks constraints" );
								DynArray.add clocks_constraints (location_index, clock_cons);

								
								print_message Verbose_low (" Added constraints: " 
																^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																^ "\n at state: " 
																^ location_index );
								

								); 
							)
						else
							(
							if check3 = false
							then
								(
								print_message Verbose_low ("\n cub constraints did not conflict with parameters constraints!!! " );
								(*Add First parameter constraints in the current submodel*)
								let new_parameters_constraints = DynArray.copy parameters_constraints in

								(*
								print_message Verbose_low ("\n Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																^ " into submodel ((parameters_constraints)) " 
																^ string_of_int (!count_m)  );
								*)

								DynArray.add parameters_constraints (true, [constr]);
								(*Create new submodel with Second constraint*)
								let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
								let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
								(* let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in *)
								(* let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in *)
								let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 nop1 in
								let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 nop2 in
								let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
								let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in

								(*
								print_message Verbose_low ("\n Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																^ " into submodel (parameters_constraints) " 
																^ string_of_int ((DynArray.length submodels) +1) );
								print_message Verbose_low (" Add constraint " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																^ " into submodel (parameters_constraints) " 
																^ string_of_int ((DynArray.length submodels) +1) );

								*)

								DynArray.add new_parameters_constraints (false, [constr1;constr2]);
								if is_parameters_constraints_false new_parameters_constraints
								then 
									(
									print_message Verbose_low ("\n New parameters relations all False!!!, not created new submodel!!");
									)
								else
									(
									let new_clocks_constraints = DynArray.make 0 in
									(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
									(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
									DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
									); 
								);
							);
						()
						);
					);
				(* none reset zone - end*)
				);
			(*Case 8 - end*)
	);
	()


(* [CUB-PTA Tranformation] This use for filtering infinities in constraint *)
let filter_inf model cons = 
	let inequalities = LinearConstraint.pxd_get_inequalities cons in
	let tuple_inequalities 	= (convert_inequality_list_2_tuple_list model inequalities) in
	let zero_term = (LinearConstraint.make_p_linear_term [] NumConst.zero) in
	let ls_temp = ref [] in
	( 
	match tuple_inequalities with
	| [] -> raise (InternalError(" filter_inf: Input list is empty!!! "))
	| _  -> 
			(	
			List.iter ( fun (clock_index, op, linear_term) -> 
				if (op != LinearConstraint.Op_ge &&  linear_term != zero_term)
				then
					(
					ls_temp := !ls_temp@[(clock_index, op, linear_term )];
					);
			) tuple_inequalities;
			);
	);

	(* )
	if !ls_temp = []
	then
		( 
		(* ls := !ls@[( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero )];  *)

		raise (InternalError(" filter_inf: Output list is empty!!! "));
		);
	*)
	
	let result_cons = ref (LinearConstraint.pxd_true_constraint ()) in

	List.iter(fun (clock_index_s0 , op_s0, linear_term_s0) ->
		let con = tuple2pxd_constraint (clock_index_s0 , op_s0, linear_term_s0) in
		result_cons := LinearConstraint.pxd_intersection [!result_cons; con];
	) !ls_temp;
	!result_cons


(* [CUB-PTA Tranformation] this use for adding infinity for missing clock constraint *)
let add_inf_2_missing_c_cons model cons = 
	let con = ref cons in
	List.iter (	fun clock_index -> 
	 	if (LinearConstraint.pxd_is_constrained cons clock_index) = false
	 	then
	 		(
	 		 let missing_con = tuple2pxd_constraint (create_x_ge_zero clock_index) in
	 		 con := LinearConstraint.pxd_intersection [!con; missing_con];
	 		); 
	) model.clocks_without_special_reset_clock; 
	!con



(* [CUB-PTA TRANSFORMATION] THIS IS A FUNCTION USED FOR CUB-PTA TRANSITIONS *)
let clocks_constraints_process model adding clocks_constraints loc_clocks_constraints location_index invariant_s0 =

	
	print_message Verbose_low ("\nCLOCKS CONSTRAINTS clocks_constraints_process 1: ");
		DynArray.iter (fun (l_index, con) -> 

			
			print_message Verbose_low ("\n (WILL BE A NEW STATE): \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con) 
											^ "\n FROM LOCATION: " 
											^ l_index
											); 
			

		) clocks_constraints;
	

	let con = ref (LinearConstraint.pxd_true_constraint ()) in
	for i = 1 to (DynArray.length clocks_constraints - 1) do
		let (loc_index1, cons1) = DynArray.get clocks_constraints (i-1) in
		let (loc_index2, cons2) = DynArray.get clocks_constraints (i) in
		if loc_index1 = loc_index1 
		then
		 	(
		 	let check1 = LinearConstraint.pxd_is_true cons1 in
		 	let check2 = LinearConstraint.pxd_is_true cons2 in 
		 	match (check1, check2) with
		 		| true,  true  -> ()
		 		| true,  false -> ()
		 		| false, true  -> 	let a = if LinearConstraint.pxd_is_true invariant_s0 = false then filter_inf model invariant_s0 else invariant_s0 in
		 							if loc_index1 = location_index 
		 							then
		 							   (
		 							   	con := LinearConstraint.pxd_intersection [!con; filter_inf model cons1; a (*; (get_all_clocks_ge_zero_comstraint2 clock_index model) *)];
		 							   )
		 						   else
		 						   	   (
		 						   	   	con := LinearConstraint.pxd_intersection [!con; filter_inf model cons1 (*; (get_all_clocks_ge_zero_comstraint2 clock_index model) *)];
		 						       );
		 						  (* con := LinearConstraint.pxd_intersection [!con; filter_inf model cons1 (*; (get_all_clocks_ge_zero_comstraint2 clock_index model) *)]; *)
		 						  con := add_inf_2_missing_c_cons model !con;
		 						  let check = isConstraintContainedInClocksConstraints loc_index1 !con loc_clocks_constraints in
		 						  if LinearConstraint.pxd_is_true !con = false && check = false
		 						  then
		 						  	(
		 						  	(* add true constraint *)
		 						  	DynArray.add loc_clocks_constraints (loc_index1, !con);
		 						  	adding := true;
		 						  	);
		 						  (*add gap*)
		 						  DynArray.add loc_clocks_constraints (loc_index1, cons2);
		 						  (*reset*)
		 						  con := (LinearConstraint.pxd_true_constraint ())
		 		| false, false -> con := LinearConstraint.pxd_intersection [!con; filter_inf model cons1; filter_inf model cons2];
		 	);
	done;
	(* DynArray.clear clocks_constraints;  *)
	(* DynArray.append (DynArray.copy loc_clocks_constraints) clocks_constraints; *)
	()




(************************************************************)
(************************************************************)
(* Main function for CUB transforming *)
(************************************************************)
(************************************************************)

(* We create a new, silent action specifically for this automaton: its value is (nb of regular action) + automaton_index *)
let local_silent_action_index_of_automaton_index model automaton_index =
	model.nb_actions + automaton_index


(** Takes an abstract model as input, and convert it into an equivalent CUB-PTA *)
let cubpta_of_pta model : AbstractModel.abstract_model =
	
	(* Create and start counter *)
	let cub_transformation_counter = create_time_counter_and_register "CUB transformation" Algorithm_counter Verbose_standard in
	cub_transformation_counter#start;

	(*Array of models*)
	(* let submodels = DynArray.make 0 in *)
	
	(*------------------------------------------------------------*)
	(* Create structures for the new abstract_model *)
	(*------------------------------------------------------------*)
	
	(* Locations per PTA: Array : automaton_index : -> (List of location_index) *)
	let new_locations_per_automaton_array = Array.make (model.nb_automata) [] in
	
	(* Initial location per PTA: Array : automaton_index : -> (location_index) option *)
	let new_initial_locations_array = Array.make (model.nb_automata) None in
	
	(* Location names per PTA: Array : automaton_index : -> (Array : location_index -> location_name) *)
	let new_location_names_array = Array.make (model.nb_automata) (Array.make 0 "UNINITIALIZED") in
	
	(* Urgency in PTA: Array : automaton_index : -> (Array : location_index -> bool) *)
	let new_urgency_array = Array.make (model.nb_automata) (Array.make 0 false) in
	
	(* Number of actions: add the epsilon (1 per PTA) *)
	let new_nb_actions = model.nb_actions + model.nb_automata in

	(* New actions: *)
	let new_actions = list_of_interval 0 (new_nb_actions - 1) in
	
	(* Actions per location per PTA: Array : automaton_index : -> (Array : location_index -> action_index list) *)
	let new_actions_per_location_array = Array.make (model.nb_automata) (Array.make 0 []) in
	
	(* Invariants: Array : automaton_index  -> (Array : location_index -> invariant) *)
	let new_invariants_array : ( (LinearConstraint.pxd_linear_constraint array) array ) = Array.make (model.nb_automata) (Array.make 0 (LinearConstraint.pxd_true_constraint())) in

	(* Transitions : Array : automaton_index  -> (Array : location_index -> (Hashtbl : action_index -> transitions list)) *)
	let new_transitions_array_hashtbl : ( (( (Automaton.action_index , AbstractModel.transition list) Hashtbl.t) array) array ) = Array.make (model.nb_automata) (Array.make 0 (Hashtbl.create 0)) in

	(* Location names per PTA: Array : automaton_index : -> (Array : location_index -> location_index list) *)
	let old_locations_to_new_locations = Array.make (model.nb_automata) (Array.make 0 []) in
	
	
	
	(*------------------------------------------------------------*)
	(* *)
	(*------------------------------------------------------------*)
	
	let find_all_clocks_constraints clocks_constraints location_index = let ls = ref [] in
		DynArray.iter (fun (loc_index, cons) ->
			if loc_index = location_index 
			then
				ls := !ls@[cons];	 
		) clocks_constraints;
		!ls;
	in

	let init_loc = ref "" in

	(* initial_location *)


	(*single clock bounded*)
(* 	if List.length model.automata > 1 then(
		raise (InternalError(" Sorry, we only support for single model currently. Multi-clocks and multi-constraint will be implemented in next version! "))
	); *)
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
	(*covert input model into specific data stucture*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
	print_message Verbose_low ("\n \n \n \n ------------------ Converting input model into specific data stucture ---------------------------");
	List.iter (fun automaton_index ->
		print_message Verbose_low ("\nConverting automaton " ^ (model.automata_names automaton_index) ^ "…");
		
		(* We create a new, silent action specifically for this automaton: its value is (nb of regular action) + automaton_index *)
		(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 		let local_silent_action_index = local_silent_action_index_of_automaton_index model automaton_index in *)

		(* print_message Verbose_low ("Converting automaton: " 
										^ (model.automata_names automaton_index) 
										^ "!!!!!!!"); *)

		(*elements of a submodels*)
		(*initial*)
		let locations_ini = Hashtbl.create 0 in
		let transitions_ini = DynArray.make 0 in
		let clocks_constraints_ini = DynArray.make 0 in
		let parameters_constraints_ini = DynArray.make 0 in
		(*initial - end*)



		(*set true constraint*)
		let true_constraint = LinearConstraint.p_true_constraint () in
		DynArray.add parameters_constraints_ini (true,[true_constraint]); 


		(* ALERT BUG!!!: GETTING THE FIRST LOCATION AS THE INITIAL LOCATION CAN BE WRONG IF THIS LOCATION IS NOT DECLARED AS THE FIRST IN .IMI FILE *)
		init_loc := model.location_names automaton_index (Location.get_location model.initial_location automaton_index);
		print_message Verbose_low (" Initial location: " ^ !init_loc);
		(* init_loc := model.location_names automaton_index model.initial_location; *)

		(* THIS IS THE TABLE FOR LOOK UP LOCATION INDEX FROM ITS NAME *)
		let loc_naming_tbl = Hashtbl.create 0 in

		(*Checking bounded clocked in invariant (Location)*)
		List.iter (fun location_index -> 
			(* print_message Verbose_low ("----------------Begin at " ^ (model.location_names automaton_index location_index) ^ "-------------------");
			print_message Verbose_low ("\n");
			print_message Verbose_low (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ; *)
			let invariant1 = model.invariants automaton_index location_index in
			
			(*add locations*)
			let location_index_string = (model.location_names automaton_index location_index) in
			Hashtbl.add locations_ini location_index_string invariant1;

			Hashtbl.add loc_naming_tbl location_index_string location_index;

			(* print_message Verbose_low ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ;
			print_message Verbose_low ("\n"); *)
				(*Checking bounded clocked in guards (Transition)*)
				List.iter (fun action_index -> print_message Verbose_low (" Transition/Action: " ^ (model.action_names action_index) );
		
					List.iter (fun (guard, clock_updates, discrete_update, target_location_index) 
						-> (* print_message Verbose_low ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)); *)		
						(** WORK HERE **)
						
						(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 						let invariant2 = model.invariants automaton_index target_location_index in *)

						(* print_message Verbose_low ("\n");
						print_message Verbose_low (" State/Location(D): " ^ (model.location_names automaton_index target_location_index) ) ;
						print_message Verbose_low ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
						print_message Verbose_low ("	  ----Map:(" 
														^ (model.location_names automaton_index location_index) 
														^ ")--" ^ (model.action_names action_index) ^ "-->(" 
														^ (model.location_names automaton_index target_location_index) 
														^ ") ----" );
						print_message Verbose_low ("\n"); *)
						let clock_updates = match clock_updates with
											No_update -> []
											| Resets clock_update -> clock_update
											| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); 
						in

						(*add transitions*)
						DynArray.add transitions_ini ((model.location_names automaton_index location_index), (model.location_names automaton_index target_location_index),
														continuous_part_of_guard guard, clock_updates, action_index, discrete_update);
						
						()
					) (model.transitions automaton_index location_index action_index); 
				) (model.actions_per_location automaton_index location_index); 
				(* print_message Verbose_low ("----------------End converting " ^ (model.location_names automaton_index location_index) ^ "!!!---------------------");
				print_message Verbose_low ("\n"); *)
		) (model.locations_per_automaton automaton_index);
		(* print_message Verbose_low ("\n"); *)
	(* ) model.automata; *)
	print_message Verbose_low ("\n ------------------ End converting input model into specific data stucture --------------------------- \n \n \n \n \n \n");
	(*covert input model into specific data stucture - end*)




	(* This submodels will be the Disjunctive CUB-PTA *)
	let submodels = DynArray.make 0 in
	(*Adding the first sub-model*)
	DynArray.add submodels (locations_ini, transitions_ini, clocks_constraints_ini, parameters_constraints_ini);
	let submodels_length = DynArray.length submodels in
	print_message Verbose_low ("\n Check lenth of submodels: " ^ (string_of_int submodels_length) ^ "\n \n" );




	(*loop through each submodel*)
	let count_m = ref 1 in
	while (!count_m) <= (DynArray.length submodels) do
		

		(* print_message Verbose_low ("\n \n \n \n \n \n  ----------------Sub-model No: " ^ (string_of_int !count_m) ^ "---------------------------"); *)
		let submodel = DynArray.get submodels (!count_m - 1) in
		(* print_message Verbose_low ("\n Sub-model no: " ^ (string_of_int !count_m) ); *)
		print_message Verbose_low ("\n \n \n \n \n \n  ----------------Sub-model No: " ^ (string_of_int !count_m) ^ "---------------------------");
		print_message Verbose_low ("\n -----------------------------------------------------------------------------------");
		let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in
		print_message Verbose_low ("\n Number of locations: " ^ (string_of_int (Hashtbl.length locations)) );
		print_message Verbose_low ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );
		
		(*print_message Verbose_low ("\n ----------------Sub-model No: " ^ (string_of_int !count_m) ^ "---------------------------");*)
		let adding = ref true in

		let loc_clocks_constraints = DynArray.make 0 in

		let count_loop = ref 1 in

		while !adding = true do
		(
		adding := false;

		print_message Verbose_low ("\n \n Looping through each submodel to transform -- Loop# : " ^ (string_of_int !count_loop) );

		if !count_loop = 1 
		then
			(
			print_message Verbose_low ("\n Number of locations: " ^ (string_of_int (Hashtbl.length locations)) );
			print_message Verbose_low ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );
			(*stage 1*)
			let count_t = ref 1 in
			DynArray.iter ( fun transition -> 
				print_message Verbose_low ("\n Transition No: " ^ (string_of_int !count_t) );
				let (location_index, target_location_index, guard, clock_updates, action_index, discrete_update) = transition in
				(*work here*)
				let invariant_s0 = Hashtbl.find locations location_index in
				let guard_t = guard in
				let invariant_s1 = Hashtbl.find locations target_location_index in



				(*ppl*)
				
				(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 				let inequalities = ref [] in *)

				print_message Verbose_low (" CUB transformation, Start:");
				print_message Verbose_low ("\n");
				(*transform constraints into inequality lists*)
				let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
				let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
				let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
				(*transform inequality list into tuple inequality list*)
				(* print_message Verbose_low (" **Beginning state/location** :"); *)
				let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
				(* print_message Verbose_low (" **Transition** :"); *)
				let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
				(* print_message Verbose_low (" **Destination state/location** :"); *)
				let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in

				print_message Verbose_low ("\n --------------------1st check start---------------------- ");
				List.iter (	fun clock_index ->

				 	print_message Verbose_low ("   Checking CUB conditions at clock (" ^ (model.variable_names clock_index) ^ "):");
					let (clock_index_s0 , op_s0, linear_term_s0) = filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
					let (clock_index_t, op_t, linear_term_t) = filter_upperbound_by_clock clock_index tuple_inequalities_t in
					let (clock_index_s1, op_s1, linear_term_s1) = filter_upperbound_by_clock clock_index tuple_inequalities_s1 in

				 	print_message Verbose_total ("About to call cub_tran…");

				 	cub_tran model submodels count_m 
							locations transitions
							location_index clock_index clock_updates 
							clocks_constraints parameters_constraints
							(clock_index_s0 , op_s0, linear_term_s0) (clock_index_t, op_t, linear_term_t) (clock_index_s1, op_s1, linear_term_s1) 
							(*for printing - not important*) 
							submodel;

				) model.clocks_without_special_reset_clock; 
				print_message Verbose_low ("\n --------------------1st check end----------------------- ");

				DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));

				clocks_constraints_process model adding clocks_constraints loc_clocks_constraints location_index invariant_s0;

				DynArray.clear clocks_constraints;

				(* increase transition count *)
				count_t := !count_t+1;
				();
			(* end - transitions loop *)
			) transitions;
			(* end if loop_count condition *)
			);
		(* avoid do the same things in next loop *)
		count_loop := !count_loop + 1;

		
		let count_t = ref 1 in
		DynArray.iter ( fun transition -> 
			print_message Verbose_low ("\n Transition No: " ^ (string_of_int !count_t) );
			let (location_index, target_location_index, guard, clock_updates, action_index, discrete_update) = transition in
			let invariant_s0 = Hashtbl.find locations location_index in
			let guard_t = guard in


			print_message Verbose_low (" CUB transformation, Start:");
			print_message Verbose_low ("\n");
			(*transform constraints into inequality lists*)
			let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
			let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
			(*transform inequality list into tuple inequality list*)
			let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
			let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
			print_message Verbose_low ("\n --------------------2nd check start---------------------- ");
			(* let constraints_s1 = find_all_clocks_constraints clocks_constraints target_location_index in *)
			let constraints_s1 = find_all_clocks_constraints loc_clocks_constraints target_location_index in
			List.iter (fun c_s1 ->
				if LinearConstraint.pxd_is_true c_s1 = false
				then
					(
					print_message Verbose_low ("\n Founded constraint: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c_s1)); 
					(*transform constraints into inequality lists*)
					let inequalities_s1 = LinearConstraint.pxd_get_inequalities c_s1 in
					(*transform inequality list into tuple inequality list*)
					let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in

					List.iter (	fun clock_index -> 
					 	print_message Verbose_low ("   Checking CUB conditions at clock (" ^ (model.variable_names clock_index) ^ "):");
						let (clock_index_s0 , op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
						let (clock_index_t, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
						
						let list_s1_filtered = ref (filter_upperbound_by_clock_3 clock_index tuple_inequalities_s1) in
						if !list_s1_filtered = [] 
						then
							( 
							list_s1_filtered := !list_s1_filtered@[(clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)]; 
							);

						List.iter (	fun (clock_index_s1, op_s1, linear_term_s1) -> 
							
							let new_op_s1 = ref op_s1 in
							if ( op_s1 = LinearConstraint.Op_eq )
								then  new_op_s1 := LinearConstraint.Op_le;

							cub_tran model submodels count_m 
									locations transitions
									location_index clock_index clock_updates 
									clocks_constraints parameters_constraints
									(clock_index_s0 , op_s0, linear_term_s0) (clock_index_t, op_t, linear_term_t) (clock_index_s1, !new_op_s1, linear_term_s1) 
									(*for printing - not important*) 
									submodel;

						) !list_s1_filtered; 
					) model.clocks_without_special_reset_clock;  
					
					DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));

					clocks_constraints_process model adding clocks_constraints loc_clocks_constraints location_index invariant_s0;

					DynArray.clear clocks_constraints;

					);(* end if not true constraint *)
			) constraints_s1;
			print_message Verbose_low ("\n --------------------2nd check end----------------------- ");

			
			(*work here - end*)
			(* transiton count *)
			count_t := !count_t+1;
			();
		) transitions;
		(*stage 1 - end*)

		);
		(* end - while adding loop *)
		done;

		DynArray.append loc_clocks_constraints clocks_constraints;
		(* DynArray.clear loc_clocks_constraints; *)

		(* model count *)
		count_m := !count_m+1;
		();


	(* end - for each model *)
	done;





	(* Delete true constraints and clone location *)
	let loc_clocks_constraints = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, parameters_constraints) ->
	for i = 0 to (DynArray.length c_constraints - 1) do
		let (loc_index1, cons1) = DynArray.get c_constraints (i) in
		if (LinearConstraint.pxd_is_true cons1) = false
		then
			(
				DynArray.add loc_clocks_constraints (loc_index1, filter_inf model cons1);
			);
	done;
	DynArray.clear c_constraints;
	DynArray.append loc_clocks_constraints c_constraints;
	DynArray.clear loc_clocks_constraints;
	) submodels;
	(* end - delete true constraint *)




(*
	(* [CUB-PTA TRANSFORMATION] STAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING PRINTING *)
	print_message Verbose_low ("\nSTAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING ");
	print_message Verbose_low ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints) ->

		print_message Verbose_low ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_low ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length locations) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_low ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
			

		) locations;

		print_message Verbose_low ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, target_location_index, guard, clock_updates, action_index, discrete_update) ->
			print_message Verbose_low ("\n" 
											^ source_location_index ^ " |-----> " ^ target_location_index 
											^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)
											);
		) transitions;	

		print_message Verbose_low ("\nPARAMETERS CONSTRAINTS: ");
		DynArray.iter ( fun (is_and, constraint_list) ->
			print_message Verbose_low (" 	IS CONJUNCTION ?: " ^ string_of_bool is_and );
			List.iter (fun cons1 -> 
				print_message Verbose_low (" 	FOLLOWED CONSTRAINTS: " 
										^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
			) constraint_list;
		) p_constraints;

		print_message Verbose_low ("\nCLOCKS CONSTRAINTS: ");
		DynArray.iter (fun (l_index, con) -> 
			print_message Verbose_low ("\n (WILL BE A NEW STATE): \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con) 
											^ "\n FROM LOCATION: " 
											^ l_index
											); 
		) c_constraints;

		print_message Verbose_low ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );

		model_count := !model_count+1;

	) submodels;
	print_message Verbose_low ("\nSTAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING - END");
*)
	



	(* [CUB-PTA TRANSFORMATION] STAGE 2 - ADDING LOCATIONS *)

	let newSubModels = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints) ->
		let count = ref 1 in 
		let index = Hashtbl.create 0 in

		DynArray.iter (fun (location, constr) ->
			let locName = ( "cub"^ (string_of_int !count) ^ "_" ^ location  ) in 
			(* Hashtbl.add locations locName constr; *)
			Hashtbl.add locations locName (LinearConstraint.pxd_intersection [(Hashtbl.find locations location); constr]);
			Hashtbl.add index location locName ;
			count :=  !count + 1;

			(* ADD NEW LOCATIONS INTO THE LOCATION NAMING INDEX TABLE *)
			let index = Hashtbl.find loc_naming_tbl location in
			Hashtbl.add loc_naming_tbl locName index;

		) c_constraints;
		
		let init_locs = (Hashtbl.find_all index !init_loc) in
		DynArray.add newSubModels (locations, transitions, c_constraints, p_constraints, index, init_locs@[!init_loc]);


	) submodels;




(*
	(* [CUB-PTA TRANSFORMATION] STAGE 2 - ADDING LOCATIONS PRINTING *)
	print_message Verbose_low ("\nSTAGE 2 - ADDING LOCATIONS");
	print_message Verbose_low ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_low ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_low ("\nCLOCKS CONSTRAINTS: ");
		DynArray.iter (fun (loc_index, constr) -> 
			print_message Verbose_low ("\n (WILL BE A NEW STATE): \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr) 
											^ "\n FROM LOCATION: " 
											^ loc_index
											); 
		) c_constraints;

		print_message Verbose_low ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length locations) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_low ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
		) locations;

		print_message Verbose_low ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_low ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;


		print_message Verbose_low ("\n INITIAL LOCATION:" );
		List.iter (fun initial_loc ->
			print_message Verbose_low ("\n LOCATION: " ^ initial_loc );		
		) init_locs;



		print_message Verbose_low ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_low ("\nSTAGE 2 - ADDING LOCATIONS PRINTING - END ");
*)




	(* [CUB-PTA TRANSFORMATION] STAGE 3 - ADDING TRANSITIONS *)
	DynArray.iter (fun (_, transitions, _, _, index, init_locs) ->

		DynArray.iter ( fun (source_location_index, target_location_index, guard, clock_updates, action_index, discrete_update) ->
			let listCubLoc1 = Hashtbl.find_all index source_location_index in
			let listCubLoc2 = Hashtbl.find_all index target_location_index in

			List.iter (fun loc1 ->  
				DynArray.add transitions (loc1, target_location_index, guard, clock_updates, action_index, discrete_update);
			) listCubLoc1;

			List.iter (fun loc2 ->  
				DynArray.add transitions (source_location_index, loc2, guard, clock_updates, action_index, discrete_update);
			) listCubLoc2;

			List.iter (fun loc1 -> 
				List.iter (fun loc2 -> 
					DynArray.add transitions (loc1, loc2, guard, clock_updates, action_index, discrete_update);
				) listCubLoc2;
			) listCubLoc1;

		) transitions;
	) newSubModels;




(*
	(* [CUB-PTA TRANSFORMATION] STAGE 3 - ADDING TRANSITIONS PRINTING *)
	print_message Verbose_low ("\nSTAGE 3 - ADDING TRANSITIONS ");
	print_message Verbose_low ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_low ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_low ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, target_location_index, guard, clock_updates, action_index, discrete_update) ->
			print_message Verbose_low ("\n" 
											^ source_location_index ^ " |-----> " ^ target_location_index 
											(* ^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) *)
											);
		) transitions;	

		print_message Verbose_low ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_low ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;

		print_message Verbose_low ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_low ("\nSTAGE 3 - ADDING TRANSITIONS - END ");
*)




	(* [CUB-PTA TRANSFORMATION] STAGE 4 - REMOVING PROBLEMATIC TRANSITIONS *)
	print_message Verbose_low ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS ");
	let new_transitions = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->
		for i = 1 to (DynArray.length transitions) do
			let (location_index, target_location_index, guard, clock_updates, action_index, discrete_update) = DynArray.get transitions (i-1) in
			let s0_cons = Hashtbl.find locations location_index in
			let s1_cons = Hashtbl.find locations target_location_index in

			(*
			print_message Verbose_low ("\nPARAMETERS CONSTRAINTS: ");
			DynArray.iter ( fun (is_and, constraint_list) ->
				print_message Verbose_low (" 	IS CONJUNCTION ?: " ^ string_of_bool is_and );
				List.iter (fun cons1 -> 
					
					print_message Verbose_low (" 	FOLLOWED CONSTRAINTS: " 
											^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
					
				) constraint_list;
			) p_constraints;
			*)

			(*
			print_message Verbose_low ("\nINV LOC 1: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names s0_cons) 
									^ "\nGUARD T: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) 
									^ "\nINV LOC 2: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names s1_cons) 
									); 
			*)

			let isCUB = check_problematic_transition model (s0_cons, Continuous_guard guard, s1_cons, clock_updates, p_constraints) in 
			if isCUB = true
			then
				(
					DynArray.add new_transitions (DynArray.get transitions (i-1)); 
					print_message Verbose_low ("\nOK!! CUB ");
				)
			else
				( 
					print_message Verbose_low ("\nCONFLICTED!! REMOVED! ");
				);
			print_message Verbose_low ("\n-------------------------------------");
		done;
		DynArray.clear transitions;
		DynArray.append new_transitions transitions;
		DynArray.clear new_transitions;
	) newSubModels;
	print_message Verbose_low ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS - END ");





(*
	(* [CUB-PTA TRANSFORMATION] STAGE 4 - REMOVE PROBLEMATIC TRANSITIONS PRINTING *)
	print_message Verbose_low ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS ");
	print_message Verbose_low ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in

	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_low ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_low ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, target_location_index, guard, clock_updates, action_index, discrete_update) ->
			print_message Verbose_low ("\n" 
											^ source_location_index ^ " |-----> " ^ target_location_index 
											(* ^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) *)
											);
		) transitions;	

		print_message Verbose_low ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_low ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;

		print_message Verbose_low ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_low ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS - END ");
*)


	

	

	
	
	
	(* [CUB-PTA TRANSFORMATION] FINAL STAGE - MERGING SUB-MODELS 
		New final stage code - More convenient for distributed version and more 
		Create more initial states, one for each CUB-PTA and one for all CUB-PTA (Disjunctive CUB-PTA).

	*)
	
	(* First create a normalized location name *)
	(*** BADPROG…… ***)

	let location_name_of_location_index_and_submodel_index location_index submodel_index =
		location_index ^ "_m" ^ (string_of_int submodel_index)
	in
	
	(* Handle initial location *)
	(*** BADPROG: give a string name to the new location (argh) ***)
	let new_initial_location_name  = ("disjunctive_cub_pta_init_" ^ string_of_int (automaton_index + 1)) in

	(* ADD new_initial_location_name INTO loc_naming_tbl *)
	Hashtbl.add loc_naming_tbl new_initial_location_name (Hashtbl.length loc_naming_tbl);
	
	let submodel_index = ref 1 in
	
	(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 	let numberOfAction = List.length model.actions in  *)

	(* FINAL MODEL LOCATIONS - Data structure: location_name -> invariant *)
	let new_invariants_per_location_hashtbl =  Hashtbl.create 0 in
	(* Adding the initial state *)
	Hashtbl.add new_invariants_per_location_hashtbl new_initial_location_name (LinearConstraint.pxd_true_constraint ());
	(* FINAL MODEL TRANSITIONS - Data structure: location_name -> invariant *)
	let newtransitions = DynArray.make 0 in

	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		let new_sub_initial_location_name  = ("cub_pta_init_" ^ string_of_int (!submodel_index )) in
		Hashtbl.add loc_naming_tbl new_sub_initial_location_name (Hashtbl.length loc_naming_tbl);
		Hashtbl.add new_invariants_per_location_hashtbl new_sub_initial_location_name (LinearConstraint.pxd_true_constraint ());
		DynArray.add newtransitions (new_initial_location_name, new_sub_initial_location_name, (LinearConstraint.pxd_true_constraint ()), [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ;
		
		Hashtbl.iter (fun location_index cons -> 
			let newloc = location_name_of_location_index_and_submodel_index location_index !submodel_index in
			Hashtbl.add new_invariants_per_location_hashtbl newloc cons;

			(* ADD NEW LOCATIONS INTO THE LOCATION NAMING INDEX TABLE *)
			let indx = Hashtbl.find loc_naming_tbl location_index in
			Hashtbl.add loc_naming_tbl newloc indx;

		) locations;

		DynArray.iter (fun (location_index, target_location_index, guard, clock_updates, action_index, discrete_update) -> 
			let newloc1 = location_name_of_location_index_and_submodel_index location_index !submodel_index in
			let newloc2 = location_name_of_location_index_and_submodel_index target_location_index !submodel_index in
			DynArray.add newtransitions (newloc1, newloc2, guard, clock_updates, action_index, discrete_update);

		) transitions;

		(*adding parameter relation into the first transition*)
		let listParaRelations = disjunction_constraints p_constraints in

		List.iter( fun cons ->
			let pxd_cons = LinearConstraint.pxd_of_p_constraint cons in
			if (LinearConstraint.pxd_is_false pxd_cons = false)
			then 
				(
				List.iter (fun loc -> 
				
					(* Add a transition from the initial location to all local initial locations into the dynamic array of locations *)
					(* DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ; *)
					(* DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], 0, [] ) ; *)

					(* Add a transition from the initial CUB-PTA location to all local initial locations into the dynamic array of locations *)
					DynArray.add newtransitions (new_sub_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ;

				) init_locs;
				);
		) listParaRelations;

		(* List.iter (fun loc -> 
			(* Add a transition from the initial location to all local initial locations into the dynamic array of locations *)
			DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, LinearConstraint.pxd_true_constraint (), [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ;
			(* DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], 0, [] ) ; *)
		) init_locs *);

		incr submodel_index;
	) newSubModels;




	(*
	(* [CUB-PTA TRANSFORMATION] FINAL STAGE - MERGING SUB-MODELS *)
	
	(* First create a normalized location name *)
	(*** BADPROG…… ***)

	let location_name_of_location_index_and_submodel_index location_index submodel_index =
		location_index ^ "_m" ^ (string_of_int submodel_index)
	in
	
	(* Handle initial location *)
	(*** BADPROG: give a string name to the new location (argh) ***)
	let new_initial_location_name  = ("cub_init_a" ^ string_of_int (automaton_index + 1)) in

	(* ADD new_initial_location_name INTO loc_naming_tbl *)
	Hashtbl.add loc_naming_tbl new_initial_location_name (Hashtbl.length loc_naming_tbl);
	
	let submodel_index = ref 1 in
	
	(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 	let numberOfAction = List.length model.actions in  *)

	(* FINAL MODEL LOCATIONS - Data structure: location_name -> invariant *)
	let new_invariants_per_location_hashtbl =  Hashtbl.create 0 in
	(* Adding the initial state *)
	Hashtbl.add new_invariants_per_location_hashtbl new_initial_location_name (LinearConstraint.pxd_true_constraint ());
	(* FINAL MODEL TRANSITIONS - Data structure: location_name -> invariant *)
	let newtransitions = DynArray.make 0 in

	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->
		
		Hashtbl.iter (fun location_index cons -> 
			let newloc = location_name_of_location_index_and_submodel_index location_index !submodel_index in
			Hashtbl.add new_invariants_per_location_hashtbl newloc cons;

			(* ADD NEW LOCATIONS INTO THE LOCATION NAMING INDEX TABLE *)
			let indx = Hashtbl.find loc_naming_tbl location_index in
			Hashtbl.add loc_naming_tbl newloc indx;

		) locations;

		DynArray.iter (fun (location_index, target_location_index, guard, clock_updates, action_index, discrete_update) -> 
			let newloc1 = location_name_of_location_index_and_submodel_index location_index !submodel_index in
			let newloc2 = location_name_of_location_index_and_submodel_index target_location_index !submodel_index in
			DynArray.add newtransitions (newloc1, newloc2, guard, clock_updates, action_index, discrete_update);

		) transitions;

		(*adding parameter relation into the first transition*)
		let listParaRelations = disjunction_constraints p_constraints in

		List.iter( fun cons ->
			let pxd_cons = LinearConstraint.pxd_of_p_constraint cons in
			if (LinearConstraint.pxd_is_false pxd_cons = false)
			then 
				(
				List.iter (fun loc -> 
				
					(* Add a transition from the initial location to all local initial locations into the dynamic array of locations *)
					DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ;
					(* DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], 0, [] ) ; *)

				) init_locs;
				);
		) listParaRelations;

		(* List.iter (fun loc -> 
			(* Add a transition from the initial location to all local initial locations into the dynamic array of locations *)
			DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, LinearConstraint.pxd_true_constraint (), [], local_silent_action_index_of_automaton_index model automaton_index, [] ) ;
			(* DynArray.add newtransitions (new_initial_location_name, location_name_of_location_index_and_submodel_index loc !submodel_index, pxd_cons, [], 0, [] ) ; *)
		) init_locs *);

		incr submodel_index;
	) newSubModels;
	*)










	(* FINAL OUTPUT MODEL *)
	(*** NOTE: unused code written by Gia, removed by ÉA (2017/02/08) ***)
(* 	let finalModel = (new_invariants_per_location_hashtbl, newtransitions, loc_naming_tbl) in  *)
	(* additional stage - end *)




(*
	(* [CUB-PTA TRANSFORMATION] FINAL STAGE - MERGING SUB-MODELS *)
	print_message Verbose_low ("\nFINAL STAGE - MERGING SUB-MODELS ");

		print_message Verbose_low ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length new_invariants_per_location_hashtbl) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_low ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
		) new_invariants_per_location_hashtbl;

		print_message Verbose_low ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length newtransitions) );
		DynArray.iter ( fun (source_location_index, target_location_index, guard, clock_updates, action_index, discrete_update) ->

			
			print_message Verbose_low ("\n" 
											^ source_location_index ^ " |-----> " ^ target_location_index 
											^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)
											^ "\n Action index: " 
											^ (string_of_int action_index)
											);
			

		) newtransitions;	
		(* print_message Verbose_low ("\n Number of locations :"^ string_of_int (Hashtbl.length newstates) );
		Hashtbl.iter (fun location_index cons ->
			print_message Verbose_low ("\n" ^ location_index );
		) newstates;	 *)
	print_message Verbose_low ("\nFINAL STAGE - MERGING SUB-MODELS - END ");
*)

	
	
	(*------------------------------------------------------------*)
	(* Now update the data structures for THIS PTA *)
	(*------------------------------------------------------------*)
	
	(* First get the number of new locations for this PTA *)
	let new_nb_locations = Hashtbl.length new_invariants_per_location_hashtbl in

	(* print_message Verbose_low ("\n New number of locations :"^ string_of_int new_nb_locations ); *)
	
	(* Create the list of location_index for this PTA *)
	let list_of_location_index_for_this_pta = (* list_of_interval 0 (new_nb_locations-1) in *)
	let ls = ref [] in
	if new_nb_locations != 0
	then
		(
		for i = 0 to new_nb_locations - 1 do
	      ls := !ls@[i];
	    done
		);
	!ls;
	in

	(* print_message Verbose_low ("\n list_of_location_index_for_this_pta :"^ string_of_int (List.length list_of_location_index_for_this_pta) ); *)

	
	(* 1) Handle locations per automaton *)
	new_locations_per_automaton_array.(automaton_index) <- list_of_location_index_for_this_pta;
	
	
	(* 2a) Convert location_names into location_index *)
	(* 2b) Handle invariants *)

	(* Create the structure location_name -> location_index *)
	let location_index_of_location_name = Hashtbl.create new_nb_locations in
	
	(* Create the structure location_index -> location_name *)
	let location_name_of_location_index = Array.make new_nb_locations "UNINITIALIZED" in
	
	(* Create the structure location_index -> urgent (bool) *)
	(*** NOTE: quite a hack, we set all locations to be urgent, and then all old locations will be erased to their former value; so new (initial) locations will automatically be urgent! ***)
	let urgency_of_location_index = Array.make new_nb_locations true in
	
	(* Create the structure location_index -> action_index list *)
	let actions_per_location_array = Array.make new_nb_locations [] in
	
	(* Initialize the invariants for this PTA (initially all true) *)
	(*** NOTE: would be better to first create to a dummy constraint, instead of creating a new p_true_constraint() for each cell, that will be overwritten anyway ***)
	new_invariants_array.(automaton_index) <- Array.make new_nb_locations (LinearConstraint.pxd_true_constraint());
	


	(* THIS IS THE INDEX OF OLD LOCATION TO NEW LOCATION(S) *)
	(* let new_loc_index_tbl = Hashtbl.create 0 in *)

	old_locations_to_new_locations.(automaton_index) <- Array.make (List.length (model.locations_per_automaton automaton_index)) [];

	let current_location_index = ref 0 in



	(* Fill location_index_of_location_name , location_name_of_location_index and new_invariants_array *)
	Hashtbl.iter (fun location_name location_invariant ->
		(* Add the binding location_name , location_index to the new structure *)
		Hashtbl.add location_index_of_location_name location_name !current_location_index;

		(* ADD new_loc_index_tbl, USED FOR TRACING NEW LOCATION FROM THE OLD LOCATION *)
		let old_loc_index = Hashtbl.find loc_naming_tbl location_name in
		(* ELIMINATE THE NEW CUB INITIAL LOCATION WHICH IS NOT IN OLD MODEL LOCATIONS *)
		if ( List.mem old_loc_index (model.locations_per_automaton automaton_index) )
		then
			( 
			print_message Verbose_low ("\nHashtbl.length: " ^string_of_int  (List.length (model.locations_per_automaton automaton_index)) ^ "\nNAME: " ^ location_name ^ "\n OLD LOCAION: " ^ string_of_int old_loc_index ^ " NEW LOCAION: " ^ string_of_int !current_location_index );
			(* Hashtbl.add new_loc_index_tbl old_loc_index !current_location_index; *)
			
			(* Update the urgency *)
			(* urgency_of_location_index.(!current_location_index) <- (model.is_urgent automaton_index old_loc_index); *)
			old_locations_to_new_locations.(automaton_index).(old_loc_index) <- !current_location_index :: old_locations_to_new_locations.(automaton_index).(old_loc_index);
			
			(* TESTING INFORNATION IN new_loc_index_tbl *)

			urgency_of_location_index.(!current_location_index) <- (model.is_urgent automaton_index old_loc_index);
			);
		
		(* Add the binding location_index , location_name to the new structure *)
		location_name_of_location_index.(!current_location_index) <- location_name;

		(*
		print_message Verbose_low ("\n Bug1!");
		(* Update the urgency *)
		(* urgency_of_location_index.(!current_location_index) <- (model.is_urgent automaton_index old_loc_index); *)
		print_message Verbose_low ("\n Bug2!");
		*)

		(* And we update the invariant as well *)
		new_invariants_array.(automaton_index).(!current_location_index) <- location_invariant;

		(* Increment the current location_index *)
		incr current_location_index;
	
	) new_invariants_per_location_hashtbl;


	(* Hashtbl.iter (fun old_loc_index new_loc_index -> 
		print_message Verbose_low ( "\n OLD LOCAION: " ^ string_of_int old_loc_index ^ " NEW LOCAION: " ^ string_of_int new_loc_index );
	) new_loc_index_tbl; *)

	
	
	(* 3) Handle location names *)
	new_location_names_array.(automaton_index) <- location_name_of_location_index;

	(* 3a) Handle urgency *)
	new_urgency_array.(automaton_index) <- urgency_of_location_index;

	(* 3b) Handle initial location *)
	(* Update the array of new initial location *)
	let initial_location_index = Hashtbl.find location_index_of_location_name new_initial_location_name in
	new_initial_locations_array.(automaton_index) <- Some initial_location_index;
	
	
	(* 4) Transitions *)
	
	(* First initialize the array for this PTA *)
	new_transitions_array_hashtbl.(automaton_index) <- Array.make new_nb_locations (Hashtbl.create 0);
	
	(* Sort the transitions according to their origin location index using a structure Array : location_index -> (action_index, transition) list *)
	let transitions_per_location = Array.make new_nb_locations [] in
	
	print_message Verbose_low ("\nSorting new transitions per origin location…");
	
	DynArray.iter (fun (newloc1, newloc2, guard, clock_updates, action_index, discrete_update) -> 
	
		(* Print some information *)
		if verbose_mode_greater Verbose_low then(
			print_message Verbose_low ("  Considering transition (newloc1 = " ^ (newloc1) ^ ", newloc2 = " ^ (newloc2) ^ ", guard = " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) ^ ", clock_updates = reset [" ^ (string_of_list_of_string_with_sep "-" (List.map model.variable_names clock_updates)) ^ "], action_index = " ^ (string_of_int action_index) ^ ", discrete_update = TODO)…");
		);
	
		(* Get the source location index *)
		(*** WARNING: no exception mechanism! ***)
		let source_location_index = Hashtbl.find location_index_of_location_name newloc1 in
		let target_location_index = Hashtbl.find location_index_of_location_name newloc2 in
		
		(* Convert to abstract_model.transition *)
		(* type transition = guard * clock_updates * discrete_update list * location_index *)

		(*** WARNING: other updates than clock updates not considered so far ***)
		
		let new_transition = action_index , (Continuous_guard guard, Resets clock_updates, discrete_update, target_location_index) in
		
		(* Add to array *)
		transitions_per_location.(source_location_index) <- new_transition :: transitions_per_location.(source_location_index);
	) newtransitions;
	
	
	
	(* We take the first action used in this PTA *)
	if List.length (model.actions_per_automaton automaton_index) = 0 then(
		raise (InternalError("Case with 0 action in a PTA not considered :("))
	);

	(* Iterate on locations for this automaton *)
	List.iter (fun location_index ->
		(* Initialize the hashtbl for this PTA and this location *)
		(*** NOTE: VERY necessary!!! as the "Array.make new_nb_locations (Hashtbl.create 0)" above puts the SAME hashtable everywhere in the array!!! ***)
		new_transitions_array_hashtbl.(automaton_index).(location_index) <- Hashtbl.create 0;
		
		(* Retrieve the transitions for this location *)
		let transitions_for_this_location = transitions_per_location.(location_index) in
		
		(*** BADPROG: unnecessary exponential blow-up (consider all transitions for EACH action); but anyway the model is still "relatively" small so that shouldn't be harmful ***)
		List.iter(fun action_index ->
			(* First retrieve the transitions for this location and for this action *)
			let actions_and_transitions_for_this_location_and_action = List.filter (fun (action_index', _) -> action_index' = action_index) transitions_for_this_location in

			(* Then only keep the transitions (not the actions, as they are now all equal to action_index) *)
			let _ , (transitions_for_this_location_and_action : AbstractModel.transition list) = List.split actions_and_transitions_for_this_location_and_action in

			Hashtbl.add new_transitions_array_hashtbl.(automaton_index).(location_index) action_index transitions_for_this_location_and_action;
		) new_actions;
		
		
		(* Also find the set of actions available in this location *)
		(*** BADPROG: clearly not optimal way of computing the set of actions; but anyway the model is still "relatively" small so that shouldn't be harmful ***)
		let multiply_defined_actions, _ = List.split transitions_for_this_location in
		let actions_for_this_location = list_only_once multiply_defined_actions in
		
		(* Set the actions for this location *)
		actions_per_location_array.(location_index) <- actions_for_this_location;
		
	) list_of_location_index_for_this_pta;

	
	(* 5) Handle actions per location *)
	new_actions_per_location_array.(automaton_index) <- actions_per_location_array;



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
	) model.automata; (* end List.iter on automata *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**)
	
	



	(*------------------------------------------------------------*)
	(* Convert to the desired functional style *)
	(*------------------------------------------------------------*)
	
	
	(* Action names: identical to before transformation, with the exception of the new local epsilon action in each PTA *)
	let new_action_names action_index =
		(* Regular action: call old function *)
		if action_index < model.nb_actions then model.action_names action_index
		(* Too big: exception! *)
		else if action_index > model.nb_actions + model.nb_automata then(
			raise (InternalError("Value of action_index '" ^ (string_of_int action_index) ^ "' exceeds the number of special silent actions required in the CUB transformer!"))
		)
		(* Case local silent action: *)
		else "epsilon_" ^ (string_of_int action_index)
	in
	
	(* Action types: identical to before transformation, with the exception of the new local epsilon action in each PTA *)
	let new_action_types action_index =
		(* Regular action: call old function *)
		if action_index < model.nb_actions then model.action_types action_index
		(* Too big: exception! *)
		else if action_index > model.nb_actions + model.nb_automata then(
			raise (InternalError("Type of action_index '" ^ (string_of_int action_index) ^ "' exceeds the number of special silent actions required in the CUB transformer!"))
		)
		(* Case local silent action: *)
		else Action_type_nosync
	in
	
	
	(* Actions per PTA: identical to before transformation, with the exception of the new local epsilon action in each PTA *)
	let new_actions_per_automaton automaton_index =
		(* Get the local epsilon action *)
		let local_silent_action_index = local_silent_action_index_of_automaton_index model automaton_index in
		(* Add it to the old list *)
		local_silent_action_index :: (model.actions_per_automaton automaton_index)
	in
	
	(* PTA per action: identical to before transformation, with the exception of the new local epsilon action in each PTA *)
	let new_automata_per_action action_index =
		(* Case new epsilon action *)
		if action_index >= model.nb_actions && action_index < new_nb_actions then
			let automaton_for_this_action = action_index - model.nb_actions in
			(* Return singleton *)
			[automaton_for_this_action]
		else model.automata_per_action action_index
	in
	
	let new_invariants_function automaton_index location_index = new_invariants_array.(automaton_index).(location_index) in
	
	
	let new_locations_per_automaton_function automaton_index = new_locations_per_automaton_array.(automaton_index) in
	
	
	let new_actions_per_location_function automaton_index location_index = new_actions_per_location_array.(automaton_index).(location_index) in
	
	
	let new_location_names_function automaton_index location_index = new_location_names_array.(automaton_index).(location_index) in
	
	let new_urgency_function automaton_index location_index = new_urgency_array.(automaton_index).(location_index) in
	

 	let new_initial_location =

		(* Shortcut *)
		let former_initial_location = model.initial_location in

		(* First get the new initial locations *)
		let initial_PTA_locations = List.map (fun automaton_index ->
			(* First get the new location index *)
			let initial_location_index =
			match new_initial_locations_array.(automaton_index) with
				| None -> raise (InternalError "Initial location index not found in NZ CUB transformation")
				| Some location_index -> location_index
			in
			
			automaton_index, initial_location_index
		) model.automata in

		(* Second get the discrete values from the former initial location *)	
		let discrete_values = List.map (fun discrete_index -> discrete_index , (Location.get_discrete_value former_initial_location discrete_index)) model.discrete in

		Location.make_location initial_PTA_locations discrete_values
	in


	(* 	transitions : automaton_index -> location_index -> action_index -> (transition list); *)
	let new_transitions_function automaton_index location_index action_index =
		(* Retrieve the hash table *)
		let hashtable_for_this_location = new_transitions_array_hashtbl.(automaton_index).(location_index) in
		
		(* Try to get the list of transitions for this actions (which may not be defined) *)
		try(
			Hashtbl.find hashtable_for_this_location action_index
		) with
			Not_found -> []
	in
	
	(* Transform the property *)
	(*** NOTE! only support reachability properties for now ***)
	let new_unreachable_locations =
		match model.correctness_condition with
		(* No property *)
		| None -> []
		
		(*** NOTE: for now, we only support ONE automaton and ONE location per OR constraint, i.e., we allow disjunctions but not conjunctions of bad locations. Reason: since one location can be translated to many new locations, if there is more than one automaton in a disjunct, we need to explore all possible combinations, which is a bit cumbersome for now. ***)
		| Some (Unreachable unreachable_global_location_list) ->
			(* We iterate in the OR list *)
			(*** NOTE: we use a fold_left instead of a map as one disjunct can be translated in several disjuncts ***)
			List.fold_left (fun current_OR_list unreachable_global_location ->

				(* Retrieve the structure fields *)
				let unreachable_locations = unreachable_global_location.unreachable_locations in
				let discrete_constraints = unreachable_global_location.discrete_constraints in
				
				(*** NOTE: here: we want a SINGLE pair ***)
				let automaton_index, location_index = match unreachable_locations with
				| [automaton_index, location_index] -> automaton_index, location_index
				| _ -> raise (NotImplemented "So far we can only transform properties of the form loc[pta1] = loc1 OR loc[pta2] = loc2 OR … That is, conjunctions of locations are not allowed (discrete variables are ok).")
				in
				
				(* Get the new location(s) in the transformed model *)
				let new_locations = old_locations_to_new_locations.(automaton_index).(location_index) in
				
				(* Convert to structures *)
				let new_unreachable_global_locations = List.map (fun new_location_index ->
					(* Recreate the structure unreachable_global_location *)
					{
						unreachable_locations	= [automaton_index, new_location_index];
						discrete_constraints	= discrete_constraints;
					}
				) new_locations
				in
				
				(* Add them to the other locations computer so far *)
				List.rev_append new_unreachable_global_locations current_OR_list
			) [] unreachable_global_location_list
			
(*
		| Some (Unreachable unreachable_global_location_list) ->
			List.map (fun unreachable_global_location ->

				(* Retrieve the structure fields *)
				let unreachable_locations = unreachable_global_location.unreachable_locations in
				let discrete_constraints = unreachable_global_location.discrete_constraints in
				
				(* Convert the list of unreachable locations using the new indexes *)
				let new_unreachable_locations = List.fold_left (fun current_list (automaton_index, location_index) -> 
					(* Get the new location(s) in the transformed model *)
					let new_locations = old_locations_to_new_locations.(automaton_index).(location_index) in
					
					(* Convert to pairs automaton_index, location_index *)
					let new_locations_with_automaton_index = List.map (fun location_index -> automaton_index, location_index) new_locations in
					
					(* Add them to the other locations computer so far *)
					List.rev_append current_list new_locations_with_automaton_index
				) [] unreachable_locations in
				
				(* Recreate the structure unreachable_global_location *)
				{
					unreachable_locations	= new_unreachable_locations;
					discrete_constraints	= discrete_constraints;
				}
			
			) unreachable_global_location_list
		*)
		(* Other cases not supported (in the entire tool) *)
		| _ -> raise (NotImplemented "Properties different from reachability using a single automaton and a single location are not supported yet in the CUB transformation")
	in
		
	
	(*------------------------------------------------------------*)
	(* Print some information *)
	(*------------------------------------------------------------*)
	if verbose_mode_greater Verbose_low then(
		
	
		print_message Verbose_low ("\nNew locations per automaton:");
		Array.iteri(fun automaton_index list_of_locations ->
			print_message Verbose_low ("Automaton #" ^ (string_of_int automaton_index ) ^ " -> " ^ (
				string_of_list_of_string_with_sep ", " (List.map (fun location_index -> "l_" ^ (string_of_int location_index)) list_of_locations)
			) ^ "");
		) new_locations_per_automaton_array;
	
	
		print_message Verbose_low ("\nNew location names:");
		(* Iterate on automata *)
		Array.iteri(fun automaton_index array_of_names ->
			print_message Verbose_low ("Automaton #" ^ (string_of_int automaton_index ) ^ ":");
			(* Iterate on locations for this automaton *)
			Array.iteri(fun location_index location_name ->
				print_message Verbose_low ("  Location l_" ^ (string_of_int location_index ) ^ " -> " ^ location_name);
			) array_of_names;
		) new_location_names_array;
	
	
		print_message Verbose_low ("\nAll actions:");
		print_message Verbose_low (string_of_list_of_string_with_sep " - " (List.map new_action_names model.actions) );

		
		print_message Verbose_low ("\nNew actions per location:");
		(* Iterate on automata *)
		Array.iteri(fun automaton_index array_of_names ->
			print_message Verbose_low ("Automaton #" ^ (string_of_int automaton_index ) ^ ":");
			(* Iterate on locations for this automaton *)
			Array.iteri(fun location_index location_name ->
				print_message Verbose_low ("  Location l_" ^ (string_of_int location_index ) ^ " -> " ^ "");
				let actions_for_this_location = new_actions_per_location_function automaton_index location_index in
				print_message Verbose_low (string_of_list_of_string_with_sep " - " (List.map new_action_names actions_for_this_location) );
			) array_of_names;
		) new_location_names_array;
	
	
		print_message Verbose_low ("\nNew invariants:");
		(* Iterate on automata *)
		Array.iteri(fun automaton_index array_of_invariants ->
			print_message Verbose_low ("Automaton #" ^ (string_of_int automaton_index ) ^ ":");
			(* Iterate on locations for this automaton *)
			Array.iteri(fun location_index invariant ->
				print_message Verbose_low ("  Location l_" ^ (string_of_int location_index ) ^ " -> " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant));
			) array_of_invariants;
		) new_invariants_array;
	
	
		(* Dummy pretty-printing of clock updates *)
				(*** TODO ***)
		let string_of_clock_updates clock_updates = 
			"TODO"
		in
	
		(* Dummy pretty-printing of transitions *)
		let string_of_transition automaton_index (guard , clock_updates , discrete_update, target_location_index) = 
			"["
				^ "g=" ^ (ModelPrinter.string_of_guard model.variable_names guard)
				^
				", Xupdates=" ^ (string_of_clock_updates clock_updates)
				^
				(*** TODO ***)
				", Dupdates=TODO"
				^
				", target=" ^ (new_location_names_function automaton_index target_location_index)
				^
			"]"
		in
		
		let string_of_transitions automaton_index transition_list =
			string_of_list_of_string_with_sep "\n    "
				(List.map (string_of_transition automaton_index) transition_list)
		in
		
		print_message Verbose_low ("\nNew transitions:");
		(* Iterate on automata *)
		Array.iteri(fun automaton_index array_of_hashtables ->
			print_message Verbose_low ("Automaton #" ^ (string_of_int automaton_index ) ^ ":");
			(* Iterate on locations for this automaton *)
			Array.iteri(fun location_index action_hashtable ->
				print_message Verbose_low ("  Location l_" ^ (string_of_int location_index ) ^ ":");
				(* Iterate on actions for this automaton *)
				Hashtbl.iter(fun action_index transitions ->
					print_message Verbose_low ("    Transitions via action " ^ (string_of_int action_index) ^ ":\n    " ^ (string_of_transitions automaton_index transitions) ^ "");
				)action_hashtable;
			) array_of_hashtables;
		) new_transitions_array_hashtbl;
		print_message Verbose_low ("\n(end transitions)");
	
	); (* end Verbose_low *)
	
		
	(************************************************************)
	(** Build the abstract model *)
	(************************************************************)
	let transformed_abstract_model =
	{
		(** General information **)
		(* Cardinality *)
		nb_automata = model.nb_automata;
		nb_actions = new_nb_actions;
		nb_clocks = model.nb_clocks;
		nb_discrete = model.nb_discrete;
		nb_parameters = model.nb_parameters;
		nb_variables = model.nb_variables;
		
		(* Is there any stopwatch in the model? *)
		has_stopwatches = model.has_stopwatches;
		(* Is the model an L/U-PTA? *)
		(*** TODO (for now, we just assume that after transformation not an L/U anymore ***)
		lu_status = PTA_notLU;

		(** Content of the PTA **)
		(* The observer *)
	
		(*** TODO ***)
	
		observer_pta = None;
		is_observer = (fun _ -> false);

		(* The list of clock indexes *)
		clocks = model.clocks;
		(* True for clocks, false otherwise *)
		is_clock = model.is_clock;
		(* Index of the special clock to be reset at each transition to measure time elapsing (only used in NZ checking) *)
 		special_reset_clock = model.special_reset_clock;
 		(* The list of clock indexes except the reset clock (used, e.g., to print the model *)
 		clocks_without_special_reset_clock = model.clocks_without_special_reset_clock;
		(* The list of discrete indexes *)
		discrete = model.discrete;
		(* True for discrete, false otherwise *)
		is_discrete = model.is_discrete;
		(* The list of parameter indexes *)
		parameters = model.parameters;
		(* The non parameters (clocks and discrete) *)
		clocks_and_discrete = model.clocks_and_discrete;
		(* The non clocks (parameters and discrete) *)
		parameters_and_discrete = model.parameters_and_discrete;
		(* The function = variable_index -> variable name *)
		variable_names = model.variable_names;
		(* The type of variables *)
		type_of_variables = model.type_of_variables;
		
		(* The automata *)
		automata = model.automata;
		(* The automata names *)
		automata_names = model.automata_names;
		
		(* The locations for each automaton *)
		locations_per_automaton = new_locations_per_automaton_function;
		(* The location names for each automaton *)
		location_names = new_location_names_function;
		(* The urgency for each location *)
		is_urgent = new_urgency_function;
		(*** TODO: all new initial locations shall be urgent! ***)

		(* All action indexes *)
		actions = new_actions;
		(* Action names *)
		action_names = new_action_names;
		(* The type of actions *)
		action_types = new_action_types;
		(* The list of actions for each automaton *)
		actions_per_automaton = new_actions_per_automaton;
		(* The list of automatons for each action *)
		automata_per_action = new_automata_per_action;
		(* The list of actions for each automaton for each location *)
		actions_per_location = new_actions_per_location_function;

		(* The cost for each automaton and each location *)
		(*** TODO ***)
		(*** NOTE: dummy function ***)
		costs = (fun _ _ -> None);
		
		(* The invariant for each automaton and each location *)
		invariants = new_invariants_function;
		
		(* The transitions for each automaton and each location and each action *)
		transitions = new_transitions_function;
		(* The list of clocks stopped for each automaton and each location *)
		(*** TODO ***)
		(*** NOTE: dummy function ***)
		stopwatches = (fun _ _-> []);

	
		(* All clocks non-negative *)
		px_clocks_non_negative = model.px_clocks_non_negative;
		(* Initial location of the model *)
			(*** TODO ***)
		initial_location = new_initial_location;
		(* Initial constraint of the model *)
		initial_constraint = model.initial_constraint;
		(* Initial constraint of the model projected onto P *)
		initial_p_constraint = model.initial_p_constraint;
		(* Initial constraint of the model projected onto P and all clocks non-negative *)
		px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint;

		(* Property defined by the user *)
		(*** TODO ***)
		(*** WARNING: any property will be turned into an (equivalent) reachability property, i.e., the original user property is lost ***)
		user_property = if new_unreachable_locations = [] then Noproperty else Unreachable_locations new_unreachable_locations;
		(* Property defined by the model *)
		correctness_condition = if new_unreachable_locations = [] then None else Some (Unreachable new_unreachable_locations);
		(* List of parameters to project the result onto *)
		projection = model.projection;
		(* Parameter to be minimized or maximized *)
		optimized_parameter = model.optimized_parameter;
	}
	
	in
	
	(* Stop counter *)
	cub_transformation_counter#stop;
	
	(* Return *)
	transformed_abstract_model
