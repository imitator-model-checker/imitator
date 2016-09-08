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
 * Last modified     : 2016/09/05
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



(************************************************************)
(************************************************************)
(* Useful global functions *)
(************************************************************)
(************************************************************)


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
						) model.clocks; 
			!list_s0

	| _ ->	(* print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) Start:"); *)
			List.iter 	(fun inequality -> (
						let (clock_index_2, operator, parametric_linear_term) = LinearConstraint.clock_guard_of_linear_inequality inequality in
						list_s0 := !list_s0@[(clock_index_2, operator, parametric_linear_term)]; 
						(* print_message Verbose_standard (" inequality: " ^ (model.variable_names clock_index_2) 
														^ " " ^ (LinearConstraint.operator2string operator) 
														^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
														^ " added!"); *)
							) 
						) inequalities; 
			(* print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) End!");
			print_message Verbose_standard ("\n"); *)
			!list_s0



(* ONE UPPER-BOUNDS OF THE SAME CLOCK *)
let filter_upperbound_by_clock clock_index tuple_inequalities_s0 =	
	(* print_message Verbose_standard (" 	filtering upper-bound of clock (" ^ (model.variable_names clock_index) ^ ") in this list of tuple inequalities:"); *)
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

		if ( (clock_index == clock_index_2) && ( operator == LinearConstraint.Op_l || operator == LinearConstraint.Op_le ) )
		then 
			(
			(* print_message Verbose_standard ("	Upper-bounded found: " ^ (model.variable_names clock_index_2) 
											^ " " ^ (LinearConstraint.operator2string operator) 
											^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
											^ " !"); *)
			index := i; 
			count := (!count + 1);
			if !count > 1
			then  raise (InternalError("Detected more than 1 different upperbounds of the same clock in same constraint!!! "))

			);
	done;
	(*just for printing, start*)
	(* if !count = 0
	then 
		print_message Verbose_standard ("	Upper-bound not found:!, return clock  " ^ (model.variable_names clock_index) ^">= 0" );  *)
		(*just for printing, end*)
		(* if there is no upper-bound -> reutrn (clock >= 0) *)

	if !count = 0
	then
		( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero ) (* make_linear_term [] NumConst.zero) *)
	else
		(* result *)
		List.nth tuple_inequalities_s0 !index



(* 
THIS IS USED FOR FILTERING TUPLES [CUB-PTA TRANSFORMATION], IF THERE IS EMPTY MUST RETURN THE CLOCK >= 0, 
IN CASE THE EMPTY LIST RETURNED, THE RESULT WILL BE WRONG! (EX: LIST.ITER), 
*)
let filter_upperbound_by_clock_2 clock_index tuple_inequalities_s0 =
	let ls = ref
	( 
	match tuple_inequalities_s0 with
	| [] ->  raise (InternalError("Detected empty list, check again the input inequalities or it might be True constraint "))
	| _  -> 
	(List.find_all (fun (index, op,_) -> index == clock_index && (op == LinearConstraint.Op_le ||op == LinearConstraint.Op_l || op == LinearConstraint.Op_eq) ) tuple_inequalities_s0);
	);
	in
	if !ls = []
	then
		(
		ls := [( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero )]; 		
		);
	!ls												




let make_CUB_inequality (op1, linear_term1) (op2, linear_term2) = 	
	let linear_term = LinearConstraint.sub_p_linear_terms linear_term1 linear_term2 in
	match op1, op2 with
	(*linear_term1 < linear_term2*)
	| LinearConstraint.Op_le, LinearConstraint.Op_l -> LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_l
	(*linear_term1 <= linear_term2*)
	| _, _ -> LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_le



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
		(* print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ) ; *)
		if LinearConstraint.p_is_false con_intersection
		then
			(
			(* print_message Verbose_standard ("\n Conflict!!! "); *)
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
		if verbose_mode_greater Verbose_standard then(
			(* Get the model *)
			(*** NOTE: should be safe (this is for pretty-printing anyway) but note that we are modifying the model ***)
			let model = Input.get_model() in
	
			print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
				^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
		);
		if not (LinearConstraint.p_is_false con_intersection)
		then
			(
			print_message Verbose_standard ("\n Conflict!!! ");
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
		if verbose_mode_greater Verbose_standard then(
			(* Get the model *)
			(*** NOTE: should be safe (this is for pretty-printing anyway) but note that we are modifying the model ***)
			let model = Input.get_model() in
	
			print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con)  
											^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
		);
		if not (LinearConstraint.pxd_is_false con_intersection)
		then
			(
			print_message Verbose_standard ("\n Conflict!!! ");
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
			let con1 = List.hd cons in
			if (LinearConstraint.p_is_equal con1 con)
			then check := true; 
			);
	) p_cons;
	!check




let getInfoCurrentModel model submodel =
	let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in
	print_message Verbose_standard ("\n ------------------------SUBMODEL INFO------------------------------- ");
	print_message Verbose_standard ("\n NUMBER OF LOCATIONS: " ^ string_of_int (Hashtbl.length locations) );
	print_message Verbose_standard ("\n NUMBER OF TRANSITIONS: " ^ string_of_int (DynArray.length transitions) );
	(*for checking the first parameters constraints*)
	print_message Verbose_standard ("\n CURRENT PARAMETERS RELATION!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_standard ("\n IS CONJUNCTION: " ^ string_of_bool is_and ^ ": ");
		List.iter (fun cons -> 
			print_message Verbose_standard (" 	CONSTRAINTS: \n" 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons) );
		) constraint_list;
	) parameters_constraints;
	(*for checking the first parameters constraints - end*)
	(*clocks constraitns*)
	print_message Verbose_standard ("\n CURRENT CLOCKS CONSTRAINS!!!!: ");
	(
	DynArray.iter (fun (index, cons) -> 
		print_message Verbose_standard ("\n LOCATION: " ^ index );
		
		print_message Verbose_standard ("  CONSTRAINTS: \n" 
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) );
	) clocks_constraints;
	);
	(*clocks constraitns*)
	print_message Verbose_standard ("\n ------------------------SUBMODEL INFO END--------------------------- ");
	()




(************************************************************)
(************************************************************)
(* Main function for CUB checking *)
(************************************************************)
(************************************************************)



(*attention: multi inequalities of the same clock in model*)

(*let check_cub_condition *)

(*check CUB condition function -> return boolean value*)


(*** NOTE: this function is NEVER called ***)
let cub_check_2 model invariant_s0 guard_t invariant_s1 clock_updates = 
	(*ppl*)
	(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
	let inequalities = ref [] in
	print_message Verbose_standard (" CUB check, Start:");
	print_message Verbose_standard ("\n");

	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

	print_message Verbose_standard (" **Beginning state/location** :");
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
	print_message Verbose_standard (" **Transition** :");
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
	print_message Verbose_standard (" **Destination state/location** :");
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in
	
	let isCUB_PTA = ref true in

	List.iter (	fun clock_index -> 
		let inequalities_need_to_solve = ref [] in
		print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

		print_message Verbose_standard ("\n 	**Beginning state/location** :");
		let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
		print_message Verbose_standard ("\n 	**Transition** :");
		let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
		print_message Verbose_standard ("\n 	**Destination state/location** :");
		let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in



		print_message Verbose_standard ("\n");
		print_message Verbose_standard ("Comparing: ");

		let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
		let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
		let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in

		print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
		
		print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");

		(* if List.mem clock_index reset_clocks = true 
		then lower_inequality := linear_term_t; *)

		(* let result = ref true in *)
		let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

			| (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
																									true;

			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
																									false;

			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
				print_message Verbose_standard (" 	 Case 3 " );
				if List.mem clock_index clock_updates = true
				then
					(
					let _ = print_message Verbose_standard (" 	 Detected " 
													^ (model.variable_names clock_index) 
													^ " was a reset clock!\n 	 skipping the process: (" 
													^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
					in
					true
					)
				else
					(
					false;
					);

		|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
																									false; 

		|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 4 " );
																									true;

		|_							, _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 5 " );
				let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
				let constr = make_CUB_constraint [ineq] in
				
				if LinearConstraint.p_is_true constr
				then true
				else
					(
					if LinearConstraint.p_is_false constr
					then false
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
						false;
						);
					);



		|_							, (LinearConstraint.Op_ge, _), _							->	print_message Verbose_standard (" 	 Case 5 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(
				let _ = print_message Verbose_standard (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
				in
				true
				)
			else
				(
				let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
				let constr = make_CUB_constraint [ineq] in
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
						false;
						);
					);
				);
																										

		| _							, _							 , _							-> 	print_message Verbose_standard (" 	 Case 6 " );
			(*reset*)
			if List.mem clock_index clock_updates = true
			then
				(
				print_message Verbose_standard (" 	 Detected " 
												^ (model.variable_names clock_index) 
												^ " was a reset clock!\n 	 skipping the process: (" 
												^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
				
				let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
				let constr = make_CUB_constraint [ineq] in
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
						false;
						);
					);
				)
			else
				(
				let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in

				let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in

				let constr = make_CUB_constraint [ineq1;ineq2] in
				if LinearConstraint.p_is_true constr
				then 
					true
					else
					(
					if LinearConstraint.p_is_false constr
					then 
						let clock_linear_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
						false
					else
						(
						inequalities_need_to_solve := !inequalities_need_to_solve@[ineq1;ineq2];
						false;
						);
					);
				);


		in

		if (result = false && !inequalities_need_to_solve = [])
		then 	(
				isCUB_PTA := false;
				(*comment this line below for the CUB-PTA transformation*)
				(* raise (InternalError("   The model is impossible CUB-PTA! ")); *) 
				);
		
		inequalities := !inequalities@(!inequalities_need_to_solve);

		if (result = false)
		then

			print_message Verbose_standard (" This is not satisfied CUB-PTA! ")
		else 
			print_message Verbose_standard (" This is satisfied CUB-PTA! ");



	print_message Verbose_standard ("\n");
	) model.clocks; (* end List.iter *)

	print_message Verbose_standard ("\n");
	print_message Verbose_standard (" CUB check, End!");
	print_message Verbose_standard ("\n");
	(!isCUB_PTA, !inequalities)



let tuple2pxd_constraint (clock_index, op, linear_term) = 
	let clock_term = LinearConstraint.make_p_linear_term [NumConst.one, clock_index] NumConst.zero in
	let linear_inequality = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term) op in
	let constr = LinearConstraint.make_p_constraint [linear_inequality] in
	let constr = LinearConstraint.pxd_of_p_constraint constr in
	constr
	

let create_x_ge_zero clock_index = (clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)


let get_all_clocks_ge_zero_comstraint model = 
	let ls = ref [] in
	List.iter (fun clock_index ->
		ls := !ls@[tuple2pxd_constraint (create_x_ge_zero clock_index)];
	) model.clocks;
	let cons = LinearConstraint.pxd_intersection !ls in
	cons



	



(* print_message Verbose_standard ("\n const: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names const) );

print_message Verbose_standard (" Inequality s0 <= t: \n" 
								^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");

print_message Verbose_standard (" Inequality s0 <= s1: \n" 
								^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
(* ^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ); *) *)



(*

let isCUB_PTA = ref true in
let inequalities_need_to_solve = ref [] in

(*main function for CUB-PTA*)
List.iter (fun automaton_index -> print_message Verbose_standard ("Automaton: " ^ (model.automata_names automaton_index) );

		(*Checking bounded clocked in invariant (Location)*)
        List.iter (fun location_index -> print_message Verbose_standard ("----------------Begin checking at " ^ (model.location_names automaton_index location_index) ^ "-------------------");

        		print_message Verbose_standard ("\n");

        		print_message Verbose_standard (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ;

        		let invariant1 = model.invariants automaton_index location_index in
        
                print_message Verbose_standard ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ;
						

						print_message Verbose_standard ("\n");

                	(*Checking bounded clocked in guards (Transition)*)
                	List.iter (fun action_index -> print_message Verbose_standard (" Transition/Action: " ^ (model.action_names action_index) );
            
                    	List.iter (fun (guard, clock_updates, _, destination_location_index) 
                    		-> print_message Verbose_standard ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));		

                        	(** WORK HERE **)

                        	let invariant2 = model.invariants automaton_index destination_location_index in

							print_message Verbose_standard ("\n");
                			print_message Verbose_standard (" State/Location(D): " ^ (model.location_names automaton_index destination_location_index) ) ;
                			print_message Verbose_standard ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
                			print_message Verbose_standard ("	  ----Map:(" 
                											^ (model.location_names automaton_index location_index) 
                											^ ")--" ^ (model.action_names action_index) ^ "-->(" 
                											^ (model.location_names automaton_index destination_location_index) 
                											^ ") ----" );
                			print_message Verbose_standard ("\n");

                        	let clock_updates = match clock_updates with
                        						  No_update -> []
												| Resets clock_update -> clock_update
												| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); in


                			let (result, inequalities) = cub_check_2 model invariant1 guard invariant2 clock_updates in
                			
                			inequalities_need_to_solve := !inequalities_need_to_solve@inequalities;
                			if result = false
							then
    							isCUB_PTA := false;
                			()

                    	) (model.transitions automaton_index location_index action_index); 

                	) (model.actions_per_location automaton_index location_index); 

            		 print_message Verbose_standard ("----------------End checking " ^ (model.location_names automaton_index location_index) ^ "---------------------");

            		 print_message Verbose_standard ("\n");

        ) (model.locations_per_automaton automaton_index);

        print_message Verbose_standard ("\n");

) model.automata;



if (!isCUB_PTA && !inequalities_need_to_solve = []) = true 
then
    print_message Verbose_standard ("   The model is CUB-PTA! ")
else 
	(
	let constraint_for_cub = LinearConstraint.make_p_constraint (*p_linear_inequality_list*) !inequalities_need_to_solve in 

   	print_message Verbose_standard ("   The model is possible CUB-PTA! \nbut you need to solve the inequalities below!!: ");
   	print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constraint_for_cub)); 
    );

terminate_program();

*)

(**************************************************)
(* PART 1 END *)
(**************************************************)




(** Takes an abstract model as input, and infers a constraint on the parameters (possibly false) under which this PTA is a CUB-PTA *)
let check_cub abstract_model =
	raise (InternalError "not implemented")



(************************************************************)
(************************************************************)
(* Main function for CUB transforming *)
(************************************************************)
(************************************************************)

(** Takes an abstract model as input, and convert it into an equivalent CUB-PTA *)
let cubpta_of_pta model : AbstractModel.abstract_model =

	(* [CUB-PTA TRANSFORMATION] THIS FUNCTION USED FOR REMOVING PROBLEMATIC TRANSITIONS *)
	let check_problematic_transition (invariant_s0, guard_t, invariant_s1, clock_updates, parameters_constraints) = 	
		(* print_message Verbose_standard ("\nCHECKING FOR REMOVING PROBLEMATIC TRANSITIONS!" ); *)
		let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
		let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
		let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
		let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
		let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
		let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in
		let isCUB = ref true in
		let inequalities_need_to_solve = ref [] in
		List.iter (	fun clock_index -> 
			let ls_tup_ineq_s0 	= (filter_upperbound_by_clock_2 clock_index tuple_inequalities_s0) in
			let ls_tup_ineq_t 	= (filter_upperbound_by_clock_2 clock_index tuple_inequalities_t) in
			let ls_tup_ineq_s1 	= (filter_upperbound_by_clock_2 clock_index tuple_inequalities_s1) in
			List.iter (fun (clock_index_s0, op_s0, linear_term_s0) -> 
				List.iter (fun (clock_index_t, op_t, linear_term_t) -> 
					List.iter (fun (clock_index_s1, op_s1, linear_term_s1) -> 
						match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
						|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
							(*Case 1*)
					 		(* print_message Verbose_standard ("\nCASE 1 " );
					 		print_message Verbose_standard ("SATISFIED CUB! " ); *)
					 		();
					 		(*Case 1 - end*)
					 	|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
							(*Case 2*)
							(* print_message Verbose_standard ("\nCASE 2 " );
							print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
							(* none reset zone *)
							isCUB := false;
							(* none reset zone - end *)
							(*Case 2 - end*)
						|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
							(*Case 3*)
							(*reset*)
							(* print_message Verbose_standard ("\nCASE 3 " ); *)
							if List.mem clock_index clock_updates = true
							then
								(
								(* reset zone *)
								(* print_message Verbose_standard ("DETECTED RESET CLOCK! " );
								print_message Verbose_standard ("SATISFIED CUB! " ); *)
								();
								(* reset zone - end *)
								)
							else
								(
								(* none reset zone *)
								(* print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
								isCUB := false;
								(* none reset zone - end *)
								);
							(*Case 3 - end*)
						|(LinearConstraint.Op_ge, _), _							 , _							->
							(*Case 4*)
							(*reset but useless*)
							(* print_message Verbose_standard ("\nCASE 4 " );
							print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
							isCUB := false;
							(*Case 4 - end*)
						|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
							(*Case 5*)
							(* print_message Verbose_standard ("\nCASE 5 " );
							print_message Verbose_standard ("SATISFIED CUB! " ); *)
							();
							(*Case 5 - end*)
						|_							, _							 , (LinearConstraint.Op_ge, _)	->	
							(*Case 6*)
							(* none reset zone *)
							(* print_message Verbose_standard ("\nCASE 6 " ); *)
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							(* print_message Verbose_standard ("FORMING INEQUALITY: " 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
															^ "!!!\n"); *)
							let constr = make_CUB_constraint [ineq] in
				
							if LinearConstraint.p_is_true constr
							then
								(
								(* print_message Verbose_standard ("TRUE, COMPARABLE! "); 
								print_message Verbose_standard ("SATISFIED CUB! " ); *)
								();
								)
							else
								(
								if LinearConstraint.p_is_false constr
								then 
									(
									(* print_message Verbose_standard ("FALSE, COMPARABLE! ");
									print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
									isCUB := false;
									)
								else
									(
									(* print_message Verbose_standard ("NOT DETERMINED! "); *)
									if isContraintConflictsParametersConstraints constr parameters_constraints
									then
										(
										(* print_message Verbose_standard ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
										print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
										isCUB := false;
										)
									else
										(
										(* print_message Verbose_standard ("NOT CONFLICTED! " );
										print_message Verbose_standard ("SATISFIED CUB! " ); *)
										);
									);
								);
							(* none reset zone - end*)
							(*Case 6 - end*)
						|_							, (LinearConstraint.Op_ge, _), _							->	
							(*Case 7*)
							(* print_message Verbose_standard ("\nCASE 7 " ); *)
							(*reset*)
							if List.mem clock_index clock_updates = true
							then
								(
								(* reset zone *)
								(* print_message Verbose_standard ("DETECTED RESET CLOCK! " );
								print_message Verbose_standard ("SATISFIED CUB! " ); *)
								();
								(* reset zone - end*)
								)
							else
								(
								(* none reset zone *)
								let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
								(* print_message Verbose_standard ("FORMING INEQUALITY: " 
																^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
																^ "!!!\n"); *)
								let constr = make_CUB_constraint [ineq] in
								if LinearConstraint.p_is_true constr
								then 
									(
									(* print_message Verbose_standard ("TRUE, COMPARABLE! ");
									print_message Verbose_standard ("SATISFIED CUB! " ); *)
									();
									)
								else
									(
									if LinearConstraint.p_is_false constr
									then 
										(
										(* print_message Verbose_standard ("FALSE, COMPARABLE! ");
										print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
										isCUB := false;

										)
									else
										(
										(* print_message Verbose_standard ("FALSE, NOT DETERMINED! "); *)
										if isContraintConflictsParametersConstraints constr parameters_constraints
										then
											(
											(* print_message Verbose_standard ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
											print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
											isCUB := false;
											)
										else
											(
											(* print_message Verbose_standard ("NOT CONFLICTED! " );
											print_message Verbose_standard ("SATISFIED CUB! " ); *)
											);
										);
									);
								(* none reset zone *)
								);
							(*Case 7 - end*)																						
					| _							, _							 , _							-> 	
						(*Case 8*)
						(* print_message Verbose_standard ("\nCASE 8 " ); *)
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							(* print_message Verbose_standard ("DETECTED RESET CLOCK! " ); *)
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							(* print_message Verbose_standard ("FORMING INEQUALITY: " 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
															^ "!!!\n"); *)
							let constr = make_CUB_constraint [ineq] in
							if LinearConstraint.p_is_true constr
							then 
								(
								(* print_message Verbose_standard ("TRUE, COMPARABLE! ");
								print_message Verbose_standard ("SATISFIED CUB! " ); *)
								();
								)
							else
								(

								if LinearConstraint.p_is_false constr
								then 
									(
									(* print_message Verbose_standard ("FALSE, COMPARABLE! ");
									print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
									isCUB := false;
									)
								else
									(
									(* print_message Verbose_standard ("FALSE, NOT DETERMINED "); *)
									if isContraintConflictsParametersConstraints constr parameters_constraints
									then
										(
										(* print_message Verbose_standard ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
										print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
										isCUB := false;
										)
									else
										(
										(* print_message Verbose_standard ("NOT CONFLICTED! " );
										print_message Verbose_standard ("SATISFIED CUB! " ); *)
										);
									);
								);
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)
							let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							(* print_message Verbose_standard ("INEQUALITY S0 =< T: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 
															^ "!!!\n"); *)
							let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							(* print_message Verbose_standard ("INEQUALITY S0 =< S1: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 
															^ "!!!\n"); *)
							let constr = make_CUB_constraint [ineq1;ineq2] in

							if LinearConstraint.p_is_true constr
							then 
								(
								(* print_message Verbose_standard ("TRUE, COMPARABLE! ");
								print_message Verbose_standard ("SATISFIED CUB! " ); *)
								();
								)
							else
								(
								if LinearConstraint.p_is_false constr
								then 
									(
									(* print_message Verbose_standard ("FALSE, COMPARABLE! ");
									print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
									isCUB := false;

									)
								else
									(
									(* print_message Verbose_standard ("FALSE, NOT DETERMINED! "); *)
									if isContraintConflictsParametersConstraints constr parameters_constraints
									then
										(
										(* print_message Verbose_standard ("ONE OF PARAMETER RELATIONS CONFLICTED! " );
										print_message Verbose_standard ("NOT SATISFIED CUB! " ); *)
										isCUB := false;
										)
									else
										(
										(* print_message Verbose_standard ("NOT CONFLICTED! " );
										print_message Verbose_standard ("SATISFIED CUB! " ); *)
										);
									);
								);
							(* none reset zone - end*)
							);
						(*Case 8 - end*)
					) ls_tup_ineq_s1;
				) ls_tup_ineq_t;
			) ls_tup_ineq_s0;
		) model.clocks; 
		(* print_message Verbose_standard ("CHECKING FOR REMOVING PROBLEMATIC TRANSITIONS - END!!" ); *)
		!isCUB;
		in




	(*Array of models*)
	let submodels = DynArray.make 0 in

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
	if List.length model.automata > 1 then(
		raise (InternalError(" Sorry, we only support for single model currently. Multi-clocks and multi-constraint will be implemented in next version! "))
	);


	(*covert input model into specific data stucture*)
	List.iter (fun automaton_index -> 
		(* print_message Verbose_standard ("Converting automaton: " 
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

		init_loc := (model.location_names automaton_index (List.hd (model.locations_per_automaton automaton_index)));


		(*Checking bounded clocked in invariant (Location)*)
		List.iter (fun location_index -> 
			(* print_message Verbose_standard ("----------------Begin at " ^ (model.location_names automaton_index location_index) ^ "-------------------");
			print_message Verbose_standard ("\n");
			print_message Verbose_standard (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ; *)
			let invariant1 = model.invariants automaton_index location_index in
			
			(*add locations*)
			let location_index_string = (model.location_names automaton_index location_index) in
			Hashtbl.add locations_ini location_index_string invariant1;

			(* print_message Verbose_standard ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ;
			print_message Verbose_standard ("\n"); *)
				(*Checking bounded clocked in guards (Transition)*)
				List.iter (fun action_index -> print_message Verbose_standard (" Transition/Action: " ^ (model.action_names action_index) );
		
					List.iter (fun (guard, clock_updates, _, destination_location_index) 
						-> (* print_message Verbose_standard ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)); *)		
						(** WORK HERE **)
						let invariant2 = model.invariants automaton_index destination_location_index in
						(* print_message Verbose_standard ("\n");
						print_message Verbose_standard (" State/Location(D): " ^ (model.location_names automaton_index destination_location_index) ) ;
						print_message Verbose_standard ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
						print_message Verbose_standard ("	  ----Map:(" 
														^ (model.location_names automaton_index location_index) 
														^ ")--" ^ (model.action_names action_index) ^ "-->(" 
														^ (model.location_names automaton_index destination_location_index) 
														^ ") ----" );
						print_message Verbose_standard ("\n"); *)
						let clock_updates = match clock_updates with
											No_update -> []
											| Resets clock_update -> clock_update
											| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); 
						in

						(*add transitions*)
						DynArray.add transitions_ini ((model.location_names automaton_index location_index), (model.location_names automaton_index destination_location_index), guard, clock_updates);
						
						()
					) (model.transitions automaton_index location_index action_index); 
				) (model.actions_per_location automaton_index location_index); 
				(* print_message Verbose_standard ("----------------End converting " ^ (model.location_names automaton_index location_index) ^ "!!!---------------------");
				print_message Verbose_standard ("\n"); *)
		) (model.locations_per_automaton automaton_index);
		(* print_message Verbose_standard ("\n"); *)
	(* ) model.automata; *)
	(*covert input model into specific data stucture - end*)






	(*Adding the first sub-model*)
	DynArray.add submodels (locations_ini, transitions_ini, clocks_constraints_ini, parameters_constraints_ini);
	let submodels_length = DynArray.length submodels in
	print_message Verbose_standard ("\n Check lenth of submodels: " ^ (string_of_int submodels_length) );




	(*loop through each submodel*)
	let count_m = ref 1 in
	while (!count_m) <= (DynArray.length submodels) do
		
		let submodel = DynArray.get submodels (!count_m - 1) in
		print_message Verbose_standard ("\n Sub-model no: " ^ (string_of_int !count_m) );
		let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in
		print_message Verbose_standard ("\n Number of locations: " ^ (string_of_int (Hashtbl.length locations)) );
		print_message Verbose_standard ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );
		
		print_message Verbose_standard ("\n ----------------Sub-model No: " ^ (string_of_int !count_m) ^ "---------------------------");
		let adding = ref true in


		let loc_clocks_constraints = DynArray.make 0 in

		let count_loop = ref 1 in

		while !adding = true do
		(
		adding := false;

		if !count_loop = 1 
		then
		(

		(* let submodel = DynArray.get submodels (!count_m - 1) in *)
		

		(* let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in *)
		print_message Verbose_standard ("\n Number of locations: " ^ (string_of_int (Hashtbl.length locations)) );
		print_message Verbose_standard ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );

		(* let (_, transitions, _, _) = submodel in *)
		(*stage 1*)
		let count_t = ref 1 in
		DynArray.iter ( fun transition -> 
			(* let (locations, transitions, clocks_constraints, parameters_constraints) = submodel in *)
			print_message Verbose_standard ("\n Transition No: " ^ (string_of_int !count_t) );
			let (location_index, destination_location_index, guard, clock_updates) = transition in
			(*work here*)
			let invariant_s0 = Hashtbl.find locations location_index in
			let guard_t = guard in
			let invariant_s1 = Hashtbl.find locations destination_location_index in
			(*ppl*)
			(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
			let inequalities = ref [] in
			print_message Verbose_standard (" CUB transformation, Start:");
			print_message Verbose_standard ("\n");
			(*transform constraints into inequality lists*)
			let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
			let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
			let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
			(*transform inequality list into tuple inequality list*)
			(* print_message Verbose_standard (" **Beginning state/location** :"); *)
			let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
			(* print_message Verbose_standard (" **Transition** :"); *)
			let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
			(* print_message Verbose_standard (" **Destination state/location** :"); *)
			let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in
			(*single clock bounded*)
			(* if List.length model.clocks > 1
			then raise (InternalError(" Sorry, we only support for single clock currently. Multi-clocks and multi-constraint will be implemented in next version! ")); *)
			(* let result = ref true in *)
			print_message Verbose_standard ("\n --------------------1st check start---------------------- ");
			List.iter (	fun clock_index -> 
			 	let inequalities_need_to_solve = ref [] in
			 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):"); 	
			 	(*get each element of tuple of each clock - NOTE: the input musts contain 1 upper-bounded*)
			 	(* print_message Verbose_standard ("\n 	**Beginning state/location** :"); *)
				let (clock_index_s0 , op_s0, linear_term_s0) 	= List.hd (filter_upperbound_by_clock_2 clock_index tuple_inequalities_s0) in
				(* print_message Verbose_standard ("\n 	**Transition** :"); *)
				let (clock_index_t, op_t, linear_term_t) 	= List.hd (filter_upperbound_by_clock_2 clock_index tuple_inequalities_t) in
				(* print_message Verbose_standard ("\n 	**Destination state/location** :"); *)
				let (clock_index_s1, op_s1, linear_term_s1) 	= List.hd (filter_upperbound_by_clock_2 clock_index tuple_inequalities_s1) in

				(*convert back to constraint for each inequality*)
				(* print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term));  *)
				let constraint_s0 = tuple2pxd_constraint (clock_index_s0 , op_s0, linear_term_s0) in
				(* print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0));  *)
				let constraint_t = tuple2pxd_constraint (clock_index_t, op_t, linear_term_t) in
				(* print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t)); *)
				let constraint_s1 = tuple2pxd_constraint (clock_index_s1, op_s1, linear_term_s1) in
				(* print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));   *)

				print_message Verbose_standard ("\n");
				print_message Verbose_standard ("Comparing: ");
				(*just for printing*)
				let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
				let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
				let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
				print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
				print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
				print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
				print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
				(*just for printing - end*)
				
				(*check 1*)
				let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
					 	(*Case 1*)
					 	print_message Verbose_standard (" 	 Case 1 " );
					 	true;
					 	(*Case 1 - end*)
					|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 2*)
						print_message Verbose_standard (" 	 Case 2 " );
						(* none reset zone *)
						let clock_cons = (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
						let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, clock_cons);
							);
						false;
						(* none reset zone - end *)
						(*Case 2 - end*)
					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
						(*Case 3*)
						(*reset*)
						print_message Verbose_standard (" 	 Case 3 " );
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							let _ = print_message Verbose_standard (" 	 Detected " 
																	^ (model.variable_names clock_index) 
																	^ " was a reset clock!\n 	 skipping the process: (" 
																	^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str 
																	^ ")!" ) 
							in
							true
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)

							let clock_cons = (LinearConstraint.pxd_intersection [constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if check2 = false 
							then
								(
								DynArray.add clocks_constraints (location_index, clock_cons);
								);
							false;

							(* none reset zone - end *)
							);
						(*Case 3 - end*)
					|(LinearConstraint.Op_ge, _), _							 , _							->
						(*Case 4*)
						(*reset but useless*)
						print_message Verbose_standard (" 	 Case 4 " );
						(*reset*)
						let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
						(* reset zone *)
						if (List.mem clock_index clock_updates) = true
						then
							(
							clock_cons := (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]);
							);
						(* reset zone - end*)

						let check2 = isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, !clock_cons);
							);
						false; 
						(*Case 4 - end*)
					|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
						(*Case 5*)
						print_message Verbose_standard (" 	 Case 5 " );
						true;
						(*Case 5 - end*)
					|_							, _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 6*)
						(* none reset zone *)
						print_message Verbose_standard (" 	 Case 6 " );
						let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						print_message Verbose_standard (" Forming inequality: " 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
														^ "!!!\n");
						let constr = make_CUB_constraint [ineq] in
			
						if LinearConstraint.p_is_true constr
						then
							(
							print_message Verbose_standard (" true, comparable "); 
							true
							)
						else
							(
							let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if LinearConstraint.p_is_false constr
							then 
								(
								print_message Verbose_standard (" false, comparable ");

								if check2 = false 
								then
									(
									DynArray.add clocks_constraints (location_index, clock_cons);
									);
								false

								)
							else
								(
								print_message Verbose_standard (" false, not determined ");
								(*submodel info*)
								getInfoCurrentModel model submodel;
								let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
								let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
								print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
																^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

								if check1 = true
								then
									(
									if check2 = false
									then
										(
										print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
										print_message Verbose_standard (" Adding new clocks constraints" );
										DynArray.add clocks_constraints (location_index, clock_cons);
										(* print_message Verbose_standard (" Added constraints: " 
																		^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																		^ "\n at state: " 
																		^ string_of_int location_index ); *)
										);
									)
								else
									(
									if check3 = false
									then
										(
										print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
										(*Add First parameter constraints in the current submodel*)
										let new_parameters_constraints = DynArray.copy parameters_constraints in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																		^ " into submodel ((parameters_constraints)) " 
																		^ string_of_int (!count_m)  );
										DynArray.add parameters_constraints (true, [constr]);
										(*Create new submodel with Second constraint*)
										let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
										let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
										let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										DynArray.add new_parameters_constraints (false, [constr1]);
										if is_parameters_constraints_false new_parameters_constraints
										then 
											(
											print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
								false;
								);
							);
						(* none reset zone - end*)
						(*Case 6 - end*)
					|_							, (LinearConstraint.Op_ge, _), _							->	
						(*Case 7*)
						print_message Verbose_standard (" 	 Case 7 " );
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							let _ = print_message Verbose_standard (" 	 Detected " 
															^ (model.variable_names clock_index) 
															^ " was a reset clock!\n 	 skipping the process: (" 
															^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
							in
							true
							(* reset zone - end*)
							)
						else
							(
							(* none reset zone *)
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
							let constr = make_CUB_constraint [ineq] in
							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								true
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
										(
										DynArray.add clocks_constraints (location_index, clock_cons);
										);
									false

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");
									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											);
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);

											(*Create new submodel with Second constraint*)
											let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
											let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
											let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
											print_message Verbose_standard (" Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr2]);

											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									false;
									);
								);
							(* none reset zone *)
							);
						(*Case 7 - end*)																						
					| _							, _							 , _							-> 	
						(*Case 8*)
						print_message Verbose_standard (" 	 Case 8 " );
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							print_message Verbose_standard (" 	 Detected " 
															^ (model.variable_names clock_index) 
															^ " was a reset clock!\n 	 skipping the process: (" 
															^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
							
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							print_message Verbose_standard (" Forming inequality: " 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
							let constr = make_CUB_constraint [ineq] in

							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								true
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
									DynArray.add clocks_constraints (location_index, clock_cons);
									false

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");

									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											);
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);
											(*Create new submodel with Second constraint*)
											let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
											let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
											let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr1]);
											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									false;
									);
								);
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)
							let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							print_message Verbose_standard (" Inequality s0 <= t: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");
							let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							print_message Verbose_standard (" Inequality s0 <= s1: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
							let constr = make_CUB_constraint [ineq1;ineq2] in

							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								true
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
										(
										DynArray.add clocks_constraints (location_index, clock_cons);
										);
									false

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");
									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											); 
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);
											(*Create new submodel with Second constraint*)
											let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
											let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
											let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
											let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
											let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
											let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											print_message Verbose_standard (" Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr1;constr2]);
											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									false;
									);
								);
							(* none reset zone - end*)
							);
						(*Case 8 - end*)
				in
				(*check 1 - end*)
				print_message Verbose_standard ("\n --------------------1st check end----------------------- ");

			print_message Verbose_standard ("\n");
			) model.clocks; 

			DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));

			(* let loc_clocks_constraints = DynArray.make 0 in *)
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
				 		| true,  false -> (* con := LinearConstraint.pxd_intersection [!con; cons2]  *) ()
				 		| false, true  -> con := LinearConstraint.pxd_intersection [!con; cons1; (get_all_clocks_ge_zero_comstraint model)];
				 						  let check = isConstraintContainedInClocksConstraints loc_index1 !con loc_clocks_constraints in
				 						  if LinearConstraint.pxd_is_true !con = false && check = false
				 						  then
				 						  	(
				 						  	(* add true constraint *)
				 						  	DynArray.add loc_clocks_constraints (loc_index1, !con);
				 						  	adding := true;
				 						  	);
				 						  DynArray.add loc_clocks_constraints (loc_index1, cons2);
				 						  con := (LinearConstraint.pxd_true_constraint ())
				 		| false, false -> con := LinearConstraint.pxd_intersection [!con; cons1; cons2];
				 	)
				else
				 	(
				 		(*  *)
				 	);

			done;
			DynArray.clear clocks_constraints; 
			DynArray.append (DynArray.copy loc_clocks_constraints) clocks_constraints;  

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

			print_message Verbose_standard ("\n Transition No: " ^ (string_of_int !count_t) );
			let (location_index, destination_location_index, guard, clock_updates) = transition in

			let invariant_s0 = Hashtbl.find locations location_index in
			let guard_t = guard in

			print_message Verbose_standard (" CUB transformation, Start:");
			print_message Verbose_standard ("\n");
			(*transform constraints into inequality lists*)
			let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
			let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in

			(*transform inequality list into tuple inequality list*)
			let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
			let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in

			print_message Verbose_standard ("\n --------------------2nd check start---------------------- ");

			let constraints_s1 = find_all_clocks_constraints clocks_constraints destination_location_index in

			List.iter (fun c_s1 ->
			if LinearConstraint.pxd_is_true c_s1 = false
			then
			(

			print_message Verbose_standard ("\n Founded constraint: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c_s1));

			(*transform constraints into inequality lists*)
			let inequalities_s1 = LinearConstraint.pxd_get_inequalities c_s1 in
			(*transform inequality list into tuple inequality list*)
			let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in


			List.iter (	fun clock_index -> 
			 	let inequalities_need_to_solve = ref [] in
			 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");
			
			 	(* print_message Verbose_standard ("\n 	**Beginning state/location** :"); *)
				let (clock_index_s0 , op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
				let (clock_index_t, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
				let list_s1_filtered = ref (filter_upperbound_by_clock_2 clock_index tuple_inequalities_s1) in
				if !list_s1_filtered = [] 
				then list_s1_filtered := !list_s1_filtered@[(clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)]; 

				(*convert back to constraint for each inequality*)
				(* print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term));  *)
				let constraint_s0 = tuple2pxd_constraint (clock_index_s0 , op_s0, linear_term_s0) in
				(* print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0));  *)
				let constraint_t = tuple2pxd_constraint (clock_index_t, op_t, linear_term_t) in
				(* print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t)); *)
				

				print_message Verbose_standard ("\n");
				print_message Verbose_standard ("Comparing: ");
				(*just for printing*)
				let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
				let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
				print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
				print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
				List.iter (	fun (clock_index_s1, op_s1, linear_term_s1) -> 
				let constraint_s1 = tuple2pxd_constraint (clock_index_s1, op_s1, linear_term_s1) in
				(* print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));   *)
				let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " 
										^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
				print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
				print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
				(*just for printing - end*)

				let _ =
				( 
					match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	-> 
						(*Case 1*)
					 	print_message Verbose_standard (" 	 Case 1 (2) " );
					 	();
					 	(*Case 1 - end*)

					|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 2*)
						print_message Verbose_standard (" 	 Case 2 (2) " );
						(* none reset zone *)
						let clock_cons = (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
						let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, clock_cons);
							);
						(* none reset zone - end *)
						(*Case 2 - end*)

					|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
						(*Case 3*)
						(*reset*)
						print_message Verbose_standard (" 	 Case 3 (2) " );
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							let _ = print_message Verbose_standard (" 	 Detected " 
																	^ (model.variable_names clock_index) 
																	^ " was a reset clock!\n 	 skipping the process: (" 
																	^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str 
																	^ ")!" ) 
							in
							();
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)

							let clock_cons = (LinearConstraint.pxd_intersection [constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if check2 = false 
							then
								(
								DynArray.add clocks_constraints (location_index, clock_cons);
								);
							();

							(* none reset zone - end *)
							);
						(*Case 3 - end*)

					|(LinearConstraint.Op_ge, _), _							 , _							->
						(*Case 4*)
						(*reset but useless*)
						print_message Verbose_standard (" 	 Case 4 (2) " );
						(*reset*)
						let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
						(* reset zone *)
						if (List.mem clock_index clock_updates) = true
						then
							(
							clock_cons := (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]);
							);
						(* reset zone - end*)

						let check2 = isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, !clock_cons);
							);
						(); 
						(*Case 4 - end*)

					|(LinearConstraint.Op_ge, _), _		, _						->
						(*Case *)
						print_message Verbose_standard (" 	 Case  (2) " );
						print_message Verbose_standard (" false, comparable ");
						let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1; get_all_clocks_ge_zero_comstraint model]) in
						if (List.mem clock_index clock_updates) = true
						then
							(
							clock_cons := (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]);
							);
						(* let check2 = (isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints) in *)
						(* if check2 = false 
						then *)
							(
							DynArray.add clocks_constraints (location_index, !clock_cons);
							print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names !clock_cons)  
																			^ "\n at state: " 
																			^ location_index );
							);
						(*Case  - end*)

					|_							, (LinearConstraint.Op_ge, _), _	->	
						(*Case 3*)
						print_message Verbose_standard (" 	 Case 3 (2) " );
						if (List.mem clock_index clock_updates) = false
						then
							(
							let ineq2 = ref [] in
							List.iter (fun  (_, op_s1, linear_term_s1) ->
								ineq2 := !ineq2@[(make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1))];
							) tuple_inequalities_s1;

							let cub_cons = make_CUB_constraint (!ineq2) in
							print_message Verbose_standard ("\n CUB constraint: " 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons));
							if LinearConstraint.p_is_true cub_cons
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false cub_cons
								then 
									(
									print_message Verbose_standard (" false, comparable ");
									if check2 = false 
									then
										(
										DynArray.add clocks_constraints (location_index, clock_cons);
										);
									)
								else
									(
									print_message Verbose_standard (" false, not determined ");

									(*get submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints cub_cons parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints cub_cons parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );

											(* test *)
											let new_parameters_constraints = DynArray.copy parameters_constraints in

											let linear_term_2 = ref [] in
											List.iter (fun  (_, op_s1, linear_term_s1) ->
												linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
											) tuple_inequalities_s1;

											let linear_inequality_2 = ref [] in
											List.iter (fun term ->
												linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
											) !linear_term_2;

											let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

											let conj =  (LinearConstraint.p_intersection [constr2]) in
											let check = isContraintAllConflictsParametersConstraints conj parameters_constraints in

											if not check
											then
												(
												DynArray.add parameters_constraints (false, [constr2]);
												DynArray.add clocks_constraints (location_index, clock_cons);
												);
											(* test - end *)

											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											);
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );

											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [cub_cons]);

											let linear_term_2 = ref [] in
											List.iter (fun  (_, op_s1, linear_term_s1) ->
												linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
											) tuple_inequalities_s1;

											let linear_inequality_2 = ref [] in
											List.iter (fun term ->
												linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
											) !linear_term_2;

											let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

											let checkConflict2 = isContraintConflictsParametersConstraints constr2 new_parameters_constraints in
											(
											match checkConflict2 with
											| true (* false *)  -> print_message Verbose_standard ("\n conflict!!");

											| false (* true *) -> print_message Verbose_standard ("\n not conflict!!");
																DynArray.add new_parameters_constraints (false, [constr2]);
																print_message Verbose_standard (" Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											);

											if (is_parameters_constraints_false new_parameters_constraints) || checkConflict2
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
												)
											else
												(
												(* let new_clocks_constraints = Hashtbl.create 0 in
												Hashtbl.add new_clocks_constraints location_index clock_cons;
												DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints); *)
												
												(* let new_clocks_constraints = DynArray.make 0 in *)
												let new_clocks_constraints = DynArray.copy clocks_constraints in
												(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
												DynArray.add submodels (Hashtbl.copy locations, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
												(* () *)
												(* raise (InternalError(" Sorry!!!!!! ")); *)
												);

											);
										);
									);
								);

							);
						(*Case 3 - end*)

					|(LinearConstraint.Op_ge, _), _							 , _							->
						(*Case 4*)
						(*reset but useless*)
						print_message Verbose_standard (" 	 Case 4 (2) " );
						(*reset*)
						let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
						(* reset zone *)
						if (List.mem clock_index clock_updates) = true
						then
							(
							clock_cons := (LinearConstraint.pxd_intersection [constraint_t; (get_all_clocks_ge_zero_comstraint model)]);
							);
						(* reset zone - end*)

						let check2 = isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, !clock_cons);
							);
						(); 
						(*Case 4 - end*)
					|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
						(*Case 5*)
						print_message Verbose_standard (" 	 Case 5 (2) " );
						();
						(*Case 5 - end*)
					|_							, _							 , (LinearConstraint.Op_ge, _)	->	
						(*Case 6*)
						(* none reset zone *)
						print_message Verbose_standard (" 	 Case 6 (2) " );
						let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						print_message Verbose_standard (" Forming inequality: " 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
														^ "!!!\n");
						let constr = make_CUB_constraint [ineq] in
			
						if LinearConstraint.p_is_true constr
						then
							(
							print_message Verbose_standard (" true, comparable "); 
							()
							)
						else
							(
							let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if LinearConstraint.p_is_false constr
							then 
								(
								print_message Verbose_standard (" false, comparable ");

								if check2 = false 
								then
									(
									DynArray.add clocks_constraints (location_index, clock_cons);
									);
								()

								)
							else
								(
								print_message Verbose_standard (" false, not determined ");
								(*submodel info*)
								getInfoCurrentModel model submodel;
								let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
								let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
								print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
																^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

								if check1 = true
								then
									(
									if check2 = false
									then
										(
										print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
										print_message Verbose_standard (" Adding new clocks constraints" );
										DynArray.add clocks_constraints (location_index, clock_cons);
										(* print_message Verbose_standard (" Added constraints: " 
																		^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																		^ "\n at state: " 
																		^ string_of_int location_index ); *)
										);
									)
								else
									(
									if check3 = false
									then
										(
										print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
										(*Add First parameter constraints in the current submodel*)
										let new_parameters_constraints = DynArray.copy parameters_constraints in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																		^ " into submodel ((parameters_constraints)) " 
																		^ string_of_int (!count_m)  );
										DynArray.add parameters_constraints (true, [constr]);
										(*Create new submodel with Second constraint*)
										let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
										let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
										let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										DynArray.add new_parameters_constraints (false, [constr1]);
										if is_parameters_constraints_false new_parameters_constraints
										then 
											(
											print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
								();
								);
							);
						(* none reset zone - end*)
						(*Case 6 - end*)
					|_							, (LinearConstraint.Op_ge, _), _							->	
						(*Case 7*)
						print_message Verbose_standard (" 	 Case 7 (2) " );
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							let _ = print_message Verbose_standard (" 	 Detected " 
															^ (model.variable_names clock_index) 
															^ " was a reset clock!\n 	 skipping the process: (" 
															^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
							in
							()
							(* reset zone - end*)
							)
						else
							(
							(* none reset zone *)
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
							let constr = make_CUB_constraint [ineq] in
							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								()
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
										(
										DynArray.add clocks_constraints (location_index, clock_cons);
										);
									()

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");
									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											);
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);

											(*Create new submodel with Second constraint*)
											let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
											let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
											let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
											print_message Verbose_standard (" Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr2]);

											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									();
									);
								);
							(* none reset zone *)
							);
						(*Case 7 - end*)																						
					| _							, _							 , _							-> 	
						(*Case 8*)
						print_message Verbose_standard (" 	 Case 8 (2) " );
						(*reset*)
						if List.mem clock_index clock_updates = true
						then
							(
							(* reset zone *)
							print_message Verbose_standard (" 	 Detected " 
															^ (model.variable_names clock_index) 
															^ " was a reset clock!\n 	 skipping the process: (" 
															^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
							
							let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							print_message Verbose_standard (" Forming inequality: " 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
							let constr = make_CUB_constraint [ineq] in

							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								()
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
									DynArray.add clocks_constraints (location_index, clock_cons);
									()

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");

									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											);
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);
											(*Create new submodel with Second constraint*)
											let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
											let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
											let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr1]);
											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									();
									);
								);
							(* reset zone - end *)
							)
						else
							(
							(* none reset zone *)
							let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
							print_message Verbose_standard (" Inequality s0 <= t: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");
							let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
							print_message Verbose_standard (" Inequality s0 <= s1: \n" 
															^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
							let constr = make_CUB_constraint [ineq1;ineq2] in

							if LinearConstraint.p_is_true constr
							then 
								(
								print_message Verbose_standard (" true, comparable ");
								()
								)
							else
								(
								let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; constraint_s1; (get_all_clocks_ge_zero_comstraint model)]) in
								let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
								if LinearConstraint.p_is_false constr
								then 
									(
									print_message Verbose_standard (" false, comparable ");

									if check2 = false 
									then
										(
										DynArray.add clocks_constraints (location_index, clock_cons);
										);
									()

									)
								else
									(
									print_message Verbose_standard (" false, not determined ");
									(*submodel info*)
									getInfoCurrentModel model submodel;
									let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
									let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
									print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																	^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																	^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
									if check1 = true
									then
										(
										if check2 = false
										then
											(
											print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
											print_message Verbose_standard (" Adding new clocks constraints" );
											DynArray.add clocks_constraints (location_index, clock_cons);
											(* print_message Verbose_standard (" Added constraints: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																			^ "\n at state: " 
																			^ string_of_int location_index ); *)
											); 
										)
									else
										(
										if check3 = false
										then
											(
											print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
											(*Add First parameter constraints in the current submodel*)
											let new_parameters_constraints = DynArray.copy parameters_constraints in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																			^ " into submodel ((parameters_constraints)) " 
																			^ string_of_int (!count_m)  );
											DynArray.add parameters_constraints (true, [constr]);
											(*Create new submodel with Second constraint*)
											let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
											let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
											let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
											let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
											let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
											let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
											print_message Verbose_standard ("\n Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											print_message Verbose_standard (" Add constraint " 
																			^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																			^ " into submodel (parameters_constraints) " 
																			^ string_of_int ((DynArray.length submodels) +1) );
											DynArray.add new_parameters_constraints (false, [constr1;constr2]);
											if is_parameters_constraints_false new_parameters_constraints
											then 
												(
												print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
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
									();
									);
								);
							(* none reset zone - end*)
							);
						(*Case 8 - end*)

					);
					in ();

			print_message Verbose_standard ("\n");

			) !list_s1_filtered; 

			) model.clocks;  

			
			DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));
			);(* end if not true constraint *)

			
			) constraints_s1;

			(*check 2 - end*)
			print_message Verbose_standard ("\n --------------------2nd check end----------------------- ");

			DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));
			
			(* let loc_clocks_constraints = DynArray.make 0 in *)
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
				 		| true,  false -> (* con := LinearConstraint.pxd_intersection [!con; cons2]  *) ()
				 		| false, true  -> con := LinearConstraint.pxd_intersection [!con; cons1; get_all_clocks_ge_zero_comstraint model];
				 						  let check = isConstraintContainedInClocksConstraints loc_index1 !con loc_clocks_constraints in
				 						  if check = false
				 						  then
				 						  	(
				 						  	DynArray.add loc_clocks_constraints (loc_index1, !con);
				 						  	adding := true;
				 						  	);
				 						  DynArray.add loc_clocks_constraints (loc_index1, cons2);
				 						  con := (LinearConstraint.pxd_true_constraint ())
				 		| false, false -> con := LinearConstraint.pxd_intersection [!con; cons1; cons2];
				 	)
				else
				 	(
				 		(*  *)
				 	);
			done;
			DynArray.clear clocks_constraints; 
			DynArray.append (DynArray.copy loc_clocks_constraints) clocks_constraints; 

	 		(* if !adding = false
			then
				(
				DynArray.clear loc_clocks_constraints; 
				); *)	 
			
			(*work here - end*)
			(* transiton count *)
			count_t := !count_t+1;
			();
		) transitions;
		(*stage 1 - end*)

	

		);
		(* end - while adding loop *)
		done;

		(* model count *)
		print_message Verbose_standard ("\n ----------------Sub-model No: " ^ (string_of_int !count_m) ^ " end-----------------------");
		count_m := !count_m+1;
		();
	(* end - for each model *)
	done;





	(* Delete true constraints *)
	let loc_clocks_constraints = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, parameters_constraints) ->
	for i = 0 to (DynArray.length c_constraints - 1) do
		let (loc_index1, cons1) = DynArray.get c_constraints (i) in
		if (LinearConstraint.pxd_is_true cons1) = false
		then
			(
				DynArray.add loc_clocks_constraints (loc_index1, cons1);
			);
	done;
	DynArray.clear c_constraints;
	DynArray.append loc_clocks_constraints c_constraints;
	DynArray.clear loc_clocks_constraints;
	) submodels;
	(* end - delete true constraint *)





	(* [CUB-PTA TRANSFORMATION] STAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING PRINTING *)
	print_message Verbose_standard ("\nSTAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING ");
	print_message Verbose_standard ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints) ->

		print_message Verbose_standard ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_standard ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length locations) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_standard ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
		) locations;

		print_message Verbose_standard ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, destination_location_index, guard, clock_updates) ->
			print_message Verbose_standard ("\n" 
											^ source_location_index ^ " |-----> " ^ destination_location_index 
											^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)
											);
		) transitions;	

		print_message Verbose_standard ("\nPARAMETERS CONSTRAINTS: ");
		DynArray.iter ( fun (is_and, constraint_list) ->
			print_message Verbose_standard (" 	IS CONJUNCTION ?: " ^ string_of_bool is_and );
			List.iter (fun cons1 -> 
				print_message Verbose_standard (" 	FOLLOWED CONSTRAINTS: " 
										^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
			) constraint_list;
		) p_constraints;

		print_message Verbose_standard ("\nCLOCKS CONSTRAINTS: ");
		DynArray.iter (fun (l_index, con) -> 
			print_message Verbose_standard ("\n (WILL BE A NEW STATE): \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con) 
											^ "\n FROM LOCATION: " 
											^ l_index
											); 
		) c_constraints;

		print_message Verbose_standard ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );

		model_count := !model_count+1;

	) submodels;
	print_message Verbose_standard ("\nSTAGE 1 - PARAMETERS, LOCATION CONSTRAINT GERNERATING - END");
	
	



	(* [CUB-PTA TRANSFORMATION] STAGE 2 - ADDING LOCATIONS *)
	let newSubModels = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints) ->
		let count = ref 1 in 
		let index = Hashtbl.create 0 in

		DynArray.iter (fun (location, constr) ->
			let locName = ("cub-l" ^ (string_of_int !count)) in
			Hashtbl.add locations locName constr;
			Hashtbl.add index location locName ;
			count :=  !count + 1;
		) c_constraints;
		
		let init_locs = (Hashtbl.find_all index !init_loc) in
		DynArray.add newSubModels (locations, transitions, c_constraints, p_constraints, index, init_locs@[!init_loc]);
	) submodels;





	(* [CUB-PTA TRANSFORMATION] STAGE 2 - ADDING LOCATIONS PRINTING *)
	print_message Verbose_standard ("\nSTAGE 2 - ADDING LOCATIONS");
	print_message Verbose_standard ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_standard ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_standard ("\nCLOCKS CONSTRAINTS: ");
		DynArray.iter (fun (loc_index, constr) -> 
			print_message Verbose_standard ("\n (WILL BE A NEW STATE): \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr) 
											^ "\n FROM LOCATION: " 
											^ loc_index
											); 
		) c_constraints;

		print_message Verbose_standard ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length locations) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_standard ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
		) locations;

		print_message Verbose_standard ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_standard ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;

		print_message Verbose_standard ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_standard ("\nSTAGE 2 - ADDING LOCATIONS PRINTING - END ");





	(* [CUB-PTA TRANSFORMATION] STAGE 3 - ADDING TRANSITIONS *)
	DynArray.iter (fun (_, transitions, _, _, index, init_locs) ->

		DynArray.iter ( fun (source_location_index, destination_location_index, guard, clock_updates) ->
			let listCubLoc1 = Hashtbl.find_all index source_location_index in
			let listCubLoc2 = Hashtbl.find_all index destination_location_index in

			List.iter (fun loc1 ->  
				DynArray.add transitions (loc1, destination_location_index, guard, clock_updates);
			) listCubLoc1;

			List.iter (fun loc2 ->  
				DynArray.add transitions (source_location_index, loc2, guard, clock_updates);
			) listCubLoc2;

			List.iter (fun loc1 -> 
				List.iter (fun loc2 -> 
					DynArray.add transitions (loc1, loc2, guard, clock_updates);
				) listCubLoc2;
			) listCubLoc1;

		) transitions;
	) newSubModels;





	(* [CUB-PTA TRANSFORMATION] STAGE 3 - ADDING TRANSITIONS PRINTING *)
	print_message Verbose_standard ("\nSTAGE 3 - ADDING TRANSITIONS ");
	print_message Verbose_standard ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_standard ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_standard ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, destination_location_index, guard, clock_updates) ->
			print_message Verbose_standard ("\n" 
											^ source_location_index ^ " |-----> " ^ destination_location_index 
											(* ^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) *)
											);
		) transitions;	

		print_message Verbose_standard ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_standard ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;

		print_message Verbose_standard ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_standard ("\nSTAGE 3 - ADDING TRANSITIONS - END ");





	(* [CUB-PTA TRANSFORMATION] STAGE 4 - REMOVING PROBLEMATIC TRANSITIONS *)
	let new_transitions = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->
		for i = 1 to (DynArray.length transitions) do
			let (location_index, destination_location_index, guard, clock_updates) = DynArray.get transitions (i-1) in
			let s0_cons = Hashtbl.find locations location_index in
			let s1_cons = Hashtbl.find locations destination_location_index in
			(* print_message Verbose_standard ("\nINV LOC 1: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names s0_cons) 
									^ "\nGUARD T: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) 
									^ "\nINV LOC 2: "
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names s1_cons) 
									); *)
			let isCUB = check_problematic_transition (s0_cons, guard, s1_cons, clock_updates, p_constraints) in 
			if isCUB = true
			then
				(
					DynArray.add new_transitions (DynArray.get transitions (i-1)); 
					(* print_message Verbose_standard ("\nOK!! CUB "); *)
				)
			else
				( 
					(* print_message Verbose_standard ("\nCONFLICTED!! REMOVED! "); *)
				);
		done;
		DynArray.clear transitions;
		DynArray.append new_transitions transitions;
		DynArray.clear new_transitions;
	) newSubModels;





	(* [CUB-PTA TRANSFORMATION] STAGE 4 - REMOVE PROBLEMATIC TRANSITIONS PRINTING *)
	print_message Verbose_standard ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS ");
	print_message Verbose_standard ("\nNUMBER OF MODELS: " ^ (string_of_int (DynArray.length submodels) ) );
	let model_count = ref 1 in

	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->

		print_message Verbose_standard ("\n----------------SUB MODEL: "^ (string_of_int !model_count) ^"----------------" );

		print_message Verbose_standard ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length transitions) );
		DynArray.iter ( fun (source_location_index, destination_location_index, guard, clock_updates) ->
			print_message Verbose_standard ("\n" 
											^ source_location_index ^ " |-----> " ^ destination_location_index 
											(* ^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) *)
											);
		) transitions;	

		print_message Verbose_standard ("\n INDEX TABLE:" );
		Hashtbl.iter (fun from_loc cub_loc ->
			print_message Verbose_standard ("\n CUB LOCATION: " ^ cub_loc ^ " COMES FROM: " ^ from_loc );		
		) index;

		print_message Verbose_standard ("\n----------------SUB MODEL "^ (string_of_int !model_count) ^" - END!----------------" );
		model_count := !model_count+1;
	) newSubModels;
	print_message Verbose_standard ("\nSTAGE 4 - REMOVE PROBLEMATIC TRANSITIONS - END ");





	(* [CUB-PTA TRANSFORMATION] FINAL STAGE - MERGING SUB-MODELS *)
	let i = ref 1 in
	let s0 = "cub-init" in
	let newslocations =  Hashtbl.create 0 in
	Hashtbl.add newslocations s0 (LinearConstraint.pxd_true_constraint ());

	let newtransitions = DynArray.make 0 in
	DynArray.iter (fun (locations, transitions, c_constraints, p_constraints, index, init_locs) ->
		
		Hashtbl.iter (fun location_index cons -> 
			let newloc = (location_index ^ "-m" ^ (string_of_int !i) ) in
			Hashtbl.add newslocations newloc cons;
		) locations;

		DynArray.iter (fun (location_index, destination_location_index, guard, clock_updates) -> 
			let newloc1 = (location_index ^ "-m" ^ (string_of_int !i) ) in
			let newloc2 = (destination_location_index ^ "-m" ^ (string_of_int !i) ) in
			DynArray.add newtransitions (newloc1, newloc2, guard, clock_updates);
		) transitions;

		let listParaRelations = disjunction_constraints p_constraints in
		List.iter( fun cons ->
			let pxd_cons = LinearConstraint.pxd_of_p_constraint cons in
			if (LinearConstraint.pxd_is_false pxd_cons = false)
			then 
				(
				List.iter (fun loc -> 
					DynArray.add newtransitions (s0, (loc ^ "-m" ^ (string_of_int !i)), pxd_cons, [] ) ;
				) init_locs;
				);
		) listParaRelations;

		i := !i + 1;

	) newSubModels;

	let finalModel = (newslocations, newtransitions) in 
	(* additional stage - end *)





	(* [CUB-PTA TRANSFORMATION] FINAL STAGE - MERGING SUB-MODELS *)
	print_message Verbose_standard ("\nFINAL STAGE - MERGING SUB-MODELS ");

		print_message Verbose_standard ("\nNUMBER OF LOCATIONS:"^ string_of_int (Hashtbl.length newslocations) );
		Hashtbl.iter (fun location invariant ->
			print_message Verbose_standard ("\n LOCATION: "^ location 
											^ "\n INVARIANT: \n" 
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant) ); 		
		) newslocations;

		print_message Verbose_standard ("\nNUMBER OF TRANSITIONS :"^ string_of_int (DynArray.length newtransitions) );
		DynArray.iter ( fun (source_location_index, destination_location_index, guard, clock_updates) ->
			print_message Verbose_standard ("\n" 
											^ source_location_index ^ " |-----> " ^ destination_location_index 
											^ "\n GUARD: \n"
											^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)
											);
		) newtransitions;	
		(* print_message Verbose_standard ("\n Number of locations :"^ string_of_int (Hashtbl.length newstates) );
		Hashtbl.iter (fun location_index cons ->
			print_message Verbose_standard ("\n" ^ location_index );
		) newstates;	 *)
	print_message Verbose_standard ("\nFINAL STAGE - MERGING SUB-MODELS - END ");





(**************************************************)
(* GIA'S TESTING *)
(**************************************************)




(**************************************************)
(* PART 2 - CUB-PTA Transformation*)
(*
This is the beta version (simple), allow one clock, one constraint for each clock.
released version will be soon!!!!
*)
(**************************************************)
(*
let cub_check_3 model invariant_s0 guard_t invariant_s1 clock_updates = 	
	(*ppl*)
	(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
	let inequalities = ref [] in
	print_message Verbose_standard (" CUB check, Start:");
	print_message Verbose_standard ("\n");

	(*transform constraints into inequality lists*)
	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

	(*transform inequality list into tuple inequality list*)
	print_message Verbose_standard (" **Beginning state/location** :");
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
	print_message Verbose_standard (" **Transition** :");
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
	print_message Verbose_standard (" **Destination state/location** :");
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in


	let isCUB_PTA = ref true in

	(*check inequalities for each clock c on s0 -t-> s1*)
	List.iter (	fun clock_index -> 
	 	let inequalities_need_to_solve = ref [] in
	 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

	 	(*get each element of tuple of each clock - NOTE: the input musts contain 1 upper-bounded*)
	 	print_message Verbose_standard ("\n 	**Beginning state/location** :");
		let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
		print_message Verbose_standard ("\n 	**Transition** :");
		let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
		print_message Verbose_standard ("\n 	**Destination state/location** :");
		let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in

		(*convert back to constraint for each inequality*)
		let clock_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
		print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term)); 
		let linear_inequality_s0 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s0) op_s0 in
		let constraint_s0 = LinearConstraint.make_p_constraint [linear_inequality_s0] in
		let constraint_s0 = LinearConstraint.pxd_of_p_constraint constraint_s0 in
		print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0)); 
		let linear_inequality_t = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_t) op_t in
		let constraint_t = LinearConstraint.make_p_constraint [linear_inequality_t] in
		let constraint_t = LinearConstraint.pxd_of_p_constraint constraint_t in
		print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t));
		let linear_inequality_s1 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s1) op_s1 in
		let constraint_s1 = LinearConstraint.make_p_constraint [linear_inequality_s1] in
		let constraint_s1 = LinearConstraint.pxd_of_p_constraint constraint_s1 in
		print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));  


		print_message Verbose_standard ("\n");
		print_message Verbose_standard ("Comparing: ");

		(*just for printing*)
		let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
		let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
		let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
		print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
		print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
		(*just for printing*)

		(* if List.mem clock_index reset_clocks = true 
		then lower_inequality := linear_term_t; *)

		(* let result = ref true in *)
		let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
			 																							true;

			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
																										false;

			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
																										print_message Verbose_standard (" 	 Case 3 " );
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											let _ = print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
																											in
																											true
																											)
																										else
																											(
																											false;
																											);

			|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
																										false; 

			|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 4 " );
																										true;

			|_							, _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 5 " );
																										let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																										print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																										let constr = make_CUB_constraint [ineq] in
																										
																										if LinearConstraint.p_is_true constr
																										then true
																										else
																											(
																											if LinearConstraint.p_is_false constr
																											then 
																												(*inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];*)
																												false
																											else
																												(
																												inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
																												false;
																												);
																											);



			|_							, (LinearConstraint.Op_ge, _), _							->	print_message Verbose_standard (" 	 Case 5 " );
																										(*reset*)
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											let _ = print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
																											in
																											true
																											)
																										else
																											(
																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																											let constr = make_CUB_constraint [ineq] in
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
																													false;
																													);
																												);
																											);
																											

			| _							, _							 , _							-> 	print_message Verbose_standard (" 	 Case 6 " );
																										(*reset*)
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
																											
																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																											let constr = make_CUB_constraint [ineq] in
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
																													false;
																													);
																												);
																											)
																										else
																											(

																											
																											let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");
																											let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");

																											(*let constr = make_CUB_constraint [ineq1;ineq2] in*)
																											let constr = LinearConstraint.make_pxd_constraint (inequalities_s0@inequalities_t@inequalities_s1) in
																												print_message Verbose_standard ("\n constr:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr)); 
																											
																											(*testing ppl function*)
																											let constr_inter = LinearConstraint.pxd_intersection [constr] in
																												print_message Verbose_standard ("\n constr_inter:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr_inter)); 

																											let constr1 = LinearConstraint.make_p_constraint ([ineq1]) in
																												print_message Verbose_standard ("\n constr1:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1)); 

																											let constr2 = LinearConstraint.make_p_constraint ([ineq2]) in
																												print_message Verbose_standard ("\n constr2:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2)); 

																											let abc = LinearConstraint.pxd_is_leq invariant_s0 guard_t in
																												print_message Verbose_standard ("\n abc:" ^ string_of_bool abc ); 
																											(*testing ppl function*)
																											


																											
																											
																											if LinearConstraint.pxd_is_true constr
																											then 
																												(
																												print_message Verbose_standard (" true ");
																												true
																												)
																											else
																												(
																												if LinearConstraint.pxd_is_false constr
																												then 
																													(
																													print_message Verbose_standard (" false ");
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq2];
																													false
																													)
																												else
																													(
																													print_message Verbose_standard (" false, not determined ");
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq1;ineq2];
																													false;
																													);
																												);
																											

																											);

																										
																									

																										




			in
			

			inequalities := !inequalities@(!inequalities_need_to_solve);

			if (result = false)
			then
				(
			isCUB_PTA := false;
				print_message Verbose_standard (" This is not satisfied CUB-PTA! ");
				)
			else 
				print_message Verbose_standard (" This is satisfied CUB-PTA! ");



	print_message Verbose_standard ("\n");
	) model.clocks; 

	print_message Verbose_standard ("\n");
	print_message Verbose_standard (" CUB check, End!");
	print_message Verbose_standard ("\n");

	(!isCUB_PTA, !inequalities);
	in
*)





	) model.automata; (* end List.iter on automata *)
	
	
	(************************************************************)
	(** Return the abstract model *)
	(************************************************************)
	{
		(** General information **)
		(* Cardinality *)
		nb_automata = model.nb_automata;
		nb_actions = model.nb_actions;
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
			(*** TODO ***)
		locations_per_automaton = (*automaton_index -> location_index list*)raise (InternalError ("Not finished yet"));
		(* The location names for each automaton *)
			(*** TODO ***)
		location_names = (*automaton_index -> location_index -> location_name*)(fun automaton_index location_index -> "A_" ^ (string_of_int automaton_index) ^ ":l_" ^ (string_of_int location_index));
		(* The urgency for each location *)
			(*** TODO ***)
		is_urgent = (*automaton_index -> location_index -> bool*)raise (InternalError ("Not finished yet"));

		(* All action indexes *)
		actions = model.actions;
		(* Action names *)
		action_names = model.action_names;
		(* The type of actions *)
		action_types = model.action_types;
		(* The list of actions for each automaton *)
		actions_per_automaton = model.actions_per_automaton;
		(* The list of automatons for each action *)
		automata_per_action = model.automata_per_action;
		(* The list of actions for each automaton for each location *)
			(*** TODO ***)
		actions_per_location = model.actions_per_location;

		(* The cost for each automaton and each location *)
		(*** TODO ***)
		costs = model.costs;
		
		(* The invariant for each automaton and each location *)
			(*** TODO ***)
		invariants = (*automaton_index -> location_index -> invariant*)raise (InternalError ("Not finished yet"));
		
		(* The transitions for each automaton and each location and each action *)
			(*** TODO ***)
		transitions = raise (InternalError ("Not finished yet"))(*automaton_index -> location_index -> action_index -> (transition list)*);
		(* The list of clocks stopped for each automaton and each location *)
		(*** TODO ***)
		stopwatches = model.stopwatches;

		(* Initial location of the model *)
			(*** TODO ***)
		initial_location = raise (InternalError ("Not finished yet"))(*Location.global_location*);
		(* Initial constraint of the model *)
		initial_constraint = model.initial_constraint;
		(* Initial constraint of the model projected onto P *)
		initial_p_constraint = model.initial_p_constraint;

		(* Property defined by the user *)
		(*** TODO ***)
		user_property = model.user_property;
		(* Property defined by the model *)
		(*** TODO ***)
		correctness_condition = model.correctness_condition;
		(* List of parameters to project the result onto *)
		projection = model.projection;
	}


(* terminate_program(); *)

(**************************************************)
(* PART 2 END *)
(**************************************************)


(**************************************************)
(* GIA'S TESTING *)
(**************************************************)


(*
let constr = LinearConstraint.make_pxd_constraint (inequalities_s0@inequalities_t@inequalities_s1) in
	print_message Verbose_standard ("\n constr:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr)); 
(*testing ppl function*)
let constr_inter = LinearConstraint.pxd_intersection [constr] in
	print_message Verbose_standard ("\n constr_inter:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr_inter)); 
let constr1 = LinearConstraint.make_p_constraint ([ineq1]) in
	print_message Verbose_standard ("\n constr1:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1)); 
let constr2 = LinearConstraint.make_p_constraint ([ineq2]) in
	print_message Verbose_standard ("\n constr2:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2)); 
let abc = LinearConstraint.pxd_is_leq invariant_s0 guard_t in
	print_message Verbose_standard ("\n abc:" ^ string_of_bool abc ); 
(*testing ppl function*)
*)




(*olde version code, maybe reuse in future*)



(*
let get_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then coeff2 
										else coeff1
										in 
*)


(*
let is_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then false
										else true
										in 
*)


(*
(*
get lower upper-bound
return a tupel (number, linear_term)
number:
1: u1 is smaller than u2
2: u1 is smaller than u2
3: u1 is equal u2
4: u1, u2 are incomparable
*)
let get_lower_upperbound (op1, term1) (op2, term2) =	let (isComparable, linear_term_number ) = LinearConstraint.isComparable_linear_terms term1 term2 in
														(*testing*)
														let ineq = LinearConstraint.isComparable_linear_terms_2 term1 term2 in
														print_message Verbose_standard ("   Testing constraint!!: ");
   														print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names ineq));
   														(*testing*)
														print_message Verbose_standard ("\n isComparable: " ^ string_of_bool isComparable );
														(*check the input, start*)
														match op1, op2 with
														| LinearConstraint.Op_ge, LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g , LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_ge, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g , LinearConstraint.Op_g -> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))

														| LinearConstraint.Op_ge, _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| LinearConstraint.Op_g , _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| _		  				, LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														| _		  				, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														(*check the input, end*)
														| _		  				, _						-> 
																											match  (isComparable, linear_term_number ) with
																											| true 	, LinearConstraint.First -> 	(
																																					match (op1, op2) with
																																					| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (LinearConstraint.Second, op2, term2)
																																					| _						  , _						-> (LinearConstraint.First, op1, term1) 
																																					);

																											| true 	, LinearConstraint.Second -> 	(
																																					match (op2, op1) with
																																					| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (LinearConstraint.First, op1, term1)
																																					| _						  , _						-> (LinearConstraint.Second, op2, term2) 
																																					);

																											| true 	, _ 							-> 	raise (InternalError("error!!! "))

																											| false	, _ 							-> 	(LinearConstraint.NotDetermine, op1, term1);
														in
*)


(*
let cub_check invariant_s0 guard_t invariant_s1 clock_updates = 	
																(*ppl*)
																(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
																let inequalities = ref [] in
																print_message Verbose_standard (" CUB check, Start:");
																print_message Verbose_standard ("\n");

																let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
																let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
																let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

																(*testing*)
																(*
																let ineq = inequalities_s0@inequalities_t@inequalities_s1
																let constraint_for_cub = LinearConstraint.make_pxd_constraint (*p_linear_inequality_list*) ineq in 
   																print_message Verbose_standard ("   inequalities sdfdsf!!: ");
   																print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_for_cub));
   																*)
   																(*testing*)

																print_message Verbose_standard (" **Beginning state/location** :");
																let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list model inequalities_s0 in
																print_message Verbose_standard (" **Transition** :");
																let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list model inequalities_t in
																print_message Verbose_standard (" **Destination state/location** :");
																let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list model inequalities_s1 in
																
																let isCUB_PTA = ref true in

																 List.iter (	fun clock_index -> 
																 	let inequalities_need_to_solve = ref [] in
																 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

																 	print_message Verbose_standard ("\n 	**Beginning state/location** :");
	             													let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
	             													print_message Verbose_standard ("\n 	**Transition** :");
                													let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
                													print_message Verbose_standard ("\n 	**Destination state/location** :");
                													let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in



                													print_message Verbose_standard ("\n");
                													print_message Verbose_standard ("Comparing: ");

                													let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
                													let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
                													let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in

                													print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
                													
                													print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");

                													(* if List.mem clock_index reset_clocks = true 
                													then lower_inequality := linear_term_t; *)
                
                													(* let result = ref true in *)
                													let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

														 			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
														 			 																							true;

														 			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
														 																										false;

														 			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
														 																										print_message Verbose_standard (" 	 Case 3 " );
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																									
														 																										else
														 																											(
														 																											false;
														 																											);

														 			|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
														 																										false; 

 																	|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	true;

 																	|_							, _							 , (LinearConstraint.Op_ge, _)	->	let (typ, op, term) = get_lower_upperbound (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																										(
														 																										match typ with
														 																										| LinearConstraint.NotDetermine -> 	(
														 																																			inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																			false
														 																																			);
														 																										| LinearConstraint.First 		-> 	true
														 																										| LinearConstraint.Second 		-> 	false;
														 																										);

 																	|_							, (LinearConstraint.Op_ge, _), _							->	(*reset*)
 																																								if List.mem clock_index clock_updates = true
														 																										then
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																										else
														 																											(
 																																									let (typ, op, term) = get_lower_upperbound (op_s0, linear_term_s0)  (op_s1, linear_term_s1) in
														 																											(*let _ = get_intersec_upperbounds (op_s0, linear_term_s0)  (op_s1, linear_term_s1) in*)
														 																											match typ with
														 																											| LinearConstraint.NotDetermine -> 	(
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First 		-> 	true
														 																											| LinearConstraint.Second 		-> 	false;
														 																											);
														 																											

														 			| _							, _							 , _							-> 	(*reset*)
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																											(*let _ = get_intersec_upperbounds (op_s0, linear_term_s0) (op_t, linear_term_t) in*)
														 																											print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
														 																											(
														 																											match typ2 with
														 																											| LinearConstraint.NotDetermine -> 	(
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First 		-> 	true
														 																											| LinearConstraint.Second 		-> 	false;
														 																											);
														 																										else
														 																											(
														 																											let (typ1, op1, term1) = get_lower_upperbound (op_t, linear_term_t)  (op_s1, linear_term_s1) in 
														 																											(*let _ = get_intersec_upperbounds (op_t, linear_term_t)  (op_s1, linear_term_s1) in*)
														 																											
														 																											match typ1 with
														 																											| LinearConstraint.NotDetermine -> 	(

														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First -> (
														 																																		let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op1, term1) in
														 																																		match typ2 with
														 																																		| LinearConstraint.NotDetermine -> 	(
														 																																											inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op1, term1)] ; 
														 																																											false
														 																																											);
														 																																		| LinearConstraint.First 		-> 	true
														 																																		| LinearConstraint.Second 		-> 	false
														 																																		);
														 																											| LinearConstraint.Second -> 	(
														 																																			let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op1, term1) in
														 																																			match typ2 with
														 																																			| LinearConstraint.NotDetermine -> 	(
														 																																												inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op1, term1)] ; 
														 																																												false
														 																																												);
														 																																			| LinearConstraint.First 		-> 	true
														 																																			| LinearConstraint.Second 		-> 	false
														 																																			);
														 																											
														 																											);

														 																										
														 																									

														 																										




														 			in

														 			if (result = false && !inequalities_need_to_solve = [])
														 			then 	(
														 					isCUB_PTA := false;
														 					raise (InternalError("   The model is impossible CUB-PTA! "));
														 					);
														 			
														 			inequalities := !inequalities@(!inequalities_need_to_solve);

														 			if (result = false)
														 			then
			
														 				print_message Verbose_standard (" This is not satisfied CUB-PTA! ")
														 			else 
														 				print_message Verbose_standard (" This is satisfied CUB-PTA! ");



                												print_message Verbose_standard ("\n");
                												) model.clocks; 

																print_message Verbose_standard ("\n");
																print_message Verbose_standard (" CUB check, End!");
																print_message Verbose_standard ("\n");

																(!isCUB_PTA, !inequalities);
																in
*)


(*for testing*)
(*



let valuate1 var = NumConst.one in

let abc = LinearConstraint.evaluate_p_linear_term valuate1 linear_term_s0 in 

print_message Verbose_standard ( NumConst.string_of_numconst abc );

(*test included constraint*)

if LinearConstraint.pxd_is_leq invariant_s0 guard_t 
then print_message Verbose_standard ("s0 is included in t")
else  print_message Verbose_standard ("s0 is not included in t");

*)