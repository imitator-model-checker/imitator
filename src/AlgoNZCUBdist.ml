(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA). Distributed version.
 * 
 * File contributors : Nguyen Hoang Gia, Étienne André
 * Created           : 2017/10/03
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
open AlgoNZCUB
open DistributedUtilities






(************************************************************)
(************************************************************)
(* Useful functions *)
(************************************************************)
(************************************************************)



(* increase initial state list index *)
let increase setup_index max_index = 
	let current_setup_index = ref setup_index in
	let current_dimension = ref 0 in
	let not_is_max = ref true in
		while !not_is_max do
			(* Try to increment the local dimension *)
			let current_dimension_incremented = !current_setup_index.(!current_dimension) + 1 in
			if current_dimension_incremented <= max_index.(!current_dimension) then (
				(* Increment this dimension *)
				!current_setup_index.(!current_dimension) <- current_dimension_incremented;
				not_is_max := false;
			)
			else ( 
				(*reset*)
				!current_setup_index.(!current_dimension) <- 0;
				current_dimension := !current_dimension + 1;
				if !current_dimension > (Array.length max_index -1) then(
					not_is_max := false;
				)
			);
		done;
!current_setup_index


let compare current_setup_index max_index = 
	let b = ref true in
	for i = 0 to (Array.length max_index -1) do
  		if ( current_setup_index.(i) <> max_index.(i) ) then b := false;
	done;
!b


(* Create 2 dimensional array, first d is model index, second d contains tupples (automaton_index, target_location_index, guard) - each cell is a initial state  *)
(* Testing - OK *)
let decentralized_initial_loc model initial_global_location =
	print_message Verbose_low ("Decentralizing initial global location \n");
	(* let location_index = initial_location in *)
	let init_constr_loc = ref [||] in
	List.iter (fun automaton_index -> 
		print_message Verbose_low ("Automaton: " ^ (model.automata_names automaton_index) );
		let temp = ref [||] in
		let location_index = DiscreteState.get_location initial_global_location automaton_index in
		print_message Verbose_low ("\n");
		print_message Verbose_low (" Initial location name: " ^ (model.location_names automaton_index location_index) ) ;
		let invariant1 = model.invariants automaton_index location_index in
        print_message Verbose_low ("   Invariant (Initial location): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ; 
			print_message Verbose_low ("\n");
        	(*Checking bounded clocked in guards (Transition)*)
        	List.iter (fun action_index -> print_message Verbose_low (" Transition: " ^ (model.action_names action_index) );
            	List.iter (fun (guard, clock_updates, _, target_location_index) -> 
            		print_message Verbose_low ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names ( CUBchecker.continuous_part_of_guard guard ) ));	
                	let invariant2 = model.invariants automaton_index target_location_index in
					print_message Verbose_low ("\n");
        			print_message Verbose_low (" Location(Target location): " ^ (model.location_names automaton_index target_location_index) ) ;
        			print_message Verbose_low ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
        			print_message Verbose_low ("	  ----Map:(" 
        											^ (model.location_names automaton_index location_index) 
        											^ ")--" ^ (model.action_names action_index) ^ "-->(" 
        											^ (model.location_names automaton_index target_location_index) 
        											^ ") ----" );
        			print_message Verbose_low ("\n");
(*                	begin match clock_updates with
						(*** WARNING (ÉA, 06/04/2018): this expression should have type unit! In fact, I just remove it all… ***)
						| No_update -> []
						| Resets clock_update -> clock_update
						| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! "));
					end;*)
					(* init_constr_loc := ( Array.append !init_constr_loc [|(automaton_index, target_location_index, guard)|] ); *)
					temp := ( Array.append !temp [|(automaton_index, target_location_index, guard)|] );
					()
            	) (model.transitions automaton_index location_index action_index); 
        	) (model.actions_per_location automaton_index location_index); 
    		print_message Verbose_low ("----------------End checking " ^ (model.location_names automaton_index location_index) ^ "---------------------");
    		print_message Verbose_low ("\n");
	        init_constr_loc := ( Array.append !init_constr_loc [|!temp|] );
	) model.automata;
	(* print_message Verbose_low ("lenght!!!! " ^ string_of_int (Array.length !init_constr_loc) ); *)
!init_constr_loc

(* Testing - OK *)


(* Flattening the array above to a list *)
let init_state_list model initial_loc_array = 
	print_message Verbose_low (" init_state_list function ");
	(* create array index *)
	let max_index = ref [||] in
	(* count from 0 *)
	Array.iter ( fun sub_array -> 
		let len = (Array.length sub_array) -1 in
		(* print_message Verbose_low (" len sub array " ^ string_of_int (len) ); *)
		max_index := ( Array.append !max_index [|len|] );
	) initial_loc_array;

	(* create the current index with 0(s) *)
	let current_setup_index = ref (Array.make  (Array.length initial_loc_array) 0) in
	(* create index list *)
	let setup_index_list = ref [Array.copy !current_setup_index] in
	(* let count = ref 0 in *)
	while not (compare !current_setup_index !max_index) do 
	(* while (!count < !number_of_setups - 1) do *)
		current_setup_index := Array.copy (increase !current_setup_index !max_index);
		setup_index_list := ([Array.copy !current_setup_index])@(!setup_index_list);	
		(* count := !count + 1; *)
	done;


	(* Testing - OK *)
	(*print_message Verbose_low (" len setup_index_list " ^ string_of_int (List.length !setup_index_list) );*)
	let global_init_location_constr = ref [] in
	List.iter ( fun setup_index -> 
		let init_constr = ref (LinearConstraint.pxd_true_constraint()) in  
		let initial_locations_list = ref [] in 
		
		for i = 0 to (Array.length setup_index -1) do
			let j = setup_index.(i) in 
			let model_init_locs = initial_loc_array.(i) in 
			let (automaton_index, location_index, guard) = model_init_locs.(j) in 

			print_message Verbose_low (" Testing new initial location.....");
			print_message Verbose_low (" Automaton name: " ^ (model.automata_names automaton_index) );
			print_message Verbose_low (" Initial location name: " ^ (model.location_names automaton_index location_index));
			print_message Verbose_low (" Constraint: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (CUBchecker.continuous_part_of_guard guard)));
			print_message Verbose_low ("\n");

			initial_locations_list := (automaton_index, location_index)::!initial_locations_list;
  			init_constr := LinearConstraint.pxd_intersection [!init_constr; CUBchecker.continuous_part_of_guard guard]; 
		done;
		
		let former_initial_location = model.initial_location in
  		let initial_PTA_locations = !initial_locations_list in
  		let discrete_values = List.map (fun discrete_index -> discrete_index , (DiscreteState.get_discrete_value former_initial_location discrete_index)) model.discrete in
  		let global_init_location = DiscreteState.make_location initial_PTA_locations discrete_values in
  		
  		global_init_location_constr := ((global_init_location, (LinearConstraint.pxd_hide_discrete_and_collapse !init_constr)))::(!global_init_location_constr);
	
	) !setup_index_list;

!global_init_location_constr
(* Testing - OK *)



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNZCUBdist (model : AbstractModel.abstract_model) =
	object (self) inherit algoNZCUB model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	val mutable global_init_loc_constr : 'a list ref = ref []
	
	val mutable nb_actions : int ref = ref 0

	val mutable nb_variables : int ref = ref 0

	val mutable nb_automata : int ref = ref 0
	
	val no_nodes : int = DistributedUtilities.get_nb_nodes ()









	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "NZCUBdist"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)



	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		super#compute_result




	method private run_master = 
		self#print_algo_message Verbose_medium ("Master algorithm starting…\n");

		(* Get number of processes - index: from 0 -> no_nodes-1 *)
		print_message Verbose_medium (" Number of nodes " ^ (string_of_int no_nodes) );
		(* Get number of setup *)
		let no_setups = List.length !global_init_loc_constr in 
		print_message Verbose_medium (" Number of setups " ^ (string_of_int no_setups) );


		let from_worker = ref 0 in
		let to_worker = ref 0 in

		let current = ref 0 in 
		let counter = ref 0 in

		if no_setups = 1  then 
		(
			for source_rank = 1  to no_nodes - 1 do
				send_terminate source_rank;
			done;
			raise (InternalError("[Master] Detected only 1 PTA in the input disjunctive CUB-PTA. Therefore, please use the the single mode for better performance."));
		);
		
		
		(* Early terminating - later *)
		if no_nodes > no_setups 
		then
		(
			from_worker := no_setups + 1;
			to_worker := no_nodes - 1;

			for i = !from_worker  to !to_worker do
				send_terminate i;
				counter := !counter + 1; 
			done;


			print_message Verbose_medium (" Number of terminated workers " ^ (string_of_int (!to_worker - !from_worker + 1)) );

			for source_rank = 1  to (!from_worker - 1) do
				send_init_state !current source_rank;
				current := !current + 1;
			done;



		)
		else
		(

			for source_rank = 1  to no_nodes - 1 do
				send_init_state !current source_rank;
				current := !current + 1;
			done;
		);
		(* testing - ok *)
		


		(* Create the final good_or_bad_constraint *)
		let final_good_or_bad_constraint = ref ( Good_constraint (LinearConstraint.false_p_nnconvex_constraint () , Result.Constraint_exact) ) in 
		(* let final_good_or_bad_constraint = ref ( Good_constraint (LinearConstraint.true_p_nnconvex_constraint () , Result.Constraint_exact) ) in *)
		(* let final_good_or_bad_constraint = ref ( Good_constraint ( LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint model.initial_p_constraint , Result.Constraint_exact) ) in *)

		while !counter <> no_nodes -1 do

		let pull_request = (receive_pull_request_NZCUB ()) in
		(
		match pull_request with 
		
		(*
		(* use for testing *)
		(*Pull Tag*)
		| PullOnly source_rank ->
			print_message Verbose_low ("[Master] Received a pull request from Worker " ^ (string_of_int source_rank) ^ "");
			(* check to delete if the worker comeback *)

			let current_rank =  DistributedUtilities.get_rank () in 
			print_message Verbose_low (" Current rank " ^ (string_of_int current_rank) ); 

			
			if !current < no_setups
			then 
				(
				send_init_state !current source_rank;
				(* print_message Verbose_low (" Send!!!!!! ");  *)
				print_message Verbose_low ("[Master] sent an initial state configuration to worker " ^ (string_of_int source_rank) ^ "");
				current := !current + 1;
				)
			else
				(
				(* print_message Verbose_standard ("[Master] sent a termination to worker " ^ (string_of_int source_rank) ^ ""); *)
				send_terminate source_rank;
				print_message Verbose_low ("[Master] sent a termination to worker " ^ (string_of_int source_rank) ^ "");
				counter := !counter + 1; 
				); 

			 (* print_message Verbose_medium ("[Master] Received a pull request from worker " ^ (string_of_int source_rank) ^ "; end."); *)
		*)


		| Good_or_bad_constraint (source_rank, worker_good_or_bad_constraint) ->

			print_message Verbose_low ("[Master] Received a Good_or_bad_constraint from worker " ^ (string_of_int source_rank) ^ ""); 
			 begin 
			(* if verbose_mode_greater Verbose_low then( *) 


				(
				match worker_good_or_bad_constraint, !final_good_or_bad_constraint with
					(* Only good valuations *)
					| Good_constraint constraint_and_soundness_1, Good_constraint constraint_and_soundness_2 -> 


						print_message Verbose_low (" [Master] The both constraints are Good_constraint ");

						print_message Verbose_low (" [Master] This is the constraint result received from worker : " 
						^ ResultProcessor.string_of_good_or_bad_constraint model.variable_names worker_good_or_bad_constraint );
						
						print_message Verbose_low (" [Master] This is the current constraint result : " 
						^ ResultProcessor.string_of_good_or_bad_constraint model.variable_names worker_good_or_bad_constraint );
						
						(* Get constraint and soundness of worker_good_or_bad_constraint *)
						let constr_1, soundness_1 = constraint_and_soundness_1 in 

						(* Get constraint and soundness of final_good_or_bad_constraint *)
						let constr_2, soundness_2 = constraint_and_soundness_2 in 

						(* Because of the next function, then this final_constr is created *)
						let final_constr = constr_2 in

						(* "merge" (union) the good_or_bad_constraint with worker_good_or_bad_constraint *)
						LinearConstraint.p_nnconvex_union final_constr constr_1;

						(* final constraint *)
						final_good_or_bad_constraint := ( Good_constraint (final_constr , soundness_2) ); 

						print_message Verbose_low (" [Master] This is the final constraint: " ^ ResultProcessor.string_of_good_or_bad_constraint model.variable_names !final_good_or_bad_constraint );


						if !current < no_setups
						then 
							(
							send_init_state !current source_rank;
							(* print_message Verbose_low (" Send!!!!!! ");  *)
							print_message Verbose_low ("[Master] sent an initial state configuration to worker " ^ (string_of_int source_rank) ^ "");
							current := !current + 1;
							)
						else
							(
							(* print_message Verbose_standard ("[Master] sent a termination to worker " ^ (string_of_int source_rank) ^ ""); *)
							send_terminate source_rank;
							print_message Verbose_low ("[Master] sent a termination to worker " ^ (string_of_int source_rank) ^ "");
							counter := !counter + 1; 
							); 


						()
					
					(* Other valuations *)
					| _ -> raise (InternalError("[Master] encountered an unexpected constraint result."))
				);

				()

			 end; 
			 
		(*0ther cases*)
		|_ -> raise (InternalError("not implemented."))
	 	 
	 	);


		done;

		(* Print some explanations for the result *)
		print_warning "The returned state space and the returned algorithm termination are dummy values; please do not consider it.";
		print_warning "The computation time is the local time on the master (NOT the sum of all workers computation times)";

		(* Return the entire result *)
		Result.Single_synthesis_result
			{
				(* Good and/or bad valuations *)
				result				= !final_good_or_bad_constraint;
				
				(* Explored state space *)
				state_space			= StateSpace.make 0; (*** NOTE: dummy value! ***)
				
				(* Total computation time of the algorithm *)
				computation_time	= time_from start_time;
				
				(* Termination *)
				termination			= Result.Regular_termination; (*** NOTE: dummy value! ***)
			}





	method private run_worker = 
		print_message Verbose_medium ("Hello, I am Worker!!!!!!…\n");

		let current_rank =  DistributedUtilities.get_rank () in 
		print_message Verbose_medium (" Current rank " ^ (string_of_int current_rank) ); 
		
		let finished = ref false in

		
		while (not !finished) do 


			let work = receive_work_NZCUB () in
			(
			print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] received work!!! ");

			match work with

			| Initial_state index -> 

					(* testing - global constraints *)
					let (init_loc, init_constr) = List.nth !global_init_loc_constr index in 
					(* let (init_loc, init_constr) = List.hd !global_init_loc_constr in *)
					(* let init_state = (init_loc, LinearConstraint.px_copy init_constr) in *) 

					(* Compute initial state *)
					(* 1/ Get the very first initial state - using for testing only *)
					(* let init_state = AlgoStateBased.compute_initial_state_or_abort() in *)
					(* 2/ Actual initial state *)
					let init_state = (init_loc, init_constr) in 

					(* Set up the initial state constraint *)
					(* initial_constraint <- Some init_constr; *)
			(*		(*Initialization of slast : used in union mode only*)
					slast := [];*)
					
					(* Print some information *)
					print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] is starting running algorithm " ^ self#algorithm_name ^ "…\n");
					(* Variable initialization *)
					print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] is initializing the algorithm local variables…");
					self#initialize_variables;
					(* Debut prints *)
					print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] is starting exploring the parametric zone graph from the following initial state:");
					print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] Desired initial state" ^ ModelPrinter.string_of_state model init_state);
					(* Guess the number of reachable states *)
					let guessed_nb_states = 10 * (!nb_actions + !nb_automata + !nb_variables) in 
					let guessed_nb_transitions = guessed_nb_states * !nb_actions in 
					print_message Verbose_medium ("[Worker" ^ (string_of_int current_rank) ^ "] I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
					(* Create the state space *)
					state_space <- StateSpace.make guessed_nb_transitions;

					(* Check if the initial state should be kept according to the algorithm *)
					(* let initial_state_added = self#process_initial_state init_state in *)

					(************************************************************)
					(** Recreate the abstract model *)
					(************************************************************)
					let new_model =
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
					
						observer_pta = None;
						is_observer = (fun _ -> false);

						(* The global time clock *)
						global_time_clock = model.global_time_clock;

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
						locations_per_automaton = model.locations_per_automaton;
						(* The location names for each automaton *)
						location_names = model.location_names;
						(* The urgency for each location *)
						is_urgent = model.is_urgent;
						(*** TODO: all new initial locations shall be urgent! ***)
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
						actions_per_location = model.actions_per_location;
						(* The cost for each automaton and each location *)
						(*** TODO ***)
						(*** NOTE: dummy function ***)
						costs = (fun _ _ -> None);
						(* The invariant for each automaton and each location *)
						invariants = model.invariants;
						
						(* The transitions for each automaton and each location and each action *)
						transitions = model.transitions;
						(* The list of clocks stopped for each automaton and each location *)
						(*** TODO ***)
						(*** NOTE: dummy function ***)
						stopwatches = (fun _ _-> []);
						(* All clocks non-negative *)
						px_clocks_non_negative = model.px_clocks_non_negative;
						(* Initial location of the model *)
							(*** TODO ***)
						(* initial_location = new_initial_location; *)
						initial_location = init_loc;
						(* Initial constraint of the model *)
						(* initial_constraint = model.initial_constraint; *)
						(* initial_constraint = init_constr; *)
						initial_constraint = LinearConstraint.px_intersection [model.initial_constraint; init_constr; (LinearConstraint.px_of_p_constraint model.initial_p_constraint)];
						(* Initial constraint of the model projected onto P *)
						initial_p_constraint = model.initial_p_constraint;
						(* Initial constraint of the model projected onto P and all clocks non-negative *)
						px_clocks_non_negative_and_initial_p_constraint = model.px_clocks_non_negative_and_initial_p_constraint;
						(* Property defined by the user *)
						(*** TODO ***)
						(*** WARNING: any property will be turned into an (equivalent) reachability property, i.e., the original user property is lost ***)
						user_property = model.user_property;
						(* Property defined by the model *)
						correctness_condition = model.correctness_condition;
						(* List of parameters to project the result onto *)
						projection = model.projection;
						(* Parameter to be minimized or maximized *)
						optimized_parameter = model.optimized_parameter;
					}
					in

					(* Set back the model *)
					Input.set_model new_model;

					(* Run the NZ algo *)

					let algo = new AlgoNZCUB.algoNZCUB in
					
					let result = algo#run () in 


					let good_or_bad_constraint = match result with 
						(* Result for EFsynth, PDFC PRP *)
						| Single_synthesis_result single_synthesis_result -> 

							print_message Verbose_low ("[Worker" ^ (string_of_int current_rank) ^ "] The result is Single_synthesis_result "); 
							single_synthesis_result.result (***** Detected!!!! *****)
						
						| _ -> raise (InternalError("[Worker" ^ (string_of_int current_rank) ^ "] is expecting a single synthesis result, but received something else."))
					in

					print_message Verbose_low ("[Worker" ^ (string_of_int current_rank) ^ "] This is the constraint result of worker " ^ (string_of_int current_rank) 
						^" : " ^ ResultProcessor.string_of_good_or_bad_constraint model.variable_names good_or_bad_constraint );



					DistributedUtilities.send_good_or_bad_constraint good_or_bad_constraint;

					print_message Verbose_low ("[Worker" ^ (string_of_int current_rank) ^ "] sent The constraint ");



			| Terminate -> 
					print_message Verbose_low ("[Worker" ^ (string_of_int current_rank) ^ "] Terminate ");
					(* print_message Verbose_standard ("Hello, I terminated!!!!!!…\n"); *)
					(* print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to terminate work."); *)
					finished := true;
					(* Result.Distributed_worker_result *) 
			
			
				
			| _ -> 		
					(* print_message Verbose_low ("[Worker" ^ (string_of_int current_rank) ^ "] Error! not implemented."); *)
					raise (InternalError("not implemented."));

			);

		done; 
		

		
		Result.Distributed_worker_result
		
	








	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =
		(* print_message Verbose_standard ("This is the main run method for switching between Master and Worker!…\n"); *)

		(* Get some variables *)
		(* let nb_actions = model.nb_actions in *)
		nb_actions := model.nb_actions;
		(* let nb_variables = model.nb_variables in *)
		nb_variables := model.nb_variables;
		(* let nb_automata = model.nb_automata in *)
		nb_automata := model.nb_automata;
		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		(* Compute initial state *)
		let init_state = self#compute_initial_state_or_abort in
		
		(* copy init state, as it might be destroyed later *)
		(*** NOTE: this operation appears to be here totally useless ***)
		let init_loc, init_constr = init_state in

		(* Get a list of global initial location and initial constraint *)
		(* from 0 -> Array.lenght -1 *)
		let init_constr_loc = decentralized_initial_loc model init_loc in 
		(* let global_init_loc_constr = init_state_list model init_constr_loc in *)

		global_init_loc_constr := init_state_list model init_constr_loc;

	
		(* Branch between master and worker *)
		if DistributedUtilities.is_master () 
		then
			(
			self#run_master;
			)
		else
			(
			self#run_worker;
			);


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result
		

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)








