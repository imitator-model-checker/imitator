(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: Non-zenoness emptiness check using CUB transformation (synthesizes valuations for which there exists a non-zeno loop in the PTA). Distributed version.
 * 
 * File contributors : Étienne André
 * Created           : 2017/10/03
 * Last modified     : 2017/10/03
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






(************************************************************)
(************************************************************)
(* Useful functions *)
(************************************************************)
(************************************************************)




(* Note: duplicate function *)
let continuous_part_of_guard (*: LinearConstraint.pxd_linear_constraint*) = function
	| True_guard -> LinearConstraint.pxd_true_constraint()
	| False_guard -> LinearConstraint.pxd_false_constraint()
	| Discrete_guard discrete_guard -> LinearConstraint.pxd_true_constraint()
	| Continuous_guard continuous_guard -> continuous_guard
	| Discrete_continuous_guard discrete_continuous_guard -> discrete_continuous_guard.continuous_guard


let increase current_setup_index max_index = 

	let current_setup_index = ref current_setup_index in

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

  		if ( current_setup_index.(i) != max_index.(i) ) 

  		then 

  			b := false;

	done;

!b


let decentralized_initial_loc model initial_global_location =

	print_message Verbose_low ("decentralized_initial_loc function \n");

	(* let location_index = initial_location in *)

	let init_constr_loc = ref [||] in
	

	List.iter (fun automaton_index -> print_message Verbose_low ("Automaton: " ^ (model.automata_names automaton_index) );

				let temp = ref [||] in

					let location_index = Location.get_location initial_global_location automaton_index in

	        		print_message Verbose_low ("\n");

	        		print_message Verbose_low (" Initial location name: " ^ (model.location_names automaton_index location_index) ) ;

	        		let invariant1 = model.invariants automaton_index location_index in
	        
	                print_message Verbose_low ("   Invariant(Initial location): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ; 
							

							print_message Verbose_low ("\n");

	                	(*Checking bounded clocked in guards (Transition)*)
	                	List.iter (fun action_index -> print_message Verbose_low (" Transition/Action: " ^ (model.action_names action_index) );
	            
	                    	List.iter (fun (guard, clock_updates, _, target_location_index) 
	                    		-> 
	                    		print_message Verbose_low ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (  continuous_part_of_guard guard ) ));	

	                        	(** WORK HERE **)

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
	                			

	                        	let clock_updates = match clock_updates with
	                        						  No_update -> []
													| Resets clock_update -> clock_update
													| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); in

								(* init_constr_loc := ( Array.append !init_constr_loc [|(automaton_index, target_location_index, guard)|] ); *)
								temp := ( Array.append !temp [|(automaton_index, target_location_index, guard)|] );
								

								()

	                    	) (model.transitions automaton_index location_index action_index); 

	                	) (model.actions_per_location automaton_index location_index); 

	            		 print_message Verbose_low ("----------------End checking " ^ (model.location_names automaton_index location_index) ^ "---------------------");

	            		 print_message Verbose_low ("\n");

	        print_message Verbose_low ("\n");

	        init_constr_loc := ( Array.append !init_constr_loc [|!temp|] );

	) model.automata;
	print_message Verbose_low ("lenght!!!! " ^ string_of_int (Array.length !init_constr_loc) );

!init_constr_loc




let init_state_list model initial_loc_array = 

	print_message Verbose_low (" init_state_list function ");

	let max_index = ref [||] in

	let number_of_setups = ref 0 in 

	(* count from 0 *)
	Array.iter ( fun sub_array -> 

		let len = (Array.length sub_array) -1 in

		print_message Verbose_low (" len sub array " ^ string_of_int (len) );

		number_of_setups := !number_of_setups + (Array.length sub_array);

		print_message Verbose_low (" Number of initial state setups " ^ string_of_int (!number_of_setups) );

		max_index := ( Array.append !max_index [|len|] );

	) initial_loc_array;

	let current_setup_index = ref (Array.create (Array.length initial_loc_array) 0) in

	

	Array.iter ( fun element -> 

		print_message Verbose_low (" element " ^ string_of_int (element) );

	) !current_setup_index;



	let setup_index_list = ref [|!current_setup_index|] in

	print_message Verbose_low (" current increased " );
	
	while not (compare !current_setup_index !max_index) do
		
		current_setup_index := increase !current_setup_index !max_index;
		
		setup_index_list := Array.append !setup_index_list [|!current_setup_index|];
		
		for i = 0 to (Array.length !max_index -1) do
  		
  			print_message Verbose_low (string_of_int (!current_setup_index.(i)) );
		
		done;

	done;
	



	let global_init_location_constr = ref [] in

	Array.iter ( fun setup_index -> 

		let init_constr = ref (LinearConstraint.pxd_true_constraint()) in  

		let initial_locations_list = ref [] in 

		for i = 0 to (Array.length setup_index -1) do

			let j = !current_setup_index.(i) in 

			let model_init_locs = initial_loc_array.(i) in 
			
			let (automaton_index, location_index, guard) = model_init_locs.(j) in 

			initial_locations_list := (automaton_index, location_index)::!initial_locations_list;
  			
  			init_constr := LinearConstraint.pxd_intersection [!init_constr; continuous_part_of_guard guard]; 


			()

		done;

		let former_initial_location = model.initial_location in

  		let initial_PTA_locations = !initial_locations_list in

  		let discrete_values = List.map (fun discrete_index -> discrete_index , (Location.get_discrete_value former_initial_location discrete_index)) model.discrete in

  		let global_init_location = Location.make_location initial_PTA_locations discrete_values in

  		global_init_location_constr := (global_init_location,!init_constr)::(!global_init_location_constr);

	
	) !setup_index_list;



	for i = 0 to (Array.length initial_loc_array -1) do
  		
  		print_message Verbose_low (string_of_int (!current_setup_index.(i)) );

	done;

!global_init_location_constr




(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNZCUBdist =
	object (self) inherit algoNZCUB as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
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


	method private run_master () = 
		print_message Verbose_standard ("Hello, I am Master!!!!!!…\n")


	method private run_worker () = 
		print_message Verbose_standard ("Hello, I am Worker!!!!!!…\n")


		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =

		print_message Verbose_standard ("Hello, this is the main run method for switching between Master and Worker!!!!!!…\n");

		
		(* Get some variables *)
		let nb_actions = model.nb_actions in
		let nb_variables = model.nb_variables in
		let nb_automata = model.nb_automata in

		(* Time counter for the algorithm *)
		start_time <- Unix.gettimeofday();

		(* Compute initial state *)
		let init_state = AlgoStateBased.compute_initial_state_or_abort () in
		
		(* copy init state, as it might be destroyed later *)
		(*** NOTE: this operation appears to be here totally useless ***)
		let init_loc, init_constr = init_state in


		(*  *)
		(* from 0 -> Array.lenght -1 *)
		let init_constr_loc = decentralized_initial_loc model init_loc in 
		let global_init_loc_constr = init_state_list model init_constr_loc in 



		(* let a = StateSpace.get_location state_space (Array.get init_constr_loc 0) in *)

		let locs_array = Location.get_locations init_loc in 
		print_int locs_array.(0);

		Array.iter ( fun loc -> print_message Verbose_low ("awdasdsads" ^ (string_of_int loc ));
			()

		) locs_array;




		let init_state = (init_loc, LinearConstraint.px_copy init_constr) in


		(* Set up the initial state constraint *)
		initial_constraint <- Some init_constr;

(*		(*Initialization of slast : used in union mode only*)
		slast := [];*)
		
		(* Print some information *)
		print_message Verbose_standard ("Starting running algorithm " ^ self#algorithm_name ^ "…\n");
		
		(* Variable initialization *)
		print_message Verbose_low ("Initializing the algorithm local variables…");
		self#initialize_variables;

		(* Debut prints *)
		print_message Verbose_low ("Starting exploring the parametric zone graph from the following initial state:");
		print_message Verbose_low (ModelPrinter.string_of_state model init_state);
		(* Guess the number of reachable states *)
		let guessed_nb_states = 10 * (nb_actions + nb_automata + nb_variables) in 
		let guessed_nb_transitions = guessed_nb_states * nb_actions in 
		print_message Verbose_high ("I guess I will reach about " ^ (string_of_int guessed_nb_states) ^ " states with " ^ (string_of_int guessed_nb_transitions) ^ " transitions.");
		
		(* Create the state space *)
		state_space <- StateSpace.make guessed_nb_transitions;

		
		(* Check if the initial state should be kept according to the algorithm *)
		let initial_state_added = self#process_initial_state init_state in
		
		(* Degenerate case: initial state cannot be kept: terminate *)
		if not initial_state_added then(
			(* Output a warning because this situation is still a little strange *)
			print_warning "The initial state is not kept. Analysis will now terminate.";
			
			(* Set the termination status *)
			termination_status <- Some (Result.Regular_termination);
			
			(* Return the algorithm-dependent result and terminate *)
			self#compute_result
		(* Else: start the algorithm in a regular manner *)
		)else(
		
		(* Add the initial state to the reachable states; no need to check whether the state is present since it is the first state anyway *)
		let init_state_index = match StateSpace.add_state state_space StateSpace.No_check init_state with
			(* The state is necessarily new as the state space was empty *)
			| StateSpace.New_state state_index -> state_index
			| _ -> raise (InternalError "The result of adding the initial state to the state space should be New_state")
		in
		
		(* Increment the number of computed states *)
		StateSpace.increment_nb_gen_states state_space;

		(* Call generic method handling BFS *)
		begin
		match options#exploration_order with
			| Exploration_layer_BFS -> super#explore_layer_bfs init_state_index;
			| Exploration_queue_BFS -> super#explore_queue_bfs init_state_index;
			| Exploration_queue_BFS_RS -> super#explore_queue_bfs init_state_index;
			| Exploration_queue_BFS_PRIOR -> super#explore_queue_bfs init_state_index;
		end;

		(* Return the algorithm-dependent result *)
		super#compute_result
		
		(*** TODO: split between process result and return result; in between, add some info (algo_name finished after….., etc.) ***)
		) (* end if initial state added *)

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result
	

	(* super#run () *)

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)








