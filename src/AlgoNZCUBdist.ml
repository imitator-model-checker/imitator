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
open DistributedUtilities






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
  		if ( current_setup_index.(i) != max_index.(i) ) then b := false;
	done;
!b


(* Testing - OK *)
let decentralized_initial_loc model initial_global_location =
	print_message Verbose_low ("Decentralizing initial global location \n");
	(* let location_index = initial_location in *)
	let init_constr_loc = ref [||] in
	List.iter (fun automaton_index -> 
		print_message Verbose_low ("Automaton: " ^ (model.automata_names automaton_index) );
		let temp = ref [||] in
		let location_index = Location.get_location initial_global_location automaton_index in
		print_message Verbose_low ("\n");
		print_message Verbose_low (" Initial location name: " ^ (model.location_names automaton_index location_index) ) ;
		let invariant1 = model.invariants automaton_index location_index in
        print_message Verbose_low ("   Invariant (Initial location): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ; 
			print_message Verbose_low ("\n");
        	(*Checking bounded clocked in guards (Transition)*)
        	List.iter (fun action_index -> print_message Verbose_low (" Transition: " ^ (model.action_names action_index) );
            	List.iter (fun (guard, clock_updates, _, target_location_index) -> 
            		print_message Verbose_low ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (  continuous_part_of_guard guard ) ));	
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
	        init_constr_loc := ( Array.append !init_constr_loc [|!temp|] );
	) model.automata;
	(* print_message Verbose_low ("lenght!!!! " ^ string_of_int (Array.length !init_constr_loc) ); *)
!init_constr_loc

(* Testing - OK *)

let init_state_list model initial_loc_array = 
	print_message Verbose_low (" init_state_list function ");
	let max_index = ref [||] in
	(* count from 0 *)
	Array.iter ( fun sub_array -> 
		let len = (Array.length sub_array) -1 in
		(* print_message Verbose_low (" len sub array " ^ string_of_int (len) ); *)
		max_index := ( Array.append !max_index [|len|] );
	) initial_loc_array;
	let current_setup_index = ref (Array.make  (Array.length initial_loc_array) 0) in
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
			print_message Verbose_low (" Constraint: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names (continuous_part_of_guard guard)));
			print_message Verbose_low ("\n");

			initial_locations_list := (automaton_index, location_index)::!initial_locations_list;
  			init_constr := LinearConstraint.pxd_intersection [!init_constr; continuous_part_of_guard guard]; 
		done;
		
		let former_initial_location = model.initial_location in
  		let initial_PTA_locations = !initial_locations_list in
  		let discrete_values = List.map (fun discrete_index -> discrete_index , (Location.get_discrete_value former_initial_location discrete_index)) model.discrete in
  		let global_init_location = Location.make_location initial_PTA_locations discrete_values in
  		
  		global_init_location_constr := (global_init_location,!init_constr)::(!global_init_location_constr);
	
	) !setup_index_list;

!global_init_location_constr
(* Testing - OK *)



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


	method private run_master = 
		print_message Verbose_standard ("Hello, I am Master!!!!!!…\n");

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


		(* Get a list of global initial location and initial constraint *)
		(* from 0 -> Array.lenght -1 *)
		let init_constr_loc = decentralized_initial_loc model init_loc in 
		let global_init_loc_constr = init_state_list model init_constr_loc in 



		(* Get number of processes - index: from 0 -> no_nodes-1 *)
		let no_nodes = DistributedUtilities.get_nb_nodes () in  
		print_message Verbose_low (" number of nodes " ^ (string_of_int no_nodes) );

		(* Get number of setup *)
		let no_setups = List.length global_init_loc_constr in 
		print_message Verbose_low (" number of setups " ^ (string_of_int no_setups) );
		
		let current_rank =  DistributedUtilities.get_rank () in 
		print_message Verbose_low (" Current rank " ^ (string_of_int current_rank) ); 


		(*
		(* Early terminating - later *)
		if no_nodes > no_setups 
		then
		(
			for i = no_setups + 1  to no_nodes - 1 do
				DistributedUtilities.send_terminate i;
			done;
		);
		*)

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
			self#compute_result 
		
		(*** TODO: split between process result and return result; in between, add some info (algo_name finished after….., etc.) ***)
		) (* end if initial state added *)

	

	(* super#run () *)





	method private run_worker () = 
		print_message Verbose_standard ("Hello, I am Worker!!!!!!…\n");

		let current_rank =  DistributedUtilities.get_rank () in 
		print_message Verbose_low (" Current rank " ^ (string_of_int current_rank) ); 

		let finished = ref false in

		while (not !finished) do

			let work = receive_work () in

			match work with
	
				
			| Terminate -> 
					print_message Verbose_medium (" Terminate ");
					(* print_message Verbose_medium ("[Worker " ^ (string_of_int rank) ^ "] I was just told to terminate work."); *)
					finished := true
				
			| _ -> 		print_message Verbose_medium ("error!!! not implemented.");
					raise (InternalError("not implemented."));



		done;

		(* Result.Distributed_worker_result *)
		
	








	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method run () =

		print_message Verbose_standard ("Hello, this is the main run method for switching between Master and Worker!!!!!!…\n");


	
		(* Branch between master and worker *)
		if DistributedUtilities.is_master () 
		then
			(
			self#run_master;
			(* self#compute_result; *)
			(* Result.Distributed_worker_result; *) 
			)
		else
			(
			self#run_worker;
			(* self#compute_result; *)
			Result.Distributed_worker_result;
			);
		
	(* method virtual compute_result : Result.imitator_result *)
	







	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Packaging the result at the end of the exploration (to be defined in subclasses) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method virtual compute_result : Result.imitator_result
		

(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)








