(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Parametric timed game with reachability condition
 * 
 * File contributors : Étienne André
 * Created           : 2022/11/29
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
open AbstractProperty
open Result
open AlgoStateBased
open Statistics
open State

(* Notation and shorthands *)
let (>>) f g x = g(f(x)) 
let (>>>) f g x y = g(f x y) 

let nn = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint
let project_params = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse 
let is_empty =  LinearConstraint.px_nnconvex_constraint_is_false 
let (#!=) = LinearConstraint.px_nnconvex_constraint_is_equal >>> not
let (#<-) queue elem = Queue.add elem queue   
let (#<--) queue1 queue2 = Queue.transfer queue2 queue1
let bot = LinearConstraint.false_px_nnconvex_constraint

let (|||) = fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a  
let (&&&) = fun a b -> LinearConstraint.px_nnconvex_intersection_assign a b; a
let print_PTG = print_message Verbose_medium

type edge = {state: state_index; action: Automaton.action_index; transition: StateSpace.combined_transition; state': state_index}

type edge_status = BackpropLosing | BackpropWinning | Unexplored
module type Default = sig  
	type elem
	type key
	val str_of_elem : elem -> string
	val str_of_key : key -> string
	val tbl : (key, elem) Hashtbl.t
	val default : key -> elem
	val model : AbstractModel.abstract_model option ref
end

module DefaultHashtbl (D : Default) = struct 
	let model = D.model
	let str_of_elem = D.str_of_elem
	let tbl = D.tbl
	let find key = 
		try Hashtbl.find tbl key with
			Not_found -> D.default key
	let replace = Hashtbl.replace tbl
	let to_str () = "[" ^ 
		Seq.fold_left 
			(fun acc (key, elem) -> Printf.sprintf "%s, %s -> %s\n" acc (D.str_of_key key) (D.str_of_elem elem)) 
			("")
			(Hashtbl.to_seq tbl) 
		^ "]"
end



let edge_to_str = fun ({state; action; state';_} : edge) model -> 	
	let action_str = model.action_names action in 
	Printf.sprintf "%d --%s-> %d" state action_str state'

let edge_seq_to_str seq model = "[" ^ 
	Seq.fold_left 
		(fun acc edge -> Printf.sprintf "%s, %s" acc (edge_to_str edge model))
		("") (
		seq) 
	^ "]"

module WinningZone = DefaultHashtbl (struct 
	let model = ref None
	type elem = LinearConstraint.px_nnconvex_constraint
	type key = state_index
	let tbl = Hashtbl.create 100
	let default = fun _ -> LinearConstraint.false_px_nnconvex_constraint()
	let str_of_elem zone = match !model with 
		| Some model -> LinearConstraint.string_of_px_nnconvex_constraint model.variable_names zone
		| None -> "No model provided"
	let str_of_key = string_of_int >> (^) ("s")
end)  

module LosingZone = DefaultHashtbl (struct 
	let model = ref None
	type elem = LinearConstraint.px_nnconvex_constraint
	type key = state_index
	let tbl = Hashtbl.create 100
	let default = fun _ -> LinearConstraint.false_px_nnconvex_constraint()
	let str_of_elem zone = match !model with 
		| Some model -> LinearConstraint.string_of_px_nnconvex_constraint model.variable_names zone
		| None -> "No model provided"
	let str_of_key = string_of_int >> (^) ("s")
end)  

module Depends = DefaultHashtbl (struct 
	let model = ref None
	type elem = edge Queue.t 
	type key = state_index
	let tbl = Hashtbl.create 100
	let default = fun idx -> let q = Queue.create () in Hashtbl.add tbl idx q; q
	let str_of_elem queue = match !model with 
		| Some model -> edge_seq_to_str (Queue.to_seq queue) model
		| None -> "No model provided"
	let str_of_key = string_of_int
end)


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoPTG (model : AbstractModel.abstract_model) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoStateBased model as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Depth in the explored state space *)
	(*** NOTE: private ***)
	val mutable bfs_current_depth = 0

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)

	(* The target state has been found *)
	val counter_found_target = create_discrete_counter_and_register "found target state" PPL_counter Verbose_low

	(* Methods counters *)
	val counter_process_state = create_hybrid_counter_and_register "EFsynth.process_state" States_counter Verbose_experiments
	val counter_add_a_new_state = create_hybrid_counter_and_register "EFsynth.add_a_new_state" States_counter Verbose_experiments


	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;
		
		(*** NOTE: duplicate operation ***)
		synthesized_constraint <- LinearConstraint.false_p_nnconvex_constraint ();

		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PTG"


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Process a symbolic state: returns false if the state is a target state (and should not be added to the next states to explore), true otherwise *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private process_state (state : State.state) : bool =
	
		(* Statistics *)
		counter_process_state#increment;
		counter_process_state#start;
	
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering process_state…";
		);

		true

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns None unless the initial state cannot be kept, in which case the algorithm returns an imitator_result *)
	(*** NOTE: this function is redefined here ***)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method try_termination_at_initial_state : Result.imitator_result option =
		(* Retrieve the initial state *)
		let initial_px_constraint : LinearConstraint.px_linear_constraint = self#get_initial_px_constraint_or_die in
		let initial_state : State.state = {global_location = model.initial_location ; px_constraint = initial_px_constraint} in


		if self#process_state initial_state then None
		else(
			(* Set termination status *)
			termination_status <- Some (Result.Regular_termination);

			(* Terminate *)
			Some (self#compute_result)
		)



	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state space (if indeed needed) *)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(* Can raise an exception TerminateAnalysis to lead to an immediate termination *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: return the list of actually added states ***)
	(*** WARNING/BADPROG: the following is partially copy/paste to AlgoPRP.ml ***)
	method add_a_new_state source_state_index combined_transition new_state =
		(* Statistics *)
		counter_add_a_new_state#increment;
		counter_add_a_new_state#start;
		
		(* Print some information *)
		if verbose_mode_greater Verbose_medium then(
			self#print_algo_message Verbose_medium "Entering `add_a_new_state` (and reset cache)…";
		);
		
		(* Reset the mini-cache (for the p-constraint) *)
		self#reset_minicache;
		
		(* Try to add the new state to the state space *)
		let addition_result = state_space#add_state options#comparison_operator new_state in
		
		(* Boolean to check whether the state is a target state *)
		let is_target = ref false in
		
		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()

		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			self#update_statespace_nature new_state;
			
			(* Will the state be added to the list of new states (the successors of which will be computed)? *)
			(*** NOTE: if the answer is false, then the new state is a target state ***)
			(*** BADPROG: ugly bool ref that may be updated in an IF condition below ***)
			let to_be_added = ref (self#process_state new_state) in
			
			(* Update the target flag *)
			is_target := not !to_be_added;
			
			(* If to be added: if the state is included into the synthesized constraint, no need to explore further, and hence do not add *)
			if !to_be_added then(
			
				(*** NOTE: don't perform this test if the associated option is enabled ***)
				if not options#no_leq_test_in_ef then(
					(* Check whether new_state.px_constraint <= synthesized_constraint *)
					if self#check_whether_px_included_into_synthesized_constraint new_state.px_constraint then(
						(* Print some information *)
						self#print_algo_message Verbose_low "Found a state included in synthesized valuations; cut branch.";

						(* Do NOT compute its successors; cut the branch *)
						to_be_added := false;
					);
				);
			);

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			if !to_be_added then
				new_states_indexes <- new_state_index :: new_states_indexes;
			
		end (* end if new state *)
		;
		
		(*** TODO: move the two following statements to a higher level function? (post_from_one_state?) ***)
		
		(* Retrieve the new state index *)
		(*** HACK ***)
		let new_state_index = match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index in
		
		(* Add the transition to the state space *)
		self#add_transition_to_state_space (source_state_index, combined_transition, new_state_index) addition_result;

		
		
		(* Case accepting state *)
		if !is_target then(
			(* 1. Construct counterexample if requested by the algorithm (and stop termination by raising a TerminateAnalysis exception, if needed) *)
			let property = Input.get_property() in
			if property.synthesis_type = Exemplification then(
				self#construct_counterexamples new_state_index;
			);
			
			(* 2. If #witness mode, then we will throw an exception *)
			self#terminate_if_witness;
		); (* end if target *)


		
		(* Statistics *)
		counter_add_a_new_state#stop;

		(* The state is kept in any case *)
		true
(*** WARNING/BADPROG: what precedes is partially copy/paste to AlgoPRP.ml ***)
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Actions to perform when meeting a state with no successors: nothing to do for this algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_deadlock_state state_index = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (post_n : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run the BFS algorithm  *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** WARNING (2022/11, ÉA): copied from AlgoStateBased ***)
	method explore_layer_bfs init_state_index =

		(* Statistics *)
(*		counter_explore_using_strategy#increment;
		counter_explore_using_strategy#start;*)

		(* Set the depth to 1 *)
		bfs_current_depth <- 1;

		(* To check whether the time limit / state limit is reached *)
		limit_reached <- Keep_going;

		(* Flag modified by the algorithm to perhaps terminate earlier *)
		algorithm_keep_going <- true;


		(*------------------------------------------------------------*)
		(* Perform the post^* *)
		(*------------------------------------------------------------*)
		(* Set of states computed at the previous depth *)
		let post_n = ref [init_state_index] in

		(* Explore further until the limit is reached or the list of states computed at the previous depth is empty *)
		while limit_reached = Keep_going && !post_n <> [] && algorithm_keep_going do
			(* Print some information *)
			if verbose_mode_greater Verbose_standard then (
				print_message Verbose_low ("\n");
				print_message Verbose_standard ("Computing post^" ^ (string_of_int bfs_current_depth) ^ " from "  ^ (string_of_int (List.length !post_n)) ^ " state" ^ (s_of_int (List.length !post_n)) ^ ".");
			);

			(* Count the states for verbose purpose: *)
			let num_state = ref 0 in

			(* Statistics *)
(*			counter_nplus1#increment;
			counter_nplus1#start;*)

			(* The concrete function post_from_one_state may raise exception TerminateAnalysis, and update termination_status *)
			let post_n_plus_1 =
			try(
			(* For each newly found state: *)
			List.fold_left (fun current_post_n_plus_1 source_state_index ->
				(* Count the states for verbose purpose: *)
				num_state := !num_state + 1;

				(* Perform the post *)
				let new_states = self#post_from_one_state source_state_index in

				(* Print some information *)
				if verbose_mode_greater Verbose_medium then (
					let beginning_message = if new_states = [] then "Found no new state" else ("Found " ^ (string_of_int (List.length new_states)) ^ " new state" ^ (s_of_int (List.length new_states)) ^ "") in
					print_message Verbose_medium (beginning_message ^ " for the post of state " ^ (string_of_int !num_state) ^ " / " ^ (string_of_int (List.length !post_n)) ^ " in post^" ^ (string_of_int bfs_current_depth) ^ ".\n");
				);

				(* Return the concatenation of the new states *)
				(**** OPTIMIZED: do not care about order (else shoud consider 'list_append current_post_n_plus_1 (List.rev new_states)') *)
				List.rev_append current_post_n_plus_1 new_states
			) [] !post_n
			)
			with TerminateAnalysis ->(
				(* Set the flag *)
				algorithm_keep_going <- false;
				(* If analysis terminated: successors are just the empty list *)
				(*** TODO: it should be possible to change the flag algorithm_keep_going from inside the function instead of deleting this list ***)
				[]
			)

			in

			(* Statistics *)
(* 			counter_nplus1#stop; *)

			(* Statistics *)
(*			counter_process_post_n#increment;
			counter_process_post_n#start;*)

			self#process_post_n !post_n;

			(* Statistics *)
(* 			counter_process_post_n#stop; *)

			(*------------------------------------------------------------*)
			(* Begin merging *)
			(*------------------------------------------------------------*)
			(* Merge states! *)
			let new_states_after_merging = ref post_n_plus_1 in
			(*** HACK here! For #merge_before, we should ONLY merge here; but, in order not to change the full structure of the post computation, we first merge locally before the pi0-compatibility test, then again here ***)

            (*** BEGIN CALL OF MERGING ***)
			begin
            match options#merge_algorithm with
            | Merge_reconstruct
            | Merge_onthefly    ->
                new_states_after_merging := state_space#merge !new_states_after_merging;
            | Merge_212 ->
                let eaten_states = state_space#merge212 !new_states_after_merging in
                new_states_after_merging := list_diff !new_states_after_merging eaten_states;
            | Merge_none ->
                ()
            end;
			(*** END CALL OF MERGING ***)

			(* Update the post_n, i.e., at that point we replace the post^n by post^n+1 in our BFS algorithm, and go one step deeper in the state space *)
			post_n := !new_states_after_merging;
			(*------------------------------------------------------------*)
			(* End merging *)
			(*------------------------------------------------------------*)

			(* Print some information *)
			if verbose_mode_greater Verbose_medium then (
				let beginning_message = if !post_n = [] then "\nFound no new state" else ("\nFound " ^ (string_of_int (List.length !post_n)) ^ " new state" ^ (s_of_int (List.length !post_n)) ^ "") in
				print_message Verbose_medium (beginning_message ^ " for post^" ^ (string_of_int bfs_current_depth) ^ ".\n");
			);

			(* If acyclic option: empty the list of already reached states for comparison with former states *)
			if options#acyclic then(
				print_message Verbose_low ("\nMode acyclic: empty the list of states to be compared.");
				state_space#empty_states_for_comparison;
			);

			(* Print some memory information *)
			if options#statistics then(
				(*** TODO ***)
			);

			(* Statistics *)
(*			counter_gcmajor#increment;
			counter_gcmajor#start;

			(* Statistics *)
			counter_gcmajor#stop;*)


			(* Go one step deeper *)
			bfs_current_depth <- bfs_current_depth + 1;

			(* Check if the limit has been reached *)
			(*** NOTE (ÉA, 2022/11): disabled so far (would check the time limit, states limit, depth limit…) ***)
(* 			self#check_and_update_layer_bfs_limit; *)

			(* If still going, ask the concrete algorithm whether it wants to terminate for other reasons *)
			if limit_reached = Keep_going then(
				(* Print some information *)
				(*** HACK: 'bfs_current_depth - 1' because bfs_current_depth was just incremented… ***)
				self#print_algo_message Verbose_low("Checking termination at post^" ^ (string_of_int (bfs_current_depth - 1)) ^ " with a queue of " ^ (string_of_int (List.length !post_n)) ^ " unexplored state" ^ (s_of_int (List.length !post_n)) ^ "…");

				if self#check_termination_at_post_n then(
					algorithm_keep_going <- false;
				);
			);

		done; (* END WHILE *)

		(* Were they any more states to explore? *)
		let nb_unexplored_successors = List.length !post_n in

		(* Set the list of states with unexplored successors, if any *)
		if nb_unexplored_successors > 0 then(
			(*** NOTE: if an exception TerminateAnalysis was raised, this list is empty :( ***)
			unexplored_successors <- UnexSucc_some !post_n;
		);

		(* Update termination condition *)
		begin
		match limit_reached with
			(* No limit: regular termination *)
			(*** NOTE: check None, as it may have been edited from outside, in which case it should not be Regular_termination ***)
			| Keep_going when termination_status = None -> termination_status <- Some (Result.Regular_termination)
			(*** NOTE: obliged to comment out the condition below, otherwise there is a compiling warning… ***)
			| Keep_going (*when termination_status <> None*) -> ()

			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some (Result.Time_limit nb_unexplored_successors)

			(* Termination due to state space depth limit reached *)
			| Depth_limit_reached -> termination_status <- Some (Result.Depth_limit nb_unexplored_successors)

			(* Termination due to a number of explored states reached *)
			| States_limit_reached -> termination_status <- Some (Result.States_limit nb_unexplored_successors)

			(* Termination because a witness has been found *)
			(*** NOTE/TODO: add a new result termination type? ***)
			| Witness_found -> termination_status <- Some (Result.Regular_termination)
		end
		;

		(* Print some information *)
		(*** NOTE: must be done after setting the limit (above) ***)
		(*** NOTE (ÉA, 2022/11): disabled so far (would check the time limit, states limit, depth limit…) ***)
(* 		self#bfs_print_warnings_limit (); *)

		if not algorithm_keep_going && nb_unexplored_successors > 0 then(
			self#print_algo_message Verbose_standard ("A sufficient condition to ensure termination was met although there were still " ^ (string_of_int nb_unexplored_successors) ^ " state" ^ (s_of_int nb_unexplored_successors) ^ " to explore");
		);


		print_message Verbose_standard (
			let nb_states = state_space#nb_states in
			let nb_transitions = state_space#nb_transitions in
			let fixpoint_str = if nb_unexplored_successors > 0 then "State space exploration stopped" else "Fixpoint reached" in
			"\n" ^ fixpoint_str ^ " at a depth of "
			^ (string_of_int bfs_current_depth) ^ ""
			^ ": "
			^ (string_of_int nb_states) ^ " state" ^ (s_of_int nb_states)
			^ " with "
			^ (string_of_int nb_transitions) ^ " transition" ^ (s_of_int nb_transitions) ^ " in the final state space."
		);
		(*** NOTE: in fact, more states and transitions may have been explored (and deleted); here, these figures are the number of states in the state space. ***)

		(* Statistics *)
		counter_explore_using_strategy#stop;

		(* The end *)
		()
	(*** END WARNING (2022/11, ÉA): copied from AlgoStateBased ***)


	
	(* === BEGIN API FOR REFACTORING === *)
	
	(* AlgoStateBased. *)
	method private compute_symbolic_successors state = state_space#get_successors_with_combined_transitions state
	

	method private constr_of_state_index state = (state_space#get_state state).px_constraint
	method private get_global_location state = state_space#get_location (state_space#get_global_location_index state)

	(* AlgoStateBased.create_initial_state *)
	method private get_initial_state_index = state_space#get_initial_state_index

	(* Computes the predecessor zone of current_zone using edge *)
	method private predecessor_nnconvex edge current_zone = 
		let {state; transition; _} = edge in 
		let guard = state_space#get_guard model state transition in
		let pred_zone = self#constr_of_state_index state in 
		let constraints = List.map (fun z -> 
			(* TODO : Become independent on DeadlockExtra  - ie. make general method for convex pred *)
			let pxd_pred = DeadlockExtra.dl_predecessor state_space state pred_zone guard z transition in 	
			let px_pred = LinearConstraint.pxd_hide_discrete_and_collapse pxd_pred in 
			LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_pred
			) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint current_zone in 
		List.fold_left (|||) (bot ()) constraints



	(* === END API FOR REFACTORING === *)

	val init_losing_zone_changed = ref false 
	
	(* Initialize the Winning and Depend tables with our model - only affects printing information in terminal *)
	method private initialize_tables () = 
		WinningZone.model := Some model;
		LosingZone.model := Some model;
		Depends.model := Some model

	(* Edges from a symbolic state *)
	method private get_edges state = 
		let successors = self#compute_symbolic_successors state in
		let action_state_list = List.map (fun (transition, state') ->
			let action = StateSpace.get_action_from_combined_transition model transition in
			{state; action; transition; state'}, Unexplored
			) successors in
		action_state_list

	(* Edges from a symbolic state as a queue *)
	method private get_edge_queue state_index =
		let queue = Queue.create () in 
		let add elem = Queue.add elem queue in 
		List.iter add @@ self#get_edges state_index;
		queue	

	(* Methods for getting (un)controllable edges - should be replaced at some point to actually use proper syntax of imitator *)
	method private get_controllable_edges = self#get_edges >> List.map fst >> List.filter (fun e -> model.is_controllable_action e.action)
	method private get_uncontrollable_edges = self#get_edges >> List.map fst >> List.filter (fun e -> not @@ model.is_controllable_action e.action)

	(* Whether or not a state is accepting  *)
	method private matches_state_predicate state_index =
		let state = (state_space#get_state state_index) in
		(State.match_state_predicate model state_predicate state) 

	(* Decides if a symbolic state is a deadlock state (no outgoing controllable actions). Used for Losing Zones *)
	method private is_dead_lock state = 
		self#get_controllable_edges state = [] && not @@ self#matches_state_predicate state

	(* Negate a zone within a state (corresponds to taking the complement) *)
	method private negate_zone zone state_index = 
			let complete_zone = (self#constr_of_state_index >> nn) state_index in 
			LinearConstraint.px_nnconvex_difference_assign complete_zone zone;
			complete_zone
		
	(* Initial constraint of the automata as a lambda - to reuse it multiple times without mutation *)
	method private initial_constraint = fun _ -> LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint model.initial_constraint 
		
	(* Computes the safe timed predecessors of (nn_convex) zone g avoiding (nn_convex) zone b coming from a state *)
	method private safe_timed_pred (g : LinearConstraint.px_nnconvex_constraint) (b:  LinearConstraint.px_nnconvex_constraint) (state : state_index)  = 
		let nn_of_linear = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint in  
		let (||) = fun a b -> LinearConstraint.px_nnconvex_px_union_assign a b; a in  
		let (--) = fun a b -> LinearConstraint.px_nnconvex_difference_assign a b; a in 
		let (&&) = fun a b -> LinearConstraint.px_intersection_assign a [b]; a in
		let global_location = self#get_global_location state in

		let backward (x : LinearConstraint.px_linear_constraint) =
			let constr_d = LinearConstraint.pxd_of_px_constraint x in 
			apply_time_past global_location constr_d;  
			LinearConstraint.pxd_hide_discrete_and_collapse constr_d
		in

		let safe_time_pred_conv g b = 
			let g_past = backward g in 
			let b_past = backward b in 
			let g_constr = LinearConstraint.px_copy g in
			let b_constr = LinearConstraint.px_copy b in 
			let g_past_minus_b_past = nn_of_linear g_past -- nn_of_linear b_past in 
			let g_intersect_b_past_minus_b = nn_of_linear (g_constr && b_past) -- nn_of_linear b_constr in 
			List.fold_left (||) g_past_minus_b_past  
							(List.map (backward) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint g_intersect_b_past_minus_b)
		in
		List.fold_left (fun acc i -> acc ||| i) (bot ())
			(List.map (fun g_i -> 
				let t = List.fold_left (&&&) (nn @@ backward g_i)
					(List.map (fun b_j -> safe_time_pred_conv g_i b_j) @@
					LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint b) in 
				t
			) @@
			LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint g)

	(* Explores forward in order to discover winning states *)
	method private forward_exploration e waiting passed= 
		let {state';_} = e in 
		print_PTG ("I've not seen state " ^ (string_of_int state') ^ " before.	Exploring: ");
		passed#add state';
		(Depends.find state') #<- e;

		if self#matches_state_predicate state' then WinningZone.replace state' @@ (self#constr_of_state_index >> nn) state';
		if self#is_dead_lock state' then LosingZone.replace state' @@ (self#constr_of_state_index >> nn) state'; 
		waiting #<-- (self#get_edge_queue state') ;

		print_PTG ("\n\tAdding successor edges to waiting list. New waiting list: " ^ edge_seq_to_str (Seq.map fst @@ Queue.to_seq waiting) model); 
		
		if not @@ is_empty (WinningZone.find state') then waiting #<- (e, BackpropWinning);
		if not @@ is_empty (LosingZone.find state') then (waiting #<- (e, BackpropLosing); init_losing_zone_changed := true)

	(* Append a status to queue of edges (linear time in size of queue) *)
	method private append_edge_status edge_queue status = 
		Queue.of_seq @@ Seq.map (fun e -> (e, status)) @@ Queue.to_seq @@ edge_queue

	(* General method for backpropagation of winning/losing zones *)
	method private backtrack_gen e find replace to_str good_edge bad_edge precedence callback = 
		let {state; state';_}= e in 
		let get_pred_from_edges default edges zone = 
			List.fold_left (|||) default @@
				List.map (fun edge -> 
					self#predecessor_nnconvex edge (zone edge.state')
				)
			edges
		in
		let g_init = LinearConstraint.px_nnconvex_copy @@ WinningZone.find state in 
		let g = get_pred_from_edges g_init (good_edge state) find in
		let b = get_pred_from_edges (bot ()) (bad_edge state) (fun x -> self#negate_zone (find x) x) in 
		
		if precedence then LinearConstraint.px_nnconvex_difference_assign b g;
		let new_zone = self#safe_timed_pred g b state in 
		print_PTG (Printf.sprintf "\tPred_t(G, B) = %s" @@ LinearConstraint.string_of_px_nnconvex_constraint model.variable_names new_zone);
		if (find state) #!= new_zone then
			begin
				replace state new_zone;
				print_PTG "Updating zones to:";
				print_PTG (to_str ());
				callback state
			end;
		(Depends.find state') #<- e


	(* Backtracks in order to update losing zones in the simulation graph *)	
	method private backtrack_losing e waiting = 
		print_PTG "\tLOSING ZONE PROPAGATION:";
		let callback  state = 
			waiting #<-- (self#append_edge_status (Depends.find state) BackpropLosing);
			if state = self#get_initial_state_index then init_losing_zone_changed := true 
		in 
		self#backtrack_gen e LosingZone.find LosingZone.replace LosingZone.to_str 
											 self#get_uncontrollable_edges self#get_controllable_edges true
											 callback



	(* Backtracks in order to update winning zones in the simulation graph *)	
	method private backtrack_winning e waiting =
		print_PTG "\tWINNING ZONE PROPAGATION:"; 
		let callback  state = 
			waiting #<-- (self#append_edge_status (Depends.find state) BackpropWinning);
		in 
		self#backtrack_gen e WinningZone.find WinningZone.replace WinningZone.to_str 
											 self#get_controllable_edges self#get_uncontrollable_edges false
											 callback

	(* Initial state is lost if initial constraint is included in losing zone *)
	method private init_is_lost init =
		LinearConstraint.px_nnconvex_constraint_is_leq (self#initial_constraint ()) (LosingZone.find init)

	(* Initial state is exact if winning and losing zone covers initial zone  *)
	method private init_is_exact init = 
		let init_zone_nn = nn @@ self#constr_of_state_index init in 
		let winning_and_losing_zone = LinearConstraint.px_nnconvex_copy @@ WinningZone.find init ||| LosingZone.find init in
		LinearConstraint.px_nnconvex_constraint_is_leq init_zone_nn winning_and_losing_zone
	
	(* Returns true if the algorithm should terminate, depending on the criteria for termination *)
	method private termination_criteria waiting init = 
		let queue_empty = Queue.is_empty waiting in
		let property = Input.get_property() in
		let complete_synthesis = (property.synthesis_type = Synthesis) in
		
		let init_lost, init_exact = 
			match !init_losing_zone_changed, complete_synthesis with
			| true, true -> init_losing_zone_changed := false; self#init_is_lost init, self#init_is_exact init
			| true, false -> init_losing_zone_changed := false; self#init_is_lost init, false
			| false, _ -> false, false
		in

		init_lost || queue_empty ||
		if complete_synthesis then
			init_exact
		else
			LinearConstraint.px_nnconvex_constraint_is_leq (self#initial_constraint ()) (WinningZone.find init)



	(* Computes the parameters for which a winning strategy exists and saves the result in synthesized_constraint *)
	method private compute_PTG = 
		self#initialize_tables();

		(* === ALGORITHM INITIALIZATION === *)
		let init = self#get_initial_state_index in 
		
		let passed = new State.stateIndexSet in 
		passed#add init;
		let waiting = self#get_edge_queue init in 

		(* If goal is init then initial winning zone is it's own constraint*)
		if self#matches_state_predicate init then
			WinningZone.replace init @@ (self#constr_of_state_index >> nn) init;

		(* === ALGORITHM MAIN LOOP === *)
		while (not @@ self#termination_criteria waiting init) do
			print_PTG ("\nEntering main loop with waiting list: " ^ edge_seq_to_str (Seq.map fst @@ Queue.to_seq waiting) model);
			let e, edge_status = Queue.pop waiting in 			
			print_PTG (Printf.sprintf "I choose edge: \027[92m %s \027[0m" (edge_to_str e model));
			if not @@ passed#mem e.state' then  
				self#forward_exploration e waiting passed
			else
				match edge_status with
					| Unexplored -> 
						self#backtrack_winning e waiting;
						self#backtrack_losing e waiting
					| BackpropWinning -> 
						self#backtrack_winning e waiting
					| BackpropLosing -> 
						self#backtrack_losing e waiting
		done;
		print_PTG "After running AlgoPTG I found these winning zones:";
		print_PTG (WinningZone.to_str ());

		print_PTG "And these losing zones: ";
		print_PTG (LosingZone.to_str());

		let winning_parameters = project_params (self#initial_constraint () &&& WinningZone.find init) in
		synthesized_constraint <- winning_parameters

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		self#compute_PTG; 

		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)

		(* Retrieve the property *)
		let abstract_property = Input.get_property() in

		(* Projecting onto SOME parameters if required *)
		(*** NOTE: Useless test as we are in EF, so there is a property ***)
		let result =
			match abstract_property.projection with
				(* No projection: copy the initial p constraint *)
				| None -> synthesized_constraint
				(* Project *)
				| Some parameters ->
					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "Projecting the bad constraint onto some of the parameters.";
						self#print_algo_message Verbose_medium "Before projection:";
						print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names synthesized_constraint);
					);

					(*** TODO! do only once for all… ***)
					let all_but_projectparameters = list_diff model.parameters parameters in

					(* Eliminate other parameters *)
					let projected_synthesized_constraint = LinearConstraint.p_nnconvex_hide all_but_projectparameters synthesized_constraint in

					(* Print some information *)
					if verbose_mode_greater Verbose_medium then(
						self#print_algo_message Verbose_medium "After projection:";
						print_message Verbose_medium (LinearConstraint.string_of_p_nnconvex_constraint model.variable_names projected_synthesized_constraint);
					);

					(* Return *)
					projected_synthesized_constraint
		in

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		(* Return the result *)
		Single_synthesis_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the desired states *)
			result				= Good_constraint (result, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint guaranteeing the existence of a winning strategy";

			(* Explored state space *)
			state_space			= state_space;

			(* Total computation time of the algorithm *)
			computation_time	= time_from start_time;

			(* Termination *)
			termination			= termination_status;
		}


	
(************************************************************)
(************************************************************)
end;;
(************************************************************)
(************************************************************)
