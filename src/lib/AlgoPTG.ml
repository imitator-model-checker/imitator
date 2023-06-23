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
class algoPTG (model : AbstractModel.abstract_model) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoStateBased model options (*as super*)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)

	(* Methods counters *)
	val counter_add_a_new_state = create_hybrid_counter_and_register "EFsynth.add_a_new_state" States_counter Verbose_experiments


	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method! initialize_variables =
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
	method private process_state (_ : State.state) : bool = true


	
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
			
				(*** NOTE: do NOT perform this test depending on the option ***)
				if options#cumulative_pruning then(
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
	method process_deadlock_state _ = ()
	
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform at the end of the computation of the *successors* of post^n (i.e., when this method is called, the successors were just computed). Nothing to do for this algorithm. *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_post_n (_ : State.state_index list) = ()

	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Check whether the algorithm should terminate at the end of some post, independently of the number of states to be processed (e.g., if the constraint is already true or false) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*** TODO: could be stopped when the bad constraints are equal to the initial p-constraint ***)
	method check_termination_at_post_n = false

	
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
	val init_winning_zone_changed = ref false
	
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
			apply_time_past model global_location constr_d;
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
		if not @@ is_empty (LosingZone.find state') then waiting #<- (e, BackpropLosing)

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
		let g_init = LinearConstraint.px_nnconvex_copy @@ find state in 
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
		let callback state = 
			waiting #<-- (self#append_edge_status (Depends.find state) BackpropLosing);
			if state = self#get_initial_state_index then init_losing_zone_changed := true 
		in 
		self#backtrack_gen e LosingZone.find LosingZone.replace LosingZone.to_str 
											 self#get_uncontrollable_edges self#get_controllable_edges true
											 callback



	(* Backtracks in order to update winning zones in the simulation graph *)	
	method private backtrack_winning e waiting =
		print_PTG "\tWINNING ZONE PROPAGATION:"; 
		let callback state = 
			waiting #<-- (self#append_edge_status (Depends.find state) BackpropWinning);
			if state = self#get_initial_state_index then init_winning_zone_changed := true 		in 
		self#backtrack_gen e WinningZone.find WinningZone.replace WinningZone.to_str 
											 self#get_controllable_edges self#get_uncontrollable_edges false
											 callback

	(* Initial state is lost if initial constraint is included in losing zone *)
	method private init_is_lost init =
		init_losing_zone_changed := false;
		LinearConstraint.px_nnconvex_constraint_is_leq (self#initial_constraint ()) (LosingZone.find init)

	(* Initial state is exact if winning and losing zone covers initial zone  *)
	method private init_is_exact init = 
		init_losing_zone_changed := false;
		init_winning_zone_changed := false;
		let init_zone_nn = nn @@ self#constr_of_state_index init in 
		let winning_and_losing_zone = LinearConstraint.px_nnconvex_copy @@ WinningZone.find init ||| LosingZone.find init in
		LinearConstraint.px_nnconvex_constraint_is_leq init_zone_nn winning_and_losing_zone
	
	(* Returns true if the algorithm should terminate, depending on the criteria for termination *)
	method private termination_criteria waiting init = 
		let queue_empty = Queue.is_empty waiting in
		let property = Input.get_property() in
		let complete_synthesis = (property.synthesis_type = Synthesis) in

		let recompute_init_lost = !init_losing_zone_changed in 
		let recompute_init_exact = complete_synthesis && (!init_losing_zone_changed || !init_winning_zone_changed) in

		let init_lost = if recompute_init_lost then self#init_is_lost init else false in
		let init_exact = if recompute_init_exact then self#init_is_exact init else false in

		queue_empty || init_lost ||
		if complete_synthesis then
			init_exact
		else
			not @@ is_empty (self#initial_constraint () &&& WinningZone.find init)



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

		(* If init is deadlock then initial losing zone is it's own constraint*)
		if self#matches_state_predicate init then
			(LosingZone.replace init @@ (self#constr_of_state_index >> nn) init; init_losing_zone_changed := true);

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
		let soundness = if termination_status = Regular_termination && (Input.get_property()).synthesis_type = Synthesis then Constraint_exact else Constraint_maybe_under in

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
