(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * 
 * Module description: Parametric timed game with reachability condition
 * 
 * File contributors : Mikael Bisgaard Dahlsen-Jensen, Étienne André
 * Created           : 2022/11/29
 *
 ************************************************************)


(************************************************************)
(************************************************************)
(* Modules *)
(************************************************************)
(************************************************************)
open ImitatorUtilities
open AbstractModel
open AbstractProperty
open Result
open AlgoGeneric
open State
open DefaultHashTable


let nn_of_lin = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint

type timestamp = int

type item = 
| EXPLORE of state_index
| UPDATE of {state_index : state_index; timestamp: timestamp}


type strategy_action = 
  | Wait
  | Action of {action_index: Automaton.action_index; transition: StateSpace.combined_transition; dst: DiscreteState.global_location}


let string_of_state_index state_space model state_index = 
	let location = Array.get (DiscreteState.get_locations ((state_space#get_state state_index).global_location)) 0 in
	let location_name = model.location_names 0 location in
	Printf.sprintf "s%d/loc %s" state_index location_name

let item_to_str = fun model state_space item -> 
	match item with 
	| EXPLORE state_index -> 
		Printf.sprintf "EXPLORE(%s)" (string_of_state_index state_space model state_index)
	| UPDATE {state_index;_} ->
		Printf.sprintf "UPDATE(%s)" (string_of_state_index state_space model state_index)
	
	


let item_list_to_str list model state_space = 
	"[" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (item_to_str model state_space) list) ^ "]"

class stateUnionZoneMap = 
[state_index,  LinearConstraint.px_nnconvex_constraint] defaultHashTable 
(fun _ -> LinearConstraint.false_px_nnconvex_constraint ())

class locationUnionZoneMap = 
[DiscreteState.global_location, LinearConstraint.px_nnconvex_constraint] defaultHashTable
(fun _ -> LinearConstraint.false_px_nnconvex_constraint ())

class dependsMap =
[state_index, State.stateIndexSet] defaultHashTable
(fun _ -> new State.stateIndexSet)

class timeStampMap = 
[state_index, int] defaultHashTable
(fun _ -> 0)

(* State in or out of state space *)
type ptg_state = 
| InSP of state_index
| NotInSP of State.state

class virtual stateSpacePTG  = object(self)
	val mutable state_space : StateSpace.stateSpace = new StateSpace.stateSpace 0
	method state_space = state_space
	method virtual initialize_state_space : unit -> unit
	method virtual compute_symbolic_successors : state_index -> state_index list
	method virtual get_partioned_edges : state_index -> (StateSpace.combined_transition * ptg_state) list * (StateSpace.combined_transition * ptg_state) list
	method virtual unexplored_successors : int
	method virtual merge_mapping : state_index -> state_index
	method virtual merge_occured : bool
	method virtual passed_states : State.stateIndexSet
	initializer 
		self#initialize_state_space ()
end

let add_transitions_and_states_to_state_space state_space transitions_and_states comparison_operator callback = 
	List.filter_map (fun (transition, s) -> 
		let addition_result =  state_space#add_state comparison_operator None s in 
		callback addition_result transition
	) transitions_and_states

class stateSpacePTG_OTF model options = object(self)
	inherit stateSpacePTG
	method unexplored_successors = 0
	val including_check = 
	options#comparison_operator = AbstractAlgorithm.Double_inclusion_check || 
	options#comparison_operator = AbstractAlgorithm.Including_check ||
	options#comparison_operator = AbstractAlgorithm.Strong_Double_Inclusion_check
	
	(* Explored: Internal set keeping track of states have had their successors computed *)
	val explored_states = new State.stateIndexSet
	
	(* Passed: Exposed to and modified by algorithm. Things in here might not be explored but they have been queued for exploration
		Modified by state space in case of successful including checks *)
	val passed_states = new State.stateIndexSet
	method passed_states = passed_states

	(* Merge mapping: Mapping from states removed by merging (strong including check) to their representative state *)
	val merge_mapping = Hashtbl.create 20
	method merge_mapping state_index = try Hashtbl.find merge_mapping state_index with Not_found -> state_index

	(* Merge occured: Only true if a merge by including check has happened in the last state space expansion *)
	val mutable merge_occured = false
	method merge_occured = if merge_occured then (merge_occured <- false; true) else false

	(* Optimization: Only recompute successors in updates IF an included check has succeeded *)
	val recompute_successors = new State.stateIndexSet 
	method initialize_state_space () = 		
		let state = AlgoStateBased.create_initial_state options model false in
		let _ = state_space#add_state AbstractAlgorithm.No_check None state in ()
	method private compute_symbolic_successors_with_transitions source_state_index = 
		if explored_states#mem source_state_index then 
			state_space#get_successors_with_combined_transitions source_state_index
		else 
		begin
			explored_states#add source_state_index;
			let state = state_space#get_state source_state_index in 
			let successors = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model state in
			add_transitions_and_states_to_state_space state_space successors options#comparison_operator 
			(fun addition_result transition -> 
				match addition_result with 
				(* Including check *)
				| State_replacing state_index ->
					state_space#add_transition (source_state_index, transition, state_index);
				  recompute_successors#add state_index;
					explored_states#remove_or_do_nothing state_index;
					passed_states#remove_or_do_nothing state_index;
					Some (transition, state_index)
					
				| New_state state_index
				(* Inclusion check *)
				| State_already_present state_index -> 
					state_space#add_transition (source_state_index, transition, state_index);
					Some (transition, state_index)

				(* Strong including check *)
				| State_replacing_several (state_index, eaten_states) -> 
					state_space#add_transition (source_state_index, transition, state_index);
					recompute_successors#add state_index;
					explored_states#remove_or_do_nothing state_index;
					passed_states#remove_or_do_nothing state_index;
					merge_occured <- true;

					List.iter (fun x -> Hashtbl.add merge_mapping x state_index) eaten_states; 
					if verbose_mode_greater Verbose_low then 
						print_message Verbose_low (Printf.sprintf "STRONG INCLUSION HAPPENED: ate %d states" @@ List.length eaten_states);
					Some (transition, state_index)
			)
		end
	method compute_symbolic_successors source_state_index = 
		List.map snd (self#compute_symbolic_successors_with_transitions source_state_index)
	method get_partioned_edges state_index = 
		if including_check && recompute_successors#mem state_index then 
			(recompute_successors#remove state_index;
			let state = state_space#get_state state_index in 
			let successors = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model state in
			(List.partition_map (fun (transition, state) -> 
				let edge = transition, NotInSP state in 
				let action = StateSpace.get_action_from_combined_transition model transition in 
				if model.is_controllable_action action then Left edge else Right edge)
			successors))
		else
			(let successors = self#compute_symbolic_successors_with_transitions state_index in			
			List.partition_map (fun (transition, state_index) -> 
			let edge = transition, InSP state_index in 
			let action = StateSpace.get_action_from_combined_transition model transition in 
			if model.is_controllable_action action then Left edge else Right edge)
			successors)
end

class stateSpacePTG_full model options = object(self)
	inherit stateSpacePTG
	val explored_states = new State.stateIndexSet
	val passed_states = new State.stateIndexSet
	method passed_states = passed_states
	val mutable unexplored_successors = 0
	method merge_mapping x = x
	method merge_occured = false
	method unexplored_successors = unexplored_successors
	method initialize_state_space () = 		
		let state = AlgoStateBased.create_initial_state options model false in
		let _ = state_space#add_state AbstractAlgorithm.No_check None state in
		let process_successors_from_state_index source_state_index = 
			let state = state_space#get_state source_state_index in 
			let successors = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model state in
			add_transitions_and_states_to_state_space state_space successors options#comparison_operator 
			(fun addition_result transition -> 
				match addition_result with 
				| New_state new_state_index
				| State_replacing new_state_index
				| State_already_present new_state_index -> 
					state_space#add_transition (source_state_index, transition, new_state_index);
					if explored_states#mem new_state_index then 
						None
					else 
					(explored_states#add new_state_index;
					Some new_state_index)
				| State_replacing_several (state_index, _) -> 
					state_space#add_transition (source_state_index, transition, state_index);
					if explored_states#mem state_index then 
						None
					else 
					(explored_states#add state_index;
					Some state_index)
			)
		in

		let depth_limit = match options#depth_limit with
			| Some d -> d
			| None -> -1
		in 

		let rec bfs unexplored_state_indices depth = 
			let unexplored_state_indices' = List.fold_left (fun acc state_index -> 
				(process_successors_from_state_index state_index) @ acc) [] unexplored_state_indices in 
			if depth = depth_limit then
				unexplored_successors <- List.length unexplored_state_indices' 
			else if unexplored_state_indices' = [] then () else bfs unexplored_state_indices' (depth+1)
		in
		let initial_state_index = state_space#get_initial_state_index in 
		explored_states#add initial_state_index;
		bfs [initial_state_index] 1;

	method compute_symbolic_successors source_state_index = 
		state_space#get_successors_with_combined_transitions source_state_index |> List.map snd
	method get_partioned_edges state_index =
		let successors_with_transitions = state_space#get_successors_with_combined_transitions state_index in 
		List.partition_map (fun (transition, state_index) -> 
			let edge = transition, InSP state_index in 
			let action = StateSpace.get_action_from_combined_transition model transition in 
			if model.is_controllable_action action then Left edge else Right edge)
		successors_with_transitions
end



class virtual ['a] nextItem = object
	method virtual add : 'a -> unit
	method virtual extract : 'a
	method virtual is_empty : bool
	method virtual to_list : 'a list
	method virtual length : int
	method virtual add_all : 'a list -> unit
	method virtual unexplored_successors : int
	method virtual apply_merge : (state_index -> state_index) -> unit
end


(* TODO: possible to speedup with some fancy path compression (have to use hashtbl directly then. )*)
let rec lookup_merge_map merge_map state_index = 
	if merge_map state_index = state_index then state_index else 
		let merger_state_index = merge_map state_index in 
		lookup_merge_map merge_map merger_state_index

class nextItem_single_queue = object
	inherit ([item] nextItem)
	val mutable queue = Queue.create ()
	method add e = Queue.add e queue
	method extract = Queue.pop queue
	method is_empty = Queue.is_empty queue
	method to_list = List.of_seq (Queue.to_seq queue)
	method length = Queue.length queue
	method add_all list = List.iter (fun e -> Queue.add e queue) list
	method unexplored_successors = 0
	method apply_merge merge_mapping = 
		let new_queue = Queue.create () in 
		let represented_explore = new State.stateIndexSet in
		let represented_update = new State.stateIndexSet in
		Seq.iter (fun item -> 
			match item with 
			| EXPLORE state_index -> 
				let merger_state_index = lookup_merge_map merge_mapping state_index in
				if merger_state_index = state_index then 
					Queue.add item new_queue
				else if not (represented_explore#mem merger_state_index) then
					(Queue.add (EXPLORE merger_state_index) new_queue;
					represented_explore#add merger_state_index)
			| UPDATE {state_index;timestamp} -> 
				let merger_state_index = lookup_merge_map merge_mapping state_index in
				if merger_state_index = state_index then 
					Queue.add item new_queue
				else if not (represented_update#mem merger_state_index) then 
					Queue.add (UPDATE {state_index = merger_state_index; timestamp}) new_queue;
					represented_update#add merger_state_index
				
		)
		(Queue.to_seq queue);
		queue <- new_queue
end

type phase = Initial | Exploring | Updating
class nextItem_frontier (init_depth : int) (explore_depth : int) (update_depth : int) (total_depth_limit : int) = object(self)
	inherit ([item] nextItem)
	val mutable explore = []
	val mutable explore' = []
	val mutable update = []
	val mutable update' = []
	val mutable phase = Initial
	val mutable depth = explore_depth - init_depth
	val mutable total_depth = 0
	val mutable unexplored_successors = 0
	method add e = match e with 
	| EXPLORE _ -> if total_depth != total_depth_limit then explore' <- e::explore' else unexplored_successors <- unexplored_successors + 1
	| UPDATE _ -> update' <- e::update'
	method extract = 
		print_message Verbose_experiments (Printf.sprintf "Explore: %d\tExplore': %d\tUpdate: %d\tUpdate': %d\t Frontier depth: %d\t Phase: %s\tTotal exploration depth: %d" 
		(List.length explore) (List.length explore') (List.length update) (List.length update') 
		(match phase with Initial -> depth - explore_depth + init_depth | _ -> depth)
		(match phase with Exploring -> "Exploring" | Initial -> "Initial Exploring" |_ -> "Updating")
		total_depth);

		let swap_phase () = 
			(match phase with  
			| Initial -> if update' = [] then phase <- Exploring else phase <- Updating
			| Exploring -> if update' != [] then phase <- Updating
			| Updating -> if explore' != [] then phase <- Exploring);
			depth <- 0
		in
		let increment_depth () = 
			match phase with 
			| Initial | Exploring -> 
				if explore' = [] then swap_phase () else 
					(explore <- explore'; explore' <- []; 
					total_depth <- total_depth + 1;
					depth <- depth + 1;)
			| Updating -> if update' = [] then swap_phase () else 
					(update <- update'; update' <- []; 
					depth <- depth + 1)
		in

		let rec extract_aux () = 
			let depth_limit, curr_list, update_curr_list = match phase with 
			| Initial | Exploring -> explore_depth, explore, fun xs -> explore <- xs 
			| _ -> update_depth, update, fun xs -> update <- xs in 
			match curr_list with 
			| [] -> 
				if depth = depth_limit then
					swap_phase ()
				else 
					increment_depth ();
				extract_aux ()
			| x::xs -> 	update_curr_list xs; x 
		in
		extract_aux ()

	method is_empty = List.length update = 0 && List.length update' = 0 && List.length explore = 0 && List.length explore' = 0
	method to_list = explore @ explore' @ update @ update'
	method length = List.length update + List.length update' + List.length explore + List.length explore'
	method add_all list = List.iter self#add list
	method unexplored_successors = unexplored_successors
	method apply_merge _ = ()
end

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoPTG (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) (state_space_ptg : stateSpacePTG)=
	object (self) inherit algoGeneric model options (*as super*)
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)

	val mutable state_space : StateSpace.stateSpace = state_space_ptg#state_space

	(** Non-necessarily convex constraint storing the parameter synthesis result *)
	val mutable synthesized_constraint : LinearConstraint.p_nnconvex_constraint = LinearConstraint.false_p_nnconvex_constraint ()

	(*------------------------------------------------------------*)
	(* Counters *)
	(*------------------------------------------------------------*)

	val cumulative_pruning_counter = Statistics.create_discrete_counter_and_register "PTG Cumulative pruning count: " Statistics.States_counter Verbose_experiments
	val coverage_pruning_counter = Statistics.create_discrete_counter_and_register "PTG Coverage pruning count: " Statistics.States_counter Verbose_experiments
	val update_pruning_counter = Statistics.create_discrete_counter_and_register "PTG Update pruning count: " Statistics.Global_counter Verbose_experiments
	val update_counter = Statistics.create_discrete_counter_and_register "PTG Total updates count " Statistics.Global_counter Verbose_experiments

	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PTG"


	val mutable termination_status = Regular_termination

	val winningZone = new stateUnionZoneMap
	val forcedMoves = new stateUnionZoneMap
	val depends = new dependsMap
	val lastUpdate = new timeStampMap

	val locationWinningZone = new locationUnionZoneMap
	val locationStrategy = new AlgoPTGStrategyGenerator.locationStrategyMap


	val waiting : item nextItem = 
		let depth_limit = match options#depth_limit with Some d -> d | None -> -1 in
		match options#ptg_picking_strategy with 
		| AbstractAlgorithm.Frontier {init; step; update} -> new nextItem_frontier init step update depth_limit
		| AbstractAlgorithm.SingleQueue -> new nextItem_single_queue
	
	val fresh_timestamp : unit -> timestamp = 
		let ts_r = ref 1 in
		fun () -> let ts = !ts_r in ts_r := ts + 1; ts


	method private constr_of_state_index state = (state_space#get_state state).px_constraint
	method private get_global_location state = state_space#get_location (state_space#get_global_location_index state)

	(* Computes the predecessor zone of current_zone using edge *)
	method private predecessor_nnconvex transition state_index current_zone = 
		let guard = state_space#get_guard model state_index transition in
		let pred_zone = self#constr_of_state_index state_index in 
		let constraints = List.map (fun z -> 
			(* TODO : Become independent on DeadlockExtra  - ie. make general method for convex pred *)
			let pxd_pred = DeadlockExtra.dl_predecessor model state_space state_index pred_zone guard z transition in 	
			let px_pred = LinearConstraint.pxd_hide_discrete_and_collapse pxd_pred in 
			LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_pred
			) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint current_zone in 
		let result = LinearConstraint.false_px_nnconvex_constraint () in 
		List.iter (LinearConstraint.px_nnconvex_union_assign result) constraints;
		result

	val init_winning_zone_changed = ref false

	(* Whether or not a state is accepting  *)
	method private matches_state_predicate state_index =
		let state = (state_space#get_state state_index) in
		(State.match_state_predicate model state_predicate state) 

	(* Negate a zone within a state (corresponds to taking the complement) *)
	method private negate_zone zone state_index = 
			let complete_zone = nn_of_lin (self#constr_of_state_index state_index) in 
			LinearConstraint.px_nnconvex_difference_assign complete_zone zone;
			complete_zone
		
	(* Initial constraint of the automata as a lambda - to reuse it multiple times without mutation *)
	method private initial_constraint = fun _ -> LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint model.initial_constraint 
		

	method private backward (state_index : state_index) (px_linear : LinearConstraint.px_linear_constraint) = 
		let global_location = self#get_global_location state_index in
		let constr_d = LinearConstraint.pxd_of_px_constraint px_linear in 
		AlgoStateBased.apply_time_past model global_location constr_d;
		LinearConstraint.pxd_hide_discrete_and_collapse constr_d
	
	
	(* Computes the safe timed predecessors of (convex) zone g avoiding (convex) zone b coming from a state *)
	method private safe_timed_pred_conv_g_b (state_index : state_index) (g : LinearConstraint.px_linear_constraint) (b:  LinearConstraint.px_linear_constraint) = 
		let result = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint @@ self#backward state_index g in 
		let b_past = self#backward state_index b in 
		let b_past_nn = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint b_past in 
		let g_copy = LinearConstraint.px_copy g in
		LinearConstraint.px_nnconvex_difference_assign result b_past_nn;

		LinearConstraint.px_intersection_assign g_copy [b_past];
		let g_copy_nn = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint g_copy in 

		LinearConstraint.px_nnconvex_difference_assign g_copy_nn (LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint b);

		let g_intersect_b_past_minus_b_past_zones = List.map (self#backward state_index) (LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint g_copy_nn) in 

		List.iter (fun px_linear -> LinearConstraint.px_nnconvex_px_union_assign result px_linear) g_intersect_b_past_minus_b_past_zones;

		result
	
	(* Computes the safe timed predecessors of (convex) zone g avoiding (nn_convex) zone b coming from a state *)
	method private safe_timed_pred_conv_g (state_index : state_index) (g : LinearConstraint.px_linear_constraint) (b : LinearConstraint.px_nnconvex_constraint) = 
		let result = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint @@ self#backward state_index g in 

		List.iter (fun b_j -> 
			LinearConstraint.px_nnconvex_intersection_assign result (self#safe_timed_pred_conv_g_b state_index g b_j))
			(LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint b);
		result


	(* Computes the safe timed predecessors of (nn_convex) zone g avoiding (nn_convex) zone b coming from a state *)
	method private safe_timed_pred (state_index : state_index) (g : LinearConstraint.px_nnconvex_constraint) (b : LinearConstraint.px_nnconvex_constraint) = 
		let result = LinearConstraint.false_px_nnconvex_constraint () in 

		List.iter (fun g_i -> LinearConstraint.px_nnconvex_union_assign result (self#safe_timed_pred_conv_g state_index g_i b)) 
			(LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint g);
		result 

	
	(* Compute the forced moves of a state *)
	method private save_forced_moves state_index = 
		let controllable_edges, uncontrollable_edges = state_space_ptg#get_partioned_edges state_index in 
		let uncontrollable_guards = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ List.map (
			fun (transition, _) -> 
				LinearConstraint.pxd_hide_discrete_and_collapse @@ state_space#get_guard model state_index transition) 
				uncontrollable_edges
		in 
		let controllable_guards = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ List.map (
			fun (transition,_) -> 
				LinearConstraint.pxd_hide_discrete_and_collapse @@ state_space#get_guard model state_index transition) 
				controllable_edges
		in 
		let uncontrollable_guards_closed = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ 
			List.map LinearConstraint.close_upper_clocks_px_linear_constraint @@ 
			LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint uncontrollable_guards 
		in
		let controllable_guards_closed = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ 
			List.map LinearConstraint.close_upper_clocks_px_linear_constraint @@ 
			LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint controllable_guards 
		in

		let invariant = self#constr_of_state_index state_index in
		let global_location = (state_space#get_state state_index).global_location in

		(* forced moves are different if location is urgent! *)
		let forced_moves = match AbstractModelUtilities.is_global_location_urgent model global_location with 
			| true -> 
				let forced_moves = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint invariant in

				LinearConstraint.px_nnconvex_intersection_assign forced_moves uncontrollable_guards;
				LinearConstraint.px_nnconvex_difference_assign forced_moves controllable_guards;
				forced_moves
			| false ->
				let inv_bound_in, inv_bound_out = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint invariant in 
				
				LinearConstraint.px_nnconvex_intersection_assign inv_bound_in uncontrollable_guards;
				LinearConstraint.px_nnconvex_intersection_assign inv_bound_out uncontrollable_guards_closed;

				LinearConstraint.px_nnconvex_difference_assign inv_bound_in controllable_guards;
				LinearConstraint.px_nnconvex_difference_assign inv_bound_out controllable_guards_closed; 

				LinearConstraint.px_nnconvex_union_assign inv_bound_in inv_bound_out;
				inv_bound_in
		in
			
		forcedMoves#replace state_index forced_moves;
		if verbose_mode_greater Verbose_medium then 
			print_message Verbose_low (Printf.sprintf "Computed forced moves for state %d: %s" state_index (LinearConstraint.string_of_px_nnconvex_constraint model.variable_names forced_moves))
		

	(* Takes a state index and decides whether to prune (stop exploration of ) its succesors based on the global parameter constraint *)
	method private global_constraint_pruning state_index = 
		if options#cumulative_pruning then 
			let constr = self#constr_of_state_index state_index in 
			let constr_params = LinearConstraint.px_hide_nonparameters_and_collapse constr in 
			let constr_params_nnconvex = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint constr_params in 
			LinearConstraint.p_nnconvex_constraint_is_leq constr_params_nnconvex synthesized_constraint
		else
			false

	(* Take a set of state_index turn it into a list of UPDATE items with fresh timestamps *)
	method private state_set_to_update_items state_set = 
		let timestamp = fresh_timestamp () in 
		List.map (fun state_index -> UPDATE{state_index; timestamp}) state_set#all_elements

	(* Explores forward in order to discover winning states *)
	method private explore state_index =
		if not options#ptg_no_forced_uncontrollables then 
			self#save_forced_moves state_index;

		let coverage_pruning = ref false in 
		if self#matches_state_predicate state_index then 
			begin 
				winningZone#replace state_index (nn_of_lin (self#constr_of_state_index state_index));
				let location = (state_space#get_state state_index).global_location in 
				let winning_zone_loc = locationWinningZone#find location in 
				LinearConstraint.px_nnconvex_px_union_assign winning_zone_loc (self#constr_of_state_index state_index); 
				waiting#add_all (self#state_set_to_update_items (depends#find state_index));
				coverage_pruning := true
			end;

		coverage_pruning := !coverage_pruning && options#coverage_pruning;

		begin 
			match self#global_constraint_pruning state_index, !coverage_pruning with 
				|	true, _ -> 
					cumulative_pruning_counter#increment;
					print_message Verbose_low (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (cumulative)" state_index)
				| _, true -> 
					coverage_pruning_counter#increment;
					print_message Verbose_low (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (coverage)" state_index)
				| _ ->
					let successors = state_space_ptg#compute_symbolic_successors state_index in
					List.iter (fun s -> (depends#find s)#add state_index) successors;
					let found_existing_state = 
						List.fold_left (fun acc succ -> 
						if state_space_ptg#passed_states#mem succ then 
							(print_message Verbose_medium (Printf.sprintf "Already passed state %s before - not adding for exploration" 
							(string_of_state_index state_space model succ));
							true)
						else 
							(waiting#add (EXPLORE succ);
							state_space_ptg#passed_states#add succ;
							acc)
						) false successors
					in 
					if found_existing_state then 
						waiting#add (UPDATE {state_index; timestamp = fresh_timestamp ()});
					if state_space_ptg#merge_occured then 
						(waiting#apply_merge (state_space_ptg#merge_mapping);
						winningZone#merge_keys state_space_ptg#merge_mapping (fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a);
						forcedMoves#merge_keys state_space_ptg#merge_mapping (fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a);
						depends#merge_keys state_space_ptg#merge_mapping (fun a b -> a#union b; a);
						lastUpdate#merge_keys state_space_ptg#merge_mapping min);
					if verbose_mode_greater Verbose_medium then 
						print_message Verbose_medium ("\n\tAdding successor edges to waiting list. New waiting list: " ^ item_list_to_str waiting#to_list model state_space)
		end;


	method private process_convex_winning_move state action bad_zone (winning_move : LinearConstraint.px_linear_constraint) =
		
		let safe_timed_pred = self#safe_timed_pred_conv_g state winning_move bad_zone in
		LinearConstraint.px_nnconvex_px_intersection_assign safe_timed_pred (self#constr_of_state_index state);
		
		let global_location_src = (state_space#get_state state).global_location in

		let current_winning_zone_glob = locationWinningZone#find global_location_src in

		let winning_zone_changed = 
			let current_winning_zone_state = winningZone#find state in
			(* Extend winning zone of STATE with newly found safe timed pred *)
			if not @@ LinearConstraint.px_nnconvex_constraint_is_equal current_winning_zone_state safe_timed_pred then
				(LinearConstraint.px_nnconvex_union_assign current_winning_zone_state safe_timed_pred;
				true)
			else false
		in

		(* Make safe_timed_pred a partition of winning zone of LOCATION *)
		LinearConstraint.px_nnconvex_difference_assign safe_timed_pred current_winning_zone_glob;		

		if not @@ LinearConstraint.px_nnconvex_constraint_is_false safe_timed_pred then
			begin
				let open AlgoPTGStrategyGenerator in 
				(* Extend the winning zone of LOCATION with new partition *)
				LinearConstraint.px_nnconvex_union_assign current_winning_zone_glob safe_timed_pred;

				let strategy_entry = match action with 
				| Action {action_index; transition; dst} -> 
						(* Extend strategy with new partition *)

						ActionEntry {
								action = action_index;
								winning_move;
								transition;
								prioritized_winning_zone = safe_timed_pred;
								destination = dst
						}
				| Wait ->
					(* Extend strategy with a wait entry *)
					WaitEntry {prioritized_winning_zone = safe_timed_pred}
				in
				let strategy = locationStrategy#find global_location_src in 
				strategy := strategy_entry :: !strategy;
			end;
		winning_zone_changed
		


	method private process_nnconvex_winning_move state action bad_zone winning_move = 
		List.fold_left (||) false 
		(List.map (fun g_i -> self#process_convex_winning_move state action bad_zone g_i) 
		(LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint winning_move))


	(* Handle backtracking for a single edge, updating the winning zone and the associated strategy 
		 return true if winning zone was changed otherwise false	 
	*)
	method private backtrack_single_controllable_edge (transition, (dst : ptg_state)) src bad_zone =
		let winning_move, dst_global_location = match dst with 
			| NotInSP {global_location;px_constraint} ->
				let target_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_constraint in 
				LinearConstraint.px_nnconvex_intersection_assign target_zone (locationWinningZone#find global_location);
				self#predecessor_nnconvex transition src target_zone, global_location
			| InSP state_index -> 
				let target_zone = winningZone#find state_index in 
				self#predecessor_nnconvex transition src target_zone, (state_space#get_state state_index).global_location
			
		in
			
		(* Remove bad zone from winning move *)
		LinearConstraint.px_nnconvex_difference_assign winning_move bad_zone;
		let action_index = StateSpace.get_action_from_combined_transition model transition in 
		self#process_nnconvex_winning_move src (Action {action_index;transition;dst = dst_global_location}) bad_zone winning_move

	(* Process a forced move of the environment 
		return true if the winning zone was changed otherwise false *)
	method private process_forced_move state bad_zone forced_move = 
		self#process_nnconvex_winning_move state Wait bad_zone forced_move


	(* Method for backpropagation of winning zones *)
	method private update state_index = 

		(* Compute moves to successors restricted to certain target zones based on the provided function  *)
		let compute_moves_to_succesors init successor_list target_zone_of_state = 
			List.iter (fun (transition, ptg_state) ->
					let target_zone = target_zone_of_state ptg_state in 
					let move_to_target = self#predecessor_nnconvex transition state_index target_zone in
					LinearConstraint.px_nnconvex_union_assign init move_to_target
			) successor_list;
			init
		in 

		let controllable_edges, uncontrollable_edges = state_space_ptg#get_partioned_edges state_index in

		let uncontrollable_part = compute_moves_to_succesors 
			(LinearConstraint.false_px_nnconvex_constraint ())
			uncontrollable_edges
			(function 
				| InSP state_index -> 
					let target_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint (self#constr_of_state_index state_index) in 
					LinearConstraint.px_nnconvex_difference_assign target_zone (winningZone#find state_index);
					target_zone
				| NotInSP {global_location;px_constraint} -> 
					let target_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_constraint in 
					LinearConstraint.px_nnconvex_difference_assign target_zone (locationWinningZone#find global_location);
					target_zone
			)
		in

		let forced_moves_changed_winning_zone = self#process_forced_move state_index uncontrollable_part (forcedMoves#find state_index) in 
		let winning_moves_changed_winning_zone = 
			if options#ptg_no_strategy_generation then 
				let {global_location;px_constraint} = (state_space#get_state state_index) in 

				let controllable_part = compute_moves_to_succesors 
					(LinearConstraint.px_nnconvex_copy @@ winningZone#find state_index)
					controllable_edges
					(function 
						| InSP state_index -> winningZone#find state_index
						| NotInSP {global_location;px_constraint} -> 
							let target_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_constraint in 
							LinearConstraint.px_nnconvex_intersection_assign target_zone (locationWinningZone#find global_location);
							target_zone
					)
				in
				let safe_timed_pred =  self#safe_timed_pred state_index controllable_part uncontrollable_part in 
				LinearConstraint.px_nnconvex_px_intersection_assign safe_timed_pred px_constraint;
				let location_winning_zone = locationWinningZone#find global_location in 
				LinearConstraint.px_nnconvex_union_assign location_winning_zone safe_timed_pred;
				let changed = not (LinearConstraint.px_nnconvex_constraint_is_equal (winningZone#find state_index) safe_timed_pred) in
				if changed then (winningZone#replace state_index safe_timed_pred; true) else false
			else
			List.fold_left (||) false
				(List.map(fun edge -> self#backtrack_single_controllable_edge edge state_index uncontrollable_part) controllable_edges) in 
		if winning_moves_changed_winning_zone || forced_moves_changed_winning_zone then 
		begin
			waiting#add_all (self#state_set_to_update_items (depends#find state_index));
			if state_index = state_space#get_initial_state_index then init_winning_zone_changed := true
		end

	(* Initial state is won if parameter valuations in its winning zone is non-empty *)
	method private init_has_winning_witness =
		init_winning_zone_changed := false;
		not @@ LinearConstraint.p_nnconvex_constraint_is_false synthesized_constraint

	(* Initial state is exact if winning zone covers initial zone  *)
	method private init_is_exact init = 
		init_winning_zone_changed := false;
		let init_zone_nn = nn_of_lin @@ self#constr_of_state_index init in 
		let winning_zone_nn = LinearConstraint.px_nnconvex_copy @@ winningZone#find init in 
		LinearConstraint.px_nnconvex_constraint_is_leq init_zone_nn winning_zone_nn
	
	(* Returns true if the algorithm should terminate, depending on the criteria for termination *)
	method private termination_criteria init = 
		let queue_empty = waiting#is_empty in
		let complete_synthesis = (property.synthesis_type = Synthesis) in

		if !init_winning_zone_changed then 
			(let initial_winning_valuations = 
				let initial = self#initial_constraint () in 
				LinearConstraint.px_nnconvex_intersection_assign initial (winningZone#find init); 
				initial in 
			synthesized_constraint <- LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse initial_winning_valuations);

		let recompute_init_has_winning_witness = not complete_synthesis && !init_winning_zone_changed in  
		let recompute_init_exact = complete_synthesis && !init_winning_zone_changed in

		let init_has_winning_witness = if recompute_init_has_winning_witness then self#init_has_winning_witness else false in 
		let init_exact = if recompute_init_exact then self#init_is_exact init else false in

		let time_out = match options#time_limit with 
			Some time_limit -> ImitatorUtilities.time_from start_time > float_of_int time_limit
			| None -> false
		in

		if time_out then termination_status <- Time_limit (Result.Number state_space#nb_states);

		queue_empty ||	init_exact || init_has_winning_witness || time_out

	method private is_update_relevant state_index timestamp =
		if timestamp > lastUpdate#find state_index then 
			(update_counter#increment; lastUpdate#replace state_index (fresh_timestamp ()); true)
		else 
			(update_pruning_counter#increment; false)


	(* Computes the parameters for which a winning strategy exists and saves the result in synthesized_constraint *)
	method private compute_PTG = 
		(* === ALGORITHM INITIALIZATION === *)
		let initial_state_index = state_space#get_initial_state_index in 

		if not options#ptg_no_forced_uncontrollables then
			self#save_forced_moves initial_state_index;
		
		waiting#add (EXPLORE initial_state_index);
		state_space_ptg#passed_states#add initial_state_index;

		(* If goal is init then initial winning zone is it's own constraint*)
		if self#matches_state_predicate initial_state_index then
			winningZone#replace initial_state_index (nn_of_lin (self#constr_of_state_index initial_state_index));
			init_winning_zone_changed := true;


		(* === ALGORITHM MAIN LOOP === *)
		while (not @@ self#termination_criteria initial_state_index) do
			if verbose_mode_greater Verbose_medium then 
				print_message Verbose_medium ("\nEntering main loop with waiting list: " ^ item_list_to_str waiting#to_list model state_space);
			let item = waiting#extract in 			
			if verbose_mode_greater Verbose_medium then 
				print_message Verbose_medium (Printf.sprintf "Processing item: \027[92m %s \027[0m" (item_to_str model state_space item));
			match item with 
				| EXPLORE state_index -> 
					self#explore state_index
				| UPDATE {state_index; timestamp} -> 
					if self#is_update_relevant state_index timestamp then 
						self#update state_index
		done;

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)	
	method run =

		start_time <- Unix.gettimeofday();
		
		(* Compute the parametric timed game *)
		self#compute_PTG;

		self#print_algo_message_newline Verbose_experiments (
			"Parameter synthesis algorithm completed " ^ after_seconds () ^ "."
		);
		if not @@ options#ptg_no_strategy_generation && not @@ LinearConstraint.p_nnconvex_constraint_is_false synthesized_constraint then
		begin
			if not @@ options#ptg_no_strategy_printing then
			begin 
				AlgoPTGStrategyGenerator.print_strategy
				model 
				~strategy:locationStrategy;
				self#print_algo_message_newline Verbose_experiments (
					"Printed strategy " ^ after_seconds () ^ "."
				);
			end;
		
			AlgoPTGStrategyGenerator.controller_synthesis
			model
			state_space
			options
			locationStrategy
			~callback:(fun () -> 
				self#print_algo_message_newline Verbose_experiments ("Strategy -> Controller algorithm completed " ^ after_seconds () ^ "."));

		end;

		(* Return the result *)
		self#compute_result;

		(* The end *)


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
 & MINWAIT > 5*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		self#print_algo_message_newline Verbose_standard (
			Printf.sprintf "Size of explored state space: %d" (state_space#nb_states);
		);

		(* Projecting onto some parameters if required by the property *)
		let result = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property synthesized_constraint in


		(* In the case of frontier waiting list strategy *)
		if waiting#unexplored_successors != 0 then termination_status <- Depth_limit (Number waiting#unexplored_successors);
		(* In the case of full state space compution (not on the fly) *)
		if state_space_ptg#unexplored_successors != 0 then termination_status <- Depth_limit (Number state_space_ptg#unexplored_successors);

		let soundness = if property.synthesis_type = Synthesis && termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

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
