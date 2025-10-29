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

(* Pretty printing *)
let wrap code s =
  Printf.sprintf "\027[%sm%s\027[0m" code s

let red    = wrap "91"
let green  = wrap "92"
let yellow = wrap "93"
let blue   = wrap "94"
let magenta = wrap "95"
let cyan   = wrap "96"

let bold = wrap "1"

let format_zone_string (string : string) = 
  let b = Buffer.create 10 in
  String.iter (fun c -> if c == '\n' then Buffer.add_char b ' ' else Buffer.add_char b c) string;
  String.trim @@ Buffer.contents b
let string_of_zone variable_names px_constraint = 
	px_constraint |>
	LinearConstraint.string_of_px_linear_constraint variable_names |>
	format_zone_string

let string_of_nnc_zone variable_names px_constraint = 
	px_constraint |>
	LinearConstraint.string_of_px_nnconvex_constraint variable_names |>
	format_zone_string

let string_of_state_index ?(include_zone = false) state_space model state_index  = 
	let state = (state_space#get_state state_index) in 
	let location = Array.get (DiscreteState.get_locations (state.global_location)) 0 in
	let location_name = model.location_names 0 location in
	if include_zone then 
		Printf.sprintf "s%d = loc %s | %s" state_index location_name 
		(string_of_zone model.variable_names state.px_constraint)
	else Printf.sprintf "s%d/loc %s" state_index location_name

let item_to_str = fun ?(include_zone = false) model state_space item -> 
	let explore : ('a, 'b, 'c) format = if include_zone then "[EXPLORE] %s" else "EXPLORE(%s)" in 
	let update : ('a, 'b, 'c) format = if include_zone then "[UPDATE] %s" else "UPDATE(%s)" in 
	match item with 
	| EXPLORE state_index -> 
		blue @@ Printf.sprintf explore (string_of_state_index state_space model state_index ~include_zone)
	| UPDATE {state_index;_} ->
		magenta @@ Printf.sprintf update (string_of_state_index state_space model state_index ~include_zone)
	
let item_list_to_str list model state_space = 
	"[" ^ OCamlUtilities.string_of_list_of_string_with_sep ", " (List.map (item_to_str model state_space) list) ^ "]"

let print_delta_list_with_reason model state_space items reason = 
	if items <> [] then
	print_message Verbose_low (Printf.sprintf "\tQ+=%s\n\tReason: %s" 
	(item_list_to_str items model state_space) @@ reason);



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

let zone_of_ptg_state state_space = function
	| InSP state_index -> let state : State.state = (state_space#get_state state_index) in state.px_constraint
	| NotInSP state -> state.px_constraint

class virtual stateSpacePTG  = object(self)
	val mutable state_space : StateSpace.stateSpace = new StateSpace.stateSpace 0
	method state_space = state_space
	method virtual initialize_state_space : unit -> unit
	method virtual compute_symbolic_successors : state_index -> state_index list
	method virtual get_partioned_edges : state_index -> (StateSpace.combined_transition * ptg_state) list * (StateSpace.combined_transition * ptg_state) list
	method virtual unexplored_successors : int
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
	options#ptg_abstraction = AbstractAlgorithm.Convex_Hull

	val reexploration_counter = Statistics.create_hybrid_counter_and_register "PTG Total reexplorations: " Statistics.States_counter Verbose_experiments

	
	(* Explored: Internal set keeping track of states have had their successors computed *)
	val explored_states = new State.stateIndexSet
	
	(* Passed: Exposed to and modified by algorithm. Things in here might not be explored but they have been queued for exploration
		Modified by state space in case of successful including checks *)
	val passed_states = new State.stateIndexSet
	method passed_states = passed_states

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
			)
		end
	method compute_symbolic_successors source_state_index = 
		List.map snd (self#compute_symbolic_successors_with_transitions source_state_index)
	method get_partioned_edges state_index = 
		if including_check && recompute_successors#mem state_index then 
			(reexploration_counter#increment;
			reexploration_counter#start;
			recompute_successors#remove state_index;
			let state = state_space#get_state state_index in 
			let successors = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model state in
			let edges = (List.partition_map (fun (transition, state) -> 
				let edge = transition, NotInSP state in 
				let action = StateSpace.get_action_from_combined_transition model transition in 
				if model.is_controllable_action action then Left edge else Right edge)
			successors) in 
			reexploration_counter#stop;
			edges)
		else
			(let successors = self#compute_symbolic_successors_with_transitions state_index in			
			List.partition_map (fun (transition, state_index) -> 
			let edge = transition, InSP state_index in 
			let action = StateSpace.get_action_from_combined_transition model transition in 
			if model.is_controllable_action action then Left edge else Right edge)
			successors)
end

class stateSpacePTG_full model options = object
	inherit stateSpacePTG
	val explored_states = new State.stateIndexSet
	val passed_states = new State.stateIndexSet
	method passed_states = passed_states
	val mutable unexplored_successors = 0
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

type merge_dependant_datastructures = {lastUpdate : timeStampMap; depends : dependsMap; winningZone : stateUnionZoneMap; forcedMoves : stateUnionZoneMap}

class virtual ['a] nextItem = object
	method virtual add : 'a -> unit
	method virtual extract : merge_dependant_datastructures -> 'a
	method virtual is_empty : bool
	method virtual to_list : 'a list
	method virtual length : int
	method virtual add_all : 'a list -> unit
	method virtual unexplored_successors : int
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
	method extract _ = Queue.pop queue
	method is_empty = Queue.is_empty queue
	method to_list = List.of_seq (Queue.to_seq queue)
	method length = Queue.length queue
	method add_all list = List.iter (fun e -> Queue.add e queue) list
	method unexplored_successors = 0
end

type phase = Initial | Exploring | Updating
class nextItem_frontier (state_space : StateSpace.stateSpace) (options : Options.imitator_options) (init_depth : int) (explore_depth : int) (update_depth : int) (total_depth_limit : int) = object(self)
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
	method extract {lastUpdate; depends; winningZone; forcedMoves} = 
		print_message Verbose_experiments (Printf.sprintf "Explore: %d\tExplore': %d\tUpdate: %d\tUpdate': %d\t Frontier depth: %d\t Phase: %s\tTotal exploration depth: %d" 
		(List.length explore) (List.length explore') (List.length update) (List.length update') 
		(match phase with Initial -> depth - explore_depth + init_depth | _ -> depth)
		(match phase with Exploring -> "Exploring" | Initial -> "Initial Exploring" |_ -> "Updating")
		total_depth);

		let attempt_merge candidates = 
			match options#merge_algorithm with 
			| AbstractAlgorithm.Merge_none -> candidates
			| _ -> 
				let merge_mapping_tbl = Hashtbl.create 100 in
				
				let candidate_indices = List.map (fun item -> match item with EXPLORE i -> i | _ -> raise (Exceptions.InternalError "Error in extraction: Candidates cannot be UPDATE items.")) candidates in 
													(* merger <- mergee *)
				let merge_callback merger mergee = Hashtbl.add merge_mapping_tbl mergee merger in 
				
				(* Call merge algorithm and build the merge mapping table via callback*)
				let merged_indices = state_space#merge candidate_indices merge_callback in

				let merge_mapping state_index = try Hashtbl.find merge_mapping_tbl state_index with Not_found -> state_index in 
				
				(* Merge all table datastructures *)
				winningZone#merge_keys merge_mapping (fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a);
				forcedMoves#merge_keys merge_mapping (fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a);
				depends#merge_keys merge_mapping (fun a b -> a#union b; a);
				lastUpdate#merge_keys merge_mapping min;

				
				(* Apply merge map to the queued updates as well *)
				update' <- List.map (fun item -> 
					match item with 
					| EXPLORE _ -> raise (Exceptions.InternalError "Error in extraction: Update list cannot contain EXPLORE items")
					| UPDATE {state_index; timestamp} -> 
							let merger = lookup_merge_map merge_mapping state_index in 
							UPDATE {state_index = merger; timestamp}
					) update';

				List.map (fun i -> EXPLORE i) merged_indices
		in
				

		let swap_phase () = 
			(match phase with  
			| Initial -> if update' = [] then phase <- Exploring else phase <- Updating
			| Exploring -> if update' != [] then phase <- Updating
			| Updating -> if explore' != [] then phase <- Exploring);
			depth <- 0
		in
		let next_layer () = 
			match phase with 
			| Initial | Exploring -> 
				if explore' = [] then swap_phase () else 
					(explore <- attempt_merge (List.rev explore'); explore' <- []; 
					total_depth <- total_depth + 1;
					depth <- depth + 1;)
			| Updating -> if update' = [] then swap_phase () else 
					(update <- List.rev update'; update' <- []; 
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
					next_layer ();
				extract_aux ()
			| x::xs -> 	update_curr_list xs; x 
		in
		extract_aux ()

	method is_empty = List.length update = 0 && List.length update' = 0 && List.length explore = 0 && List.length explore' = 0
	method to_list = explore @ explore' @ update @ update'
	method length = List.length update + List.length update' + List.length explore + List.length explore'
	method add_all list = List.iter self#add list
	method unexplored_successors = unexplored_successors
end

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoPTG (model : AbstractModel.abstract_model) (property : AbstractProperty.abstract_property) (options : Options.imitator_options) (state_predicate : AbstractProperty.state_predicate) ?state_predicate_avoid (state_space_ptg : stateSpacePTG)=
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

	method private print_delta_list_with_reason = print_delta_list_with_reason model state_space

	val waiting : item nextItem = 
		let depth_limit = match options#depth_limit with Some d -> d | None -> -1 in
		match options#ptg_picking_strategy with 
		| AbstractAlgorithm.Frontier {init; step; update} -> new nextItem_frontier (state_space_ptg#state_space) options init step update depth_limit
		| AbstractAlgorithm.SingleQueue -> new nextItem_single_queue
	
	val fresh_timestamp : unit -> timestamp = 
		let ts_r = ref 1 in
		fun () -> let ts = !ts_r in ts_r := ts + 1; ts


	method private constr_of_state_index state = (state_space#get_state state).px_constraint
	method private get_global_location state = state_space#get_location (state_space#get_global_location_index state)

	method private predecessor_linear_general transition state_index guard pred_zone current_zone = 
		let pxd_pred = DeadlockExtra.dl_predecessor model state_space state_index pred_zone guard current_zone transition in 	
		LinearConstraint.pxd_hide_discrete_and_collapse pxd_pred

	method private predecessor_linear transition state_index current_zone =
		let guard = state_space#get_guard model state_index transition in
		let pred_zone = self#constr_of_state_index state_index in 
		self#predecessor_linear_general transition state_index guard pred_zone current_zone

	(* Computes the predecessor zone of current_zone using edge *)
	method private predecessor_nnconvex transition state_index current_zone = 
		let guard = state_space#get_guard model state_index transition in
		let pred_zone = self#constr_of_state_index state_index in 
		current_zone |> 
		LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint |>
		List.map (self#predecessor_linear_general transition state_index guard pred_zone) |>
		LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints

	val init_winning_zone_changed = ref false

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
		let uncontrollable_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ List.map (
			fun (transition, succ_ptg_state) -> 
				self#predecessor_linear transition state_index @@ zone_of_ptg_state state_space succ_ptg_state) 
				uncontrollable_edges in 
		let controllable_zone = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ List.map (
			fun (transition, succ_ptg_state) ->
				self#predecessor_linear transition state_index @@ zone_of_ptg_state state_space succ_ptg_state) 
				controllable_edges
		in 
		let uncontrollable_zone_closed = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ 
			List.map LinearConstraint.close_upper_clocks_px_linear_constraint @@ 
			LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint uncontrollable_zone 
		in
		let controllable_zone_closed = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraints @@ 
			List.map LinearConstraint.close_upper_clocks_px_linear_constraint @@ 
			LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint controllable_zone 
		in

		let global_location = (state_space#get_state state_index).global_location in

		let invariant = LinearConstraint.pxd_hide_discrete_and_collapse @@ State.compute_invariant model global_location in 

		(* forced moves are different if location is urgent! *)
		let forced_moves = match AbstractModelUtilities.is_global_location_urgent model global_location with 
			| true -> 
				let forced_moves = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint invariant in

				LinearConstraint.px_nnconvex_intersection_assign forced_moves uncontrollable_zone;
				LinearConstraint.px_nnconvex_difference_assign forced_moves controllable_zone;
				forced_moves
			| false ->
				let inv_bound_in, inv_bound_out = LinearConstraint.precise_temporal_upper_bound_px_linear_constraint invariant in 
				
				if verbose_mode_greater Verbose_low then 
					print_message Verbose_low 
					(Printf.sprintf "\tFM Computation:\n\t\tunctrl: %s\n\t\tunctrl_closed: %s\n\t\tctrl: %s\n\t\tctrl_closed: %s\n\t\tinv: %s\n\t\tinv_bound_in: %s\n\t\tinv_bound_out: %s"
					(red @@ string_of_nnc_zone model.variable_names uncontrollable_zone)
					(red @@ string_of_nnc_zone model.variable_names uncontrollable_zone_closed)
					(green @@ string_of_nnc_zone model.variable_names controllable_zone)
					(green @@ string_of_nnc_zone model.variable_names controllable_zone_closed)
					(yellow @@ string_of_zone model.variable_names invariant)
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_in)
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_out)
					); 


				LinearConstraint.px_nnconvex_intersection_assign inv_bound_in uncontrollable_zone;
				LinearConstraint.px_nnconvex_intersection_assign inv_bound_out uncontrollable_zone_closed;

				if verbose_mode_greater Verbose_low then 
					print_message Verbose_low 
					(Printf.sprintf "\t\tinv_bound_in ∩ unctrl: %s\n\t\tinv_bound_out ∩ unctrl_closed: %s"
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_in)
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_out)
					); 

				LinearConstraint.px_nnconvex_difference_assign inv_bound_in controllable_zone;
				LinearConstraint.px_nnconvex_difference_assign inv_bound_out controllable_zone_closed; 


				if verbose_mode_greater Verbose_low then 
					print_message Verbose_low 
					(Printf.sprintf "\t\t(inv_bound_in ∩ unctrl) ∖ ctrl: %s\n\t\t(inv_bound_out ∩ unctrl_closed) ∖ ctrl_closed: %s"
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_in)
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_out)
					); 


				LinearConstraint.px_nnconvex_union_assign inv_bound_in inv_bound_out;
				if verbose_mode_greater Verbose_low then 
					print_message Verbose_low 
					(Printf.sprintf "\t\t((inv_bound_in ∩ unctrl) ∖ ctrl) ∪ ((inv_bound_out ∩ unctrl_closed) ∖ ctrl_closed): %s"
					(yellow @@ string_of_nnc_zone model.variable_names inv_bound_in)
					); 
				inv_bound_in
		in
			
		forcedMoves#replace state_index forced_moves;
		if verbose_mode_greater Verbose_low then 
			print_message Verbose_low (Printf.sprintf "\tFM: %s" @@ yellow (string_of_nnc_zone model.variable_names forced_moves))
		

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
		let state = state_space#get_state state_index in 
		

		let is_avoid_state = match state_predicate_avoid with 
		| Some predicate ->  (State.match_state_predicate model predicate state) 
		| None -> false in

		let is_goal_state = State.match_state_predicate model state_predicate state in

		if is_avoid_state then 
			(if verbose_mode_greater Verbose_medium then 
				print_message Verbose_medium @@ Printf.sprintf "\n\t Not adding sucessors of state %d due to avoid state" state_index)
		else 
			(if not options#ptg_no_forced_uncontrollables then self#save_forced_moves state_index;
			if is_goal_state then 
				begin 
					winningZone#replace state_index (nn_of_lin (self#constr_of_state_index state_index));
					let location = (state_space#get_state state_index).global_location in 
					let winning_zone_loc = locationWinningZone#find location in 
					LinearConstraint.px_nnconvex_px_union_assign winning_zone_loc (self#constr_of_state_index state_index); 
					let update_items = self#state_set_to_update_items (depends#find state_index) in 
					waiting#add_all update_items;
					if verbose_mode_greater Verbose_low then 
						self#print_delta_list_with_reason update_items (bold @@ cyan "Target state");
				end;

			let coverage_pruning = is_goal_state && options#coverage_pruning in 
			match self#global_constraint_pruning state_index, coverage_pruning with 
				|	true, _ -> 
					cumulative_pruning_counter#increment;
					print_message Verbose_medium (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (cumulative)" state_index)
				| _, true -> 
					coverage_pruning_counter#increment;
					print_message Verbose_medium (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (coverage)" state_index)
				| _ ->
					(let successors = state_space_ptg#compute_symbolic_successors state_index in
					List.iter (fun s -> (depends#find s)#add state_index) successors;
					let found_existing_state_with_non_empty_winning_zone = 
						List.fold_left (fun acc succ -> 
						if state_space_ptg#passed_states#mem succ then 
							(print_message Verbose_medium (Printf.sprintf "Already passed state %s before - not adding for exploration" 
							(string_of_state_index state_space model succ));
							acc || not @@ LinearConstraint.px_nnconvex_constraint_is_false @@ winningZone#find succ
							)
						else 
							(
							let item = EXPLORE succ in 	
							waiting#add item;
							state_space_ptg#passed_states#add succ;
							if verbose_mode_greater Verbose_low then
								self#print_delta_list_with_reason [item] (bold @@ red "(Partially) Unexplored State");
							acc)
						) false successors
					in 
					if found_existing_state_with_non_empty_winning_zone then 
						let item = UPDATE {state_index; timestamp = fresh_timestamp ()} in 
						waiting#add item;
						if verbose_mode_greater Verbose_low then
								self#print_delta_list_with_reason [item] (bold @@ magenta "Transition to partially winning state");
					)
			)


	method private process_convex_winning_move state action bad_zone (winning_move : LinearConstraint.px_linear_constraint) =
		
		let safe_timed_pred = self#safe_timed_pred_conv_g state winning_move bad_zone in
		LinearConstraint.px_nnconvex_px_intersection_assign safe_timed_pred (self#constr_of_state_index state);
		
		let global_location_src = (state_space#get_state state).global_location in

		let current_winning_zone_glob = locationWinningZone#find global_location_src in

		let current_winning_zone_state = winningZone#find state in
		(* Extend winning zone of STATE with newly found safe timed pred *)
		LinearConstraint.px_nnconvex_union_assign current_winning_zone_state safe_timed_pred;

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
			end
		


	method private process_nnconvex_winning_move state action bad_zone winning_move = 
		List.iter (fun g_i -> self#process_convex_winning_move state action bad_zone g_i) 
		(LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint winning_move)


	(* Handle backtracking for a single edge, updating the winning zone and the associated strategy *)
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

	(* Process a forced move of the environment *)
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

		let orig_winning_zone = LinearConstraint.px_nnconvex_copy @@ winningZone#find state_index in 
		self#process_forced_move state_index uncontrollable_part (forcedMoves#find state_index);
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
			winningZone#replace state_index safe_timed_pred
		else
			List.iter (fun edge -> self#backtrack_single_controllable_edge edge state_index uncontrollable_part) controllable_edges;
		let winning_zone_changed = not (LinearConstraint.px_nnconvex_constraint_is_equal (winningZone#find state_index) orig_winning_zone) in 
		if winning_zone_changed then 
		begin
			let update_items = self#state_set_to_update_items (depends#find state_index) in 
			if verbose_mode_greater Verbose_low then 
				print_message Verbose_low (Printf.sprintf "\t%s: %s %s %s" 
				(string_of_state_index state_space model state_index)
				(green @@ string_of_nnc_zone model.variable_names orig_winning_zone)
				(bold "→")
				(bold @@ green @@ string_of_nnc_zone model.variable_names @@ winningZone#find state_index));
				if verbose_mode_greater Verbose_low then 
					self#print_delta_list_with_reason update_items (bold @@ green @@ "Winning zone changed");
			waiting#add_all update_items;
			if state_index = state_space#get_initial_state_index then init_winning_zone_changed := true
		end

	(* Initial state is won if parameter valuations in its winning zone is non-empty *)
	method private init_has_winning_witness =
		init_winning_zone_changed := false;
		not @@ LinearConstraint.p_nnconvex_constraint_is_false synthesized_constraint

	(* Initial state is exact if winning zone covers initial zone  *)
	method private init_is_exact init = 
		init_winning_zone_changed := false;
		let winning_zone_nn = LinearConstraint.px_nnconvex_copy @@ winningZone#find init in 
		let initial_constraint = self#initial_constraint () in  

		let included = LinearConstraint.px_nnconvex_constraint_is_leq initial_constraint winning_zone_nn in
		if verbose_mode_greater Verbose_low then 
			print_message Verbose_low @@ bold @@ yellow "\tInitial winning zone has changed (checking initial constraint coverage)";
			let symbol = bold @@ if included then "⊆" else "⊄" in
			print_message Verbose_low (Printf.sprintf "\t%s %s %s" 
			(bold @@ blue @@ string_of_nnc_zone model.variable_names initial_constraint)
			symbol
			(bold @@ green @@ string_of_nnc_zone model.variable_names winning_zone_nn));
			
		included
	
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
			Some time_limit -> ImitatorUtilities.time_from start_time > time_limit
			| None -> false
		in

		if time_out then termination_status <- Time_limit (Result.Number state_space#nb_states);

		let terminate = queue_empty ||	init_exact || init_has_winning_witness || time_out in 
		if verbose_mode_greater Verbose_low && terminate then 
			print_message Verbose_low @@ bold @@ yellow @@ (Printf.sprintf "Termination reason: %s"
			(if queue_empty then "Fixed point" 
			else if init_exact then "Initial state winning" 
			else if init_has_winning_witness then "Winning witness found"
			else "Timed out"));
		terminate

	method private is_update_relevant state_index timestamp =
		if timestamp > lastUpdate#find state_index then 
			(update_counter#increment; lastUpdate#replace state_index (fresh_timestamp ()); true)
		else 
			(update_pruning_counter#increment; false)


	(* Computes the parameters for which a winning strategy exists and saves the result in synthesized_constraint *)
	method private compute_PTG = 
		(* === ALGORITHM INITIALIZATION === *)
		let initial_state_index = state_space#get_initial_state_index in 
		
		waiting#add (EXPLORE initial_state_index);
		state_space_ptg#passed_states#add initial_state_index;

		let initial_state = state_space#get_state initial_state_index in 

		(* If goal is init then initial winning zone is it's own constraint*)
		if State.match_state_predicate model state_predicate initial_state then
			(winningZone#replace initial_state_index (nn_of_lin (self#constr_of_state_index initial_state_index));
			init_winning_zone_changed := true);

		let iter = ref 1 in 
		(* === ALGORITHM MAIN LOOP === *)
		while (not @@ self#termination_criteria initial_state_index) do
			if verbose_mode_greater Verbose_low then 
				(print_message Verbose_low (yellow @@ bold @@ Printf.sprintf "- Main algorithm loop iteration %d -" !iter);
				print_message Verbose_low ("\tQ=" ^ item_list_to_str waiting#to_list model state_space);
				incr iter);
			let item = waiting#extract {lastUpdate;depends;winningZone;forcedMoves} in 			
			if verbose_mode_greater Verbose_low then 
				print_message Verbose_low ("\t" ^ item_to_str model state_space item ~include_zone:true);
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
