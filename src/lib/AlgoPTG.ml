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
(* open OCamlUtilities *)
open ImitatorUtilities
(* open Exceptions *)
open AbstractModel
open AbstractProperty
open Result
open AlgoGeneric
(* open Statistics *)
open State

(* Notation and shorthands *)
let (>>) f g x = g(f(x)) 
let (>>>) f g x y = g(f x y) 

let nn = LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint
let project_params = LinearConstraint.px_nnconvex_hide_nonparameters_and_collapse 
let is_empty =  LinearConstraint.p_nnconvex_constraint_is_false 
let (#!=) = LinearConstraint.px_nnconvex_constraint_is_equal >>> not
let bot = LinearConstraint.false_px_nnconvex_constraint

let (|||) = fun a b -> LinearConstraint.px_nnconvex_union_assign a b; a  
let (&&&) = fun a b -> LinearConstraint.px_nnconvex_intersection_assign a b; a
let print_PTG = print_message Verbose_low
let print_exp = print_message Verbose_experiments

type edge = {state: state_index; action: Automaton.action_index; transition: StateSpace.combined_transition; state': state_index}

type edge_status = BackpropLosing | BackpropWinning | Unexplored
type backtrack_type = Winning | Losing
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
(* 	let to_seq () = Hashtbl.to_seq D.tbl *)
	let tbl = D.tbl
	let replace = Hashtbl.replace tbl
	let find key = 
		try Hashtbl.find tbl key with
			Not_found -> let d = D.default key in replace key d; d
	let to_str () = "[" ^ 
		Seq.fold_left 
			(fun acc (key, elem) -> Printf.sprintf "%s, %s -> %s\n" acc (D.str_of_key key) (D.str_of_elem elem)) 
			("")
			(Hashtbl.to_seq tbl) 
		^ "]"
end


let status_to_string = function 
	| Unexplored -> "EXPLORE"
	| BackpropLosing -> "BACKPROP(LOSING)"
	| BackpropWinning -> "BACKPROP(WINNING)"

let edge_to_str = fun ({state; action; state';_}, status : edge * edge_status) model state_space -> 	
	let action_str = model.action_names action in 
	let location = Array.get (DiscreteState.get_locations ((state_space#get_state state).global_location)) 0 in 
	let location' = Array.get (DiscreteState.get_locations ((state_space#get_state state').global_location)) 0 in 
	let location_name = model.location_names 0 location in 
	let location_name' = model.location_names 0 location' in 
	

	Printf.sprintf "%d (loc %s) --%s-> %d (loc %s) (%s)" state location_name action_str state' location_name' (status_to_string status)

let edge_list_to_str seq model state_space = "[" ^ 
	List.fold_left 
		(fun acc edge -> Printf.sprintf "%s, %s" acc (edge_to_str edge model state_space))
		("") (
		seq) 
	^ "]"

module WinningZone = DefaultHashtbl (struct 
	let model = ref None
	type elem = LinearConstraint.px_nnconvex_constraint * AlgoPTGStrategyGenerator.state_strategy ref
	type key = state_index
	let tbl = Hashtbl.create 100
	let default = fun _ -> LinearConstraint.false_px_nnconvex_constraint(), ref []
	let str_of_elem (zone, _) = match !model with 
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

module EdgeSet = Set.Make(struct type t = edge let compare = Stdlib.compare end)

class edgeSet = object 
	val mutable internal_set = EdgeSet.empty
	method add e = internal_set <- EdgeSet.add e internal_set
	method mem e = EdgeSet.mem e internal_set 
	method to_list = EdgeSet.elements internal_set
	method to_seq = EdgeSet.to_seq internal_set
end

module Depends = DefaultHashtbl (struct 
	let model = ref None
	type elem = edgeSet
	type key = state_index
	let tbl = Hashtbl.create 100
	let default = fun _ -> new edgeSet
	let str_of_elem _ = match !model with
		| Some _ -> "TODO"
		| None -> "No model provided"
	let str_of_key = string_of_int
end)



class virtual stateSpacePTG  = object(self)
	val mutable state_space : StateSpace.stateSpace = new StateSpace.stateSpace 0
	method state_space = state_space
	method virtual initialize_state_space : unit -> unit
	method virtual compute_symbolic_successors : state_index -> (StateSpace.combined_transition * state_index) list
	initializer 
		self#initialize_state_space ()
end

let add_transitions_and_states_to_state_space state_space transitions_and_states comparison_operator callback = 
	List.filter_map (fun (transition, s) -> 
		let addition_result =  state_space#add_state comparison_operator None s in 
		callback addition_result transition
	) transitions_and_states

class stateSpacePTG_OTF model options = object
	inherit stateSpacePTG
	val mutable passed_states = new State.stateIndexSet
	method initialize_state_space () = 		
		let state = AlgoStateBased.create_initial_state options model false in
		let _ = state_space#add_state AbstractAlgorithm.No_check None state in ()
	method compute_symbolic_successors source_state_index = 
		if passed_states#mem source_state_index then 
			state_space#get_successors_with_combined_transitions source_state_index
		else 
		begin
			passed_states#add source_state_index;
			let state = state_space#get_state source_state_index in 
			let successors = AlgoStateBased.combined_transitions_and_states_from_one_state_functional options model state in
			add_transitions_and_states_to_state_space state_space successors options#comparison_operator 
			(fun addition_result transition -> 
				match addition_result with 
				| New_state new_state_index
				| State_replacing new_state_index
				| State_already_present new_state_index -> 
					state_space#add_transition (source_state_index, transition, new_state_index);
					Some (transition, new_state_index)
			)
		end
end

class stateSpacePTG_full model options = object
	inherit stateSpacePTG
	val mutable passed_states = new State.stateIndexSet
	method initialize_state_space () = 		
		print_exp ("PTG: Generating full statespace (not on the fly)");
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
					if passed_states#mem new_state_index then 
						None
					else 
					(passed_states#add new_state_index;
					Some new_state_index)
			)
		in

		let depth_limit = match options#depth_limit with
			| Some d -> d
			| None -> -1
		in 

		let rec bfs unexplored_state_indices depth = 
			print_exp (Printf.sprintf "Expanding frontier of %d states " (List.length unexplored_state_indices));
			let unexplored_state_indices' = List.fold_left (fun acc state_index -> 
				(process_successors_from_state_index state_index) @ acc) [] unexplored_state_indices in 
			if unexplored_state_indices' = [] || depth = depth_limit then () else bfs unexplored_state_indices' (depth+1)
		in
		let initial_state_index = state_space#get_initial_state_index in 
		passed_states#add initial_state_index;
		bfs [initial_state_index] 1;
		print_exp (Printf.sprintf "PTG: Finished generating full statespace. Total states: %d" state_space#nb_states)

	method compute_symbolic_successors source_state_index = 
		state_space#get_successors_with_combined_transitions source_state_index
end


let compare_edge e1 e2 = match e1, e2 with
	| (_, Unexplored), (_, Unexplored) -> PriorityQueue.Equal
	| _, (_, Unexplored) -> PriorityQueue.Equal
	| (_, Unexplored), _ -> PriorityQueue.Equal
	| _ -> PriorityQueue.Equal



class virtual ['a] waitingList list = object(self)
	method virtual queue : 'a
	method virtual add : edge * edge_status -> unit
	method virtual extract : edge * edge_status
	method virtual is_empty : bool
	method virtual to_list : (edge * edge_status) list
	method virtual length : int
	method virtual add_all : 'a waitingList -> unit
	method virtual of_list : (edge * edge_status) list -> unit
	initializer 
		self#of_list list
end


module EdgePriorityQueue = PriorityQueue.Make(struct type t = (edge * edge_status) let compare = compare_edge end)

(*class prioQueue list = object
	inherit ([EdgePriorityQueue.queue] waitingList list)
	val mutable queue = EdgePriorityQueue.empty 
	method queue = queue
	method add e = queue <- EdgePriorityQueue.insert queue e
	method extract = let (elt, queue') = EdgePriorityQueue.extract queue in queue <- queue'; elt
	method is_empty = EdgePriorityQueue.is_empty queue
	method to_list = EdgePriorityQueue.to_list queue
	method length = EdgePriorityQueue.length queue
	method add_all waitingList = queue <- EdgePriorityQueue.merge queue waitingList#queue
	method of_list list = queue <- EdgePriorityQueue.of_list list	
end*)

class normalQueue list = object
	inherit ([(edge * edge_status) Queue.t] waitingList list)
	val queue = Queue.create () 
	method queue = queue
	method add e = Queue.add e queue
	method extract = Queue.pop queue
	method is_empty = Queue.is_empty queue
	method to_list = List.of_seq (Queue.to_seq queue)
	method length = Queue.length queue
	method add_all waitingList = Queue.transfer waitingList#queue queue 
	method of_list list = List.iter (fun e -> Queue.add e queue) list
end

let (#<-) (queue : 'a waitingList) elem = queue#add elem   
let (#<--) (queue1 : 'a waitingList) (queue2 : 'a waitingList) = queue1#add_all queue2

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

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "PTG"


	val mutable termination_status = Regular_termination

	

	method private constr_of_state_index state = (state_space#get_state state).px_constraint
	method private get_global_location state = state_space#get_location (state_space#get_global_location_index state)

	(* Computes the predecessor zone of current_zone using edge *)
	method private predecessor_nnconvex edge current_zone = 
		let {state; transition; _} = edge in 
		let guard = state_space#get_guard model state transition in
		let pred_zone = self#constr_of_state_index state in 
		let constraints = List.map (fun z -> 
			(* TODO : Become independent on DeadlockExtra  - ie. make general method for convex pred *)
			let pxd_pred = DeadlockExtra.dl_predecessor model state_space state pred_zone guard z transition in 	
			let px_pred = LinearConstraint.pxd_hide_discrete_and_collapse pxd_pred in 
			LinearConstraint.px_nnconvex_constraint_of_px_linear_constraint px_pred
			) @@ LinearConstraint.px_linear_constraint_list_of_px_nnconvex_constraint current_zone in 
		List.fold_left (|||) (bot ()) constraints

	val init_losing_zone_changed = ref false 
	val init_winning_zone_changed = ref false
	val init_winning_zone = fun _ -> WinningZone.find state_space_ptg#state_space#get_initial_state_index
	
	(* Initialize the Winning and Depend tables with our model - only affects printing information in terminal *)
	method private initialize_tables () = 
		WinningZone.model := Some model;
		LosingZone.model := Some model;
		Depends.model := Some model

	(* Edges from a symbolic state *)
	method private get_edges state = 
		let successors = state_space_ptg#compute_symbolic_successors state in
		let action_state_list = List.map (fun (transition, state') ->
			let action = StateSpace.get_action_from_combined_transition model transition in
			{state; action; transition; state'}, Unexplored
			) successors in
		action_state_list

	(* Edges from a symbolic state as a queue *)
	method private get_edge_queue state_index =
		new normalQueue @@ self#get_edges state_index;

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
			AlgoStateBased.apply_time_past model global_location constr_d;
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

	(* Takes a state index and decides whether to prune (stop exploration of ) its succesors based on the global parameter constraint *)
	method private global_constraint_pruning state_index = 
		if options#cumulative_pruning then 
			let constr = self#constr_of_state_index state_index in 
			let constr_params = LinearConstraint.px_hide_nonparameters_and_collapse constr in 
			let constr_params_nnconvex = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint constr_params in 
			LinearConstraint.p_nnconvex_constraint_is_leq constr_params_nnconvex synthesized_constraint
		else
			false

	(* Explores forward in order to discover winning states *)
	method private forward_exploration e waiting passed= 
		let {state';_} = e in 
		print_PTG ("I've not seen state " ^ (string_of_int state') ^ " before.	Exploring: ");
		passed#add state';

		let coverage_pruning = ref false in 
		if self#matches_state_predicate state' then 
			begin 
				WinningZone.replace state' @@ ((self#constr_of_state_index >> nn) state', ref []);
				waiting #<- (e, BackpropWinning);
				coverage_pruning := true
			end;

		if self#is_dead_lock state' then 
			begin
				if options#ptg_propagate_losing_states then 
					(LosingZone.replace state' @@ (self#constr_of_state_index >> nn) state'; 
					waiting #<- (e, BackpropLosing));
				coverage_pruning := true
			end;

		coverage_pruning := !coverage_pruning && options#coverage_pruning;

		begin 
			match self#global_constraint_pruning state', !coverage_pruning with 
				|	true, _ -> print_PTG (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (cumulative)" state')
				| _, true -> print_PTG (Printf.sprintf "\n\tNot adding sucessors of state %d due to pruning (coverage)" state')
				| _ ->
					(Depends.find state')#add e;
					waiting #<-- (self#get_edge_queue state');
					print_PTG ("\n\tAdding successor edges to waiting list. New waiting list: " ^ edge_list_to_str waiting#to_list model state_space)
		end;

	(* Append a status to a set of edges and turn it into a queue (linear time in size of set) *)
	method private edge_set_to_queue_with_status edge_set status = 
		new normalQueue @@ List.map (fun e -> (e, status)) (edge_set#to_list)

	(* Handle backtracking for a single edge, updating the winning zone and the associated strategy 
		 return true if winning zone was changed otherwise false	 
	*)
	method private backtrack_single_controllable_edge edge bad_zone zone_map =
		let {state; state'; action;_} = edge in 
		let winning_move = self#predecessor_nnconvex edge (zone_map state') in

		let safe_timed_pred = self#safe_timed_pred winning_move bad_zone state in
		let current_winning_zone = WinningZone.find state |> fst in

		
		(* Intersect winning move with safe timed predecessors to remove unsafe parts *)
		LinearConstraint.px_nnconvex_intersection_assign winning_move safe_timed_pred;
		
		(* Make safe_timed_pred a partition *)
		LinearConstraint.px_nnconvex_difference_assign safe_timed_pred current_winning_zone;		

		if not @@ LinearConstraint.px_nnconvex_constraint_is_false safe_timed_pred then
			begin
				(* Extend the winning zone with new partition *)
				LinearConstraint.px_nnconvex_union_assign current_winning_zone safe_timed_pred;

				(* Extend strategy with new partition *)
				let new_strategy_entry : AlgoPTGStrategyGenerator.strategy_entry = {
						action = action;
						winning_move;
						prioritized_winning_zone = safe_timed_pred
					}
				in 
				let strategy = WinningZone.find state |> snd in 
				strategy := new_strategy_entry :: !strategy;
				true
			end
		else 
			false
			

	(* General method for backpropagation of winning/losing zones *)
	method private backtrack e waiting backtrack_type = 
		let {state; state';_} = e in 
		let get_pred_from_edges default edges zone = 
			List.fold_left (|||) default @@
				List.map (fun edge -> 
					self#predecessor_nnconvex edge (zone edge.state')
				)
			edges
		in

		let good_edges = if backtrack_type = Winning then self#get_controllable_edges state else self#get_uncontrollable_edges state in
		let bad_edges = if backtrack_type = Losing then self#get_controllable_edges state else self#get_uncontrollable_edges state in

		begin
			match backtrack_type with 
			| Winning -> 
				print_PTG "\tWINNING ZONE PROPAGATION:";
				let bad = get_pred_from_edges (bot ()) bad_edges (fun x -> self#negate_zone (fst @@ WinningZone.find x) x) in
				let winning_zone_changed = 
					List.fold_left (||) false
						(List.map(fun edge -> self#backtrack_single_controllable_edge edge bad (WinningZone.find >> fst)) good_edges) in 
				if winning_zone_changed then 
					begin
						waiting #<-- (self#edge_set_to_queue_with_status (Depends.find state) BackpropWinning);
						if state = state_space#get_initial_state_index then init_winning_zone_changed := true
					end
			| Losing -> 		
				print_PTG "\tLOSING ZONE PROPAGATION:";
				let good = get_pred_from_edges (LinearConstraint.px_nnconvex_copy @@ LosingZone.find state) good_edges LosingZone.find in
				let bad = get_pred_from_edges (bot ()) bad_edges (fun x -> self#negate_zone (LosingZone.find x) x) in
				LinearConstraint.px_nnconvex_difference_assign bad good;
				let new_zone = self#safe_timed_pred good bad state in
				if (LosingZone.find state) #!= new_zone then
					begin
						LosingZone.replace state new_zone;
						waiting #<-- (self#edge_set_to_queue_with_status (Depends.find state) BackpropLosing);
						if state = state_space#get_initial_state_index then init_losing_zone_changed := true 
					end;
		end;
		(Depends.find state')#add e

	(* Initial state is lost if initial constraint is included in losing zone *)
	method private init_is_lost init =
		init_losing_zone_changed := false;
		LinearConstraint.px_nnconvex_constraint_is_leq (self#initial_constraint ()) (LosingZone.find init)

	(* Initial state is won if parameter valuations in its winning zone is non-empty *)
	method private init_has_winning_witness =
		init_winning_zone_changed := false;
		not @@ is_empty synthesized_constraint

	(* Initial state is exact if winning and losing zone covers initial zone  *)
	method private init_is_exact init = 
		init_losing_zone_changed := false;
		init_winning_zone_changed := false;
		let init_zone_nn = nn @@ self#constr_of_state_index init in 
		let winning_and_losing_zone = LinearConstraint.px_nnconvex_copy @@ fst @@ WinningZone.find init ||| LosingZone.find init in
		LinearConstraint.px_nnconvex_constraint_is_leq init_zone_nn winning_and_losing_zone
	
	(* Returns true if the algorithm should terminate, depending on the criteria for termination *)
	method private termination_criteria waiting init = 
		let queue_empty = waiting#is_empty in
		let complete_synthesis = (property.synthesis_type = Synthesis) in
		let propagate_losing_states = options#ptg_propagate_losing_states in 

		if !init_winning_zone_changed then 
			synthesized_constraint <- project_params (self#initial_constraint () &&& fst @@ WinningZone.find init);

		let recompute_init_lost = propagate_losing_states && !init_losing_zone_changed in
		let recompute_init_has_winning_witness = not complete_synthesis && !init_winning_zone_changed in  
		let recompute_init_exact = complete_synthesis && (!init_losing_zone_changed || !init_winning_zone_changed) in

		let init_lost = if recompute_init_lost then self#init_is_lost init else false in
		let init_has_winning_witness = if recompute_init_has_winning_witness then self#init_has_winning_witness else false in 
		let init_exact = if recompute_init_exact then self#init_is_exact init else false in

		let time_out = match options#time_limit with 
			Some time_limit -> ImitatorUtilities.time_from start_time > float_of_int time_limit
			| None -> false
		in

		if time_out then termination_status <- Time_limit (Result.Number state_space#nb_states);

		queue_empty || init_lost ||	init_exact || init_has_winning_witness || time_out



	(* Computes the parameters for which a winning strategy exists and saves the result in synthesized_constraint *)
	method private compute_PTG = 
		let propagate_losing_states = options#ptg_propagate_losing_states in 
		self#initialize_tables();

		(* === ALGORITHM INITIALIZATION === *)
		let init = state_space#get_initial_state_index in 
		
		let passed = new State.stateIndexSet in 
		passed#add init;
		let waiting = self#get_edge_queue init in 

		(* If goal is init then initial winning zone is it's own constraint*)
		if self#matches_state_predicate init then
			WinningZone.replace init @@ ((self#constr_of_state_index >> nn) init, ref []);

		(* If init is deadlock then initial losing zone is it's own constraint*)
		if self#matches_state_predicate init && propagate_losing_states then
			(LosingZone.replace init @@ (self#constr_of_state_index >> nn) init; init_losing_zone_changed := true);

		(* === ALGORITHM MAIN LOOP === *)
		while (not @@ self#termination_criteria waiting init) do
			print_exp (Printf.sprintf "Waiting list size: %d, state space size: %d" waiting#length state_space#nb_states);
			print_PTG ("\nEntering main loop with waiting list: " ^ edge_list_to_str waiting#to_list model state_space);
		(*	print_message Verbose_standard (edge_list_to_str waiting#to_list model state_space);			*)
			let e, edge_status = waiting#extract in 			
			print_PTG (Printf.sprintf "I choose edge: \027[92m %s \027[0m" (edge_to_str (e, edge_status) model state_space));
			if not @@ passed#mem e.state' then  
				self#forward_exploration e waiting passed
			else
				match edge_status with
					| Unexplored -> 
						self#backtrack e waiting Winning;
						if propagate_losing_states then self#backtrack e waiting Losing
					| BackpropWinning -> 
						self#backtrack e waiting Winning
					| BackpropLosing -> 
						self#backtrack e waiting Losing
		done;
		print_PTG "After running AlgoPTG I found these winning zones:";
		print_PTG (WinningZone.to_str ());

		if propagate_losing_states then
			print_PTG (Printf.sprintf "And these losing zones: %s" (LosingZone.to_str()));

		let winning_parameters = project_params (self#initial_constraint () &&& fst @@ WinningZone.find init) in
		synthesized_constraint <- winning_parameters

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Main method to run the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)	
	method run =

		start_time <- Unix.gettimeofday();

		(* Compute the parametric timed game *)
		self#compute_PTG;
		

		AlgoPTGStrategyGenerator.print_strategy 
		model 
		~strategy:(fun state_index -> !(WinningZone.find state_index |> snd)) 
		~state_indices:(List.of_seq @@ Hashtbl.to_seq_keys WinningZone.tbl)
		~state_space:state_space;

		(* Compute the strategy *)
	(*	if options#ptg_controller_mode != AbstractAlgorithm.No_Generation then 
			AlgoPTGStrategyGenerator.generate_controller model (fun x -> WinningZone.find x |> snd) state_space options; *)

		(* Return the result *)
		self#compute_result;

		(* The end *)


		
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method private compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		self#print_algo_message_newline Verbose_standard (
			Printf.sprintf "Size of explored state space: %d" (state_space#nb_states);
		);

		(*** TODO: compute as well *good* zones, depending whether the analysis was exact, or early termination occurred ***)

		(* Projecting onto some parameters if required by the property *)
		let result = AlgoStateBased.project_p_nnconvex_constraint_if_requested model property synthesized_constraint in

(*		(* Get the termination status *)
		let termination_status = match termination_status with
		| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
		| Some status -> status
		in*)

		(* Constraint is exact if termination is normal, possibly under-approximated otherwise *)
		(*** NOTE/TODO: technically, if the constraint is true/false, its soundness can be further refined easily ***)
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
