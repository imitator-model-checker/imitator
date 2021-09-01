(************************************************************
 *
 *                       IMITATOR
 *
 * LIPN, Université Paris 13 & CNRS (France)
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: NDFS algorithms
 *
 * File contributors : Laure Petrucci, Jaco van de Pol, Étienne André
 * Created           : 2019/03/12
 * Last modified     : 2021/09/01
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
open AbstractAlgorithm
open AbstractProperty
open Result
open AlgoStateBased
open State
open Statistics

(************************************************************)
(************************************************************)
(* Object-independent functions *)
(************************************************************)
(************************************************************)

(************************************************************)
(* Exception for NDFS *)
(************************************************************)

exception DFS_Limit_detected of bfs_limit_reached



(************************************************************)
(* Type *)
(************************************************************)

module IntMap = Map.Make(struct type t = int let compare = compare end)

(************************************************************)
(* Functions *)
(************************************************************)

(*------------------------------------------------------------*)
(* add x before the first y in q such that (before x y) holds *)
(*------------------------------------------------------------*)
let rec add_ordered x q before =
	match q with
	| [] -> [x]
	| y::q' -> if before x y then x::q else y::(add_ordered x q' before)

let rec in_queue astate thequeue =
	match thequeue with
	| [] -> false
	| (thestate,_)::body ->
		if (astate = thestate) then true
		else in_queue astate body


(***********************)
(* printing the queues *)
(***********************)
let printtable (colour : string) thetable : unit =
	if verbose_mode_greater Verbose_medium then(
			let printrecord state_index u rest =
					(string_of_int state_index) ^ " " ^ rest;
			in print_message Verbose_medium("Table " ^ colour ^ " : [ "
					^ Hashtbl.fold printrecord thetable "" ^ "]")
	);
	()

(***********************)
(* Edit tables *)
(***********************)
(* Table add, test and remove; a state is present as it maps to ():unit *)
let table_add (table : (State.state_index, unit) Hashtbl.t) (state_index : State.state_index) : unit =
		Hashtbl.replace table state_index ()

let table_rem (table : (State.state_index, unit) Hashtbl.t) (state_index : State.state_index) : unit =
		Hashtbl.remove table state_index

let table_test (table : (State.state_index, unit) Hashtbl.t) (state_index : State.state_index) : bool =
		List.length (Hashtbl.find_all table state_index) > 0


let printpendingqueue (colour : string) (thequeue : (State.state_index * int) list) : unit =
	if verbose_mode_greater Verbose_medium then(
		let rec r_printqueue thequeue = match thequeue with
			| [] -> "";
			| (state_index,state_depth)::body  ->
				"(" ^ (string_of_int state_index)
					^ ", " ^ (string_of_int state_depth) ^ ") "
					^ (r_printqueue body);
		in print_message Verbose_medium("Queue " ^ colour ^ " : [ "
				^ r_printqueue thequeue ^ "]")
	);
	()


(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNDFS (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoStateBased as super

	(************************************************************)
	(* Class variables *)
	(************************************************************)

	(* for the evaluation of the synthesis result *)
	val mutable cyclecount				= 0 (* counter for the cycles found *)
	val mutable processed_blue			= 0 (* number of states processed by a blue dfs *)
		(* total count for iterative version *)
	val mutable total_cyclecount		= 0
	val mutable total_processed_blue	= 0
	val mutable depth_reached			= false (* used when a max depth has been reached *)
	val mutable execute_again			= true (* used when not doing iterative deepening for 1 execution only *)
	val mutable current_depth			= -1 (* used for iterative deepening *)
	val mutable the_depth_step			= -1 (* used for iterative deepening *)
	val mutable max_depth				= -1 (* used for iterative deepening *)
	val mutable min_depth_found			= -1 (* minimal depth at which a cycle is found *)
	val mutable max_depth_reached		= 0 (* maximum depth actually reached *)
	
	(************************************)
	(* basic queues for NDFS algorithms *)
	(************************************)
	val mutable cyan			: (State.state_index, unit) Hashtbl.t = Hashtbl.create 100
	val mutable blue			: (State.state_index, unit) Hashtbl.t = Hashtbl.create 100
	val mutable green			: (State.state_index, unit) Hashtbl.t = Hashtbl.create 100
	val mutable greendepth		: int IntMap.t = IntMap.empty
	val mutable red				: (State.state_index, unit) Hashtbl.t = Hashtbl.create 100
	val mutable pending			: (State.state_index * int) list = []; (* used in the layered algorithms *)

	val mutable pzone_table		: (State.state_index, LinearConstraint.p_linear_constraint) Hashtbl.t = Hashtbl.create 100
	val mutable pzone_nc_table	: (State.state_index, LinearConstraint.p_nnconvex_constraint) Hashtbl.t = Hashtbl.create 100

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "Cycle (NDFS)"

	(************************************)
	(* Check the states and time limits *)
	(************************************)
	method private check_and_update_queue_dfs_limit =

	try(
	(* States limit *)
	begin
	match options#states_limit with
		| None -> ()
		| Some limit -> if StateSpace.nb_states state_space > limit then(
(* 				termination_status <- States_limit; *)
			raise (DFS_Limit_detected States_limit_reached)
		)
	end
	;
	(* Time limit *)
	begin
	match options#time_limit with
		| None -> ()
		| Some limit -> if time_from start_time > (float_of_int limit) then(
(* 				termination_status <- Time_limit; *)
			raise (DFS_Limit_detected Time_limit_reached)
		)
	end
	;
	(* If reached here, then everything is fine: keep going *)
	()
	)
	(* If exception caught, then update termination status, and return the reason *)
	with DFS_Limit_detected reason ->
		limit_reached <- reason


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*                                                       *-*)
	(*-*                       NDFS code                       *-*)
	(*-*                                                       *-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	(***************************************************************************************)
	(* Compute only once the convex parameter projection of the state, and put it in a hashtable *)
	(***************************************************************************************)
	method private find_or_compute_pzone (thestate : State.state_index) : LinearConstraint.p_linear_constraint =
		try Hashtbl.find pzone_table thestate
		with Not_found -> (
			let astate : State.state = StateSpace.get_state state_space thestate in
			let linear_aconstr : LinearConstraint.p_linear_constraint = LinearConstraint.px_hide_nonparameters_and_collapse astate.px_constraint in
			Hashtbl.add pzone_table thestate linear_aconstr;
			linear_aconstr
		)

	(***************************************************************************************)
	(* Compute only once the non-convex parameter projecvtion of the state, and put it in a hashtable *)
	(***************************************************************************************)
	method private find_or_compute_pzone_nc (thestate : State.state_index) = 
		try Hashtbl.find pzone_nc_table thestate
		with Not_found -> (
			let linear_aconstr : LinearConstraint.p_linear_constraint = self#find_or_compute_pzone thestate in
			let astate_constr : LinearConstraint.p_nnconvex_constraint = LinearConstraint.p_nnconvex_constraint_of_p_linear_constraint linear_aconstr in
			Hashtbl.add pzone_nc_table thestate astate_constr;
			astate_constr
		)

	(***************************************************)
	(* Generic check of zone projection on parameters *)
	(***************************************************)
	method private parameter_projection_gen (operator : LinearConstraint.p_linear_constraint -> LinearConstraint.p_linear_constraint -> bool) (state_index1 : State.state_index) (state_index2 : State.state_index) : bool =
		let constr1 : LinearConstraint.p_linear_constraint = self#find_or_compute_pzone state_index1 in
		let constr2 : LinearConstraint.p_linear_constraint = self#find_or_compute_pzone state_index2 in
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high ("Projected constraint 1: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr1
				^ " state: "
				^ (StateSpace.string_of_state_index state_index1));
			print_message Verbose_high ("Projected constraint 2: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr2
				^ " state: "
				^ (StateSpace.string_of_state_index state_index2)));
		operator constr1 constr2


	(***************************************************)
	(* Check equality of zone projection on parameters *)
	(***************************************************)
	method private same_parameter_projection (state_index1 : State.state_index) (state_index2 : State.state_index) : bool =
		self#parameter_projection_gen LinearConstraint.p_is_equal state_index1 state_index2


	(****************************************************)
	(* Check inclusion of zone projection on parameters *)
	(****************************************************)
	method private smaller_parameter_projection (state_index1 : State.state_index) (state_index2 : State.state_index) : bool =
		self#parameter_projection_gen LinearConstraint.p_is_leq state_index1 state_index2

	(***************************)
	(* Check inclusion of zone *)
	(***************************)
	method private smaller_zone (state_index1 : State.state_index) (state_index2 : State.state_index) : bool =
		let state1	: State.state = StateSpace.get_state state_space state_index1 in
		let constr1	: LinearConstraint.px_linear_constraint = state1.px_constraint in
		let state2	: State.state = StateSpace.get_state state_space state_index2 in
		let constr2	: LinearConstraint.px_linear_constraint = state2.px_constraint in
		LinearConstraint.px_is_leq constr1 constr2


	(******************************************)
	(* printing zone projection on parameters *)
	(******************************************)
	method private print_projection verbose_level (thestate : State.state_index) : unit =
		if verbose_mode_greater verbose_level then (
			let constr = self#find_or_compute_pzone thestate in
			print_message verbose_level ("Projected constraint : \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr))


	(***************************)
	(* cyclefound *)
	(***************************)
	
	(*** NOTE: functions written by Laure Petrucci and Jaco van de Pol using a lot of copy paste; then Étienne André tried to (re)factor (2021/09) ***)
	
	method private set_termination () : unit =
		(* For synthesis: we do not stop immediately *)
		termination_status <- Some Target_found;
		()
	
	method private set_termination_if_synthesis () : unit =
		(* Get the property to check whether we are in synthesis or witness mode *)
		let property = Input.get_property() in
	(* For synthesis: we do not stop immediately *)
		if (property.synthesis_type = Synthesis) then
			termination_status <- Some Regular_termination
		else termination_status <- Some Target_found;
		()


	(*** NOTE: this function was used in the inner part of some loops; also, it is identical to "cyclefound", and therefore used there for factoring purpose (ÉA, 2021/08) ***)
	method private cyclefound_inner (termination_function : unit -> unit) (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
		(* Get the property to check whether we are in synthesis or witness mode *)
		let property = Input.get_property() in

		cyclecount <- cyclecount + 1;
		total_cyclecount <- total_cyclecount + 1;
		if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
		if (property.synthesis_type = Witness) then
			print_highlighted_message Shell_bold Verbose_standard
				("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
		else print_highlighted_message Shell_bold Verbose_standard
			("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
		if verbose_mode_greater Verbose_low then (
			print_message Verbose_medium
				(ModelPrinter.string_of_state model
					(StateSpace.get_state state_space astate));
			self#print_projection Verbose_low astate);
		
		(*** NOTE: here some factoring (ÉA) using the `termination_function` ***)
		termination_function ();
		
		let pzone = self#find_or_compute_pzone astate in
		LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
		if (property.synthesis_type = Witness) then raise TerminateAnalysis;
		()


	(*** NOTE: this function was copy/paste almost identically 4 times, and I factored it (ÉA, 2021/08) ***)
	method private cyclefound_gen (termination_function : unit -> unit) (custom_string : string) (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
		
		(* Call first part of the function *)
		self#cyclefound_inner termination_function thestate astate astate_depth;
		
		(* table_add blue astate; *)
		table_add blue thestate;
		(*** NOTE: here some factoring using the `custom_string` ***)
		printtable custom_string blue;
		(* and the current state is popped from the cyan list *)
		table_rem cyan thestate;
		()
		

	method private cyclefound_cyclefound (termination_function : unit -> unit) (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
		self#cyclefound_gen termination_function "Blue (cyclefound)" thestate astate astate_depth


	method private cyclefound_cyclecount (termination_function : unit -> unit) (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
		self#cyclefound_gen termination_function "Blue (cyclecount)" thestate astate astate_depth




	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Main method to run NDFS exploration [WORK IN PROGRESS] *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method explore_layer_bfs init_state_index =

		(* Statistics *)
		counter_explore_using_strategy#increment;
		counter_explore_using_strategy#start;

		let options = Input.get_options() in
		limit_reached <- Keep_going;
		
		(************************************)
		(* Initialize basic queues for NDFS algorithms *)
		(************************************)
		cyan			<- Hashtbl.create 100;
		blue			<- Hashtbl.create 100;
		green			<- Hashtbl.create 100;
		greendepth		<- IntMap.empty;
		red				<- Hashtbl.create 100;
		pending			<- []; (* used in the layered algorithms *)
		
		pzone_table		<- Hashtbl.create 100; (* Used as a cache of projected convex parameter zones *)
		pzone_nc_table	<- Hashtbl.create 100; (* Used as a cache of projected non-convex parameter zones *)
		
		(**************************************)
		(* variable for the synthesis results *)
		(**************************************)
		synthesized_constraint <- LinearConstraint.false_p_nnconvex_constraint();


		(***************************************)
		(* put accepting states first in queue *)
		(***************************************)
		let reorderqueue thequeue =
            if options#no_acceptfirst
            then thequeue
            else
            (* requ splits the queue in (accepting, notaccepting) states *)
            let rec requ q =
                    match q with
                    | [] -> ([],[]);
                    | s::q' -> let (a,b) = requ q' in
                            if (State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space s))
                            then (s::a,b)
                            else (a,s::b)
            in
            let (a,b) = requ thequeue in
            a @ b
		in




		(****************************************************************************************)
		(* Check if parameter constraint is included in a non-convex list of convex constraints *)
		(****************************************************************************************)
		let check_parameter_leq_list astate_index =
			print_highlighted_message Shell_bold Verbose_high
				("Testing inclusion of parameter zone in list of collected constraints");
			let astate_constr = self#find_or_compute_pzone_nc astate_index in
			if (LinearConstraint.p_nnconvex_constraint_is_leq astate_constr synthesized_constraint) then (
				print_highlighted_message Shell_bold Verbose_medium("Pruning with inclusion in collected constraints");
				true
			) else false
		in

		(*************************************)
		(* Returns True if thequeue is empty *)
		(*************************************)
(* 		let queue_is_empty thequeue = match thequeue with
			| [] -> true;
			| _ -> false;
		in
 *)
		(**************************************************)
		(* add a state and its depth to the pending queue *)
		(**************************************************)

        let before s t = (* TODO: avoid hash-table lookups for every comparison *)
            match options#pending_order with
            | Pending_none -> true
            | Pending_accept -> State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space s)
            | Pending_param -> (self#smaller_parameter_projection t s)
            | Pending_zone -> (self#smaller_zone t s)
        in
		let add_pending astate_index astate_depth =
			let before_pair (astate_index,_) (bstate_index,_) = before astate_index bstate_index in
			pending <- add_ordered (astate_index,astate_depth) pending before_pair;
			printpendingqueue "Pending (state added)" pending
        in

		(**********************************)
		(* Check the subsumption relation *)
		(**********************************)
		let subsumes bigstate_index smallstate_index =
			(* Does bigstate subsume (or equal) smallstate? *)
			(* Precondition: the states have already the same location *)
			if verbose_mode_greater Verbose_high then(
				print_message Verbose_high "Compare (big?) state:";
				print_message Verbose_high (ModelPrinter.string_of_state model
							(StateSpace.get_state state_space bigstate_index));
				print_message Verbose_high "with (small?) state:";
				print_message Verbose_high (ModelPrinter.string_of_state model
							(StateSpace.get_state state_space smallstate_index)));
			let bigstate_constr = (StateSpace.get_state state_space bigstate_index).px_constraint in
			let smallstate_constr = (StateSpace.get_state state_space smallstate_index).px_constraint in
			(LinearConstraint.px_is_leq smallstate_constr bigstate_constr)
		in

		let setsubsumes setbig smallstate =
			(* Does an element of the set subsume smallstate? *)
			if table_test setbig smallstate then true (* quick pre-check *)
                (* we traverse all states with the same location modulo hash collision *)
                else let similar_states = StateSpace.get_comparable_states state_space smallstate
                		and check_sub bigstate = (table_test setbig bigstate) && (subsumes bigstate smallstate)
					in begin
						if verbose_mode_greater Verbose_high then(
							print_message Verbose_high ("setsubsumes with " ^ string_of_int (List.length similar_states) ^ " states"));
						List.exists check_sub similar_states
					end
		in

		let subsumesset bigstate setsmall =
			(* Does bigstate subsume some element of the set? *)
			if table_test setsmall bigstate then true (* quick pre-check *)
				(* we traverse all states with the same location modulo hash collision *)
				else let similar_states = StateSpace.get_comparable_states state_space bigstate
						and check_sub smallstate = (table_test setsmall smallstate) && (subsumes bigstate smallstate)
					in begin
						if verbose_mode_greater Verbose_high then(
							print_message Verbose_high ("subsumesset with " ^ string_of_int (List.length similar_states) ^ " states"));
						List.exists check_sub similar_states
					end
		in

		let layersetsubsumes setbig smallstate =
			(* Does an element of the set subsume smallstate and is in the same layer? *)
			if table_test setbig smallstate then true (* quick pre-check *)
				(* we traverse all states with the same location modulo hash collision *)
				else let similar_states = StateSpace.get_comparable_states state_space smallstate
						and check_sub bigstate = (table_test setbig bigstate) && (subsumes bigstate smallstate)
							&& (self#same_parameter_projection bigstate smallstate)
					in begin
						if verbose_mode_greater Verbose_high then(
							print_message Verbose_high ("layersetsubsumes with " ^ string_of_int (List.length similar_states) ^ " states"));
						List.exists check_sub similar_states
					end
		in

		(***********************************)
		(* Function to perform a lookahead *)
		(***********************************)
		let withLookahead astate thesuccessors =
			if not options#no_lookahead then (
				if (State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state
						state_space astate)) then (
					(* accepting state: find cyan successor *)
					try ((List.find (fun suc_id ->
						(table_test cyan suc_id)) thesuccessors),
							true)
					with Not_found -> init_state_index, false
				) else (
					(* Not accepting state: find accepting cyan
						successor *)
					try ((List.find (fun suc_id ->
						(State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space suc_id)) &&
						(table_test cyan suc_id)) thesuccessors),
							true)
					with Not_found -> init_state_index, false
				)
			) else init_state_index, false
		in

		(*****************************)
		(* Function for no lookahead *)
		(*****************************)
		let noLookahead astate thesuccessors = init_state_index, false in

		(*******************************************************)
		(* Function for generating successors only when needed *)
		(*******************************************************)
		let find_or_compute_successors thestate =
			let successors = hashtbl_get_or_default (StateSpace.get_transitions_table state_space) thestate [] in
			if successors = [] then (let _ = self#post_from_one_state thestate in ())
			else ()
		in

		(***********************************************************)
		(* Mark a state blue if deadlock or no successor is green, *)
		(* Otherwise mark green and associate the depth in the map *)
		(***********************************************************)
		let mark_blue_or_green thestate (thedepth : int) =
			(* mark only if not blue due to lookahead *)
			(* if the green colour is not used (option no_green),
				then the state is necessarily marked blue *)
			if not (table_test blue thestate) then (
				let successors = StateSpace.get_successors state_space thestate
					and is_green astate = table_test green astate
					and is_pending_not_blue astate =
						(in_queue astate pending) &&
							not (table_test blue astate)
				in
				if ( options#no_green || List.exists is_green successors ||
					(pending <> [] &&
						List.exists is_pending_not_blue successors))
				then ( if options#recompute_green &&
						(not (is_green thestate) ||
							(IntMap.find thestate greendepth) > thedepth)
					then (greendepth <- IntMap.add thestate thedepth greendepth);
					table_add green thestate;
					printtable "Green (mark_blue_or_green)" green)
				else (  table_add blue thestate;
					printtable "Blue (mark_blue_or_green)" blue)
				)
		in

		(***********************************************)
		(* Test if a green state should be re-explored *)
		(***********************************************)
		let test_reexplore_green thestate (thedepth : int) =
			print_message Verbose_high ("?? re-explore state " ^ (string_of_int thestate));
			printtable "Green (test_reexplore_green)" green;
			if not (table_test green thestate)
			then true
			else( (* it is green => check the depth if recomputation is required *)
 				if options#recompute_green &&
					(IntMap.find thestate greendepth) > thedepth
				then( (* the reexplored state must also be removed from previous red exploration *)
					table_rem green thestate;
					table_rem red thestate;
					true
				) else false)
		in

		(***************************)
		(* General Scheme of a DFS *)
		(***************************)
		let rec rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs
			testrecursivedfs postdfs thestate thestate_depth =
			(* Check the termination condition *)
			self#check_and_update_queue_dfs_limit;
			(* Update termination condition *)
			begin
			match limit_reached with
			(*** NOTE: check None, as it may have been edited from outside, in which case it should not be Regular_termination ***)
			| Keep_going when termination_status = None -> termination_status <- Some (Result.Regular_termination)
			(*** NOTE: obliged to comment out the condition below, otherwise there is a compiling warning… ***)
			| Keep_going (*when termination_status <> None*) -> ()

			(* Termination due to time limit reached *)
			| Time_limit_reached -> termination_status <- Some (Result.Time_limit (Hashtbl.length cyan)); execute_again <- false

			(* Termination due to a number of explored states reached *)
			| States_limit_reached -> termination_status <- Some (Result.States_limit (Hashtbl.length cyan)) ; execute_again <- false

			(* Termination due to state space depth limit reached *)
			| Depth_limit_reached -> termination_status <- Some (Result.Depth_limit (Hashtbl.length cyan))
			
			(* Termination because a witness has been found *)
			(*** NOTE/TODO: add a new result termination type? ***)
			| Witness_found -> termination_status <- Some (Result.Regular_termination)
			end;
			if (limit_reached <> Keep_going) then raise (TerminateAnalysis)
			else(
			print_highlighted_message Shell_bold Verbose_medium("Executing rundfs at depth "
				^ (string_of_int thestate_depth)
				^ " with "
				^ (if State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space thestate)
					then "accepting " else "")
				^ "state "
				^ (StateSpace.string_of_state_index thestate)
				^ ":\n"
				^ (ModelPrinter.string_of_state model
					(StateSpace.get_state state_space thestate)));
			let depth_ok = match options#depth_limit with
				| None -> if the_depth_step = -1 then true
										else if (current_depth  >= thestate_depth) then true
											else (depth_reached <- true; false)
				| Some depth_value -> if the_depth_step = -1 then (if (depth_value  >= thestate_depth) then true
											else (depth_reached <- true; false)
										)
										else if (current_depth  >= thestate_depth) then true
											else (depth_reached <- true; false)
			in
(* 			let is_green astate = table_test green astate in *)
			if (depth_ok && enterdfs thestate) then (
				if (thestate_depth > max_depth_reached) then max_depth_reached <- thestate_depth;
				predfs thestate;
				let successors = reorderqueue (StateSpace.get_successors state_space thestate) in
				let rec process_sucs suclist = match suclist with
					| [] -> ();
					| suc_id::body ->
						if verbose_mode_greater Verbose_medium then(
							print_message Verbose_medium("Handling "
								^ (if State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space suc_id)
									then "accepting " else "")
								^ "successor "
								^ (ModelPrinter.string_of_state model
									(StateSpace.get_state state_space suc_id))));
						if (filterdfs thestate suc_id (thestate_depth + 1)) then (
							if (testaltdfs thestate suc_id) then (alternativedfs suc_id thestate_depth)
							else
							if (testrecursivedfs suc_id) then (
								rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs suc_id (thestate_depth + 1)
							)
						);
						process_sucs body
				in
				let cyclestate, found =
						lookahead thestate successors in
				(* if the cycle is found:
					- in the standard version, an exception is raised and the algorithm terminates
					- in the collecting version, the other sucessors cannot lead to a better zone,
						so there is no need to process them.
						However, the state must be marked blue and removed from cyan in the blue dfs *)
				if (found) then
					cyclefound thestate cyclestate thestate_depth
				else (process_sucs successors;
					postdfs thestate thestate_depth)
			) else (* thestate is not explored because it is either too deep or covered by the constraint already *)
				if not depth_ok && not (table_test blue thestate) then (
					table_add green thestate;
					if options#recompute_green
					then greendepth <- IntMap.add thestate thestate_depth greendepth;
					()
				)
		)
		in

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                     State Space Exploration                       *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(* Get the property to check whether we are in synthesis or witness mode *)
		let property = Input.get_property() in

		(* loop for iterative deepening, otherwise used only once *)

		(match options#depth_init, options#depth_step with
			| None, None -> current_depth <- -1; the_depth_step <- -1
			| Some depth_value, None ->
				current_depth <- depth_value;
				the_depth_step <- 1
			| None, Some step_value ->
				current_depth <- 0;
				if step_value > 0 then the_depth_step <- step_value
				else the_depth_step <- 1
			| Some depth_value, Some step_value ->
				current_depth <- depth_value;
				if step_value > 0 then the_depth_step <- step_value
				else the_depth_step <- 1
			);
		
		max_depth <- (match options#depth_limit with
						| None -> -1
						| Some depth_value -> depth_value);

		while execute_again do
			if the_depth_step = -1 then execute_again <- false;
			if the_depth_step <> -1 && options#depth_limit <> None && current_depth >= max_depth then(
				execute_again <- false;
				current_depth <- max_depth
			);

			print_message Verbose_standard("---------------- Starting exploration ----------------");
			if the_depth_step <> -1 then(
				print_message Verbose_standard("---------------- until depth " ^ (string_of_int current_depth) ^ " ----------------");
				(* Clear the colours of previous iteration *)
				Hashtbl.clear cyan;
				Hashtbl.clear green;
				(* We keep the blue states from the previous run unless we do not use the green colour *)
				if options#no_green then Hashtbl.clear blue;
				greendepth <- IntMap.empty;
				processed_blue <- 0;
				cyclecount <-0;
				Hashtbl.clear red;
				depth_reached <- false;
			);

			begin
			match options#layer, options#subsumption with

				(* No layer, no subsumption *)
				| false, false ->
					(* set up the dfs blue calls *)
					let enterdfs (astate : State.state_index) : bool =
						if (property.synthesis_type = Synthesis &&
								check_parameter_leq_list astate) then (
							table_add blue astate;
							printtable "Blue (enterdfs)" blue;
							false
						) else true in
					let predfs (astate : State.state_index) : unit =
						processed_blue <- processed_blue + 1;
						table_add cyan astate;
						printtable "Cyan (predfs)" cyan;
						find_or_compute_successors astate
					in
(*					let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
						cyclecount <- cyclecount + 1;
						total_cyclecount <- total_cyclecount + 1;
						if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
						if (property.synthesis_type = Witness) then
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
						else print_highlighted_message Shell_bold Verbose_standard
								("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
						if verbose_mode_greater Verbose_low then(
							print_message Verbose_medium
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							self#print_projection Verbose_low astate);
						(* For synthesis: we do not stop immediately *)
						if (property.synthesis_type = Synthesis) then
							termination_status <- Some Regular_termination
						else termination_status <- Some Target_found;
						let pzone = self#find_or_compute_pzone astate in
						LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
						if (property.synthesis_type = Witness) then raise TerminateAnalysis;
						(* table_add blue astate; *)
						table_add blue thestate;
						printtable "Blue (cyclefound)" blue;
						(* and the current state is popped from the cyan list *)
						table_rem cyan thestate;
					in*)
					let filterdfs (thestate : State.state_index) (astate : State.state_index)
						(astate_depth : int) : bool =
						not (table_test blue astate) &&
						test_reexplore_green astate astate_depth &&
						not (table_test cyan astate)
					in
					let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
						false in
					let alternativedfs (astate : State.state_index) (astate_depth : int) : unit =
						() in
					let testrecursivedfs (astate : State.state_index) : bool =
						true in
					let postdfs (astate : State.state_index) (astate_depth : int) : unit =
						(* launch red dfs only if not with a smaller constraint than a state marked by a lookahead *)
						if ((* (not (check_parameter_leq_list astate)) && *)
								(State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space astate))) then (
							(* set up the dfs red calls *)
							let enterdfs (astate : State.state_index) : bool =
								not (check_parameter_leq_list astate) in
							let predfs (astate : State.state_index) : unit =
								table_add red astate;
								printtable "Red (predfs)" red in
							(*let cyclefound2 (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
								cyclecount <- cyclecount + 1;
								total_cyclecount <- total_cyclecount + 1;
								if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
								if (property.synthesis_type = Witness) then
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
								else print_highlighted_message Shell_bold Verbose_standard
									("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
								if verbose_mode_greater Verbose_low then (
									print_message Verbose_medium
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									self#print_projection Verbose_low astate);
								(* For synthesis: we do not stop immediately *)
								termination_status <- Some Target_found;
								let pzone = self#find_or_compute_pzone astate in
								LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
								if (property.synthesis_type = Witness) then raise TerminateAnalysis;
							in*)
							let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
								(self#same_parameter_projection thestate astate)
							in
							let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
								(table_test cyan astate)
							in
							let alternativedfs (astate : State.state_index) (astate_depth : int) : unit =
								(self#cyclefound_inner self#set_termination) astate astate astate_depth
							in
							let testrecursivedfs (astate : State.state_index) : bool =
								not (table_test red astate)
							in
							let postdfs (astate : State.state_index) (astate_depth : int) : unit =
								() in
							rundfs enterdfs predfs noLookahead (self#cyclefound_inner self#set_termination) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate astate_depth
						);
						mark_blue_or_green astate astate_depth;
						table_rem cyan astate;
					in
					(try (rundfs enterdfs predfs withLookahead (self#cyclefound_cyclefound self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index 0;)
						with TerminateAnalysis -> ());
					print_message Verbose_medium("Finished the calls")

				(* No layer, subsumption *)
				| false, true ->
					(* set up the dfs blue calls *)
					let enterdfs (astate : State.state_index) : bool =
						if (property.synthesis_type = Synthesis && check_parameter_leq_list astate) then (
							(* State astate has been handled and must now become blue *)
							table_add blue astate;
							printtable "Blue (enterdfs)" blue;
							false
						) else true
					in
					let predfs (astate : State.state_index) : unit =
						processed_blue <- processed_blue + 1;
						table_add cyan astate;
						printtable "Cyan (predfs)" cyan;
						find_or_compute_successors astate
					in
					(*let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
						cyclecount <- cyclecount + 1;
						total_cyclecount <- total_cyclecount + 1;
						if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
						if (property.synthesis_type = Witness) then
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
						else print_highlighted_message Shell_bold Verbose_standard
								("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
						if verbose_mode_greater Verbose_low then (
							print_message Verbose_medium
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							self#print_projection Verbose_low astate);
						(* For synthesis: we do not stop immediately *)
						if (property.synthesis_type = Synthesis) then
							termination_status <- Some Regular_termination
						else termination_status <- Some Target_found;
						let pzone = self#find_or_compute_pzone astate in
						LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
						if (property.synthesis_type = Witness) then raise TerminateAnalysis;
						(* the state where the lookahead has found a cycle is now set blue *)
						(* table_add blue astate; *)
						table_add blue thestate;
						printtable "Blue (cyclefound)" blue;
						(* and the current state is popped from the cyan list *)
						table_rem cyan thestate;
					in*)
					let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
						not (table_test blue astate) &&
						test_reexplore_green astate astate_depth &&
						not (table_test cyan astate) &&
						not (setsubsumes red astate)
					in
					let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
						false in
					let alternativedfs (astate: State.state_index) (astate_depth : int) : unit =
						() in
					let testrecursivedfs (astate: State.state_index) : bool =
						true in
					let postdfs (astate: State.state_index) (astate_depth : int) : unit =
						(* launch red dfs only if not with a smaller constraint than a state marked by a lookahead *)
						if ((* (not (check_parameter_leq_list astate)) && *)
								(State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space astate))) then (
							(* set up the dfs red calls *)
							let enterdfs (astate: State.state_index) : bool =
								not (check_parameter_leq_list astate) in
							let predfs (astate: State.state_index) : unit =
								table_add red astate;
								printtable "Red (predfs)" red in
							(*let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
								cyclecount <- cyclecount + 1;
								total_cyclecount <- total_cyclecount + 1;
								if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
								if (property.synthesis_type = Witness) then
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
								else print_highlighted_message Shell_bold Verbose_standard
									("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
								if verbose_mode_greater Verbose_low then (
									print_message Verbose_medium
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									self#print_projection Verbose_low astate);
								(* For synthesis: we do not stop immediately *)
								if (property.synthesis_type = Synthesis) then
									termination_status <- Some Regular_termination
								else termination_status <- Some Target_found;
								let pzone = self#find_or_compute_pzone astate in
								LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
								if (property.synthesis_type = Witness) then raise TerminateAnalysis;
							in*)
							let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
								(self#same_parameter_projection thestate astate)
							in
							let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
								(subsumesset astate cyan)
							in
							let alternativedfs (astate : State.state_index) (astate_depth : int) : unit =
								(self#cyclefound_inner self#set_termination_if_synthesis) astate astate astate_depth
							in
							let testrecursivedfs (astate : State.state_index) : bool =
								not (setsubsumes red astate)
							in
							let postdfs (astate : State.state_index) (astate_depth : int) : unit =
								() in
							rundfs enterdfs predfs noLookahead (self#cyclefound_inner self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate astate_depth
						);
						mark_blue_or_green astate astate_depth;
						table_rem cyan astate;
					in
					(try (rundfs enterdfs predfs withLookahead (self#cyclefound_cyclefound self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index 0;)
						with TerminateAnalysis -> ());
					print_message Verbose_medium("Finished the calls")

				(* Layers, no subsumption *)
				| true, false ->
					(* set up the dfs blue calls *)
					add_pending init_state_index 0;
					(try (while pending <> [] do
						match pending with
						| [] -> raise (InternalError "Impossible case in NDFS (Layers, no subsumption)")
						| (thestate,thestate_depth)::body ->
							pending <- body;
							print_message Verbose_medium ("Popped state "
								^ (string_of_int thestate));
							printpendingqueue "Pending" pending;
							if (not (table_test blue thestate) &&
								test_reexplore_green thestate thestate_depth) then
							begin
							let enterdfs (astate : State.state_index) : bool =
								if (property.synthesis_type = Synthesis && check_parameter_leq_list astate) then (
									(* State astate has been handled and must now become blue *)
									table_add blue astate;
									printtable "Blue (enterdfs)" blue;
									false
								) else true
							in
							let predfs (astate : State.state_index) : unit =
								processed_blue <- processed_blue + 1;
								table_add cyan astate;
								printtable "Cyan (preds)" cyan;
								find_or_compute_successors astate
							in
							(*let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
								cyclecount <- cyclecount + 1;
								total_cyclecount <- total_cyclecount + 1;
								if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
								if (property.synthesis_type = Witness) then
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
								else print_highlighted_message Shell_bold Verbose_standard
										("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
								if verbose_mode_greater Verbose_low then (
									print_message Verbose_medium
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									self#print_projection Verbose_low astate);
								(* For synthesis: we do not stop immediately *)
								if (property.synthesis_type = Synthesis) then
									termination_status <- Some Regular_termination
								else termination_status <- Some Target_found;
								let pzone = self#find_or_compute_pzone astate in
								LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
								if (property.synthesis_type = Witness) then raise TerminateAnalysis;
								(* the state where the lookahead has found a cycle is now set blue *)
								(* table_add blue astate; *)
								table_add blue thestate;
								printtable "Blue (cyclecount)" blue;
								(* and the current state is popped from the cyan list *)
								table_rem cyan thestate;
							in*)
							let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
								not (table_test blue astate) &&
								test_reexplore_green astate astate_depth &&
								not (table_test cyan astate)
							in
							let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
								not (self#same_parameter_projection thestate astate)
							in
							let alternativedfs (astate: State.state_index) (astate_depth : int) : unit =
								add_pending astate (astate_depth + 1) in
							let testrecursivedfs (astate: State.state_index) : bool =
								true in
							let postdfs (astate: State.state_index) (astate_depth : int) : unit =
								(* launch red dfs only if not with a smaller constraint than a state marked by a lookahead *)
								if ((* (not (check_parameter_leq_list astate)) && *)
										(State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space astate))) then (
									(* set up the dfs red calls *)
									let enterdfs (astate: State.state_index) : bool =
										not (check_parameter_leq_list astate) in
									let predfs (astate: State.state_index) : unit =
										table_add red astate;
										printtable "Red (predfs)" red in
(*									let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
										cyclecount <- cyclecount + 1;
										total_cyclecount <- total_cyclecount + 1;
										if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
										if (property.synthesis_type = Witness) then
											print_highlighted_message Shell_bold Verbose_standard
												("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
										else print_highlighted_message Shell_bold Verbose_standard
											("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
										if verbose_mode_greater Verbose_low then (
											print_message Verbose_medium
												(ModelPrinter.string_of_state model
													(StateSpace.get_state state_space astate));
											self#print_projection Verbose_low astate);
										(* For synthesis: we do not stop immediately *)
										if (property.synthesis_type = Synthesis) then
											termination_status <- Some Regular_termination
										else termination_status <- Some Target_found;
										let pzone = self#find_or_compute_pzone astate in
										LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
									if (property.synthesis_type = Witness) then raise TerminateAnalysis;
									in*)
									let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
										(self#same_parameter_projection thestate astate)
									in
									let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
										(table_test cyan astate)
									in
									let alternativedfs (astate : State.state_index) (astate_depth : int) : unit =
										(self#cyclefound_inner self#set_termination_if_synthesis) astate astate astate_depth
									in
									let testrecursivedfs (astate : State.state_index) : bool =
										not (table_test red astate)
									in
									let postdfs (astate : State.state_index) (astate_depth : int) : unit =
										() in
									rundfs enterdfs predfs noLookahead (self#cyclefound_inner self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate astate_depth
								);
								mark_blue_or_green astate astate_depth;
								table_rem cyan astate;
								in
							rundfs enterdfs predfs withLookahead (self#cyclefound_cyclecount self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs thestate thestate_depth;
							end;
					done;)

								with TerminateAnalysis -> ());
					print_message Verbose_medium("Finished the calls")

				(* Layers, subsumption *)
				| true, true ->
					(* set up the dfs blue calls *)
					add_pending init_state_index 0;
					(try (while pending <> [] do
						match pending with
						| [] -> raise (InternalError "Impossible case in NDFS (Layers, subsumption)")
						| (thestate,thestate_depth)::body ->
							pending <- body;
							print_message Verbose_medium ("Popped state "
								^ (string_of_int thestate));
							printpendingqueue "Pending" pending;
							if (not (table_test blue thestate) &&
								test_reexplore_green thestate thestate_depth) then
							begin
							let enterdfs (astate : State.state_index) : bool =
								if (property.synthesis_type = Synthesis && check_parameter_leq_list astate) then (
									(* State astate has been handled and must now become blue *)
									table_add blue astate;
									printtable "Blue (enterdfs)" blue;
									false
								) else true
							in
							let predfs (astate : State.state_index) : unit =
								processed_blue <- processed_blue + 1;
								table_add cyan astate;
								printtable "Cyan (predfs)" cyan;
								find_or_compute_successors astate
							in
							(*let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
								cyclecount <- cyclecount + 1;
								total_cyclecount <- total_cyclecount + 1;
								if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
								if (property.synthesis_type = Witness) then
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
								else print_highlighted_message Shell_bold Verbose_standard
										("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
								if verbose_mode_greater Verbose_low then (
									print_message Verbose_medium
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									self#print_projection Verbose_low astate);
								(* For synthesis: we do not stop immediately *)
								if (property.synthesis_type = Synthesis) then
									termination_status <- Some Regular_termination
								else termination_status <- Some Target_found;
								let pzone = self#find_or_compute_pzone astate in
								LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
								if (property.synthesis_type = Witness) then raise TerminateAnalysis;
								(* the state where the lookahead has found a cycle is now set blue *)
								(* table_add blue astate; *)
								table_add blue thestate;
								printtable "Blue (cyclecount)" blue;
								(* and the current state is popped from the cyan list *)
								table_rem cyan thestate;
							in*)
							let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
								not (table_test blue astate) &&
								test_reexplore_green astate astate_depth &&
								not (table_test cyan astate) &&
								not (layersetsubsumes red astate)
							in
							let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
								not (self#same_parameter_projection thestate astate)
							in
							let alternativedfs (astate: State.state_index) (astate_depth : int) : unit =
								add_pending astate (astate_depth + 1) in
							let testrecursivedfs (astate: State.state_index) : bool =
								true in
							let postdfs (astate: State.state_index) (astate_depth : int) : unit =
								(* launch red DFS only if not with a smaller constraint than a state marked by a lookahead *)
								if ((* (not (check_parameter_leq_list astate)) && *)
										(State.match_state_predicate model.is_accepting state_predicate (StateSpace.get_state state_space astate))) then (
									(* set up the DFS red calls *)
									let enterdfs (astate: State.state_index) : bool =
										not (check_parameter_leq_list astate) in
									let predfs (astate: State.state_index) : unit =
										table_add red astate;
										printtable "Red (predfs)" red in
									(*let cyclefound (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : unit =
										cyclecount <- cyclecount + 1;
										total_cyclecount <- total_cyclecount + 1;
										if (astate_depth < min_depth_found || min_depth_found = -1) then min_depth_found <- astate_depth;
										if (property.synthesis_type = Witness) then
											print_highlighted_message Shell_bold Verbose_standard
												("Cycle found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth))
										else print_highlighted_message Shell_bold Verbose_standard
											("Cycle " ^ (string_of_int total_cyclecount) ^ " found at state " ^ (string_of_int astate) ^ ", depth " ^ (string_of_int astate_depth));
										if verbose_mode_greater Verbose_low then (
											print_message Verbose_medium
												(ModelPrinter.string_of_state model
													(StateSpace.get_state state_space astate));
											self#print_projection Verbose_low astate);
										(* For synthesis: we do not stop immediately *)
										if (property.synthesis_type = Synthesis) then
											termination_status <- Some Regular_termination
										else termination_status <- Some Target_found;
										let pzone = self#find_or_compute_pzone astate in
										LinearConstraint.p_nnconvex_p_union_assign synthesized_constraint pzone;
									if (property.synthesis_type = Witness) then raise TerminateAnalysis;
									in*)
									let filterdfs (thestate : State.state_index) (astate : State.state_index) (astate_depth : int) : bool =
										(self#same_parameter_projection thestate astate)
									in
									let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
										(subsumesset astate cyan)
									in
									let alternativedfs (astate : State.state_index) (astate_depth : int) : unit =
										(self#cyclefound_inner self#set_termination_if_synthesis) astate astate astate_depth
									in
									let testrecursivedfs (astate : State.state_index) : bool =
										not (layersetsubsumes red astate)
									in
									let postdfs (astate : State.state_index) (astate_depth : int) : unit =
										() in
									rundfs enterdfs predfs noLookahead (self#cyclefound_inner self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate astate_depth
								);
								mark_blue_or_green astate astate_depth;
								table_rem cyan astate;
								in
							rundfs enterdfs predfs withLookahead (self#cyclefound_cyclecount self#set_termination_if_synthesis) filterdfs testaltdfs alternativedfs testrecursivedfs postdfs thestate thestate_depth;
							end;
					done;)

								with TerminateAnalysis -> ());
					print_message Verbose_medium("Finished the calls")

(* 	                       | _ -> raise (InternalError ("Unknown exploration order in NDFS")) *)
			end;

			print_message Verbose_standard("---------------- Ending exploration ------------------");

			if not depth_reached then execute_again <- false;

			printtable "Blue (end while depth)" blue;
			printtable "Green (end while depth)" green;
			printtable "Cyan (end while depth)" cyan;
			printtable "Red (end while depth)" red;

			total_processed_blue <- total_processed_blue + processed_blue;

			if execute_again then(
				ResultProcessor.process_result self#compute_result "Iterative deepening" None;
				current_depth <- current_depth + the_depth_step;
				()
			);
		done; (* end of the big loop *)

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                 End of State Space Exploration                    *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		(* Statistics *)
		counter_explore_using_strategy#stop;

		(* The end *)
		()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;

		(* Nothing to do *)
		()

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Add a new state to the state space (if indeed needed) *)
	(* Side-effects: modify new_states_indexes *)
	(*** TODO: move new_states_indexes to a variable of the class ***)
	(* Return true if the state is not discarded by the algorithm, i.e., if it is either added OR was already present before *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method add_a_new_state source_state_index combined_transition new_state =
		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space options#comparison_operator new_state in

		begin
		match addition_result with
		(* If the state was present: do nothing *)
		| StateSpace.State_already_present _ -> ()
		(* If this is really a new state, or a state larger than a former state *)
		| StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index ->

			(* First check whether this is a bad tile according to the property and the nature of the state *)
			(*** NOTE: in fact not necessary for this algorithm ***)
			self#update_statespace_nature new_state;

			(* Add the state_index to the list of new states (used to compute their successors at the next iteration) *)
			new_states_indexes <- new_state_index :: new_states_indexes;

		end; (* end if new state *)


		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)

		(* Update the transitions *)
		self#add_transition_to_state_space (source_state_index, combined_transition, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;

		(* The state is necessarily kept by the algorithm *)
		true


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(** Actions to perform with the initial state; returns true unless the initial state cannot be kept (in which case the algorithm will stop immediately) *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method process_initial_state _ =
		(* Always keep the initial state *)
		true


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
	method check_termination_at_post_n = false


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		self#print_algo_message_newline Verbose_standard (
			"State space exploration completed " ^ (after_seconds ()) ^ "."
		);

		let nb_states = StateSpace.nb_states state_space in
		print_message Verbose_standard ("Number of computed states: " ^ (string_of_int nb_states));
		print_message Verbose_standard ("Number of processed states: " ^ (string_of_int processed_blue));
		if the_depth_step <> -1 then
			print_message Verbose_standard ("Total number of processed states: " ^ (string_of_int total_processed_blue));
		print_message Verbose_standard ("Number of cycles found: " ^ (string_of_int cyclecount));
		if the_depth_step <> -1 then
			print_message Verbose_standard ("Total number of cycles found: " ^ (string_of_int total_cyclecount));
		if (min_depth_found <> -1) then
			print_message Verbose_standard ("Minimum depth at which a cycle was found: " ^ (string_of_int min_depth_found));
		print_message Verbose_standard ("Maximal depth actually reached: " ^ (string_of_int max_depth_reached));

		(* Get the termination status *)
		let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in NDFS exploration")
			| Some status -> status
		in

 		(* LP: the condition with depth_reached is NECESSARY:
 			the depth_limit is handled with boolean depth_reached as the depth
 			exception would stop the exploration too early *)
 		let completed =
 				(termination_status = Regular_termination && not depth_reached) in
		
		(*** TODO (ÉA): is Including_check dangerous? ***)
		let dangerous_inclusion = options#comparison_operator = Inclusion_check || options#comparison_operator = Including_check || options#comparison_operator = Double_inclusion_check in
		
		if dangerous_inclusion then(
			self#print_algo_message Verbose_high "A dangerous inclusion was used: result will be at least an overapproximation (or invalid)";
		);
		if options#merge then(
			self#print_algo_message Verbose_high "Merging was used: result will be at least an overapproximation (or invalid)";
		);

		let abstracted = (dangerous_inclusion || options#merge) in

 		let soundness =
			(* EXACT if termination is normal and no inclusion nor merge *)
			if completed && not abstracted then Constraint_exact
			(* UNDER-APPROXIMATED if termination is NOT normal AND neither merging nor state inclusion was used *)
			else if not completed && not abstracted then Constraint_maybe_under
			(* OVER-APPROXIMATED if termination is normal AND merging or state inclusion was used *)
			else if completed && abstracted then Constraint_maybe_over
			(* UNKNOWN otherwise *)
			else Constraint_maybe_invalid
		in

		(* Return result *)
		Single_synthesis_result
		{
			(* Accepting_cycle_constraint *)
			result = Good_constraint (synthesized_constraint, soundness);

			(* English description of the constraint *)
			constraint_description = "constraint for detecting accepting cycles via NDFS exploration";

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
