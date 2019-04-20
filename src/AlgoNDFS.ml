(************************************************************
 *
 *                       IMITATOR
 * 
 * LIPN, Université Paris 13 (France)
 * 
 * Module description: NDFS algorithms
 * 
 * File contributors : Laure Petrucci
 * Created           : 2019/03/12
 * Last modified     : 2019/03/12
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
open AlgoStateBased
open Statistics

(************************************************************)
(************************************************************)
(* Types and exceptions for NDFS *)
(************************************************************)
(************************************************************)


exception DFS_Limit_detected of bfs_limit_reached

(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoNDFS =
	object (self) inherit algoStateBased as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	(* Parameter valuations in all |P| dimensions for which the target is reached *)
	val mutable constraint_valuations : LinearConstraint.p_nnconvex_constraint option = None
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "NDFS"	

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
		(* basic queues for NDFS algorithms *)
		(************************************)
		let cyan = ref [] in
		let blue = ref [] in
		let red = ref [] in
		let pending = ref [] in (* used in the layered algorithms *)
		let constraint_list = ref [] in (* list of results found *)

		(***********************)
		(* printing the queues *)
		(***********************)
	    let printqueue colour thequeue =
			let rec r_printqueue thequeue = match thequeue with
				| [] -> "";
				| state_index::body  ->
					(string_of_int state_index) ^ " " ^ (r_printqueue body);
			in print_message Verbose_low("Queue " ^ colour ^ " : [ "
					^ r_printqueue thequeue ^ "]")
	    in

		(***************************************)
		(* put accepting states first in queue *)
		(***************************************)
	    let reorderqueue thequeue =
	    	let newqueue = ref [] in
	    	List.iter (fun astate ->
	    		if (State.is_accepting (StateSpace.get_state state_space astate)) then (
	    			newqueue := astate::(!newqueue);
	    		) else (newqueue := (!newqueue)@[astate];)
            ) thequeue;
            (!newqueue)
	    in

		(***************************************************)
		(* Check equality of zone projection on parameters *)
		(***************************************************)
		let same_parameter_projection state1 state2 =
			let state1_loc, state1_constr = StateSpace.get_state state_space state1 in
			let constr1 = LinearConstraint.px_hide_nonparameters_and_collapse state1_constr in
			let state2_loc, state2_constr = StateSpace.get_state state_space state2 in
			let constr2 = LinearConstraint.px_hide_nonparameters_and_collapse state2_constr in
			print_message Verbose_high ("Projected contraint 1: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr1
				^ " state: "
				^ (StateSpace.string_of_state_index state1));
			print_message Verbose_high ("Projected contraint 2: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr2
				^ " state: "
				^ (StateSpace.string_of_state_index state2));
			LinearConstraint.p_is_equal constr1 constr2
		in

		(****************************************************)
		(* Check inclusion of zone projection on parameters *)
		(****************************************************)
		let smaller_parameter_projection state1 state2 =
			let state1_loc, state1_constr = StateSpace.get_state state_space state1 in
			let constr1 = LinearConstraint.px_hide_nonparameters_and_collapse state1_constr in
			let state2_loc, state2_constr = StateSpace.get_state state_space state2 in
			let constr2 = LinearConstraint.px_hide_nonparameters_and_collapse state2_constr in
			print_message Verbose_high ("Projected contraint 1: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr1
				^ " state: "
				^ (StateSpace.string_of_state_index state1));
			print_message Verbose_high ("Projected contraint 2: \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr2
				^ " state: "
				^ (StateSpace.string_of_state_index state2));
			LinearConstraint.p_is_leq constr1 constr2
		in

		(*********************************************************************)
		(* Check inclusion of zone projection on parameters wrt a constraint *)
		(*********************************************************************)
		let smaller_parameter_constraint astate aconstraint =
			let astate_loc, astate_constr = StateSpace.get_state state_space astate in
			let aconstr = LinearConstraint.px_hide_nonparameters_and_collapse astate_constr in
			print_message Verbose_high ("Projected contraint : \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names aconstr
				^ " state: "
				^ (StateSpace.string_of_state_index astate));
			LinearConstraint.p_is_leq aconstr aconstraint
		in

		(*************************************)
        (* Returns True if thequeue is empty *)
		(*************************************)
        let queue_is_empty thequeue = match thequeue with
            | [] -> true;
            | _ -> false;
        in

		(************************************)
		(* add a state to the pending queue *)
		(************************************)
		let add_pending astate =
			(* standard queuing *)
			(* pending := astate::(!pending); *)
			(* add the state in the right place in the queue:
				larger zones first *)
			if (queue_is_empty !pending) then
				pending := [astate]
			else (
				let newpending = ref [] in
				while not (queue_is_empty !pending) do
					match (!pending) with
						| first_state::body ->
							if (smaller_parameter_projection first_state astate) then (
								(* insert a state before the current state *)
								newpending := (!newpending)@[astate];
								newpending := (!newpending)@(!pending);
								pending := [];
							) else (
								newpending := (!newpending)@[first_state];
								pending := body;
								if (queue_is_empty !pending) then
									(* no more states to compare with *)
									newpending := (!newpending)@[astate];
							)
				done;
				pending := !newpending;
			);
			printqueue "Pending (state added)" !pending
		in

		(**********************************)
		(* Check the subsumption relation *)
		(**********************************)
		let subsumes bigstate smallstate =
			(* Does bigstate subsume smallstate? *)
			print_message Verbose_high "Compare (big?) state:";
			print_message Verbose_high (ModelPrinter.string_of_state model
						(StateSpace.get_state state_space bigstate));
			print_message Verbose_high "with (small?) state:";
			print_message Verbose_high (ModelPrinter.string_of_state model
						(StateSpace.get_state state_space smallstate));
			(* get the big state *)
			let stateb_loc, stateb_constr = StateSpace.get_state state_space bigstate in
			(* get the small state *)
			let states_loc, states_constr = StateSpace.get_state state_space smallstate in
			(* check that the location is the same *)
			if not (Location.location_equal stateb_loc states_loc) then false
			else (LinearConstraint.px_is_leq states_constr stateb_constr) (* check the inclusion of constraints *)
		in

		let setsubsumes setbig smallstate = 
			(* Does an element of the set subsume smallstate? *)
			List.exists (fun bigstate -> (subsumes bigstate smallstate)) setbig
		in

		let subsumesset bigstate setsmall = 
			(* Does bigstate subsume some element of the set? *)
			List.exists (fun smallstate -> (subsumes bigstate smallstate)) setsmall
		in

		let layersetsubsumes setbig smallstate = 
			(* Does an element of the set subsume smallstate and is in the same layer? *)
			List.exists (fun bigstate -> 
				((same_parameter_projection bigstate smallstate) && (subsumes bigstate smallstate))
			) setbig
		in

		(******************************************)
		(* printing zone projection on parameters *)
		(******************************************)
	    let print_projection verbose_level thestate =
			let state_loc, state_constr = StateSpace.get_state state_space thestate in
			let constr = LinearConstraint.px_hide_nonparameters_and_collapse state_constr in
			print_message verbose_level ("Projected contraint : \n"
				^ LinearConstraint.string_of_p_linear_constraint model.variable_names constr)
	    in

		(***************************)
		(* General Scheme of a DFS *)
		(***************************)
		let rec rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs
			testrecursivedfs postdfs thestate =
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
			| Time_limit_reached -> termination_status <- Some (Result.Time_limit (List.length !cyan))
						
			(* Termination due to a number of explored states reached *)
			| States_limit_reached -> termination_status <- Some (Result.States_limit (List.length !cyan))
			end;
			if (limit_reached <> Keep_going) then raise (TerminateAnalysis)
			else(
			print_message Verbose_low("Executing rundfs with "
				^ (if State.is_accepting (StateSpace.get_state state_space thestate)
					then "accepting " else "")
				^ "state "
				^ (ModelPrinter.string_of_state model
					(StateSpace.get_state state_space thestate)));
			if (enterdfs thestate) then (
				predfs thestate;
				let successors = reorderqueue (StateSpace.get_successors state_space thestate) in
				let rec process_sucs suclist = match suclist with
					| [] -> ();
					| suc_id::body -> 
						print_message Verbose_low("Handling "
							^ (if State.is_accepting (StateSpace.get_state state_space suc_id)
								then "accepting " else "")
							^ "successor "
							^ (ModelPrinter.string_of_state model
								(StateSpace.get_state state_space suc_id)));				
						if (filterdfs thestate suc_id) then (
							if (testaltdfs thestate suc_id) then (alternativedfs suc_id)
							else 
							if (testrecursivedfs suc_id) then (
								rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs suc_id)
						);
						process_sucs body;
                in
                let cyclestate, found = lookahead successors in
                (* if the cycle is found:
                	- in the standard version, an exception is raised and the algorithm terminates
                	- in the collecting version, the other sucessors cannot lead to a better zone,
                		so there is no need to process them.
                		However, the state must be marked blue and removed from cyan in the blue dfs *)
                if (found) then 
                	cyclefound thestate cyclestate
                else (process_sucs successors;
 					postdfs thestate)
			))
		in

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                     State Space Exploration                       *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		print_message Verbose_standard("---------------- Starting exploration ----------------");
 
        begin
        match options#exploration_order with
            | Exploration_NDFS -> 
(* Classical NDFS exploration *)
				print_message Verbose_standard("Using the option NDFS");
				(* set up the dfs blue calls *)
				let enterdfs (astate : State.state_index) : bool =
					true in
				let predfs (astate : State.state_index) : unit =
					cyan := astate::(!cyan);
					printqueue "Cyan" !cyan;
					self#post_from_one_state astate;
					() in
				let lookahead (thesuccessors : State.state_index list) :
					State.state_index * bool = 
					try ((List.find (fun suc_id ->
							(State.is_accepting (StateSpace.get_state state_space suc_id)) &&
								(List.mem suc_id !cyan)) thesuccessors), true)
					with Not_found -> init_state_index, false
				in
				let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
					print_highlighted_message Shell_bold Verbose_standard
						("Cycle found at state " ^ (string_of_int astate));
					print_message Verbose_standard
						(ModelPrinter.string_of_state model
							(StateSpace.get_state state_space astate));
					termination_status <- Some Target_found;
					print_projection Verbose_standard astate;
					let state_loc, state_constr = StateSpace.get_state state_space astate in
					constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
					raise TerminateAnalysis
				in
				let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					if (not (List.mem astate !blue) &&
						not (List.mem astate !cyan)) then true else false in
				let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					false in
				let alternativedfs (astate : State.state_index) : unit =
					() in
				let testrecursivedfs (astate : State.state_index) : bool =
					true in
				let postdfs (astate : State.state_index) : unit =
					if (State.is_accepting (StateSpace.get_state state_space astate)) then (
						(* set up the dfs red calls *)
						let enterdfs (astate : State.state_index) : bool =
							true in
						let predfs (astate : State.state_index) : unit =
							red := astate::(!red);
							printqueue "Red" !red in
						let lookahead (thesuccessors : State.state_index list) :
							State.state_index * bool = 
							init_state_index, false in
						let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate));
							print_message Verbose_standard
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							termination_status <- Some Target_found;
							print_projection Verbose_standard astate;
							let state_loc, state_constr = StateSpace.get_state state_space astate in
							constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
							raise TerminateAnalysis
						in
						let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							true in
						let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (List.mem astate !cyan) then true else false in
						let alternativedfs (astate : State.state_index) : unit =
							cyclefound astate astate
						in
						let testrecursivedfs (astate : State.state_index) : bool =
							if (not (List.mem astate !red)) then true else false in
						let postdfs (astate : State.state_index) : unit =
							() in					
						rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate
					);
					blue := astate::(!blue);
					printqueue "Blue" !blue;
					match !cyan with
					| astate::body ->
						cyan := body;
						printqueue "Cyan" !cyan;
					| _ -> print_message Verbose_standard "Error popping from cyan";
					() in
				(try (rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index;)
					with TerminateAnalysis -> ());
				print_message Verbose_low("Finished the calls")
            | Exploration_NDFS_sub ->
(* NDFS with subsumption *)
            	print_message Verbose_standard("Using the option NDFSsub");
				(* set up the dfs blue calls *)
				let enterdfs (astate : State.state_index) : bool =
					true in
				let predfs (astate : State.state_index) : unit =
					cyan := astate::(!cyan);
					printqueue "Cyan" !cyan;
					self#post_from_one_state astate;
					() in
				let lookahead (thesuccessors : State.state_index list) :
					State.state_index * bool = 
					try ((List.find (fun suc_id ->
							(State.is_accepting (StateSpace.get_state state_space suc_id)) &&
								(List.mem suc_id !cyan)) thesuccessors), true)
					with Not_found -> init_state_index, false
				in
				let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
					print_highlighted_message Shell_bold Verbose_standard
						("Cycle found at state " ^ (string_of_int astate));
					print_message Verbose_standard
						(ModelPrinter.string_of_state model
							(StateSpace.get_state state_space astate));
					termination_status <- Some Target_found;
					print_projection Verbose_standard astate;
					let state_loc, state_constr = StateSpace.get_state state_space astate in
					constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
					raise TerminateAnalysis
				in
				let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					if (not (List.mem astate !blue) &&
						not (List.mem astate !cyan) &&
						not (setsubsumes !red astate)) then true else false in
				let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					false in
				let alternativedfs (astate: State.state_index) : unit =
					() in
				let testrecursivedfs (astate: State.state_index) : bool =
					true in
				let postdfs (astate: State.state_index) : unit =
					if (State.is_accepting (StateSpace.get_state state_space astate)) then (
						(* set up the dfs red calls *)
						let enterdfs (astate: State.state_index) : bool =
							true in
						let predfs (astate: State.state_index) : unit =
							red := astate::(!red);
							printqueue "Red" !red in
						let lookahead (thesuccessors : State.state_index list) :
							State.state_index * bool = 
							init_state_index, false in
						let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate));
							print_message Verbose_standard
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							termination_status <- Some Target_found;
							print_projection Verbose_standard astate;
							let state_loc, state_constr = StateSpace.get_state state_space astate in
							constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
							raise TerminateAnalysis
						in
						let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (same_parameter_projection thestate astate) then true
							else false in
						let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (subsumesset astate !cyan) then true else false in
						let alternativedfs (astate : State.state_index) : unit =
							cyclefound astate astate
						in
						let testrecursivedfs (astate : State.state_index) : bool =
							if (not (setsubsumes !red astate)) then true else false in
						let postdfs (astate : State.state_index) : unit =
							() in					
						rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate
					);
					blue := astate::(!blue);
					printqueue "Blue" !blue;
					match !cyan with
					| astate::body ->
						cyan := body;
						printqueue "Cyan" !cyan;
					| _ -> print_message Verbose_standard "Error popping from cyan";
					() in
				(try (rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index;)
					with TerminateAnalysis -> ());
				print_message Verbose_low("Finished the calls")
            | Exploration_layer_NDFS_sub ->
(* NDFS with subsumption and layers *)
            	print_message Verbose_standard("Using the option layerNDFSsub");
				(* set up the dfs blue calls *)
				add_pending init_state_index;
				(try (while !pending != [] do
					match !pending with
					| [] -> print_message Verbose_standard ("Impossible case");
					| thestate::body ->
						pending := body;
						print_message Verbose_low ("Popped state "
							^ (string_of_int thestate));
						printqueue "Pending" !pending;
						if (not (List.mem thestate !blue)) then
						begin 
						let enterdfs (astate : State.state_index) : bool =
							true in
						let predfs (astate : State.state_index) : unit =
							cyan := astate::(!cyan);
							printqueue "Cyan" !cyan;
							self#post_from_one_state astate;
							() in
						let lookahead (thesuccessors : State.state_index list) :
							State.state_index * bool = 
							try ((List.find (fun suc_id ->
									(State.is_accepting (StateSpace.get_state state_space suc_id)) &&
										(List.mem suc_id !cyan)) thesuccessors), true)
							with Not_found -> init_state_index, false
						in
						let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate));
							print_message Verbose_standard
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							termination_status <- Some Target_found;
							print_projection Verbose_standard astate;
							let state_loc, state_constr = StateSpace.get_state state_space astate in
							constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
							raise TerminateAnalysis
						in
						let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (not (List.mem astate !blue) &&
								not (List.mem astate !cyan) &&
								not (layersetsubsumes !red astate)) then true else false in
						let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (not (same_parameter_projection thestate astate)) then true
							else false in
						let alternativedfs (astate: State.state_index) : unit =
							add_pending astate in
						let testrecursivedfs (astate: State.state_index) : bool =
							true in
						let postdfs (astate: State.state_index) : unit =
							if (State.is_accepting (StateSpace.get_state state_space astate)) then (
								(* set up the dfs red calls *)
								let enterdfs (astate: State.state_index) : bool =
									true in
								let predfs (astate: State.state_index) : unit =
									red := astate::(!red);
									printqueue "Red" !red in
								let lookahead (thesuccessors : State.state_index list) :
									State.state_index * bool = 
									init_state_index, false in
								let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate));
									print_message Verbose_standard
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									termination_status <- Some Target_found;
									print_projection Verbose_standard astate;
									let state_loc, state_constr = StateSpace.get_state state_space astate in
									constraint_list := [LinearConstraint.px_hide_nonparameters_and_collapse state_constr];
									raise TerminateAnalysis
								in
								let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
									if (same_parameter_projection thestate astate) then true
									else false in
								let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
									if (subsumesset astate !cyan) then true
									else false in
								let alternativedfs (astate : State.state_index) : unit =
									cyclefound astate astate
								in
								let testrecursivedfs (astate : State.state_index) : bool =
									if (not (layersetsubsumes !red astate)) then true
									else false in
								let postdfs (astate : State.state_index) : unit =
									() in					
								rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate
							);
							blue := astate::(!blue);
							printqueue "Blue" !blue;
							match !cyan with
							| astate::body ->
								cyan := body;
								printqueue "Cyan" !cyan;
							| _ -> print_message Verbose_standard "Error popping from cyan";
							() in
						rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs thestate;
						end;
				done;)
							with TerminateAnalysis -> ());
				print_message Verbose_low("Finished the calls")
            | Exploration_syn_NDFS_sub ->
(* collecting NDFS with subsumption *)
            	print_message Verbose_standard("Using the option synNDFSsub");
				(* set up the dfs blue calls *)
				let enterdfs (astate : State.state_index) : bool =
					if (List.exists (fun aconstraint ->
							smaller_parameter_constraint astate aconstraint) !constraint_list) then (
						(* State astate has been handled and must now become blue *)
						blue := astate::(!blue);
						printqueue "Blue" !blue;
						false
					) else true
				in
				let predfs (astate : State.state_index) : unit =
					cyan := astate::(!cyan);
					printqueue "Cyan" !cyan;
					self#post_from_one_state astate;
					() in
				let lookahead (thesuccessors : State.state_index list) :
					State.state_index * bool = 
					try ((List.find (fun suc_id ->
							(State.is_accepting (StateSpace.get_state state_space suc_id)) &&
								(List.mem suc_id !cyan)) thesuccessors), true)
					with Not_found -> init_state_index, false
				in
				let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
					print_highlighted_message Shell_bold Verbose_standard
						("Cycle found at state " ^ (string_of_int astate));
					print_message Verbose_standard
						(ModelPrinter.string_of_state model
							(StateSpace.get_state state_space astate));
					termination_status <- Some Target_found;
					print_projection Verbose_standard astate;
					let state_loc, state_constr = StateSpace.get_state state_space astate in
					constraint_list := (LinearConstraint.px_hide_nonparameters_and_collapse state_constr)::(!constraint_list);
					(* the state where the lookahead has found a cycle is now set blue *)
					blue := astate::(!blue);
					printqueue "Blue" !blue;
					(* and the current state is popped from the cyan list *)
					match !cyan with
					| thestate::body ->
						cyan := body;
						printqueue "Cyan" !cyan;
					| _ -> print_message Verbose_standard "Error popping from cyan";
				in
				let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					if (not (List.mem astate !blue) &&
						not (List.mem astate !cyan) &&
						not (setsubsumes !red astate)) then true else false in
				let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
					false in
				let alternativedfs (astate: State.state_index) : unit =
					() in
				let testrecursivedfs (astate: State.state_index) : bool =
					true in
				let postdfs (astate: State.state_index) : unit =
					(* launch red dfs only if not with a smmaller constraint than a state marked by a lookahead*)
					if ((not (List.exists (fun aconstraint ->
								smaller_parameter_constraint astate aconstraint) !constraint_list)) &&
							(State.is_accepting (StateSpace.get_state state_space astate))) then (
						(* set up the dfs red calls *)
						let enterdfs (astate: State.state_index) : bool =
							true in
						let predfs (astate: State.state_index) : unit =
							red := astate::(!red);
							printqueue "Red" !red in
						let lookahead (thesuccessors : State.state_index list) :
							State.state_index * bool = 
							init_state_index, false in
						let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate));
							print_message Verbose_standard
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							termination_status <- Some Target_found;
							print_projection Verbose_standard astate;
							let state_loc, state_constr = StateSpace.get_state state_space astate in
							constraint_list := (LinearConstraint.px_hide_nonparameters_and_collapse state_constr)::(!constraint_list);
						in
						let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (same_parameter_projection thestate astate) then true
							else false in
						let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (subsumesset astate !cyan) then true else false in
						let alternativedfs (astate : State.state_index) : unit =
							cyclefound astate astate
						in
						let testrecursivedfs (astate : State.state_index) : bool =
							if (not (setsubsumes !red astate)) then true else false in
						let postdfs (astate : State.state_index) : unit =
							() in					
						rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate
					);
					if (not (List.mem astate !blue)) then blue := astate::(!blue);
					printqueue "Blue" !blue;
					match !cyan with
					| astate::body ->
						cyan := body;
						printqueue "Cyan" !cyan;
					| _ -> print_message Verbose_standard "Error popping from cyan";
					() in
				(try (rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index;)
					with TerminateAnalysis -> ());
				print_message Verbose_low("Finished the calls")
            | Exploration_syn_layer_NDFS_sub ->
(* collecting NDFS with layers and subsumption *)
            	print_message Verbose_standard("Using the option synlayerNDFSsub");
				(* set up the dfs blue calls *)
				add_pending init_state_index;
				(try (while !pending != [] do
					match !pending with
					| [] -> print_message Verbose_standard ("Impossible case");
					| thestate::body ->
						pending := body;
						print_message Verbose_low ("Popped state "
							^ (string_of_int thestate));
						printqueue "Pending" !pending;
						if (not (List.mem thestate !blue)) then
						begin 
						let enterdfs (astate : State.state_index) : bool =
							if (List.exists (fun aconstraint ->
									smaller_parameter_constraint astate aconstraint) !constraint_list) then (
								(* State astate has been handled and must now become blue *)
								blue := astate::(!blue);
								printqueue "Blue" !blue;
								false
							) else true
						in
						let predfs (astate : State.state_index) : unit =
							cyan := astate::(!cyan);
							printqueue "Cyan" !cyan;
							self#post_from_one_state astate;
							() in
						let lookahead (thesuccessors : State.state_index list) :
							State.state_index * bool = 
							try ((List.find (fun suc_id ->
									(State.is_accepting (StateSpace.get_state state_space suc_id)) &&
										(List.mem suc_id !cyan)) thesuccessors), true)
							with Not_found -> init_state_index, false
						in
						let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
							print_highlighted_message Shell_bold Verbose_standard
								("Cycle found at state " ^ (string_of_int astate));
							print_message Verbose_standard
								(ModelPrinter.string_of_state model
									(StateSpace.get_state state_space astate));
							termination_status <- Some Target_found;
							print_projection Verbose_standard astate;
							let state_loc, state_constr = StateSpace.get_state state_space astate in
							constraint_list := (LinearConstraint.px_hide_nonparameters_and_collapse state_constr)::(!constraint_list);
							(* the state where the lookahead has found a cycle is now set blue *)
							blue := astate::(!blue);
							printqueue "Blue" !blue;
							(* and the current state is popped from the cyan list *)
							match !cyan with
							| thestate::body ->
								cyan := body;
								printqueue "Cyan" !cyan;
							| _ -> print_message Verbose_standard "Error popping from cyan";
						in
						let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (not (List.mem astate !blue) &&
								not (List.mem astate !cyan) &&
								not (layersetsubsumes !red astate)) then true else false in
						let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
							if (not (same_parameter_projection thestate astate)) then true
							else false in
						let alternativedfs (astate: State.state_index) : unit =
							add_pending astate in
						let testrecursivedfs (astate: State.state_index) : bool =
							true in
						let postdfs (astate: State.state_index) : unit =
							(* launch red dfs only if not with a smmaller constraint than a state marked by a lookahead*)
							if ((not (List.exists (fun aconstraint ->
										smaller_parameter_constraint astate aconstraint) !constraint_list)) &&
									(State.is_accepting (StateSpace.get_state state_space astate))) then (
								(* set up the dfs red calls *)
								let enterdfs (astate: State.state_index) : bool =
									true in
								let predfs (astate: State.state_index) : unit =
									red := astate::(!red);
									printqueue "Red" !red in
								let lookahead (thesuccessors : State.state_index list) :
									State.state_index * bool = 
									init_state_index, false in
								let cyclefound (thestate : State.state_index) (astate : State.state_index) : unit =
									print_highlighted_message Shell_bold Verbose_standard
										("Cycle found at state " ^ (string_of_int astate));
									print_message Verbose_standard
										(ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate));
									termination_status <- Some Target_found;
									print_projection Verbose_standard astate;
									let state_loc, state_constr = StateSpace.get_state state_space astate in
									constraint_list := (LinearConstraint.px_hide_nonparameters_and_collapse state_constr)::(!constraint_list);
								in
								let filterdfs (thestate : State.state_index) (astate : State.state_index) : bool =
									if (same_parameter_projection thestate astate) then true
									else false in
								let testaltdfs (thestate : State.state_index) (astate : State.state_index) : bool =
									if (subsumesset astate !cyan) then true
									else false in
								let alternativedfs (astate : State.state_index) : unit =
									cyclefound astate astate
								in
								let testrecursivedfs (astate : State.state_index) : bool =
									if (not (layersetsubsumes !red astate)) then true
									else false in
								let postdfs (astate : State.state_index) : unit =
									() in					
								rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate
							);
							if (not (List.mem astate !blue)) then blue := astate::(!blue);
							printqueue "Blue" !blue;
							match !cyan with
							| astate::body ->
								cyan := body;
								printqueue "Cyan" !cyan;
							| _ -> print_message Verbose_standard "Error popping from cyan";
							() in
						rundfs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs thestate;
						end;
				done;)
							with TerminateAnalysis -> ());
				print_message Verbose_low("Finished the calls")
            | Exploration_syn_mixed_NDFS -> print_message Verbose_standard("Using the option synMixedNDFS")
            | _ -> raise (InternalError ("Unknown variant of NDFS"))
        end;

        (* combine the linear constraints *)
		constraint_valuations <- Some (LinearConstraint.p_nnconvex_constraint_of_p_linear_constraints !constraint_list);

		print_message Verbose_standard("---------------- Ending exploration ------------------");

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
	method add_a_new_state source_state_index new_states_indexes action_index location (final_constraint : LinearConstraint.px_linear_constraint) =
		(* Retrieve the model *)
(* 		let model = Input.get_model () in *)

		(* Build the state *)
		let new_state = location, final_constraint in

		
		(* Try to add the new state to the state space *)
		let addition_result = StateSpace.add_state state_space (self#state_comparison_operator_of_options) new_state in
		
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
			new_states_indexes := new_state_index :: !new_states_indexes;
			
		end; (* end if new state *)
		
		
		(*** TODO: move the rest to a higher level function? (post_from_one_state?) ***)
		
		(* Update the transitions *)
		self#add_transition_to_state_space (source_state_index, action_index, (*** HACK ***) match addition_result with | StateSpace.State_already_present new_state_index | StateSpace.New_state new_state_index | StateSpace.State_replacing new_state_index -> new_state_index) addition_result;
	
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

		let constr_result = match constraint_valuations with
				| None -> LinearConstraint.false_p_nnconvex_constraint()
				| Some constr -> constr
		in
		(* Distributed_worker_result *)

		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError "Termination status not set in NDFS exploration")
			| Some status -> status
		in

		let soundness = if termination_status = Regular_termination then Constraint_exact else Constraint_maybe_under in

		let constr_result = match constraint_valuations with
				| None -> LinearConstraint.false_p_nnconvex_constraint()
				| Some constr -> constr
		in

		(* Return result *)
		Single_synthesis_result
		{
			result = Good_constraint (constr_result, soundness);
			(*result = Good_constraint (LinearConstraint.false_p_nnconvex_constraint(), soundness);*)

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
