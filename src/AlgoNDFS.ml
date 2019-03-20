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

	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
		
	method private get_constraint_valuations =
		match constraint_valuations with
		| Some constr -> constr
		| None -> raise (InternalError "Variable 'constraint_valuations' not initialized in AlgoNDFS although it should have been at this point")
	
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

		(************************************)
		(* basic queues for NDFS algorithms *)
		(************************************)
		let cyan = ref [] in
		let blue = ref [] in
		let pink = ref [] in 	(* Used only in some of the algorithms *)
		let red = ref [] in

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

		let rec rundfs enterdfs predfs filterdfs testaltdfs alternativedfs
			testrecursivedfs postdfs thestate =
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
					| [] ->  ();
					| suc_id::body -> 
						print_message Verbose_low("Handling "
							^ (if State.is_accepting (StateSpace.get_state state_space suc_id)
								then "accepting " else "")
							^ "successor "
							^ (ModelPrinter.string_of_state model
								(StateSpace.get_state state_space suc_id)));				
						if (filterdfs suc_id) then (
							if (testaltdfs suc_id) then (alternativedfs suc_id);
							if (testrecursivedfs suc_id) then (
								rundfs enterdfs predfs filterdfs testaltdfs alternativedfs testrecursivedfs postdfs suc_id)
						);
						process_sucs body;
                in
                process_sucs successors;
 				postdfs thestate
			)
		in

		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
		(*                     State Space Exploration                       *)
		(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

		print_message Verbose_standard("---------------- Starting exploration ----------------");
 
        match options#exploration_order with
            | Exploration_NDFS -> 
				print_message Verbose_standard("Using the option NDFS");
				(* set up the dfs blue calls *)
				let enterdfs : State.state_index -> bool =
					fun (astate : State.state_index) -> true in
				let predfs : State.state_index -> unit =
					fun (astate : State.state_index) ->
						cyan := astate::(!cyan);
						printqueue "Cyan" !cyan;
						self#post_from_one_state astate;
						() in
				let filterdfs : State.state_index -> bool =
					fun (astate : State.state_index) ->
						if (not (List.mem astate !blue) &&
							not (List.mem astate !cyan)) then true else false in
				let testaltdfs : State.state_index -> bool =
					fun (astate : State.state_index) -> false in
				let alternativedfs : State.state_index -> unit =
					fun (astate: State.state_index) -> () in
				let testrecursivedfs : State.state_index -> bool =
					fun (astate : State.state_index) -> true in
				let postdfs : State.state_index -> unit =
					fun (astate: State.state_index) -> 
						if (State.is_accepting (StateSpace.get_state state_space astate)) then (
							(* set up the dfs red calls *)
							let enterdfs : State.state_index -> bool =
								fun (astate : State.state_index) -> true in
							let predfs : State.state_index -> unit =
								fun (astate : State.state_index) ->
									red := astate::(!red);
									printqueue "Red" !red in
							let filterdfs : State.state_index -> bool =
								fun (astate : State.state_index) -> true in
							let testaltdfs : State.state_index -> bool =
								fun (astate : State.state_index) ->
									if (List.mem astate !cyan) then true else false in
							let alternativedfs : State.state_index -> unit =
								fun (astate : State.state_index) ->
									print_message Verbose_standard ("Cycle found at state "
										^ (ModelPrinter.string_of_state model
											(StateSpace.get_state state_space astate))) in
							let testrecursivedfs : State.state_index -> bool =
								fun (astate : State.state_index) ->
									if (not (List.mem astate !red)) then true else false in
							let postdfs : State.state_index -> unit =
								fun (astate : State.state_index) -> () in					
						rundfs enterdfs predfs filterdfs testaltdfs alternativedfs testrecursivedfs postdfs astate);
						() in
				rundfs enterdfs predfs filterdfs testaltdfs alternativedfs testrecursivedfs postdfs init_state_index;
				print_message Verbose_low("Finished the calls");
            | Exploration_NDFS_sub -> print_message Verbose_standard("Using the option NDFSsub")
            | Exploration_layer_NDFS_sub -> print_message Verbose_standard("Using the option layerNDFSsub")
            | Exploration_syn_NDFS_sub -> print_message Verbose_standard("Using the option synNDFSsub")
            | Exploration_syn_layer_NDFS_sub -> print_message Verbose_standard("Using the option synlayerNDFSsub")
            | Exploration_syn_mixed_NDFS -> print_message Verbose_standard("Using the option synMixedNDFS")
            | _ -> raise (InternalError ("Unknown variant of NDFS"))

		print_message Verbose_standard("---------------- Ending exploration ------------------");

		
		print_message Verbose_standard("---------------- Ending algorithm --------------------");
		
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
