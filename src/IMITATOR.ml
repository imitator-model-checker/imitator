(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Main file for IMITATOR
 * 
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2009/09/07
 * Last modified     : 2016/05/18
 *
 ************************************************************)

 

(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities

open ImitatorUtilities
open AbstractModel
open Result
open ModelPrinter
open Options
open Statistics


(*** NOTE: just to allow compiling ***)
open CUBchecker


(**************************************************
TAGS USED THROUGHOUT THIS PROJECT
- (*** BADPROG ***)
- (*** NOTE ***)
- (*** OPTIMIZED ***)
- (*** QUESTION ***)
- (*** TO OPTIMIZE ***)
- (*** TODO ***)
- (*** WARNING ***)
**************************************************)


;;


(************************************************************)
(************************************************************)
(* STARTING PROGRAM *)
(************************************************************)
(************************************************************)


(* TEST !! *)
(*LinearConstraint.test_PDBMs();
terminate_program();*)

(************************************************************)
(* Start the global counter *)
(************************************************************)
let global_counter = create_and_register "total" Global_counter Verbose_standard in
global_counter#start;


(************************************************************)
(* BEGIN EXCEPTION MECHANISM *)
(************************************************************)
begin
try(

(************************************************************)
(* Get the arguments *)
(************************************************************)

let options_parsing_counter = create_and_register "options parsing" Parsing_counter Verbose_low in
options_parsing_counter#start;

(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;

(*** BUG: verbose mode has been set before the start and the stop; this particular counter may never stop! ***)
options_parsing_counter#stop;


(************************************************************)
(* Record backtrace if verbose > standard *)
(************************************************************)
if verbose_mode_greater Verbose_low then(
	Printexc.record_backtrace true;
);


(************************************************************)
(************************************************************)
(* Print startup message *)
(************************************************************)
(************************************************************)

(* Print header *)
print_header_string();

(* Print date *)
print_message Verbose_standard ("Analysis time: " ^ (now()) ^ "\n");

(* Recall the arguments *)
options#recall(); 
    

(************************************************************)
(* Get input *)
(************************************************************)
let parsing_counter = create_and_register "model parsing" Parsing_counter Verbose_standard in
parsing_counter#start;

let model, pi0, v0 = ParsingUtility.compile options in

Input.set_model model;
Input.set_pi0 pi0;
Input.set_v0 v0;

parsing_counter#stop;


(************************************************************)
(* Debug print: model *)
(************************************************************)
if verbose_mode_greater Verbose_total then(
	print_message Verbose_total ("\nThe input model is the following one:\n" ^ (ModelPrinter.string_of_model model) ^ "\n");
);


(************************************************************)
(* Debug print: property *)
(************************************************************)
if verbose_mode_greater Verbose_low then(
	print_message Verbose_low ("\nThe property is the following one:\n" ^ (ModelPrinter.string_of_property model model.user_property) ^ "\n");
);


(************************************************************)
(* Case translation *)
(************************************************************)

(* Translation to CLP (work in progress) *)
if options#pta2clp then(
	print_message Verbose_standard ("Translating model to CLP.");
	print_warning ("Work in progress!!!!");
	print_message Verbose_standard ("\nmodel in CLP:\n" ^ (PTA2CLP.string_of_model model) ^ "\n");
	terminate_program()
);

(* Translation to GrML (experimental) *)
if options#pta2gml then(
	print_message Verbose_standard ("Translating model to GrML.");
	let translated_model = PTA2GrML.string_of_model model in
	let grml_file = options#files_prefix ^ ".grml" in
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file grml_file translated_model;
	print_message Verbose_standard ("File '" ^ grml_file ^ "' successfully created.");
	terminate_program()
);

(* Translation to HyTech *)
if options#pta2hytech then(
	print_message Verbose_standard ("Translating model to a HyTech input model.");
	let translated_model = PTA2HyTech.string_of_model model in
	let hytech_file = options#files_prefix ^ ".hy" in
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file hytech_file translated_model;
	print_message Verbose_standard ("File '" ^ hytech_file ^ "' successfully created.");
	terminate_program()
);

(* Translation to IMITATOR *)
if options#pta2imi then(
	print_message Verbose_standard ("Regenerating the input model to a new model.");
	let translated_model = ModelPrinter.string_of_model model in
	let imi_file = options#files_prefix ^ "-regenerated.imi" in
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file imi_file translated_model;
	print_message Verbose_standard ("File '" ^ imi_file ^ "' successfully created.");
	terminate_program()
);

(* Translation to JPG *)
if options#pta2jpg then(
	print_message Verbose_standard ("Translating model to a graphics.");
	let translated_model = PTA2JPG.string_of_model model in
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n" ^ translated_model ^ "\n");
	);
	Graphics.dot (options#files_prefix ^ "-pta") translated_model;
	print_message Verbose_standard ("File successfully created."); (*** TODO: add file name in a proper manner ***)
	terminate_program()
);

(* Translation to TikZ *)
if options#pta2tikz then(
	print_message Verbose_standard ("Translating model to LaTeX TikZ code.");
	let translated_model = PTA2TikZ.tikz_string_of_model model in
	let latex_file = options#files_prefix ^ ".tex" in
	if verbose_mode_greater Verbose_high then(
		print_message Verbose_high ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file latex_file translated_model;
	print_message Verbose_standard ("File '" ^ latex_file ^ "' successfully created.");
	terminate_program()
);
(* Direct cartography output *)
if options#cartonly then(
	raise (InternalError("Not implemented! "))

	(*** TODO ***)
	
(*	print_message Verbose_standard ("Direct output of a cartography (no analysis will be run).");
	(* Get the parameters *)
	let constraints , (p1_min , p1_max) , (p2_min , p2_max) = model.carto in
	(* Transform the constraint for cartography *)
	let constraints = List.map (fun (linear_constraint , tile_nature) ->
		Convex_constraint (linear_constraint , tile_nature)
	) constraints in
	(* Create the v0 *)
	let v0 = new HyperRectangle.hyper_rectangle in
	v0#set_min 0 p1_min;
	v0#set_max 0 p1_max;
	v0#set_min 1 p2_min;
	v0#set_max 1 p2_max;
	(* Call the cartography *)
	Graphics.cartography constraints options#files_prefix;
	print_message Verbose_standard ("File successfully created."); (*** TODO: add file name in a proper manner ***)
	(* The end *)
	terminate_program()*)
);



(************************************************************)
(* Preliminary checks *)
(************************************************************)

if options#imitator_mode = EF_synthesis then(
	match model.correctness_condition with
		(* Synthesis only works w.r.t. (un)reachability *)
		| Some (Unreachable _) -> ()
		| _ -> print_error ("EF-synthesis can only be run if an unreachability property is defined in the model.");
			abort_program();
);


if (options#imitator_mode = Border_cartography && model.correctness_condition = None) then(
	print_error ("In border cartography mode, a correctness property must be defined.");
	abort_program();
);



(************************************************************)
(* Dynamic clock elimination *)
(************************************************************)
(* Need to be called before initial state is created! *)
if options#dynamic_clock_elimination then (
	ClocksElimination.prepare_clocks_elimination ()
);



(*(* TESTS *) 
print_message Verbose_standard ("\nInitial constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");

(*let n = ref 1 in

List.iter (fun parameter_id ->
	LinearConstraint.time_elapse_assign [parameter_id] (list_diff model.parameters [parameter_id]) initial_constraint_after_time_elapsing;
	
	print_message Verbose_standard ("\nAfter time elapsing #" ^ (string_of_int !n) ^ " on parameter '" ^ (model.variable_names parameter_id) ^ "' :\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
	
	Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto" ^ (string_of_int !n));

	n := !n + 1;

) model.parameters;
(* Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto"); *)
terminate_program();*)


LinearConstraint.grow_to_zero_assign model.parameters model.clocks_and_discrete initial_constraint_after_time_elapsing;
print_message Verbose_standard ("\nFinal constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-cartoz");
terminate_program();*)



(*(************************************************************)
(* EXPERIMENTAL: branch and bound *)
(************************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound model pi0 init_state_after_time_elapsing;
	terminate_program();
);*)




(************************************************************)
(************************************************************)
(* Execute IMITATOR *)
(************************************************************)
(************************************************************)

(* Generic method for the cartography to create either a new IM instance, or a new PRP instance *)
(*** TODO: also add IMK, etc., if needed ***)
let new_im_or_prp =
	if options#efim then
		fun () -> new AlgoPRP.algoPRP
	else
		fun () -> new AlgoIM.algoIM
in


(* Find the correct algorithm to execute *)
let algorithm : AlgoGeneric.algoGeneric = match options#imitator_mode with
	
	(************************************************************)
	(* Exploration *)
	(************************************************************)
	| State_space_exploration ->
			(*** NOTE: this is static subclass coercition; see https://ocaml.org/learn/tutorials/objects.html ***)
		let myalgo :> AlgoGeneric.algoGeneric = new AlgoPostStar.algoPostStar in myalgo
		
		
	(************************************************************)
	(* EF-synthesis *)
	(************************************************************)
	(* Experimental mode with PointSetPowerSet *)
	| EF_synthesis when options#new_ef_mode ->
		let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFsynth.algoEFsynth in myalgo
	
	(* Normal (and old) mode *)
	| EF_synthesis when not options#new_ef_mode ->
		let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFsynthOld.algoEFsynth in myalgo
	
	
	
	(************************************************************)
	(* Parametric deadlock checking *)
	(************************************************************)
	| Parametric_deadlock_checking ->
		let myalgo :> AlgoGeneric.algoGeneric = new AlgoDeadlockFree.algoDeadlockFree in myalgo
	
	
	(************************************************************)
	(* Inverse method and variants *)
	(************************************************************)
	(* IMK *)
	(*** TODO: use four different modes ***)
	| Inverse_method when options#pi_compatible ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMK.algoIMK in myalgo

	(* PRP *)
	| Inverse_method when options#efim ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoPRP.algoPRP in myalgo

	(* IMunion *)
	| Inverse_method when options#union ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMunion.algoIMunion in myalgo

	(* Inverse Method *)
	| Inverse_method ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoIM.algoIM in myalgo



	(************************************************************)
	(* Begin distributed cartography *)
	(************************************************************)
	
	(*** WARNING:  Do not modify the following lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)
	(*(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)

	(*** NOTE: only one distribution mode so far ***)
	| Cover_cartography when options#distribution_mode <> Non_distributed ->
		let algo = match options#distribution_mode with
		
		(** Distributed mode: Master worker with sequential pi0 *)
		| Distributed_ms_sequential ->
			(* Branch between master and worker *)
			if DistributedUtilities.is_master() then
				let bc_algo = new AlgoBCCoverDistributedMSSeqMaster.algoBCCoverDistributedMSSeqMaster in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
			else
				let bc_algo = new AlgoBCCoverDistributedMSSeqWorker.algoBCCoverDistributedMSSeqWorker in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

		(** Distributed mode: Master worker with sequential pi0 shuffled *)
		| Distributed_ms_shuffle ->
			(* Branch between master and worker *)
			if DistributedUtilities.is_master() then
				let bc_algo = new AlgoBCCoverDistributedMSShuffleMaster.algoBCCoverDistributedMSShuffleMaster in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
			else
				let bc_algo = new AlgoBCCoverDistributedMSShuffleWorker.algoBCCoverDistributedMSShuffleWorker in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

		(** Distributed mode: Master worker with random pi0 and n retries before switching to sequential mode *)
		| Distributed_ms_random nb_tries ->
			(* Branch between master and worker *)
			if DistributedUtilities.is_master() then
				let bc_algo = new AlgoBCCoverDistributedMSRandomSeqMaster.algoBCCoverDistributedMSRandomSeqMaster in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_max_tries nb_tries;
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
			else
				let bc_algo = new AlgoBCCoverDistributedMSRandomSeqWorker.algoBCCoverDistributedMSRandomSeqWorker in
				(*** NOTE: very important: must set NOW the parameters ***)
(* 				bc_algo#set_max_tries nb_tries; *)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
		
		(** Distributed mode: Master worker with subdomain distribution *)
		| Distributed_ms_subpart ->
			(* Branch between master and worker *)
			if DistributedUtilities.is_master() then
				let bc_algo = new AlgoBCCoverDistributedSubdomainDynamicCoordinator.algoBCCoverDistributedSubdomainDynamicCoordinator in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
			else
				let bc_algo = new AlgoBCCoverDistributedSubdomainDynamicCollaborator.algoBCCoverDistributedSubdomainDynamicCollaborator in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

		(** Distributed mode: static distribution mode (each node has its own subdomain with no communication) *)
		| Distributed_static ->
			(* Branch between collaborator and coordinator *)
			if DistributedUtilities.is_coordinator() then
				let bc_algo = new AlgoBCCoverDistributedSubdomainStaticCoordinator.algoBCCoverDistributedSubdomainStaticCoordinator in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
			else
				let bc_algo = new AlgoBCCoverDistributedSubdomainStaticCollaborator.algoBCCoverDistributedSubdomainStaticCollaborator in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

				
		| _ -> raise (InternalError("Other distribution modes not yet implemented"))
		
		in algo
				
			
	(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)*)
	(*** WARNING:  Do not modify the previous lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)

	(************************************************************)
	(* End distributed cartography *)
	(************************************************************)
	
	
	(************************************************************)
	(* Non-distributed cartography *)
	(************************************************************)

	(* BC with full coverage *)
	| Cover_cartography ->
		let bc_algo = new AlgoBCCover.algoBCCover in
		(*** NOTE: very important: must set NOW the parameters ***)
		bc_algo#set_algo_instance_function new_im_or_prp;
		let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
		myalgo
	
	(* BC with full coverage (shuffled version) *)
	| Shuffle_cartography ->
		let bc_algo = new AlgoBCShuffle.algoBCShuffle in
		(*** NOTE: very important: must set NOW the parameters ***)
		bc_algo#set_algo_instance_function new_im_or_prp;
		let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
		myalgo
	
	| Border_cartography ->
		raise (InternalError("Not implemented !!!"))
		
	(* BC with random coverage *)
	| Random_cartography nb ->
		let bc_algo = new AlgoBCRandom.algoBCRandom in
		(*** NOTE: very important: must set NOW the parameters ***)
		bc_algo#set_max_tries nb;
		bc_algo#set_algo_instance_function new_im_or_prp;
		let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
		myalgo

	
	(* BC with random coverage followed by sequential coverage *)
	| RandomSeq_cartography nb ->
		let bc_algo = new AlgoBCRandomSeq.algoBCRandomSeq in
		(*** NOTE: very important: must set NOW the parameters ***)
		bc_algo#set_max_tries nb;
		bc_algo#set_algo_instance_function new_im_or_prp;
		let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
		myalgo
	
		
	(************************************************************)
	(* Translation has been handled already *)
	(************************************************************)

	| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");
in





(**************************************************)
(* GIA'S TESTING *)
(**************************************************)






(**************************************************)
(* PART 1 - CUB-PTA Detecting and Analyzing *)
(**************************************************)

(*attention: multi inequalities of the same clock in model*)

(*let check_cub_condition *)

(*check CUB condition function -> return boolean value*)

(* 
Simple funtion to covert from list of inequalities to list of tuple (clock; operator; linear expression) 
Note that: if there are True constraints, it will return back a list of clock greater than Zero
This only uses to indicate clock smaller than INFINITE, not for lower-bound
*)
let convert_inequality_list_2_tuple_list inequalities =	let list_s0 = ref [] in 
														match inequalities with 
														(*True constraints -> list of clocks >= 0*)
														 [] ->  List.iter 	(fun clock_index -> 
														 					list_s0 := !list_s0@[(clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)] 
																			) model.clocks; 
														 		!list_s0

														| _ ->	(* print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) Start:"); *)
																List.iter 	(fun inequality -> (
																			let (clock_index_2, operator, parametric_linear_term) = LinearConstraint.clock_guard_of_linear_inequality inequality in
                															list_s0 := !list_s0@[(clock_index_2, operator, parametric_linear_term)]; 
                															(* print_message Verbose_standard (" inequality: " ^ (model.variable_names clock_index_2) 
																											^ " " ^ (LinearConstraint.operator2string operator) 
																											^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
																											^ " added!"); *)
                																) 
                															) inequalities; 
                												(* print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) End!");
                												print_message Verbose_standard ("\n"); *)
                												!list_s0
														in



(*attention:*)
(*return tuples*)
(*Need to improve more than one upper-bounds of the same clock*)
let filter_upperbound_by_clock clock_index tuple_inequalities_s0 =	
																(* print_message Verbose_standard (" 	filtering upper-bound of clock (" ^ (model.variable_names clock_index) ^ ") in this list of tuple inequalities:"); *)
																match tuple_inequalities_s0 with
    															| [] ->  raise (InternalError("Detected empty list, check again the input inequalities or it might be True constraint "))
    															| _  -> 
																let length = List.length tuple_inequalities_s0 in
																(*index variable is to mark the position of upper-bound in the list*)
																let index = ref 0 in
																(*count is the number of upperbounds*)
																let count = ref 0 in

																for i = 0 to length -1
																do 
																	let (clock_index_2, operator, parametric_linear_term) = (List.nth tuple_inequalities_s0 i) in 

																	if ( (clock_index == clock_index_2) && ( operator == LinearConstraint.Op_l || operator == LinearConstraint.Op_le ) )
                													then 
                														(
                														(* print_message Verbose_standard ("	Upper-bounded found: " ^ (model.variable_names clock_index_2) 
																										^ " " ^ (LinearConstraint.operator2string operator) 
																										^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
																										^ " !"); *)
                														index := i; 
                														count := (!count + 1);
																		if !count > 1
																		then  raise (InternalError("Detected more than 1 different upperbounds of the same clock in same constraint!!! "))

                														);
																done;
																(*just for printing, start*)
																(* if !count = 0
																then 
																	print_message Verbose_standard ("	Upper-bound not found:!, return clock  " ^ (model.variable_names clock_index) ^">= 0" );  *)
																	(*just for printing, end*)
																	(* if there is no upper-bound -> reutrn (clock >= 0) *)

																if !count = 0
																then
																	( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero ) (* make_linear_term [] NumConst.zero) *)
																else
																	(* result *)
																	List.nth tuple_inequalities_s0 !index
															in 


let filter_upperbound_by_clock_2 clock_index tuple_inequalities_s0 =
																let ls = ref
																( 
																match tuple_inequalities_s0 with
    															| [] ->  raise (InternalError("Detected empty list, check again the input inequalities or it might be True constraint "))
    															| _  -> 
																(List.find_all (fun (index, _,_) -> index = clock_index ) tuple_inequalities_s0);
																);
																in
																if !ls = []
																then
																	(
																	ls := [( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero )]; 		
																	);
																!ls;													
															in 


let filter_upperbound_by_clock_3 clock_index tuple_inequalities_s0 =
																let ls =
																( 
																match tuple_inequalities_s0 with
    															| [] ->  raise (InternalError("Detected empty list, check again the input inequalities or it might be True constraint "))
    															| _  -> 
																(List.find_all (fun (index, op,_) -> index = clock_index && (op = LinearConstraint.Op_le ||op = LinearConstraint.Op_l) ) tuple_inequalities_s0);
																);
																in
																ls;													
															in 




let make_CUB_inequality (op1, linear_term1) (op2, linear_term2) = 	
																	let linear_term = LinearConstraint.sub_p_linear_terms linear_term1 linear_term2 in
																	match op1, op2 with
																	(*linear_term1 < linear_term2*)
																	| LinearConstraint.Op_le, LinearConstraint.Op_l -> LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_l
																	(*linear_term1 <= linear_term2*)
																	| _, _ -> LinearConstraint.make_p_linear_inequality linear_term LinearConstraint.Op_le

																 	in


let make_CUB_constraint inequalities = LinearConstraint.make_p_constraint inequalities
									in


let cub_check_2 invariant_s0 guard_t invariant_s1 clock_updates = 	
																(*ppl*)
																(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
																let inequalities = ref [] in
																print_message Verbose_standard (" CUB check, Start:");
																print_message Verbose_standard ("\n");

																let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
																let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
																let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

																print_message Verbose_standard (" **Beginning state/location** :");
																let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
																print_message Verbose_standard (" **Transition** :");
																let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
																print_message Verbose_standard (" **Destination state/location** :");
																let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in
																
																let isCUB_PTA = ref true in

																 List.iter (	fun clock_index -> 
																 	let inequalities_need_to_solve = ref [] in
																 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

																 	print_message Verbose_standard ("\n 	**Beginning state/location** :");
	             													let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
	             													print_message Verbose_standard ("\n 	**Transition** :");
                													let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
                													print_message Verbose_standard ("\n 	**Destination state/location** :");
                													let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in



                													print_message Verbose_standard ("\n");
                													print_message Verbose_standard ("Comparing: ");

                													let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
                													let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
                													let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in

                													print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
                													
                													print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");

                													(* if List.mem clock_index reset_clocks = true 
                													then lower_inequality := linear_term_t; *)
                
                													(* let result = ref true in *)
                													let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

														 			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
														 			 																							true;

														 			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
														 																										false;

														 			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
														 																										print_message Verbose_standard (" 	 Case 3 " );
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											(
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																											)
														 																										else
														 																											(
														 																											false;
														 																											);

														 			|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
														 																										false; 

 																	|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 4 " );
 																																								true;

 																	|_							, _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 5 " );
 																																								let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																										let constr = make_CUB_constraint [ineq] in
														 																										
														 																										if LinearConstraint.p_is_true constr
														 																										then true
														 																										else
														 																											(
														 																											if LinearConstraint.p_is_false constr
														 																											then false
														 																											else
														 																												(
														 																												inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
														 																												false;
														 																												);
														 																											);



 																	|_							, (LinearConstraint.Op_ge, _), _							->	print_message Verbose_standard (" 	 Case 5 " );
 																																								(*reset*)
 																																								if List.mem clock_index clock_updates = true
														 																										then
														 																											(
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																											)
														 																										else
														 																											(
														 																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
														 																											let constr = make_CUB_constraint [ineq] in
														 																											if LinearConstraint.p_is_true constr
														 																											then 
														 																												true
														 																											else
														 																												(
														 																												if LinearConstraint.p_is_false constr
														 																												then 
														 																													false
														 																												else
														 																													(
														 																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
														 																													false;
														 																													);
														 																												);
														 																											);
														 																											

														 			| _							, _							 , _							-> 	print_message Verbose_standard (" 	 Case 6 " );
														 																										(*reset*)
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											(
														 																											print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
														 																											
														 																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																											let constr = make_CUB_constraint [ineq] in
														 																											if LinearConstraint.p_is_true constr
														 																											then 
														 																												true
														 																												else
														 																												(
														 																												if LinearConstraint.p_is_false constr
														 																												then 
														 																													false
														 																												else
														 																													(
														 																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
														 																													false;
														 																													);
														 																												);
														 																											)
														 																										else
														 																											(
														 																											let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in

														 																											let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in

														 																											let constr = make_CUB_constraint [ineq1;ineq2] in
														 																											if LinearConstraint.p_is_true constr
														 																											then 
														 																												true
														 																												else
														 																												(
														 																												if LinearConstraint.p_is_false constr
														 																												then 
														 																													let clock_linear_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
														 																													false
														 																												else
														 																													(
														 																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq1;ineq2];
														 																													false;
														 																													);
														 																												);
														 																											);

														 																										
														 																									

														 																										




														 			in

														 			if (result = false && !inequalities_need_to_solve = [])
														 			then 	(
														 					isCUB_PTA := false;
														 					(*comment this line below for the CUB-PTA transformation*)
														 					(* raise (InternalError("   The model is impossible CUB-PTA! ")); *) 
														 					);
														 			
														 			inequalities := !inequalities@(!inequalities_need_to_solve);

														 			if (result = false)
														 			then
			
														 				print_message Verbose_standard (" This is not satisfied CUB-PTA! ")
														 			else 
														 				print_message Verbose_standard (" This is satisfied CUB-PTA! ");



                												print_message Verbose_standard ("\n");
                												) model.clocks; 

																print_message Verbose_standard ("\n");
																print_message Verbose_standard (" CUB check, End!");
																print_message Verbose_standard ("\n");

																(!isCUB_PTA, !inequalities);
																in



(* use for removing problematic transitions *)
let cub_check_3 invariant_s0 guard_t invariant_s1 clock_updates = 	
	let inequalities = ref [] in
	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in

	let isCUB_PTA = ref true in
	let inequalities_need_to_solve = ref [] in

	 List.iter (	fun clock_index -> 
		let ls_tup_ineq_s0 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_s0) in
		let ls_tup_ineq_t 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_t) in
		let ls_tup_ineq_s1 	= (filter_upperbound_by_clock_3 clock_index tuple_inequalities_s1) in

		if List.mem clock_index clock_updates = true
		then 
			(
			match ls_tup_ineq_s0, ls_tup_ineq_t with
			| [], [] 	-> ()
			| [], _ 	-> isCUB_PTA := false
			| _, []		-> ()
			| _, _ 		-> 	(
							List.iter (fun (_, op_s0, linear_term_s0) -> 
								List.iter (fun (_, op_t, linear_term_t) -> 
									let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
									let constr = make_CUB_constraint [ineq] in
									if LinearConstraint.p_is_true constr
									then 
										(
										)
									else
										(
										if LinearConstraint.p_is_false constr
										then 
											(
											isCUB_PTA := false;
											);
										);
								) ls_tup_ineq_t;
							) ls_tup_ineq_s0;
							);
			)
		else
			(
				match ls_tup_ineq_s0, ls_tup_ineq_t, ls_tup_ineq_s1 with
				| [], [], [] 	-> print_message Verbose_standard (" 1 " ); ()
				| [],  _, _ 	-> print_message Verbose_standard (" 2 " );()
				| _,  [], _ 	-> 	( print_message Verbose_standard (" 3 " );
									List.iter (fun (_, op_s0, linear_term_s0) -> 
										List.iter (fun (_, op_s1, linear_term_s1) -> 
										let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
										let constr = make_CUB_constraint [ineq] in
										if LinearConstraint.p_is_true constr
										then 
											(
											)
										else
											(
											if LinearConstraint.p_is_false constr
											then 
												(
												isCUB_PTA := false;
												);
											);
										) ls_tup_ineq_s1;
									) ls_tup_ineq_s0;
									);
				| _,   _, [] 	-> 	( print_message Verbose_standard (" 4 " );
									List.iter (fun (_, op_s0, linear_term_s0) -> 
										List.iter (fun (_, op_t, linear_term_t) -> 
										let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
										let constr = make_CUB_constraint [ineq] in
										if LinearConstraint.p_is_true constr
										then 
											(
											)
										else
											(
											if LinearConstraint.p_is_false constr
											then 
												(
												isCUB_PTA := false;
												);
											);
										) ls_tup_ineq_t;
									) ls_tup_ineq_s0;
									);
				| [], [], _ 	-> print_message Verbose_standard (" 5 " ); isCUB_PTA := false; 
				| [],  _, [] 	-> print_message Verbose_standard (" 6 " ); isCUB_PTA := false;
				| _,  [], [] 	-> print_message Verbose_standard (" 7 " ); ()
				| _,   _, _ 	-> 	( print_message Verbose_standard (" 8 " );
									List.iter (fun (_, op_s0, linear_term_s0) -> 
										List.iter (fun (_, op_t, linear_term_t) -> 
											List.iter (fun (_, op_s1, linear_term_s1) -> 
												let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
												let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
												let const = make_CUB_constraint [ineq1;ineq2] in
												if LinearConstraint.p_is_true const
												then 
													(
													)
												else
													(
													if LinearConstraint.p_is_false const
													then 
														(
														isCUB_PTA := false;
														);
													);
											) ls_tup_ineq_s1;
										) ls_tup_ineq_t;
									) ls_tup_ineq_s0;
									);

			); 


	) model.clocks; 


	
	(!isCUB_PTA, !inequalities);
	in



(* print_message Verbose_standard ("\n const: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names const) );

print_message Verbose_standard (" Inequality s0 <= t: \n" 
								^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");

print_message Verbose_standard (" Inequality s0 <= s1: \n" 
								^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
(* ^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ); *) *)



(*

let isCUB_PTA = ref true in
let inequalities_need_to_solve = ref [] in

(*main function for CUB-PTA*)
List.iter (fun automaton_index -> print_message Verbose_standard ("Automaton: " ^ (model.automata_names automaton_index) );

		(*Checking bounded clocked in invariant (Location)*)
        List.iter (fun location_index -> print_message Verbose_standard ("----------------Begin checking at " ^ (model.location_names automaton_index location_index) ^ "-------------------");

        		print_message Verbose_standard ("\n");

        		print_message Verbose_standard (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ;

        		let invariant1 = model.invariants automaton_index location_index in
        
                print_message Verbose_standard ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ;
						

						print_message Verbose_standard ("\n");

                	(*Checking bounded clocked in guards (Transition)*)
                	List.iter (fun action_index -> print_message Verbose_standard (" Transition/Action: " ^ (model.action_names action_index) );
            
                    	List.iter (fun (guard, clock_updates, _, destination_location_index) 
                    		-> print_message Verbose_standard ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));		

                        	(** WORK HERE **)

                        	let invariant2 = model.invariants automaton_index destination_location_index in

							print_message Verbose_standard ("\n");
                			print_message Verbose_standard (" State/Location(D): " ^ (model.location_names automaton_index destination_location_index) ) ;
                			print_message Verbose_standard ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
                			print_message Verbose_standard ("	  ----Map:(" 
                											^ (model.location_names automaton_index location_index) 
                											^ ")--" ^ (model.action_names action_index) ^ "-->(" 
                											^ (model.location_names automaton_index destination_location_index) 
                											^ ") ----" );
                			print_message Verbose_standard ("\n");

                        	let clock_updates = match clock_updates with
                        						  No_update -> []
												| Resets clock_update -> clock_update
												| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); in


                			let (result, inequalities) = cub_check_2 invariant1 guard invariant2 clock_updates in
                			
                			inequalities_need_to_solve := !inequalities_need_to_solve@inequalities;
                			if result = false
							then
    							isCUB_PTA := false;
                			()

                    	) (model.transitions automaton_index location_index action_index); 

                	) (model.actions_per_location automaton_index location_index); 

            		 print_message Verbose_standard ("----------------End checking " ^ (model.location_names automaton_index location_index) ^ "---------------------");

            		 print_message Verbose_standard ("\n");

        ) (model.locations_per_automaton automaton_index);

        print_message Verbose_standard ("\n");

) model.automata;



if (!isCUB_PTA && !inequalities_need_to_solve = []) = true 
then
    print_message Verbose_standard ("   The model is CUB-PTA! ")
else 
	(
	let constraint_for_cub = LinearConstraint.make_p_constraint (*p_linear_inequality_list*) !inequalities_need_to_solve in 

   	print_message Verbose_standard ("   The model is possible CUB-PTA! \nbut you need to solve the inequalities below!!: ");
   	print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constraint_for_cub)); 
    );

terminate_program();

*)

(**************************************************)
(* PART 1 END *)
(**************************************************)








(**************************************************)
(* PART 2 - CUB-PTA Transformation*)
(*
This is the beta version (simple), allow one clock, one constraint for each clock.
released version will be soon!!!!
*)
(**************************************************)
(*
let cub_check_3 invariant_s0 guard_t invariant_s1 clock_updates = 	
	(*ppl*)
	(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
	let inequalities = ref [] in
	print_message Verbose_standard (" CUB check, Start:");
	print_message Verbose_standard ("\n");

	(*transform constraints into inequality lists*)
	let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
	let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
	let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

	(*transform inequality list into tuple inequality list*)
	print_message Verbose_standard (" **Beginning state/location** :");
	let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
	print_message Verbose_standard (" **Transition** :");
	let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
	print_message Verbose_standard (" **Destination state/location** :");
	let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in


	let isCUB_PTA = ref true in

	(*check inequalities for each clock c on s0 -t-> s1*)
	List.iter (	fun clock_index -> 
	 	let inequalities_need_to_solve = ref [] in
	 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

	 	(*get each element of tuple of each clock - NOTE: the input musts contain 1 upper-bounded*)
	 	print_message Verbose_standard ("\n 	**Beginning state/location** :");
		let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
		print_message Verbose_standard ("\n 	**Transition** :");
		let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
		print_message Verbose_standard ("\n 	**Destination state/location** :");
		let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in

		(*convert back to constraint for each inequality*)
		let clock_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
		print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term)); 
		let linear_inequality_s0 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s0) op_s0 in
		let constraint_s0 = LinearConstraint.make_p_constraint [linear_inequality_s0] in
		let constraint_s0 = LinearConstraint.pxd_of_p_constraint constraint_s0 in
		print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0)); 
		let linear_inequality_t = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_t) op_t in
		let constraint_t = LinearConstraint.make_p_constraint [linear_inequality_t] in
		let constraint_t = LinearConstraint.pxd_of_p_constraint constraint_t in
		print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t));
		let linear_inequality_s1 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s1) op_s1 in
		let constraint_s1 = LinearConstraint.make_p_constraint [linear_inequality_s1] in
		let constraint_s1 = LinearConstraint.pxd_of_p_constraint constraint_s1 in
		print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));  


		print_message Verbose_standard ("\n");
		print_message Verbose_standard ("Comparing: ");

		(*just for printing*)
		let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
		let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
		let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
		print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
		print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
		print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
		(*just for printing*)

		(* if List.mem clock_index reset_clocks = true 
		then lower_inequality := linear_term_t; *)

		(* let result = ref true in *)
		let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
			 																							true;

			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
																										false;

			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
																										print_message Verbose_standard (" 	 Case 3 " );
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											let _ = print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
																											in
																											true
																											)
																										else
																											(
																											false;
																											);

			|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
																										false; 

			|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 4 " );
																										true;

			|_							, _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 5 " );
																										let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																										print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																										let constr = make_CUB_constraint [ineq] in
																										
																										if LinearConstraint.p_is_true constr
																										then true
																										else
																											(
																											if LinearConstraint.p_is_false constr
																											then 
																												(*inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];*)
																												false
																											else
																												(
																												inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
																												false;
																												);
																											);



			|_							, (LinearConstraint.Op_ge, _), _							->	print_message Verbose_standard (" 	 Case 5 " );
																										(*reset*)
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											let _ = print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
																											in
																											true
																											)
																										else
																											(
																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																											let constr = make_CUB_constraint [ineq] in
																											if LinearConstraint.p_is_true constr
																											then 
																												true
																											else
																												(
																												if LinearConstraint.p_is_false constr
																												then 
																													false
																												else
																													(
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
																													false;
																													);
																												);
																											);
																											

			| _							, _							 , _							-> 	print_message Verbose_standard (" 	 Case 6 " );
																										(*reset*)
																										if List.mem clock_index clock_updates = true
																										then
																											(
																											print_message Verbose_standard (" 	 Detected " 
																																			^ (model.variable_names clock_index) 
																																			^ " was a reset clock!\n 	 skipping the process: (" 
																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
																											
																											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
																											let constr = make_CUB_constraint [ineq] in
																											if LinearConstraint.p_is_true constr
																											then 
																												true
																												else
																												(
																												if LinearConstraint.p_is_false constr
																												then 
																													false
																												else
																													(
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq];
																													false;
																													);
																												);
																											)
																										else
																											(

																											
																											let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");
																											let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
																											print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");

																											(*let constr = make_CUB_constraint [ineq1;ineq2] in*)
																											let constr = LinearConstraint.make_pxd_constraint (inequalities_s0@inequalities_t@inequalities_s1) in
																												print_message Verbose_standard ("\n constr:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr)); 
																											
																											(*testing ppl function*)
																											let constr_inter = LinearConstraint.pxd_intersection [constr] in
																												print_message Verbose_standard ("\n constr_inter:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr_inter)); 

																											let constr1 = LinearConstraint.make_p_constraint ([ineq1]) in
																												print_message Verbose_standard ("\n constr1:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1)); 

																											let constr2 = LinearConstraint.make_p_constraint ([ineq2]) in
																												print_message Verbose_standard ("\n constr2:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2)); 

																											let abc = LinearConstraint.pxd_is_leq invariant_s0 guard_t in
																												print_message Verbose_standard ("\n abc:" ^ string_of_bool abc ); 
																											(*testing ppl function*)
																											


																											
																											
																											if LinearConstraint.pxd_is_true constr
																											then 
																												(
																												print_message Verbose_standard (" true ");
																												true
																												)
																											else
																												(
																												if LinearConstraint.pxd_is_false constr
																												then 
																													(
																													print_message Verbose_standard (" false ");
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq2];
																													false
																													)
																												else
																													(
																													print_message Verbose_standard (" false, not determined ");
																													inequalities_need_to_solve := !inequalities_need_to_solve@[ineq1;ineq2];
																													false;
																													);
																												);
																											

																											);

																										
																									

																										




			in
			

			inequalities := !inequalities@(!inequalities_need_to_solve);

			if (result = false)
			then
				(
			isCUB_PTA := false;
				print_message Verbose_standard (" This is not satisfied CUB-PTA! ");
				)
			else 
				print_message Verbose_standard (" This is satisfied CUB-PTA! ");



	print_message Verbose_standard ("\n");
	) model.clocks; 

	print_message Verbose_standard ("\n");
	print_message Verbose_standard (" CUB check, End!");
	print_message Verbose_standard ("\n");

	(!isCUB_PTA, !inequalities);
	in
*)
(*generated_constraints: we describe Disjunction by a list of conjunction constraints*)
let disjunction_constraints param_constraints =
	let generated_constraints = ref [LinearConstraint.p_true_constraint ()] in
	DynArray.iter ( fun (is_and, constraint_list) ->
		if(is_and = true)
		then
			(
				if List.length constraint_list <> 1
				then raise (InternalError(" Conjunction constraint_list error! "));
				generated_constraints := List.map (fun con -> 
					LinearConstraint.p_intersection ([con]@constraint_list);
				) !generated_constraints;
			)
		else 
			(
				if List.length constraint_list = 0
				then raise (InternalError(" Disjunction constraint_list error! "));
				let temp = ref [] in
				List.iter (fun cons1 -> 
					List.iter (fun cons2 ->
						let con3 = LinearConstraint.p_intersection [cons1; cons2] in 
						temp := !temp@[con3];
					) !generated_constraints
				) constraint_list;
				generated_constraints := !temp;
			);
	) param_constraints;
	!generated_constraints
in
(*generated_constraints: we describe Disjunction by a list of conjunction constraints - end*)



(*check parameters_constraints all false*)
let is_parameters_constraints_false p_cons = let ls = disjunction_constraints p_cons in
												let resut = ref true in
												List.iter (fun con ->
													if (LinearConstraint.p_is_satisfiable con) 
													then resut := false;
												) ls;
												!resut;
											in
(*check parameters_constraints all false - end*)



(*Check cub_contraint conflict with parameters_constraints*)
let isContraintConflictsParametersConstraints con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref false in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.p_intersection [con; con1] in 
		(* print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) ) ; *)
		if LinearConstraint.p_is_false con_intersection
		then
			(
			(* print_message Verbose_standard ("\n Conflict!!! "); *)
			check := true;
			);
	) disjunction_cons;
	!check;
in
(*Check cub_contraint conflict with parameters_constraints - end*)



(*Check cub_contraint all conflict with parameters_constraints - use for p and p*)
let isContraintAllConflictsParametersConstraints con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref true in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.p_intersection [con; con1] in 
		print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
		if not (LinearConstraint.p_is_false con_intersection)
		then
			(
			print_message Verbose_standard ("\n Conflict!!! ");
			check := false;
			);
	) disjunction_cons;
	!check;
in
(*Check cub_contraint conflict with parameters_constraints - end*)



(*Check cub_contraint all conflict with parameters_constraints - use for pxd and p*)
let isContraintAllConflictsParametersConstraints2 con p_cons = 
	let disjunction_cons = disjunction_constraints p_cons in
	let check = ref true in
	List.iter (fun con1 -> 
		let con_intersection = LinearConstraint.pxd_intersection [con; (LinearConstraint.pxd_of_p_constraint con1)] in 
		print_message Verbose_standard ("\n Constraint1: \n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con)  
										^ "\n Constraint2: \n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names con1) );
		if not (LinearConstraint.pxd_is_false con_intersection)
		then
			(
			print_message Verbose_standard ("\n Conflict!!! ");
			check := false;
			);
	) disjunction_cons;
	!check;
in
(*Check cub_contraint conflict with parameters_constraints - end*)




(* (* Hashtbl *)
(*Check whether a clock constraint conatained in clocks constraints*)
let isConstraintContainedInClocksConstraints loc_index con c_cons =
	let check = ref false in
	Hashtbl.iter (fun location_index con1 -> 
		if location_index = loc_index
		then (
				if LinearConstraint.pxd_is_equal con1 con
				then (
						check := true;
					);
			);
	) c_cons;
	!check
in
(*Check whether a clock constraint conatained in clocks constraints - end*) *)


(* DynArray *)
(* Check whether a clock constraint conatained in clocks constraints *)
let isConstraintContainedInClocksConstraints loc_index con c_cons =
	let check = ref false in
	DynArray.iter (fun (location_index, con1) -> 
		if location_index = loc_index
		then (
				if LinearConstraint.pxd_is_equal con1 con
				then (
						check := true;
					);
			);
	) c_cons;
	!check
in
(*Check whether a clock constraint conatained in clocks constraints - end*)



(*Check whether a constraint conatained in parameters relation*)
let isConstraintContainedInParametersConstraints con p_cons =
	let check = ref false in
	DynArray.iter (fun (b, cons) ->
		if (b = true) 
		then  
			(
			let con1 = List.hd cons in
			if (LinearConstraint.p_is_equal con1 con)
			then check := true; 
			);
	) p_cons;
	!check;
in
(*Check whether a constraint conatained in parameters relation - end*)

let getInfoCurrentModel submodel = let (states, transitions, clocks_constraints, parameters_constraints) = submodel in
								print_message Verbose_standard ("\n ------------------------Submodel Info------------------------------- ");

								
								print_message Verbose_standard ("\n Number of states: " ^ string_of_int (Hashtbl.length states) );
								print_message Verbose_standard ("\n Number of transitions: " ^ string_of_int (DynArray.length transitions) );

								(*for checking the first parameters constraints*)
								print_message Verbose_standard ("\n Current parameters relation!!!! ");
								DynArray.iter ( fun (is_and, constraint_list) ->
									print_message Verbose_standard ("\n Is Conjunction: " ^ string_of_bool is_and ^ ": ");
									List.iter (fun cons -> 
										print_message Verbose_standard (" 	Constraints: \n" 
																^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons) );
									) constraint_list;
								) parameters_constraints;
								(*for checking the first parameters constraints - end*)

								(*clocks constraitns*)
								print_message Verbose_standard ("\n Current clocks constrains!!!!: ");
								(
								DynArray.iter (fun (index, cons) -> 
									print_message Verbose_standard ("\n Location: " ^ index );
									
									print_message Verbose_standard ("  Constraints: \n" 
																^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) );
								) clocks_constraints;
								);
								(*clocks constraitns*)

								print_message Verbose_standard ("\n ------------------------Submodel Info end--------------------------- ");
							in


(*Array of models*)
let submodels = DynArray.make 0 in



let find_all_clocks_constraints clocks_constraints location_index = let ls = ref [] in
																	DynArray.iter (fun (loc_index, cons) ->
																		if loc_index = location_index 
																		then
																			ls := !ls@[cons];	 
																	) clocks_constraints;
																	!ls;
																in

let init_loc = ref "" in

(* initial_location *)


(*single clock bounded*)
if List.length model.automata > 1
then raise (InternalError(" Sorry, we only support for single model currently. Multi-clocks and multi-constraint will be implemented in next version! "));


(*covert input model into specific data stucture*)
List.iter (fun automaton_index -> 
	(* print_message Verbose_standard ("Converting automaton: " 
									^ (model.automata_names automaton_index) 
									^ "!!!!!!!"); *)

	(*elements of a submodels*)
	(*initial*)
	let states_ini = Hashtbl.create 0 in
	let transitions_ini = DynArray.make 0 in
	let clocks_constraints_ini = DynArray.make 0 in
	let parameters_constraints_ini = DynArray.make 0 in
	(*initial - end*)

	(*set true constraint*)
	let true_constraint = LinearConstraint.p_true_constraint () in
	DynArray.add parameters_constraints_ini (true,[true_constraint]); 

	init_loc := (model.location_names automaton_index (List.hd (model.locations_per_automaton automaton_index)));


	(*Checking bounded clocked in invariant (Location)*)
    List.iter (fun location_index -> 
    	(* print_message Verbose_standard ("----------------Begin at " ^ (model.location_names automaton_index location_index) ^ "-------------------");
		print_message Verbose_standard ("\n");
		print_message Verbose_standard (" State/Location(S): " ^ (model.location_names automaton_index location_index) ) ; *)
		let invariant1 = model.invariants automaton_index location_index in
		
		(*add states*)
		let location_index_string = (model.location_names automaton_index location_index) in
		Hashtbl.add states_ini location_index_string invariant1;

        (* print_message Verbose_standard ("   Invariant(S): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant1 ) )  ;
		print_message Verbose_standard ("\n"); *)
        	(*Checking bounded clocked in guards (Transition)*)
        	List.iter (fun action_index -> print_message Verbose_standard (" Transition/Action: " ^ (model.action_names action_index) );
    
            	List.iter (fun (guard, clock_updates, _, destination_location_index) 
            		-> (* print_message Verbose_standard ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard)); *)		
                	(** WORK HERE **)
                	let invariant2 = model.invariants automaton_index destination_location_index in
					(* print_message Verbose_standard ("\n");
        			print_message Verbose_standard (" State/Location(D): " ^ (model.location_names automaton_index destination_location_index) ) ;
        			print_message Verbose_standard ("   Invariant(D): " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names invariant2 ) ) ;
        			print_message Verbose_standard ("	  ----Map:(" 
        											^ (model.location_names automaton_index location_index) 
        											^ ")--" ^ (model.action_names action_index) ^ "-->(" 
        											^ (model.location_names automaton_index destination_location_index) 
        											^ ") ----" );
        			print_message Verbose_standard ("\n"); *)
                	let clock_updates = match clock_updates with
                						  No_update -> []
										| Resets clock_update -> clock_update
										| Updates clock_update_with_linear_expression -> raise (InternalError(" Clock_update are not supported currently! ")); 
					in

					(*add transitions*)
					DynArray.add transitions_ini ((model.location_names automaton_index location_index), (model.location_names automaton_index destination_location_index), guard, clock_updates);
        			
        			()
            	) (model.transitions automaton_index location_index action_index); 
        	) (model.actions_per_location automaton_index location_index); 
    		(* print_message Verbose_standard ("----------------End converting " ^ (model.location_names automaton_index location_index) ^ "!!!---------------------");
    		print_message Verbose_standard ("\n"); *)
    ) (model.locations_per_automaton automaton_index);
    (* print_message Verbose_standard ("\n"); *)
(* ) model.automata; *)
(*covert input model into specific data stucture - end*)






(*Adding the first sub-model*)
DynArray.add submodels (states_ini, transitions_ini, clocks_constraints_ini, parameters_constraints_ini);
let submodels_length = DynArray.length submodels in
print_message Verbose_standard ("\n Check lenth of submodels: " ^ (string_of_int submodels_length) );




(*loop through each submodel*)
let count_m = ref 1 in
while (!count_m) <= (DynArray.length submodels) do
	
	let submodel = DynArray.get submodels (!count_m - 1) in
	print_message Verbose_standard ("\n Sub-model no: " ^ (string_of_int !count_m) );
	let (states, transitions, clocks_constraints, parameters_constraints) = submodel in
	print_message Verbose_standard ("\n Number of states: " ^ (string_of_int (Hashtbl.length states)) );
	print_message Verbose_standard ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );
	
	print_message Verbose_standard ("\n ----------------Sub-model No: " ^ (string_of_int !count_m) ^ "---------------------------");
	let adding = ref true in


	let loc_clocks_constraints = DynArray.make 0 in

	let count_loop = ref 1 in

	while !adding = true do
	(
	adding := false;

	if !count_loop = 1 
	then
	(

	(* let submodel = DynArray.get submodels (!count_m - 1) in *)
	

	(* let (states, transitions, clocks_constraints, parameters_constraints) = submodel in *)
	print_message Verbose_standard ("\n Number of states: " ^ (string_of_int (Hashtbl.length states)) );
	print_message Verbose_standard ("\n Number of transitions: " ^ (string_of_int (DynArray.length transitions)) );

	(* let (_, transitions, _, _) = submodel in *)
	(*stage 1*)
	let count_t = ref 1 in
	DynArray.iter ( fun transition -> 
		(* let (states, transitions, clocks_constraints, parameters_constraints) = submodel in *)
		print_message Verbose_standard ("\n Transition No: " ^ (string_of_int !count_t) );
		let (location_index, destination_location_index, guard, clock_updates) = transition in
		(*work here*)
		let invariant_s0 = Hashtbl.find states location_index in
		let guard_t = guard in
		let invariant_s1 = Hashtbl.find states destination_location_index in
		(*ppl*)
		(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
		let inequalities = ref [] in
		print_message Verbose_standard (" CUB transformation, Start:");
		print_message Verbose_standard ("\n");
		(*transform constraints into inequality lists*)
		let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
		let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
		let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
		(*transform inequality list into tuple inequality list*)
		(* print_message Verbose_standard (" **Beginning state/location** :"); *)
		let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
		(* print_message Verbose_standard (" **Transition** :"); *)
		let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
		(* print_message Verbose_standard (" **Destination state/location** :"); *)
		let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in
		(*single clock bounded*)
		(* if List.length model.clocks > 1
		then raise (InternalError(" Sorry, we only support for single clock currently. Multi-clocks and multi-constraint will be implemented in next version! ")); *)
		(* let result = ref true in *)
		print_message Verbose_standard ("\n --------------------1st check start---------------------- ");
		List.iter (	fun clock_index -> 
		 	let inequalities_need_to_solve = ref [] in
		 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):"); 	
		 	(*get each element of tuple of each clock - NOTE: the input musts contain 1 upper-bounded*)
		 	(* print_message Verbose_standard ("\n 	**Beginning state/location** :"); *)
			let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
			(* print_message Verbose_standard ("\n 	**Transition** :"); *)
			let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
			(* print_message Verbose_standard ("\n 	**Destination state/location** :"); *)
			let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in
			(*get each element of tuple of each clock - end*)
			(*convert back to constraint for each inequality*)
			let clock_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
			(* print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term));  *)
			let linear_inequality_s0 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s0) op_s0 in
			let constraint_s0 = LinearConstraint.make_p_constraint [linear_inequality_s0] in
			let constraint_s0 = LinearConstraint.pxd_of_p_constraint constraint_s0 in
			(* print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0));  *)
			let linear_inequality_t = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_t) op_t in
			let constraint_t = LinearConstraint.make_p_constraint [linear_inequality_t] in
			let constraint_t = LinearConstraint.pxd_of_p_constraint constraint_t in
			(* print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t)); *)
			let linear_inequality_s1 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s1) op_s1 in
			let constraint_s1 = LinearConstraint.make_p_constraint [linear_inequality_s1] in
			let constraint_s1 = LinearConstraint.pxd_of_p_constraint constraint_s1 in
			(* print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));   *)
			(*convert back to constraint for each inequality - end*)
			print_message Verbose_standard ("\n");
			print_message Verbose_standard ("Comparing: ");
			(*just for printing*)
			let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
			let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
			let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
			print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
			print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
			print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
			print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
			(*just for printing - end*)
			
			(*check 1*)
			let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with
				|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
				 	(*Case 1*)
				 	print_message Verbose_standard (" 	 Case 1 " );
				 	true;
				 	(*Case 1 - end*)
				|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	
					(*Case 2*)
					print_message Verbose_standard (" 	 Case 2 " );
					(* none reset zone *)
					let clock_cons = (LinearConstraint.pxd_intersection [constraint_t]) in
					let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
					if check2 = false 
					then
						(
						DynArray.add clocks_constraints (location_index, clock_cons);
						(* adding := true; *)
						);
					false;
					(* none reset zone - end *)
					(*Case 2 - end*)
				|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	
					(*Case 3*)
					(*reset*)
					print_message Verbose_standard (" 	 Case 3 " );
					if List.mem clock_index clock_updates = true
					then
						(
						(* reset zone *)
						let _ = print_message Verbose_standard (" 	 Detected " 
																^ (model.variable_names clock_index) 
																^ " was a reset clock!\n 	 skipping the process: (" 
																^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str 
																^ ")!" ) 
						in
						true
						(* reset zone - end *)
						)
					else
						(
						(* none reset zone *)

						let clock_cons = (LinearConstraint.pxd_intersection [constraint_s1]) in
						let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
						if check2 = false 
						then
							(
							DynArray.add clocks_constraints (location_index, clock_cons);
							(* adding := true; *)
							);
						false;

						(* none reset zone - end *)
						);
					(*Case 3 - end*)
				|(LinearConstraint.Op_ge, _), _							 , _							->
					(*Case 4*)
					(*reset but useless*)
					print_message Verbose_standard (" 	 Case 4 " );
					(*reset*)
					let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; constraint_s1]) in
					(* reset zone *)
					if (List.mem clock_index clock_updates) = true
					then
						(
						clock_cons := (LinearConstraint.pxd_intersection [constraint_t]);
						);
					(* reset zone - end*)

					let check2 = isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints in
					if check2 = false 
					then
						(
						DynArray.add clocks_constraints (location_index, !clock_cons);
						(* adding := true; *)
						);
					false; 
					(*Case 4 - end*)
				|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	
					(*Case 5*)
					print_message Verbose_standard (" 	 Case 5 " );
					true;
					(*Case 5 - end*)
				|_							, _							 , (LinearConstraint.Op_ge, _)	->	
					(*Case 6*)
					(* none reset zone *)
					print_message Verbose_standard (" 	 Case 6 " );
					let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
					print_message Verbose_standard (" Forming inequality: " 
													^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq 
													^ "!!!\n");
					let constr = make_CUB_constraint [ineq] in
		
					if LinearConstraint.p_is_true constr
					then
						(
						print_message Verbose_standard (" true, comparable "); 
						true
						)
					else
						(
						let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t]) in
						let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
						if LinearConstraint.p_is_false constr
						then 
							(
							print_message Verbose_standard (" false, comparable ");

							if check2 = false 
							then
								(
								DynArray.add clocks_constraints (location_index, clock_cons);
								(* adding := true; *)
								);
							false

							)
						else
							(
							print_message Verbose_standard (" false, not determined ");
							(*submodel info*)
							getInfoCurrentModel submodel;
							let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
							let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
							print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
															(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
															^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

							if check1 = true
							then
								(
								if check2 = false
								then
									(
									print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
									print_message Verbose_standard (" Adding new clocks constraints" );
									DynArray.add clocks_constraints (location_index, clock_cons);
									(* print_message Verbose_standard (" Added constraints: " 
																	^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																	^ "\n at state: " 
																	^ string_of_int location_index ); *)
									(* adding := true;  *)
									);
								)
							else
								(
								if check3 = false
								then
									(
									print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
									(*Add First parameter constraints in the current submodel*)
									let new_parameters_constraints = DynArray.copy parameters_constraints in
									print_message Verbose_standard ("\n Add constraint " 
																	^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																	^ " into submodel ((parameters_constraints)) " 
																	^ string_of_int (!count_m)  );
									DynArray.add parameters_constraints (true, [constr]);
									(*Create new submodel with Second constraint*)
									let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
									let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
									let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
									print_message Verbose_standard ("\n Add constraint " 
																	^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																	^ " into submodel (parameters_constraints) " 
																	^ string_of_int ((DynArray.length submodels) +1) );
									DynArray.add new_parameters_constraints (false, [constr1]);
									if is_parameters_constraints_false new_parameters_constraints
									then 
										(
										print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
										)
									else
										(
										let new_clocks_constraints = DynArray.make 0 in
										(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
										(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
										DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
										);
									);
								);
							false;
							);
						);
					(* none reset zone - end*)
					(*Case 6 - end*)
				|_							, (LinearConstraint.Op_ge, _), _							->	
					(*Case 7*)
					print_message Verbose_standard (" 	 Case 7 " );
					(*reset*)
					if List.mem clock_index clock_updates = true
					then
						(
						(* reset zone *)
						let _ = print_message Verbose_standard (" 	 Detected " 
														^ (model.variable_names clock_index) 
														^ " was a reset clock!\n 	 skipping the process: (" 
														^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
						in
						true
						(* reset zone - end*)
						)
					else
						(
						(* none reset zone *)
						let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
						print_message Verbose_standard (" Forming inequality: " ^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
						let constr = make_CUB_constraint [ineq] in
						if LinearConstraint.p_is_true constr
						then 
							(
							print_message Verbose_standard (" true, comparable ");
							true
							)
						else
							(
							let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_s1]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if LinearConstraint.p_is_false constr
							then 
								(
								print_message Verbose_standard (" false, comparable ");

								if check2 = false 
								then
									(
									DynArray.add clocks_constraints (location_index, clock_cons);
									(* adding := true; *)
									);
								false

								)
							else
								(
								print_message Verbose_standard (" false, not determined ");
								(*submodel info*)
								getInfoCurrentModel submodel;
								let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
								let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
								print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
								if check1 = true
								then
									(
									if check2 = false
									then
										(
										print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
										print_message Verbose_standard (" Adding new clocks constraints" );
										DynArray.add clocks_constraints (location_index, clock_cons);
										(* print_message Verbose_standard (" Added constraints: " 
																		^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																		^ "\n at state: " 
																		^ string_of_int location_index ); *)
										(* adding := true;  *)
										);
									)
								else
									(
									if check3 = false
									then
										(
										print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
										(*Add First parameter constraints in the current submodel*)
										let new_parameters_constraints = DynArray.copy parameters_constraints in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																		^ " into submodel ((parameters_constraints)) " 
																		^ string_of_int (!count_m)  );
										DynArray.add parameters_constraints (true, [constr]);

										(*Create new submodel with Second constraint*)
										let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
										let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
										let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
										print_message Verbose_standard (" Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										DynArray.add new_parameters_constraints (false, [constr2]);

										if is_parameters_constraints_false new_parameters_constraints
										then 
											(
											print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
											)
										else
											(
											let new_clocks_constraints = DynArray.make 0 in
											(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
											(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
											DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
											);
										);
									);
								false;
								);
							);
						(* none reset zone *)
						);
					(*Case 7 - end*)																						
				| _							, _							 , _							-> 	
					(*Case 8*)
					print_message Verbose_standard (" 	 Case 8 " );
					(*reset*)
					if List.mem clock_index clock_updates = true
					then
						(
						(* reset zone *)
						print_message Verbose_standard (" 	 Detected " 
														^ (model.variable_names clock_index) 
														^ " was a reset clock!\n 	 skipping the process: (" 
														^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
						
						let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						print_message Verbose_standard (" Forming inequality: " 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
						let constr = make_CUB_constraint [ineq] in

						if LinearConstraint.p_is_true constr
						then 
							(
							print_message Verbose_standard (" true, comparable ");
							true
							)
						else
							(
							let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if LinearConstraint.p_is_false constr
							then 
								(
								print_message Verbose_standard (" false, comparable ");

								if check2 = false 
								then
								DynArray.add clocks_constraints (location_index, clock_cons);
								(* adding := true; *)
								false

								)
							else
								(
								print_message Verbose_standard (" false, not determined ");

								(*submodel info*)
								getInfoCurrentModel submodel;
								let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
								let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
								print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																(* ^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2 *)
																^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
								if check1 = true
								then
									(
									if check2 = false
									then
										(
										print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
										print_message Verbose_standard (" Adding new clocks constraints" );
										DynArray.add clocks_constraints (location_index, clock_cons);
										(* print_message Verbose_standard (" Added constraints: " 
																		^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																		^ "\n at state: " 
																		^ string_of_int location_index ); *)
										(* adding := true;  *)
										);
									)
								else
									(
									if check3 = false
									then
										(
										print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
										(*Add First parameter constraints in the current submodel*)
										let new_parameters_constraints = DynArray.copy parameters_constraints in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																		^ " into submodel ((parameters_constraints)) " 
																		^ string_of_int (!count_m)  );
										DynArray.add parameters_constraints (true, [constr]);
										(*Create new submodel with Second constraint*)
										let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
										let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
										let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										DynArray.add new_parameters_constraints (false, [constr1]);
										if is_parameters_constraints_false new_parameters_constraints
										then 
											(
											print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
											)
										else
											(
											let new_clocks_constraints = DynArray.make 0 in
											(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
											(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
											DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
											);
										);
									);
								false;
								);
							);
						(* reset zone - end *)
						)
					else
						(
						(* none reset zone *)
						let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
						print_message Verbose_standard (" Inequality s0 <= t: \n" 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq1 ^ "!!!\n");
						let ineq2 = make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1) in
						print_message Verbose_standard (" Inequality s0 <= s1: \n" 
														^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq2 ^ "!!!\n");
						let constr = make_CUB_constraint [ineq1;ineq2] in

						if LinearConstraint.p_is_true constr
						then 
							(
							print_message Verbose_standard (" true, comparable ");
							true
							)
						else
							(
							let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; constraint_s1]) in
							let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
							if LinearConstraint.p_is_false constr
							then 
								(
								print_message Verbose_standard (" false, comparable ");

								if check2 = false 
								then
									(
									DynArray.add clocks_constraints (location_index, clock_cons);
									(* adding := true; *)
									);
								false

								)
							else
								(
								print_message Verbose_standard (" false, not determined ");
								(*submodel info*)
								getInfoCurrentModel submodel;
								let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
								let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in
								print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );
								if check1 = true
								then
									(
									if check2 = false
									then
										(
										print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
										print_message Verbose_standard (" Adding new clocks constraints" );
										DynArray.add clocks_constraints (location_index, clock_cons);
										(* print_message Verbose_standard (" Added constraints: " 
																		^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																		^ "\n at state: " 
																		^ string_of_int location_index ); *)
										(* adding := true;  *)
										); 
									)
								else
									(
									if check3 = false
									then
										(
										print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
										(*Add First parameter constraints in the current submodel*)
										let new_parameters_constraints = DynArray.copy parameters_constraints in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																		^ " into submodel ((parameters_constraints)) " 
																		^ string_of_int (!count_m)  );
										DynArray.add parameters_constraints (true, [constr]);
										(*Create new submodel with Second constraint*)
										let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
										let linear_term_2 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1 in
										let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
										let linear_inequality_2 = LinearConstraint.make_p_linear_inequality linear_term_2 LinearConstraint.Op_g in
										let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
										let constr2 = LinearConstraint.make_p_constraint ([linear_inequality_2]) in
										print_message Verbose_standard ("\n Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										print_message Verbose_standard (" Add constraint " 
																		^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																		^ " into submodel (parameters_constraints) " 
																		^ string_of_int ((DynArray.length submodels) +1) );
										DynArray.add new_parameters_constraints (false, [constr1;constr2]);
										if is_parameters_constraints_false new_parameters_constraints
										then 
											(
											print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
											)
										else
											(
											let new_clocks_constraints = DynArray.make 0 in
											(* let new_clocks_constraints = DynArray.copy clocks_constraints in *)
											(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
											DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
											); 
										);
									);
								false;
								);
							);
						(* none reset zone - end*)
						);
					(*Case 8 - end*)

			in
			(*check 1 - end*)
			print_message Verbose_standard ("\n --------------------1st check end----------------------- ");

		print_message Verbose_standard ("\n");
		) model.clocks; 

		DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));


		(* let loc_clocks_constraints = DynArray.make 0 in *)
		let con = ref (LinearConstraint.pxd_true_constraint ()) in
		for i = 1 to (DynArray.length clocks_constraints - 1) do
			let (loc_index1, cons1) = DynArray.get clocks_constraints (i-1) in
			let (loc_index2, cons2) = DynArray.get clocks_constraints (i) in
			if loc_index1 = loc_index1 
			then
			 	(
			 	let check1 = LinearConstraint.pxd_is_true cons1 in
			 	let check2 = LinearConstraint.pxd_is_true cons2 in 
			 	match (check1, check2) with
			 		| true,  true  -> ()
			 		| true,  false -> (* con := LinearConstraint.pxd_intersection [!con; cons2]  *) ()
			 		| false, true  -> con := LinearConstraint.pxd_intersection [!con; cons1];
			 						  let check = isConstraintContainedInClocksConstraints loc_index1 !con loc_clocks_constraints in
			 						  if LinearConstraint.pxd_is_true !con = false && check = false
			 						  then
			 						  	(
			 						  	(* add true constraint *)
			 						  	DynArray.add loc_clocks_constraints (loc_index1, !con);
			 						  	adding := true;
			 						  	);
			 						  DynArray.add loc_clocks_constraints (loc_index1, cons2);
			 						  con := (LinearConstraint.pxd_true_constraint ())
			 		| false, false -> con := LinearConstraint.pxd_intersection [!con; cons1; cons2];
			 	)
			else
			 	(
			 		(*  *)
			 	);

		done;
		DynArray.clear clocks_constraints; 
		DynArray.append (DynArray.copy loc_clocks_constraints) clocks_constraints;  

		(* if !adding = false
		then
			(
			DynArray.clear loc_clocks_constraints; 
			);  *)

		(* DynArray.clear clocks_constraints; *)
		(* let conj = LinearConstraint.pxd_intersection !cons in
		DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_false_constraint ())); *)

		(* increase transition count *)
		count_t := !count_t+1;
		();
	(* end - transitions loop *)
	) transitions;
	
	(* end if loop_count condition *)
	);
	(* avoid do the same things in next loop *)
	count_loop := !count_loop + 1;

	


	let count_t = ref 1 in
	DynArray.iter ( fun transition -> 
		(* let (states, transitions, clocks_constraints, parameters_constraints) = submodel in *)
		print_message Verbose_standard ("\n Transition No: " ^ (string_of_int !count_t) );
		let (location_index, destination_location_index, guard, clock_updates) = transition in
		(*work here*)
		let invariant_s0 = Hashtbl.find states location_index in
		let guard_t = guard in
		let invariant_s1 = Hashtbl.find states destination_location_index in
		(*ppl*)
		(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
		let inequalities = ref [] in
		print_message Verbose_standard (" CUB transformation, Start:");
		print_message Verbose_standard ("\n");
		(*transform constraints into inequality lists*)
		let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
		let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
		let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in
		(*transform inequality list into tuple inequality list*)
		(* print_message Verbose_standard (" **Beginning state/location** :"); *)
		let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
		(* print_message Verbose_standard (" **Transition** :"); *)
		let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
		(* print_message Verbose_standard (" **Destination state/location** :"); *)
		let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in

		print_message Verbose_standard ("\n --------------------2nd check start---------------------- ");

		List.iter (	fun clock_index -> 
		 	let inequalities_need_to_solve = ref [] in
		 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");
		
			(*get each element of tuple of each clock - NOTE: the input musts contain 1 upper-bounded*)
		 	(* print_message Verbose_standard ("\n 	**Beginning state/location** :"); *)
			let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
			(* print_message Verbose_standard ("\n 	**Transition** :"); *)
			let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
			(* print_message Verbose_standard ("\n 	**Destination state/location** :"); *)
			let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in
			(*get each element of tuple of each clock - end*)

			(*convert back to constraint for each inequality*)
			let clock_term = LinearConstraint.make_p_linear_term [NumConst.one,clock_index] NumConst.zero in
			(* print_message Verbose_standard ("\n clock_term:" ^ (LinearConstraint.string_of_p_linear_term model.variable_names clock_term));  *)
			let linear_inequality_s0 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s0) op_s0 in
			let constraint_s0 = LinearConstraint.make_p_constraint [linear_inequality_s0] in
			let constraint_s0 = LinearConstraint.pxd_of_p_constraint constraint_s0 in
			(* print_message Verbose_standard ("\n constraint_s0:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s0));  *)
			let linear_inequality_t = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_t) op_t in
			let constraint_t = LinearConstraint.make_p_constraint [linear_inequality_t] in
			let constraint_t = LinearConstraint.pxd_of_p_constraint constraint_t in
			(* print_message Verbose_standard ("\n constraint_t:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_t)); *)
			let linear_inequality_s1 = LinearConstraint.make_p_linear_inequality (LinearConstraint.sub_p_linear_terms clock_term linear_term_s1) op_s1 in
			let constraint_s1 = LinearConstraint.make_p_constraint [linear_inequality_s1] in
			let constraint_s1 = LinearConstraint.pxd_of_p_constraint constraint_s1 in
			(* print_message Verbose_standard ("\n constraint_s1:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_s1));   *)
			(*convert back to constraint for each inequality - end*)
			print_message Verbose_standard ("\n");
			print_message Verbose_standard ("Comparing: ");
			(*just for printing*)
			let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
			let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
			let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " 
									^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in
			print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
			print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
			print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
			print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");
			(*just for printing - end*)

			(*check 2*)
			if (DynArray.length clocks_constraints) <> 0
			then
				(	
					(* print_message Verbose_standard ("\n Location: " ^ string_of_int destination_location_index ); *)
					let constraints_s0 = find_all_clocks_constraints clocks_constraints location_index in
					let constraints_s1 = find_all_clocks_constraints clocks_constraints destination_location_index in
					List.iter (fun c_s1 ->
						print_message Verbose_standard ("\n Founded constraint: " 
																			^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c_s1));
						let is_contained = ref false in
						List.iter (fun c_s0 -> 
							if LinearConstraint.pxd_is_equal c_s0 c_s1
							then is_contained := true;
						) constraints_s0;

						if (!is_contained = false)
						then
							(
								print_message Verbose_standard ("\n This constraint not contained in constraints_s0, start to check!!! " );

								let inequalities_s1 = LinearConstraint.pxd_get_inequalities c_s1 in
								let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in
								(* let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in *)

								let _ =
								( 
									match (op_s0, linear_term_s0), (op_t, linear_term_t) with
									|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	 
										(*Case 1*)
										print_message Verbose_standard (" 	 Case 1 (2) " );
										if List.mem clock_index clock_updates = false
										then
											(
											print_message Verbose_standard (" false, comparable ");
											let clock_cons = LinearConstraint.pxd_intersection ([c_s1]) in
											let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
											if check2 = false 
											then
												(
												DynArray.add clocks_constraints (location_index, clock_cons);
												(* adding := true; *)
												)
											);
										(*Case 1 - end*)

									|(LinearConstraint.Op_ge, _), _								->
										(*Case 2*)
										print_message Verbose_standard (" 	 Case 2 (2) " );
										print_message Verbose_standard (" false, comparable ");
										let clock_cons = ref (LinearConstraint.pxd_intersection [constraint_t; c_s1]) in
										if (List.mem clock_index clock_updates) = true
										then
											(
											clock_cons := (LinearConstraint.pxd_intersection [constraint_t]);
											);
										let check2 = (isConstraintContainedInClocksConstraints location_index !clock_cons clocks_constraints) in
										if check2 = false 
										then
											(
											DynArray.add clocks_constraints (location_index, !clock_cons);
											(* adding := true; *)
											);
										(*Case 2 - end*)

									|_							, (LinearConstraint.Op_ge, _)	->	
										(*Case 3*)
										print_message Verbose_standard (" 	 Case 3 (2) " );
										if (List.mem clock_index clock_updates) = false
										then
											(

											let ineq2 = ref [] in
											List.iter (fun  (_, op_s1, linear_term_s1) ->
												ineq2 := !ineq2@[(make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1))];
											) tuple_inequalities_s1;

											let cub_cons = make_CUB_constraint (!ineq2) in
											print_message Verbose_standard ("\n CUB constraint: " 
																				^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons));

											if LinearConstraint.p_is_true cub_cons
											then 
												(
												print_message Verbose_standard (" true, comparable ");
												)
											else
												(
												let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; c_s1]) in
												let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
												if LinearConstraint.p_is_false cub_cons
												then 
													(
													print_message Verbose_standard (" false, comparable ");
													if check2 = false 
													then
														(
														DynArray.add clocks_constraints (location_index, clock_cons);
														(* adding := true; *)
														);
													)
												else
													(
													print_message Verbose_standard (" false, not determined ");

													(*get submodel info*)
													getInfoCurrentModel submodel;
													let check1 = isContraintConflictsParametersConstraints cub_cons parameters_constraints in
													let check3 = isConstraintContainedInParametersConstraints cub_cons parameters_constraints in

													print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																					^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																					^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

													if check1 = true
													then
														(
														if check2 = false
														then
															(
															print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
															print_message Verbose_standard (" Adding new clocks constraints" );

															(* test *)
															let new_parameters_constraints = DynArray.copy parameters_constraints in

															let linear_term_2 = ref [] in
															List.iter (fun  (_, op_s1, linear_term_s1) ->
																linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
															) tuple_inequalities_s1;

															let linear_inequality_2 = ref [] in
															List.iter (fun term ->
																linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
															) !linear_term_2;

															let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

															let conj =  (LinearConstraint.p_intersection [constr2]) in
															let check = isContraintAllConflictsParametersConstraints conj parameters_constraints in

															if not check
															then
																(
																DynArray.add parameters_constraints (false, [constr2]);
																DynArray.add clocks_constraints (location_index, clock_cons);
																(* adding := true; *)
																);
															(* test - end *)

															(* print_message Verbose_standard (" Added constraints: " 
																							^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																							^ "\n at state: " 
																							^ string_of_int location_index ); *)
															);
														)
													else
														(
														if check3 = false
														then
															(
															print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );

															(*Add First parameter constraints in the current submodel*)
															let new_parameters_constraints = DynArray.copy parameters_constraints in
															print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons) 
																							^ " into submodel ((parameters_constraints)) " 
																							^ string_of_int (!count_m)  );
															DynArray.add parameters_constraints (true, [cub_cons]);

															let linear_term_2 = ref [] in
															List.iter (fun  (_, op_s1, linear_term_s1) ->
																linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
															) tuple_inequalities_s1;

															let linear_inequality_2 = ref [] in
															List.iter (fun term ->
																linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
															) !linear_term_2;

															let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

															let checkConflict2 = isContraintConflictsParametersConstraints constr2 new_parameters_constraints in
															(
															match checkConflict2 with
															| true (* false *)  -> print_message Verbose_standard ("\n conflict!!");

															| false (* true *) -> print_message Verbose_standard ("\n not conflict!!");
																				DynArray.add new_parameters_constraints (false, [constr2]);
																				print_message Verbose_standard (" Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );
															);

															if (is_parameters_constraints_false new_parameters_constraints) || checkConflict2
															then 
																(
																print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
																)
															else
																(
																(* let new_clocks_constraints = Hashtbl.create 0 in
																Hashtbl.add new_clocks_constraints location_index clock_cons;
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints); *)
																
																(* let new_clocks_constraints = DynArray.make 0 in *)
																let new_clocks_constraints = DynArray.copy clocks_constraints in
																(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
																(* () *)
																(* raise (InternalError(" Sorry!!!!!! ")); *)

																);

															);
														);
													);
												);

											);
										(*Case 3 - end*)

									|_							, _								->	
										(*Case 4*)
										print_message Verbose_standard (" 	 Case 4 (2) " );	

										(*reset*)
										if List.mem clock_index clock_updates = true
										then
											(
											(* reset zone *)
											print_message Verbose_standard (" 	 Detected " 
																			^ (model.variable_names clock_index) 
																			^ " was a reset clock!\n 	 skipping the process: (" 
																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
											
											let ineq = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
											print_message Verbose_standard (" Forming inequality: " 
																			^ LinearConstraint.string_of_p_linear_inequality model.variable_names ineq ^ "!!!\n");
											let constr = make_CUB_constraint [ineq] in

											if LinearConstraint.p_is_true constr
											then 
												(
												print_message Verbose_standard (" true, comparable ");
												)
											else
												(
												let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t]) in
												let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
												if LinearConstraint.p_is_false constr
												then 
													(
													print_message Verbose_standard (" false, comparable ");
													if check2 = false 
													then
														(
														DynArray.add clocks_constraints (location_index, clock_cons);
														(* adding := true; *)
														);
													)
												else
													(
													print_message Verbose_standard (" false, not determined ");

													(*submodel info*)
													getInfoCurrentModel submodel;
													let check1 = isContraintConflictsParametersConstraints constr parameters_constraints in
													let check3 = isConstraintContainedInParametersConstraints constr parameters_constraints in

													print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																					^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																					^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

													if check1 = true
													then
														(
														if check2 = false
														then
															(
															print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
															print_message Verbose_standard (" Adding new clocks constraints" );

															(* test *)
															let new_parameters_constraints = DynArray.copy parameters_constraints in

															let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in

															let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in

															let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in

															let conj =  (LinearConstraint.p_intersection [constr1]) in
															let check = isContraintAllConflictsParametersConstraints conj parameters_constraints in

															if not check
															then
																(
																DynArray.add parameters_constraints (false, [constr1]);
																DynArray.add clocks_constraints (location_index, clock_cons);
																(* adding := true; *)
																);
															(* test - end *)

															(* print_message Verbose_standard (" Added constraints: " 
																							^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																							^ "\n at state: " 
																							^ string_of_int location_index ); *)
															);
														)
													else
														(
														if check3 = false
														then
															(
															print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );
															(*Add First parameter constraints in the current submodel*)
															let new_parameters_constraints = DynArray.copy parameters_constraints in
															print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr) 
																							^ " into submodel ((parameters_constraints)) " 
																							^ string_of_int (!count_m)  );
															DynArray.add parameters_constraints (true, [constr]);
															(*Create new submodel with Second constraint*)
															let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
															let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
															let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in

															let checkConflict1 = isContraintConflictsParametersConstraints constr1 new_parameters_constraints in
															(
															match checkConflict1 with
															| true (* false *)  -> print_message Verbose_standard ("\n conflict!!");

															| false (* true *)  -> print_message Verbose_standard ("\n conflict!!");
																		DynArray.add new_parameters_constraints (false, [constr1]);
																		print_message Verbose_standard ("\n Add constraint " 
																					^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																					^ " into submodel (parameters_constraints) " 
																					^ string_of_int ((DynArray.length submodels) +1) );
															);
															if ( (is_parameters_constraints_false new_parameters_constraints) || checkConflict1 )
															then 
																(
																print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
																)
															else
																(
																(* let new_clocks_constraints = Hashtbl.create 0 in
																Hashtbl.add new_clocks_constraints location_index clock_cons;
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints); *)

																(* let new_clocks_constraints = Hashtbl.create 0 in *)
																let new_clocks_constraints = DynArray.copy clocks_constraints in
																(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
																);
															);
														);
													);
												);

											(* reset zone - end *)
											)
										else
											(
											(* none reset zone *)
											(*form parameters relation*)
											let ineq1 = make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t) in
											let ineq2 = ref [] in
											List.iter (fun  (_, op_s1, linear_term_s1) ->
												ineq2 := !ineq2@[(make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1))];
											) tuple_inequalities_s1;

											let cub_cons = make_CUB_constraint ([ineq1]@(!ineq2)) in
											print_message Verbose_standard ("\n CUB constraint: " 
																				^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons));
											if LinearConstraint.p_is_true cub_cons
											then 
												(
												print_message Verbose_standard (" true, comparable ");
												)
											else
												(
												let clock_cons = LinearConstraint.pxd_intersection ([constraint_s0; constraint_t; c_s1]) in
												let check2 = isConstraintContainedInClocksConstraints location_index clock_cons clocks_constraints in
												if LinearConstraint.p_is_false cub_cons
												then 
													(
													print_message Verbose_standard (" false, comparable ");
													if check2 = false 
													then
														(
														DynArray.add clocks_constraints (location_index, clock_cons);
														(* adding := true; *)
														);
													)
												else
													(
													print_message Verbose_standard (" false, not determined ");

													(*get submodel info*)
													getInfoCurrentModel submodel;
													let check1 = isContraintConflictsParametersConstraints cub_cons parameters_constraints in
													let check3 = isConstraintContainedInParametersConstraints cub_cons parameters_constraints in
													print_message Verbose_standard ("\n Check 1 - CUB-Cons conflicted with parameters relation: " ^ string_of_bool check1
																					^ "\n Check 2 - Constraint Contained In Clocks Constraints: " ^ string_of_bool check2
																					^ "\n Check 3 - CUB-Cons Contained In parameters relation:: " ^ string_of_bool check3 );

													if check1 = true
													then
														(
														if check2 = false
														then
															(
															print_message Verbose_standard ("\n Cub constraints conflicted with parameters constraints!!! " );
															print_message Verbose_standard (" Adding new clocks constraints" );

															(* test *)
															let new_parameters_constraints = DynArray.copy parameters_constraints in

															let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
															let linear_term_2 = ref [] in
															List.iter (fun  (_, op_s1, linear_term_s1) ->
																linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
															) tuple_inequalities_s1;

															let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
															let linear_inequality_2 = ref [] in
															List.iter (fun term ->
																linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
															) !linear_term_2;

															let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
															let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

															let conj =  (LinearConstraint.p_intersection [constr1;constr2]) in
															let check = isContraintAllConflictsParametersConstraints conj parameters_constraints in

															if not check
															then
																(
																DynArray.add parameters_constraints (false, [constr1;constr2]);
																DynArray.add clocks_constraints (location_index, clock_cons);
																(* adding := true; *)
																);
															(* test - end *)

															(* print_message Verbose_standard (" Added constraints: " 
																							^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names clock_cons)  
																							^ "\n at state: " 
																							^ string_of_int location_index ); *)
															)
														)
													else
														(

														if check3 = false
														then
															(
															print_message Verbose_standard ("\n cub constraints did not conflict with parameters constraints!!! " );

															(*Add First parameter constraints in the current submodel*)
															let new_parameters_constraints = DynArray.copy parameters_constraints in
															print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_cons) 
																							^ " into submodel ((parameters_constraints)) " 
																							^ string_of_int (!count_m)  );
															DynArray.add parameters_constraints (true, [cub_cons]);


															(*Create new submodel with Second constraint*)
															let linear_term_1 = LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_t in
															let linear_term_2 = ref [] in
															List.iter (fun  (_, op_s1, linear_term_s1) ->
																linear_term_2 := !linear_term_2@[(LinearConstraint.sub_p_linear_terms linear_term_s0 linear_term_s1)];
															) tuple_inequalities_s1;

															let linear_inequality_1 = LinearConstraint.make_p_linear_inequality linear_term_1 LinearConstraint.Op_g in
															let linear_inequality_2 = ref [] in
															List.iter (fun term ->
																linear_inequality_2 := !linear_inequality_2@[(LinearConstraint.make_p_linear_inequality term LinearConstraint.Op_g)];
															) !linear_term_2;

															let constr1 = LinearConstraint.make_p_constraint ([linear_inequality_1]) in
															let constr2 = LinearConstraint.make_p_constraint (!linear_inequality_2) in

															print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );
															print_message Verbose_standard (" Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );

															(* DynArray.add new_parameters_constraints (false, [constr1;constr2]); *)

															let checkConflict1 = isContraintConflictsParametersConstraints constr1 new_parameters_constraints in
															let checkConflict2 = isContraintConflictsParametersConstraints constr2 new_parameters_constraints in
															(
															match (checkConflict1, checkConflict2) with
															| (true , true)  -> print_message Verbose_standard ("\n 1 2 conflict!!");

															| (false, true)  -> print_message Verbose_standard ("\n 2 conflict!!");
																				DynArray.add new_parameters_constraints (false, [constr1]);
																				print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );

															| (true , false) -> print_message Verbose_standard ("\n 1 conflict!!");
																				DynArray.add new_parameters_constraints (false, [constr2]);
																				print_message Verbose_standard (" Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );

															| (false, false) -> print_message Verbose_standard ("\n not conflict!!");
																				DynArray.add new_parameters_constraints (false, [constr1;constr2]);
																				print_message Verbose_standard ("\n Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );
																				print_message Verbose_standard (" Add constraint " 
																							^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2) 
																							^ " into submodel (parameters_constraints) " 
																							^ string_of_int ((DynArray.length submodels) +1) );
															);

															if (is_parameters_constraints_false new_parameters_constraints) || (checkConflict1 && checkConflict2)
															then 
																(
																print_message Verbose_standard ("\n New parameters relations all False!!!, not created new submodel!!");
																)
															else
																(
																(* let new_clocks_constraints = Hashtbl.create 0 in
																Hashtbl.add new_clocks_constraints location_index clock_cons;
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints); *)
																
																(* let new_clocks_constraints = Hashtbl.create 0 in *)
																let new_clocks_constraints = DynArray.copy clocks_constraints in
																(* DynArray.add new_clocks_constraints (location_index, clock_cons); *)
																DynArray.add submodels (Hashtbl.copy states, DynArray.copy transitions, new_clocks_constraints, new_parameters_constraints);
																);
															);
														);
													);
												);
											(* none reset zone *)
											);
											(*Case 4 - end*)
								);
								in ();
							);
					) constraints_s1;
				);
			
			
		print_message Verbose_standard ("\n");
		) model.clocks;  

		(*check 2 - end*)
		print_message Verbose_standard ("\n --------------------2nd check end----------------------- ");

	

		DynArray.add clocks_constraints (location_index, (LinearConstraint.pxd_true_constraint ()));

		(* let loc_clocks_constraints = DynArray.make 0 in *)
		let con = ref (LinearConstraint.pxd_true_constraint ()) in
		for i = 1 to (DynArray.length clocks_constraints - 1) do
			let (loc_index1, cons1) = DynArray.get clocks_constraints (i-1) in
			let (loc_index2, cons2) = DynArray.get clocks_constraints (i) in
			if loc_index1 = loc_index1 
			then
			 	(
			 	let check1 = LinearConstraint.pxd_is_true cons1 in
			 	let check2 = LinearConstraint.pxd_is_true cons2 in 
			 	match (check1, check2) with
			 		| true,  true  -> ()
			 		| true,  false -> (* con := LinearConstraint.pxd_intersection [!con; cons2]  *) ()
			 		| false, true  -> con := LinearConstraint.pxd_intersection [!con; cons1];
			 						  let check = isConstraintContainedInClocksConstraints loc_index1 !con loc_clocks_constraints in
			 						  if check = false
			 						  then
			 						  	(
			 						  	DynArray.add loc_clocks_constraints (loc_index1, !con);
			 						  	(* adding := true; *)
			 						  	);
			 						  DynArray.add loc_clocks_constraints (loc_index1, cons2);
			 						  con := (LinearConstraint.pxd_true_constraint ())
			 		| false, false -> con := LinearConstraint.pxd_intersection [!con; cons1; cons2];
			 	)
			else
			 	(
			 		(*  *)
			 	);
		done;
		DynArray.clear clocks_constraints; 
		DynArray.append (DynArray.copy loc_clocks_constraints) clocks_constraints;


 		(* if !adding = false
		then
			(
			DynArray.clear loc_clocks_constraints; 
			); *)	 

	
		
		(*work here - end*)
		(* transiton count *)
		count_t := !count_t+1;
		();
	) transitions;
	(*stage 1 - end*)


	);
	(* end - while adding loop *)
	done;

	(* model count *)
	print_message Verbose_standard ("\n ----------------Sub-model No: " ^ (string_of_int !count_m) ^ " end-----------------------");
	count_m := !count_m+1;
	();
(* end - for each model *)
done;





(* Delete true constraints *)
let loc_clocks_constraints = DynArray.make 0 in
DynArray.iter (fun (states, transitions, c_constraints, parameters_constraints) ->
for i = 0 to (DynArray.length c_constraints - 1) do
	let (loc_index1, cons1) = DynArray.get c_constraints (i) in
	if (LinearConstraint.pxd_is_true cons1) = false
	then
	 	(
	 		DynArray.add loc_clocks_constraints (loc_index1, cons1);
	 	);
done;
DynArray.clear c_constraints;
DynArray.append loc_clocks_constraints c_constraints;
DynArray.clear loc_clocks_constraints;
) submodels;
(* end - delete true constraint *)




(*models summary*)
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 1)------------------------------- ");
print_message Verbose_standard ("\n Number of models: " ^ (string_of_int (DynArray.length submodels) ) );
let model_count = ref 1 in
DynArray.iter (fun (states, transitions, c_constraints, parameters_constraints) ->

	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^"----------------------------- " );


	print_message Verbose_standard ("\n Showing parameters relations!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_standard (" Is Disjunction: " ^ string_of_bool is_and ^ ": ");
		List.iter (fun cons1 -> 
			print_message Verbose_standard (" 	followed constraints: " 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
		) constraint_list;
	) parameters_constraints;


	print_message Verbose_standard ("\n Showing clocks constraints!!!! ");
	DynArray.iter (fun (l_index, con) -> 
		print_message Verbose_standard ("\n (will be a new state): \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names con) 
										^ " at location: " 
										^ l_index
										); 
	) c_constraints;


	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^" End!------------------------ " );

	model_count := !model_count+1;

) submodels;
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 1) End--------------------------- ");
(*models summary - end*)


(*

(* stage 2 - add states *)
let newSubModels = DynArray.make 0 in
DynArray.iter (fun (ss, ts, c_constraints, p_constraints) ->
	let count = ref 1 in 
	let inx = Hashtbl.create 0 in
	DynArray.iter (fun (loc, cons) ->
		
		let locName = ("cub-l" ^ (string_of_int !count)) in
		Hashtbl.add ss locName cons;
		Hashtbl.add inx loc locName ;
		count :=  !count + 1;

	) c_constraints;
	let init_locs = (Hashtbl.find_all inx !init_loc) in
	DynArray.add newSubModels (ss, ts, c_constraints, p_constraints, inx, init_locs@[!init_loc]);
) submodels;
(* stage 2 - end*)


(*models summary*)
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 2-add states)------------------------------- ");
print_message Verbose_standard ("\n Number of models: " ^ (string_of_int (DynArray.length submodels) ) );
let model_count = ref 1 in
DynArray.iter (fun (states, transitions, c_constraints, p_constraints, index, init_locs) ->

	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^"----------------------------- " );


	print_message Verbose_standard ("\n Showing parameters relations!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_standard (" Is Disjunction: " ^ string_of_bool is_and ^ ": ");
		List.iter (fun cons1 -> 
			print_message Verbose_standard (" 	followed constraints: " 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
		) constraint_list;
	) p_constraints;


	print_message Verbose_standard ("\n Showing clocks constraints!!!! ");
	DynArray.iter (fun (l, c) -> 
		print_message Verbose_standard ("\n State: "^ l 
										^ "(will be a new state) \n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c) ); 
	) c_constraints;


	print_message Verbose_standard ("\n Number of states :"^ string_of_int (Hashtbl.length states) );
	Hashtbl.iter (fun loc cons ->
		print_message Verbose_standard ("\n State: "^ loc 
										^ "\n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) ); 		
	) states;

	print_message Verbose_standard ("\n Index table :" );
	Hashtbl.iter (fun from_loc cub_loc ->
		print_message Verbose_standard ("\n CUB Location " ^ cub_loc ^ " comes from " ^ from_loc );		
	) index;


	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^" End!------------------------ " );

	model_count := !model_count+1;

) newSubModels;
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 2-add states) End--------------------------- ");
(*models summary - end*)



(* third stage - add transitions *)
DynArray.iter (fun (states, transitions, c_constraints, p_constraints, index, init_locs) ->

	DynArray.iter ( fun (location_index, destination_location_index, guard, clock_updates) ->
		let listCubLoc1 = Hashtbl.find_all index location_index in
		let listCubLoc2 = Hashtbl.find_all index destination_location_index in

		List.iter (fun loc1 ->  
			DynArray.add transitions (loc1, destination_location_index, guard, clock_updates);
		) listCubLoc1;

		List.iter (fun loc2 ->  
			DynArray.add transitions (location_index, loc2, guard, clock_updates);
		) listCubLoc2;

		List.iter (fun loc1 -> 
			List.iter (fun loc2 -> 
				DynArray.add transitions (loc1, loc2, guard, clock_updates);
			) listCubLoc2;
		) listCubLoc1;

	) transitions;

) newSubModels;
(* third stage - end *)



(*models summary*)
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 2-add transitions)------------------------------- ");
print_message Verbose_standard ("\n Number of models: " ^ (string_of_int (DynArray.length submodels) ) );
let model_count = ref 1 in
DynArray.iter (fun (states, transitions, c_constraints, p_constraints, index, init_locs) ->

	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^"----------------------------- " );


	print_message Verbose_standard ("\n Showing parameters relations!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_standard (" Is Disjunction: " ^ string_of_bool is_and ^ ": ");
		List.iter (fun cons1 -> 
			print_message Verbose_standard (" 	followed constraints: " 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
		) constraint_list;
	) p_constraints;


	print_message Verbose_standard ("\n Showing clocks constraints!!!! ");
	DynArray.iter (fun (l, c) -> 
		print_message Verbose_standard ("\n State: "^ l 
										^ "(will be a new state) \n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c) ); 
	) c_constraints;


	print_message Verbose_standard ("\n Number of states :"^ string_of_int (Hashtbl.length states) );
	Hashtbl.iter (fun loc cons ->
		print_message Verbose_standard ("\n State: "^ loc 
										^ "\n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) ); 		
	) states;

	print_message Verbose_standard ("\n Index table :" );
	Hashtbl.iter (fun from_loc cub_loc ->
		print_message Verbose_standard ("\n CUB Location " ^ cub_loc ^ " comes from " ^ from_loc );		
	) index;


	print_message Verbose_standard ("\n Number of transitions :"^ string_of_int (DynArray.length transitions) );
	DynArray.iter ( fun (location_index, destination_location_index, guard, clock_updates) ->
		print_message Verbose_standard ("\n" ^ location_index ^ " |-----> " ^ destination_location_index );
	) transitions;	


	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^" End!------------------------ " );

	model_count := !model_count+1;


) newSubModels;
print_message Verbose_standard ("\n ----------------------------Models Summary (stage 2-add transitions) End--------------------------- ");
(*models summary - end*)


 
(* final stage *)
let new_transitions = DynArray.make 0 in
DynArray.iter (fun (states, transitions, c_constraints, p_constraints, index, init_locs) ->

	for i = 1 to (DynArray.length transitions) do
		let (location_index, destination_location_index, guard, clock_updates) = DynArray.get transitions (i-1) in
		let s0_cons = Hashtbl.find states location_index in
		let s1_cons = Hashtbl.find states destination_location_index in

		let (a, b) = cub_check_3 s0_cons guard s1_cons clock_updates in 
		
		if a = true
		then
			(
				DynArray.add new_transitions (DynArray.get transitions (i-1)); 
			)
		else
			( 
				(* if b != []
				then
					(
					let con = LinearConstraint.pxd_of_p_constraint (LinearConstraint.make_p_constraint b) in
					let check = isContraintAllConflictsParametersConstraints2 con p_constraints in 
					if check = false
					then
						( *)
						(* DynArray.add new_transitions (DynArray.get transitions (i-1));  *)
						(* );
						
					); *)
			);
		
	done;

	DynArray.clear transitions;
	DynArray.append new_transitions transitions;
	DynArray.clear new_transitions;

) newSubModels;
(* final stage - end *)


(*models summary*)
print_message Verbose_standard ("\n ----------------------------Models Summary (Final stage-remove problematic transitions)------------------------------- ");
print_message Verbose_standard ("\n Number of models: " ^ (string_of_int (DynArray.length submodels) ) );
let model_count = ref 1 in
DynArray.iter (fun (states, transitions, c_constraints, p_constraints, index, init_locs) ->

	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^"----------------------------- " );


	print_message Verbose_standard ("\n Showing parameters relations!!!! ");
	DynArray.iter ( fun (is_and, constraint_list) ->
		print_message Verbose_standard (" Is Disjunction: " ^ string_of_bool is_and ^ ": ");
		List.iter (fun cons1 -> 
			print_message Verbose_standard (" 	followed constraints: " 
									^ (LinearConstraint.string_of_p_linear_constraint model.variable_names cons1) );
		) constraint_list;
	) p_constraints;


	print_message Verbose_standard ("\n Showing clocks constraints!!!! ");
	DynArray.iter (fun (l, c) -> 
		print_message Verbose_standard ("\n State: "^ l 
										^ "(will be a new state) \n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names c) ); 
	) c_constraints;


	print_message Verbose_standard ("\n Number of states :"^ string_of_int (Hashtbl.length states) );
	Hashtbl.iter (fun loc cons ->
		print_message Verbose_standard ("\n State: "^ loc 
										^ "\n Constraint: \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) ); 		
	) states;

	print_message Verbose_standard ("\n Index table :" );
	Hashtbl.iter (fun from_loc cub_loc ->
		print_message Verbose_standard ("\n CUB Location " ^ cub_loc ^ " comes from " ^ from_loc );		
	) index;


	print_message Verbose_standard ("\n Number of transitions :"^ string_of_int (DynArray.length transitions) );
	DynArray.iter ( fun (location_index, destination_location_index, guard, clock_updates) ->
		print_message Verbose_standard ("\n" ^ location_index ^ " |-----> " ^ destination_location_index );
	) transitions;	


	print_message Verbose_standard ("\n ----------------------Sub Model "^ (string_of_int !model_count) ^" End!------------------------ " );

	model_count := !model_count+1;


) newSubModels;
print_message Verbose_standard ("\n ----------------------------Models Summary (Final stage-remove problematic transitions) End--------------------------- ");
(*models summary - end*)





(* additional stage *)
(* let finalModel = DynArray.make 0 in *)
let i = ref 1 in
let s0 = "cub-init" in
let newstates =  Hashtbl.create 0 in
Hashtbl.add newstates s0 (LinearConstraint.pxd_true_constraint ());

let newtransitions = DynArray.make 0 in
DynArray.iter (fun (states, transitions, _, p_constraints, index, init_locs) ->
	
	Hashtbl.iter (fun location_index cons -> 
		let newloc = (location_index ^ "-m" ^ (string_of_int !i) ) in
		Hashtbl.add newstates newloc cons;
	) states;

	DynArray.iter (fun (location_index, destination_location_index, guard, clock_updates) -> 
		let newloc1 = (location_index ^ "-m" ^ (string_of_int !i) ) in
		let newloc2 = (destination_location_index ^ "-m" ^ (string_of_int !i) ) in
		DynArray.add newtransitions (newloc1, newloc2, guard, clock_updates);
	) transitions;

	let listParaRelations = disjunction_constraints p_constraints in
	List.iter( fun cons ->
		let pxd_cons = LinearConstraint.pxd_of_p_constraint cons in
		if (LinearConstraint.pxd_is_false pxd_cons = false)
		then 
			(
			List.iter (fun loc -> 
				DynArray.add newtransitions (s0, (loc ^ "-m" ^ (string_of_int !i)), pxd_cons, [] ) ;
			) init_locs;
			);
	) listParaRelations;

	(* DynArray.add finalModel (newstates, newtransitions); *)

	i := !i + 1;

) newSubModels;

let finalModel = (newstates, newtransitions) in 
(* additional stage - end *)




(*models summary*)
print_message Verbose_standard ("\n ----------------------------Models Summary Final------------------------------- ");

	print_message Verbose_standard ("\n Number of states :"^ string_of_int (Hashtbl.length newstates) );
	Hashtbl.iter (fun loc cons ->
		print_message Verbose_standard ("\n State: "^ loc 
										^ "\n Constraint(Invariant): \n" 
										^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names cons) ); 		
	) newstates;


	print_message Verbose_standard ("\n Number of transitions :"^ string_of_int (DynArray.length newtransitions) );
	DynArray.iter ( fun (location_index, destination_location_index, guard, clock_updates) ->
		print_message Verbose_standard ("\n" ^ location_index ^ " |-----> " ^ destination_location_index );
	print_message Verbose_standard ("\n Constraint(Guard): \n" 
									^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard) ); 	
	) newtransitions;	


	(* print_message Verbose_standard ("\n Number of states :"^ string_of_int (Hashtbl.length newstates) );
	Hashtbl.iter (fun location_index cons ->
		print_message Verbose_standard ("\n" ^ location_index );
	) newstates;	 *)



print_message Verbose_standard ("\n ----------------------------Models Summary Final End--------------------------- ");
(*models summary - end*)


*)


) model.automata;

terminate_program();

(**************************************************)
(* PART 2 END *)
(**************************************************)


(**************************************************)
(* GIA'S TESTING *)
(**************************************************)




(* Run! *)
let result = algorithm#run() in

(* Process *)
ResultProcessor.process_result result algorithm#algorithm_name None;


(************************************************************)
(* END EXCEPTION MECHANISM *)
(************************************************************)
) with
	e ->(
	let error_message = match e with
		| InternalError msg -> "Fatal internal error: " ^ msg ^ ""
		| Failure msg -> "'Failure' exception: '" ^ msg ^ "'"
		| Invalid_argument msg -> "'Invalid_argument' exception: '" ^ msg ^ "'"
		| SerializationError msg -> "Serialization error: " ^ msg ^ ""
		| Not_found -> "'Not_found' exception!"
		| Random_generator_initialization_exception-> "A fatal error occurred during the random generator initialization."
		| e -> "Fatal exception '" ^ (Printexc.to_string e) ^ "'."
	in
	
	print_error (error_message ^ "\nPlease (politely) insult the developers.");
	Printexc.print_backtrace Pervasives.stderr;
	
	abort_program ();
	(* Safety *)
	exit 1
	
	)
end; (* try *)



(************************************************************)
(* Handling statistics *)
(************************************************************)
global_counter#stop;
print_all_counters();


(************************************************************)
(* Bye bye! *)
(************************************************************)

terminate_program()





(*
let constr = LinearConstraint.make_pxd_constraint (inequalities_s0@inequalities_t@inequalities_s1) in
	print_message Verbose_standard ("\n constr:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr)); 
(*testing ppl function*)
let constr_inter = LinearConstraint.pxd_intersection [constr] in
	print_message Verbose_standard ("\n constr_inter:" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constr_inter)); 
let constr1 = LinearConstraint.make_p_constraint ([ineq1]) in
	print_message Verbose_standard ("\n constr1:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr1)); 
let constr2 = LinearConstraint.make_p_constraint ([ineq2]) in
	print_message Verbose_standard ("\n constr2:" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constr2)); 
let abc = LinearConstraint.pxd_is_leq invariant_s0 guard_t in
	print_message Verbose_standard ("\n abc:" ^ string_of_bool abc ); 
(*testing ppl function*)
*)




(*olde version code, maybe reuse in future*)



(*
let get_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then coeff2 
										else coeff1
										in 
*)


(*
let is_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then false
										else true
										in 
*)


(*
(*
get lower upper-bound
return a tupel (number, linear_term)
number:
1: u1 is smaller than u2
2: u1 is smaller than u2
3: u1 is equal u2
4: u1, u2 are incomparable
*)
let get_lower_upperbound (op1, term1) (op2, term2) =	let (isComparable, linear_term_number ) = LinearConstraint.isComparable_linear_terms term1 term2 in
														(*testing*)
														let ineq = LinearConstraint.isComparable_linear_terms_2 term1 term2 in
														print_message Verbose_standard ("   Testing constraint!!: ");
   														print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names ineq));
   														(*testing*)
														print_message Verbose_standard ("\n isComparable: " ^ string_of_bool isComparable );
														(*check the input, start*)
														match op1, op2 with
														| LinearConstraint.Op_ge, LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g , LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_ge, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g , LinearConstraint.Op_g -> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))

														| LinearConstraint.Op_ge, _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| LinearConstraint.Op_g , _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| _		  				, LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														| _		  				, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														(*check the input, end*)
														| _		  				, _						-> 
																											match  (isComparable, linear_term_number ) with
																											| true 	, LinearConstraint.First -> 	(
																																					match (op1, op2) with
																																					| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (LinearConstraint.Second, op2, term2)
																																					| _						  , _						-> (LinearConstraint.First, op1, term1) 
																																					);

																											| true 	, LinearConstraint.Second -> 	(
																																					match (op2, op1) with
																																					| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (LinearConstraint.First, op1, term1)
																																					| _						  , _						-> (LinearConstraint.Second, op2, term2) 
																																					);

																											| true 	, _ 							-> 	raise (InternalError("error!!! "))

																											| false	, _ 							-> 	(LinearConstraint.NotDetermine, op1, term1);
														in
*)


(*
let cub_check invariant_s0 guard_t invariant_s1 clock_updates = 	
																(*ppl*)
																(* let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in *)
																let inequalities = ref [] in
																print_message Verbose_standard (" CUB check, Start:");
																print_message Verbose_standard ("\n");

																let inequalities_s0 = LinearConstraint.pxd_get_inequalities invariant_s0 in
																let inequalities_t 	= LinearConstraint.pxd_get_inequalities guard_t in
																let inequalities_s1 = LinearConstraint.pxd_get_inequalities invariant_s1 in

																(*testing*)
																(*
																let ineq = inequalities_s0@inequalities_t@inequalities_s1
																let constraint_for_cub = LinearConstraint.make_pxd_constraint (*p_linear_inequality_list*) ineq in 
   																print_message Verbose_standard ("   inequalities sdfdsf!!: ");
   																print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names constraint_for_cub));
   																*)
   																(*testing*)

																print_message Verbose_standard (" **Beginning state/location** :");
																let tuple_inequalities_s0 	= convert_inequality_list_2_tuple_list inequalities_s0 in
																print_message Verbose_standard (" **Transition** :");
																let tuple_inequalities_t 	= convert_inequality_list_2_tuple_list inequalities_t in
																print_message Verbose_standard (" **Destination state/location** :");
																let tuple_inequalities_s1 	= convert_inequality_list_2_tuple_list inequalities_s1 in
																
																let isCUB_PTA = ref true in

																 List.iter (	fun clock_index -> 
																 	let inequalities_need_to_solve = ref [] in
																 	print_message Verbose_standard ("   Checking CUB condtions at clock (" ^ (model.variable_names clock_index) ^ "):");

																 	print_message Verbose_standard ("\n 	**Beginning state/location** :");
	             													let (_, op_s0, linear_term_s0) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s0 in
	             													print_message Verbose_standard ("\n 	**Transition** :");
                													let (_, op_t, linear_term_t) 	= filter_upperbound_by_clock clock_index tuple_inequalities_t in
                													print_message Verbose_standard ("\n 	**Destination state/location** :");
                													let (_, op_s1, linear_term_s1) 	= filter_upperbound_by_clock clock_index tuple_inequalities_s1 in



                													print_message Verbose_standard ("\n");
                													print_message Verbose_standard ("Comparing: ");

                													let s0_upperbound_str = (LinearConstraint.operator2string op_s0) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s0) in
                													let t_upperbound_str  = (LinearConstraint.operator2string op_t) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_t) in
                													let s1_upperbound_str = (LinearConstraint.operator2string op_s1) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term_s1) in

                													print_message Verbose_standard (" 	 get upper-bound s0: " ^ s0_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound t: " ^ t_upperbound_str );
                													print_message Verbose_standard (" 	 get upper-bound s1: " ^ s1_upperbound_str );
                													
                													print_message Verbose_standard (" 	 evaluating: (" ^ s0_upperbound_str ^ ") <= (" ^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!");

                													(* if List.mem clock_index reset_clocks = true 
                													then lower_inequality := linear_term_t; *)
                
                													(* let result = ref true in *)
                													let result = match (op_s0, linear_term_s0), (op_t, linear_term_t), (op_s1, linear_term_s1) with

														 			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 1 " );
														 			 																							true;

														 			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	print_message Verbose_standard (" 	 Case 2 " );
														 																										false;

														 			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
														 																										print_message Verbose_standard (" 	 Case 3 " );
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																									
														 																										else
														 																											(
														 																											false;
														 																											);

														 			|(LinearConstraint.Op_ge, _), _							 , _							-> 	(*reset but useless*)
														 																										false; 

 																	|_							, (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	true;

 																	|_							, _							 , (LinearConstraint.Op_ge, _)	->	let (typ, op, term) = get_lower_upperbound (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																										(
														 																										match typ with
														 																										| LinearConstraint.NotDetermine -> 	(
														 																																			inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																			false
														 																																			);
														 																										| LinearConstraint.First 		-> 	true
														 																										| LinearConstraint.Second 		-> 	false;
														 																										);

 																	|_							, (LinearConstraint.Op_ge, _), _							->	(*reset*)
 																																								if List.mem clock_index clock_updates = true
														 																										then
														 																											let _ = print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ) 
														 																											in
														 																											true
														 																										else
														 																											(
 																																									let (typ, op, term) = get_lower_upperbound (op_s0, linear_term_s0)  (op_s1, linear_term_s1) in
														 																											(*let _ = get_intersec_upperbounds (op_s0, linear_term_s0)  (op_s1, linear_term_s1) in*)
														 																											match typ with
														 																											| LinearConstraint.NotDetermine -> 	(
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First 		-> 	true
														 																											| LinearConstraint.Second 		-> 	false;
														 																											);
														 																											

														 			| _							, _							 , _							-> 	(*reset*)
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																											(*let _ = get_intersec_upperbounds (op_s0, linear_term_s0) (op_t, linear_term_t) in*)
														 																											print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
														 																											(
														 																											match typ2 with
														 																											| LinearConstraint.NotDetermine -> 	(
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First 		-> 	true
														 																											| LinearConstraint.Second 		-> 	false;
														 																											);
														 																										else
														 																											(
														 																											let (typ1, op1, term1) = get_lower_upperbound (op_t, linear_term_t)  (op_s1, linear_term_s1) in 
														 																											(*let _ = get_intersec_upperbounds (op_t, linear_term_t)  (op_s1, linear_term_s1) in*)
														 																											
														 																											match typ1 with
														 																											| LinearConstraint.NotDetermine -> 	(

														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_t, linear_term_t)] ; 
														 																																				inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op_s1, linear_term_s1)] ; 
														 																																				false
														 																																				);
														 																											| LinearConstraint.First -> (
														 																																		let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op1, term1) in
														 																																		match typ2 with
														 																																		| LinearConstraint.NotDetermine -> 	(
														 																																											inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op1, term1)] ; 
														 																																											false
														 																																											);
														 																																		| LinearConstraint.First 		-> 	true
														 																																		| LinearConstraint.Second 		-> 	false
														 																																		);
														 																											| LinearConstraint.Second -> 	(
														 																																			let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op1, term1) in
														 																																			match typ2 with
														 																																			| LinearConstraint.NotDetermine -> 	(
														 																																												inequalities_need_to_solve := !inequalities_need_to_solve@[make_CUB_inequality (op_s0, linear_term_s0) (op1, term1)] ; 
														 																																												false
														 																																												);
														 																																			| LinearConstraint.First 		-> 	true
														 																																			| LinearConstraint.Second 		-> 	false
														 																																			);
														 																											
														 																											);

														 																										
														 																									

														 																										




														 			in

														 			if (result = false && !inequalities_need_to_solve = [])
														 			then 	(
														 					isCUB_PTA := false;
														 					raise (InternalError("   The model is impossible CUB-PTA! "));
														 					);
														 			
														 			inequalities := !inequalities@(!inequalities_need_to_solve);

														 			if (result = false)
														 			then
			
														 				print_message Verbose_standard (" This is not satisfied CUB-PTA! ")
														 			else 
														 				print_message Verbose_standard (" This is satisfied CUB-PTA! ");



                												print_message Verbose_standard ("\n");
                												) model.clocks; 

																print_message Verbose_standard ("\n");
																print_message Verbose_standard (" CUB check, End!");
																print_message Verbose_standard ("\n");

																(!isCUB_PTA, !inequalities);
																in
*)


(*for testing*)
(*



let valuate1 var = NumConst.one in

let abc = LinearConstraint.evaluate_p_linear_term valuate1 linear_term_s0 in 

print_message Verbose_standard ( NumConst.string_of_numconst abc );

(*test included constraint*)

if LinearConstraint.pxd_is_leq invariant_s0 guard_t 
then print_message Verbose_standard ("s0 is included in t")
else  print_message Verbose_standard ("s0 is not included in t");

*)