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

(*attention: multi inequalities of the same clock in model*)

(*let check_cub_condition *)

(*check CUB condition function -> return boolean value*)


let get_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then coeff2 
										else coeff1
										in 


let is_smaller_coeff coeff1 coeff2 =	let first_minus_second = Gmp.Z.compare (NumConst.get_num coeff1) (NumConst.get_num coeff2) in
										if first_minus_second >= 0 
										then false
										else true
										in 


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
														print_message Verbose_standard ("\n isComparable: " ^ string_of_bool isComparable );
														(*check the input, start*)
														match op1, op2 with
														| LinearConstraint.Op_ge, LinearConstraint.Op_ge-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g, LinearConstraint.Op_ge	-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_ge, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))
														| LinearConstraint.Op_g, LinearConstraint.Op_g 	-> raise (InternalError("Detected lower bounded contraints in both inputs!!! "))

														| LinearConstraint.Op_ge, _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| LinearConstraint.Op_g, _						-> raise (InternalError("Detected lower bounded contraints in the first input!!! "))
														| _		  			, LinearConstraint.Op_ge	-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														| _		  				, LinearConstraint.Op_g	-> raise (InternalError("Detected lower bounded contraints in the second input!!! "))
														(*check the input, end*)
														| _		  					, _					-> 
																											match  (isComparable, linear_term_number ) with
																											| true, 1 -> (match (op1, op2) with
																															| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (2, op2, term2)
																															| _						  , _						-> (1, op1, term1) );
																											| true, 2 -> (match (op2, op1) with
																															| (LinearConstraint.Op_le), (LinearConstraint.Op_l)	-> (1, op1, term1)
																															| _						  , _						-> (2, op2, term2) );
																											| false, _ -> (0, op1, term1);
														in



(* 
Simple funtion to covert from list of inequalities to list of tuple (clock; operator; linear expression) 
Note that: if there are True constraints, it will return back a list of clock greater than Zero
This only uses to indicate clock smaller than INFINITE, not for lower-bound
*)
let convert_inequality_list_2_tuple_list inequalities =	let list_s0 = ref [] in 
														match inequalities with 
														(*True constraints -> list of clocks >= 0*)
														 [] ->  List.iter (	fun clock_index -> 
														 		list_s0 := !list_s0@[(clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero)] 
																) model.clocks; 
														 		!list_s0

														| _ ->	print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) Start:");
																List.iter (fun inequality -> (
																	let (clock_index_2, operator, parametric_linear_term) = LinearConstraint.clock_guard_of_linear_inequality inequality in
                													list_s0 := !list_s0@[(clock_index_2, operator, parametric_linear_term)]; 
                													print_message Verbose_standard (" inequality: " ^ (model.variable_names clock_index_2) 
																									^ " " ^ (LinearConstraint.operator2string operator) 
																									^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
																									^ " added!");
                														) 
                													) inequalities; 
                												print_message Verbose_standard (" Covert inequalities -> list(clock; operator; linear expression) End!");
                												print_message Verbose_standard ("\n");
                												!list_s0
														in




let get_clock_inequalities clock_index converted_inequalities =	let list_s0 = ref [] in
                											List.iter (	fun (clock_index_2, operator, parametric_linear_term) -> 
                															if ( clock_index == clock_index_2 )
                															then 
                															list_s0 := !list_s0@[(clock_index_2, operator, parametric_linear_term)] 
                														) converted_inequalities; 
															!list_s0
															in 



(*attention:

*)
let filter_upperbound_by_clock clock_index tuple_inequalities_s0 =	
																print_message Verbose_standard (" 	filtering upper-bound of clock (" ^ (model.variable_names clock_index) ^ ") in this list of tuple inequalities:");
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
                													then (
                													print_message Verbose_standard ("	Upper-bounded found: " ^ (model.variable_names clock_index_2) 
																									^ " " ^ (LinearConstraint.operator2string operator) 
																									^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names parametric_linear_term) 
																									^ " !");
                													index := i; 
                													count := (!count + 1);
																	if !count > 1
																	then  raise (InternalError("Detected more than 1 different upperbounds of the same clock in same constraint!!! "))

                													);
																done;
																(*just for printing, start*)
																if !count = 0
																then print_message Verbose_standard ("	Upper-bound not found:!, return clock  " ^ (model.variable_names clock_index) ^">= 0" );
																(*just for printing, end*)
																(* if there is no upper-bound -> reutrn (clock >= 0) *)
																if !count = 0
																then
																	( clock_index, LinearConstraint.Op_ge, LinearConstraint.make_p_linear_term [] NumConst.zero ) (* make_linear_term [] NumConst.zero) *)
																else
																	(* result *)
																	List.nth tuple_inequalities_s0 !index
															in 
																	




let cub_check invariant_s0 guard_t invariant_s1 clock_updates = 	
																(*ppl*)
																let inequalities_need_to_solve : (LinearConstraint.op * LinearConstraint.p_linear_term) list ref = ref [] in
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

														 			 (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _)	->	true;

														 			|(LinearConstraint.Op_ge, _), _							 , (LinearConstraint.Op_ge, _)	->	false;

														 			|(LinearConstraint.Op_ge, _), (LinearConstraint.Op_ge, _), _							->	(*reset*)
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
														 																										if typ = 1
														 																										then true
														 																										else false;

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
														 																											(*if typ = 1
														 																												then true
														 																												else false;*)
														 																											(
														 																											match typ with
														 																											| 0 -> (inequalities_need_to_solve := !inequalities_need_to_solve@[(op_s0, linear_term_s0); (op_s1, linear_term_s1)] ; 
														 																													false);
														 																											| 1 -> true
														 																											| 2 -> false;
														 																											);
														 																											);

														 			| _							, _							 , _							-> 	(*reset*)
														 																										if List.mem clock_index clock_updates = true
														 																										then
														 																											let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op_t, linear_term_t) in
														 																											print_message Verbose_standard (" 	 Detected " 
														 																																			^ (model.variable_names clock_index) 
														 																																			^ " was a reset clock!\n 	 skipping the process: (" 
														 																																			^ t_upperbound_str ^ ") /\\ (" ^ s1_upperbound_str ^ ")!" ); 
														 																											(
														 																											match typ2 with
														 																											| 0 -> (inequalities_need_to_solve := !inequalities_need_to_solve@[(op_s0, linear_term_s0); (op_t, linear_term_t)] ; 
														 																														false);
														 																											| 1 -> true
														 																											| 2 -> false;
														 																											);
														 																										else
														 																											(
														 																											let (typ1, op1, term1) = get_lower_upperbound (op_t, linear_term_t)  (op_s1, linear_term_s1) in 
														 																											(
														 																											match typ1 with
														 																											| 0 -> (inequalities_need_to_solve := !inequalities_need_to_solve@[(op_t, linear_term_t); (op_s1, linear_term_s1)] ; 
														 																														false);
														 																											| 1 -> true
														 																											| 2 -> false;
														 																											);

														 																											if typ1 != 2
														 																											then 
														 																												let (typ2, op2, term2) = get_lower_upperbound (op_s0, linear_term_s0) (op1, term1) in
														 																												match typ2 with
														 																												| 0 -> (inequalities_need_to_solve := !inequalities_need_to_solve@[(op_s0, linear_term_s0); (op1, term1)] ; 
														 																															false);
														 																												| 1 -> true
														 																												| 2 -> false
														 																											else
														 																												false
														 																											);

														 																										
														 																									

														 																										




														 			in

														 			if (result = false)
														 			then isCUB_PTA := false;

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

																(!isCUB_PTA, !inequalities_need_to_solve);
																in





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
                    		
                	

                		(*let inv1_inequalities = LinearConstraint.pxd_get_inequalities invariant1 in*)
						

						print_message Verbose_standard ("\n");

                	(*Checking bounded clocked in guards (Transition)*)
                	List.iter (fun action_index -> print_message Verbose_standard (" Transition/Action: " ^ (model.action_names action_index) );
            
                    	List.iter (fun (guard, clock_updates, _, destination_location_index) 
                    		-> print_message Verbose_standard ("   Guard: " ^ (LinearConstraint.string_of_pxd_linear_constraint model.variable_names guard));
					
                        
                    		(* List.iter (fun clock_index -> print_message Verbose_standard ("     Checking clock " ^ (model.variable_names clock_index));	
                            	if LinearConstraint.pxd_is_constrained guard clock_index then (print_message Verbose_standard "      This clock is bound!!");
                        	) model.clocks; *)

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

                			(*let result = ref true in*)


                			let (result, inequalities) = cub_check invariant1 guard invariant2 clock_updates in

                			if !isCUB_PTA = false && !inequalities_need_to_solve = []
                			then raise (InternalError("   The model is impossible CUB-PTA! "));
                			
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



if !isCUB_PTA = true 
then
    print_message Verbose_standard ("   The model is CUB-PTA! ")
else 
	(
	(*if (!inequalities_need_to_solve = [])
	then 
    	print_message Verbose_standard ("   The model is impossible CUB-PTA! ") 
    else*)
    
	(* Convert the inequalities to LinearConstraint.linear_inequality *)
	let p_linear_inequality_list = List.map (fun (op, linear_term) -> 
		LinearConstraint.make_p_linear_inequality linear_term (LinearConstraint.reverse_op op)
	) !inequalities_need_to_solve in
	
	(* Create the parametric constraint *)
	let constraint_for_cub = LinearConstraint.make_p_constraint p_linear_inequality_list in
	
	(* Print some information *)
   	print_message Verbose_standard ("   The model is possible CUB-PTA! \nbut you need to solve the inequalities below!!: ");
   	print_message Verbose_standard ("\n" ^ (LinearConstraint.string_of_p_linear_constraint model.variable_names constraint_for_cub));
   	
(*    	List.iter (fun (op, linear_term) -> 
    		print_message Verbose_standard ( (LinearConstraint.operator2string op) ^ " " ^ (LinearConstraint.string_of_p_linear_term model.variable_names linear_term) );
    	) !inequalities_need_to_solve*)
    );



terminate_program();







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
