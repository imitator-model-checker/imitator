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
 * Last modified     : 2016/03/10
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



(**************************************************

TAGS USED THROUGHOUT THIS PROJECT
- (*** BADPROG ***)
- (*** NOTE ***)
- (*** OPTIMIZED ***)
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
(* BEGIN EXCEPTION MECHANISM *)
(************************************************************)
begin
try(


(************************************************************)
(* Get the arguments *)
(************************************************************)
(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;


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
let model, pi0, v0 = ParsingUtility.compile options in

Input.set_model model;
Input.set_pi0 pi0;
Input.set_v0 v0;


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
	| EF_synthesis ->
		let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFsynth.algoEFsynth in myalgo
	
	
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

		(*** TODO: PRP ***)
		
		| Distributed_ms_sequential ->
			(* Branch between master and worker *)
			if DistributedUtilities.get_rank() = DistributedUtilities.masterrank then
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoBCCoverDistributedMSSeqMaster.algoBCCoverDistributedMSSeqMaster in myalgo
			else
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoBCCoverDistributedMSSeqWorker.algoBCCoverDistributedMSSeqWorker in myalgo
		| Distributed_ms_random _
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
	(* PRPC *)
	(*** TODO: use two different modes for PRPC and BC ***)
	| Cover_cartography when options#efim ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoPRPC.algoPRPC in myalgo

	(* BC with full coverage *)
	| Cover_cartography ->
			let myalgo :> AlgoGeneric.algoGeneric = new AlgoBCCover.algoBCCover in myalgo
	
	| Border_cartography ->
(* 			Cartography.cover_behavioral_cartography model *)
		raise (InternalError("Not implemented !!!"))
		
	(* BC with random coverage *)
	| Random_cartography nb ->
		let algo_bcrandom = new AlgoBCRandom.algoBCRandom in
		(*** NOTE: very important: must set NOW the maximum number of tries! ***)
		algo_bcrandom#set_max_tries nb;
		let myalgo :> AlgoGeneric.algoGeneric = algo_bcrandom in
	
(* 		let myalgo :> AlgoGeneric.algoGeneric = new AlgoBCRandom.algoBCRandom in *)
		(* Return the algo *)
		myalgo
		
	
		
	(************************************************************)
	(* Translation has been handled already *)
	(************************************************************)

	| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");
in

(* Run! *)
let result = algorithm#run() in

(* Process *)
ResultProcessor.process_result result algorithm#algorithm_name None;

		

(************************************************************)
(* END EXCEPTION MECHANISM *)
(************************************************************)
) with
(*** TODO: factorize a bit ***)
	| InternalError msg -> (
		print_error ("Fatal internal error: " ^ msg ^ "\nPlease (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Failure msg -> (
		print_error ("'Failure' exception: '" ^ msg ^ "'\nPlease (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Invalid_argument msg -> (
		print_error ("'Invalid_argument' exception: '" ^ msg ^ "'\nPlease (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| SerializationError msg -> (
		print_error ("Serialization error: " ^ msg ^ "\nPlease (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| Not_found -> (
		print_error ("'Not_found' exception!\nPlease (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
	| _ -> (
		print_error ("An unknown exception occurred. Please (politely) insult the developers.");
		abort_program ();
		(* Safety *)
		exit 1
	);
end; (* try *)



(************************************************************)
(* Bye bye! *)
(************************************************************)

terminate_program()
