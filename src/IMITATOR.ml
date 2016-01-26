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
 * Last modified     : 2016/01/26
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
open Reachability


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
if verbose_mode_greater Verbose_total then
	print_message Verbose_total ("\nModel:\n" ^ (ModelPrinter.string_of_model model) ^ "\n");


(************************************************************)
(* Debug print: property *)
(************************************************************)
if verbose_mode_greater Verbose_low then
	print_message Verbose_low ("\nProperty:\n" ^ (ModelPrinter.string_of_property model model.user_property) ^ "\n");


(************************************************************)
(* Case distributed *)
(************************************************************)
(*** WARNING:  Do not modify the following lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)
(*(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)
begin
match options#distribution_mode with
	(* Fork if distributed *)
	| Non_distributed -> ()
	| _ -> (RunPaTATOR.run(); exit(0))
end;
(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)*)
(*** WARNING:  Do not modify the previous lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)



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
	let gml_file = options#files_prefix ^ ".grml" in
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file gml_file translated_model;
	terminate_program()
);

(* Translation to JPG *)
if options#pta2hytech then(
	print_message Verbose_standard ("Translating model to a HyTech input model.");
	let translated_model = PTA2HyTech.string_of_model model in
	let hytech_file = options#files_prefix ^ ".hy" in
	if verbose_mode_greater Verbose_total then(
		print_message Verbose_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file hytech_file translated_model;
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
	terminate_program()
);
(* Direct cartography output *)
if options#cartonly then(
	print_message Verbose_standard ("Direct output of a cartography (no analysis will be run).");
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
	(* The end *)
	terminate_program()
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
(* Execute IMITATOR *)
(************************************************************)

begin
	match options#imitator_mode with
		| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");

		
		(* Exploration *)
		| State_space_exploration
			->
			(*** WARNING: work in progress here ***)
			let algo = new AlgoPostStar.algoPostStar in
			let result = algo#run() in
			ResultProcessor.process_result result;
			(*** WARNING: work in progress here ***)
			
(* 			Reachability.full_state_space_exploration model; *)
			
		(* Synthesis *)
		| EF_synthesis 
			->
			(*** WARNING: work in progress here ***)
			let algo = new AlgoEFsynth.algoEFsynth in
			let result = algo#run() in
			ResultProcessor.process_result result;
			(*** WARNING: work in progress here ***)

(* 			Reachability.ef_synthesis model *)

			
		(* Inverse Method *)
		| Inverse_method ->
			(*** HACK to call the good class ***)
			(*** TODO: rewrite this part in a more generic manner ***)
			if options#pi_compatible then
				(*** WARNING: work in progress here ***)
				let algo = new AlgoIMK.algoIMK in
				let result = algo#run() in
				ResultProcessor.process_result result;
				(*** WARNING: work in progress here ***)
			else
			if options#efim then
				(
(*					(*** WARNING!!! Why a dedicated function here, whereas for BC+EFIM this function is not (?) called? ***)
				Reachability.efim model;*)
				(*** WARNING: work in progress here ***)
				let algo = new AlgoPRP.algoPRP in
				let result = algo#run() in
				ResultProcessor.process_result result;
				(*** WARNING: work in progress here ***)
				)
			else
			if options#union then
				(
(* 				Reachability.inverse_method model; *)
				(*** WARNING: work in progress here ***)
				let algo = new AlgoIMunion.algoIMunion in
				let result = algo#run() in
				ResultProcessor.process_result result;
				(*** WARNING: work in progress here ***)
				)
				else(
				(* Classical IM *)
				(*** WARNING: work in progress here ***)
				let algo = new AlgoIM.algoIM in
				let result = algo#run() in
				ResultProcessor.process_result result;
				(*** WARNING: work in progress here ***)
			)


		| Cover_cartography ->
			(*** WARNING: work in progress here ***)
			let algo = new AlgoBCCover.algoBCCover in
			let result = algo#run() in
			ResultProcessor.process_result result;
			(*** WARNING: work in progress here ***)
		
		
		| (*Cover_cartography | *)Border_cartography ->
		(* Behavioral cartography algorithm with full coverage *)
			Cartography.cover_behavioral_cartography model
			
			
		| Random_cartography nb ->
		(* Behavioral cartography algorithm with random iterations *)
			Cartography.random_behavioral_cartography model nb;

end;


(************************************************************)
(* END EXCEPTION MECHANISM *)
(************************************************************)
) with
(*** TODO: factorize a bit ***)
	| InternalError e -> (
		print_error ("Fatal internal error: " ^ e ^ "\nPlease (politely) insult the developers.");
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
