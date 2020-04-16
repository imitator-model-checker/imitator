(************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Main file for IMITATOR
 *
 * File contributors : Ulrich Kühne, Étienne André, Laure Petrucci
 * Created           : 2009/09/07
 * Last modified     : 2020/04/16
 *
 ************************************************************)



(************************************************************)
(* Internal modules *)
(************************************************************)
open Exceptions
open OCamlUtilities

open ImitatorUtilities
open AbstractModel
open AbstractAlgorithm
open AbstractProperty
open Result
open Options
open Statistics


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


(************************************************************)
(* Start the global counters *)
(************************************************************)
(* Counter counting everything from beginning to the end *)
let global_counter = create_time_counter_and_register "total" Global_counter Verbose_standard in
global_counter#start;

(* Counter counting everything except the final processing (graphics, external files generation, etc.) *)
let counter_algorithm_and_parsing = create_time_counter_and_register "main algorithm + parsing" Algorithm_counter Verbose_standard in
counter_algorithm_and_parsing#start;

(* Counter counting everything except the final processing (graphics, external files generation, etc.) and the parsing *)
let counter_main_algorithm = create_time_counter_and_register "main algorithm" Algorithm_counter Verbose_standard in



(************************************************************)
(* BEGIN EXCEPTION MECHANISM *)
(************************************************************)

begin
try(


(************************************************************)
(* Get the arguments *)
(************************************************************)

(* let options_parsing_counter = create_time_counter_and_register "options parsing" Parsing_counter Verbose_low in
options_parsing_counter#start;*)

(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;

(*** BUG: verbose mode has been set before the start and the stop; this particular counter may never stop! ***)
(* options_parsing_counter#stop; *)


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
print_message Verbose_experiments ("Analysis time: " ^ (now()) ^ "\n");

(* Recall the arguments *)
options#recall();


(************************************************************)
(* Get input *)
(************************************************************)
let parsing_counter = create_time_counter_and_register "model parsing and converting" Parsing_counter Verbose_standard in
parsing_counter#start;



(*------------------------------------------------------------*)
(* Parse the model and the property *)
(*------------------------------------------------------------*)

(*** TODO !!! 
(* Should we add a special clock reset at each transition? *)
let with_special_reset_clock =
match options#imitator_mode with
	| Parametric_NZ_CUB | Parametric_NZ_CUBcheck | Parametric_NZ_CUBtransform | Parametric_NZ_CUBtransformDistributed -> true
	| _ -> false
in

***)

let model, property_option = ParsingUtility.compile_model_and_property options in

(*------------------------------------------------------------*)
(* Set both abstract structures *)
(*------------------------------------------------------------*)

print_message Verbose_low "Set the model.";
Input.set_model model;

begin
match property_option with
	| None ->
		print_message Verbose_low "No property to set.";
		()
	| Some property ->
		print_message Verbose_low "Set the property.";
		Input.set_property property;
end;



(*(*------------------------------------------------------------*)
(* Parse the additional file (pi0 or v0) *)
(*------------------------------------------------------------*)
if is_mode_IM options#imitator_mode then(
	let pi0 = ParsingUtility.compile_pi0 options in
	Input.set_pi0 pi0;
);

if is_mode_cartography options#imitator_mode then(
	let v0 = ParsingUtility.compile_v0 options in
	Input.set_v0 v0;
);*)

(* End of parsing *)
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
if AbstractAlgorithm.property_needed options#imitator_mode then(
	let property = Input.get_property() in
	
	(*** TODO ***)
	print_message Verbose_low ("\nThe property is the following one:\n" ^ (ModelPrinter.string_of_abstract_property model property) ^ "\n");
);

(* Statistics *)
counter_main_algorithm#start;


(************************************************************)
(************************************************************)
(* Start branching depending on the algorithm *)
(************************************************************)
(************************************************************)

begin
match options#imitator_mode with
	| Syntax_check ->

	(************************************************************)
	(* Case no analysis *)
	(************************************************************)
	(* Generate directly the "empty" result for syntax check *)
	ResultProcessor.process_result Syntax_check_result "syntax check" None;
	
	(* If arrived here, syntax is correct *)
	print_message Verbose_standard "Syntax is correct. Have fun!";
	
	terminate_program()


	(************************************************************)
	(* Case translation *)
	(************************************************************)
	(* Translation to text language (IMITATOR, other model checker, TikZ) *)
	| Translation IMI | Translation HyTech | Translation TikZ | Translation Uppaal ->
	
		(*** NOTE: not super nice… ***)
		let printer = match options#imitator_mode with
			| Translation IMI		-> ModelPrinter.string_of_model
			| Translation HyTech	-> PTA2HyTech.string_of_model
			| Translation TikZ		-> PTA2TikZ.tikz_string_of_model
			| Translation Uppaal	-> PTA2Uppaal.string_of_model
			| _						-> raise (InternalError ("Impossible situation: No target for translation was found, although it should have been"))
		in
	
		(*** NOTE: not super nice… ***)
		let suffix = match options#imitator_mode with
			| Translation IMI		-> "-regenerated" ^ Constants.model_extension
			| Translation HyTech	-> ".hy"
			| Translation TikZ		-> ".tex"
			| Translation Uppaal	->  "-uppaal.xml"
			| _						-> raise (InternalError ("Impossible situation: No target for translation was found, although it should have been"))
		in
	
		print_message Verbose_standard ("Regenerating the input model to a new model.");
		let translated_model = printer model in
		let target_language_file = options#files_prefix ^ suffix in
		if verbose_mode_greater Verbose_total then(
			print_message Verbose_total ("\n" ^ translated_model ^ "\n");
		);
		(* Write *)
		write_to_file target_language_file translated_model;
		print_message Verbose_standard ("File '" ^ target_language_file ^ "' successfully created.");
		
		(* Create a file with some statistics on the origina model if requested *)
		ResultProcessor.process_result Translation_result ("translation to " ^ (AbstractAlgorithm.string_of_translation
			(match options#imitator_mode with Translation translation -> translation | _ -> raise (InternalError ("Impossible situation: No target for translation was found, although it should have been"))
			)) ) None;

		terminate_program()

	(* Translation to a graphics *)
	| Translation JPG | Translation PDF | Translation PNG ->
		print_message Verbose_standard ("Translating model to a graphics.");
		let translated_model = PTA2JPG.string_of_model model in
		if verbose_mode_greater Verbose_high then(
			print_message Verbose_high ("\n" ^ translated_model ^ "\n");
		);
		(*** NOTE: not so nice… ***)
		let extension = match options#imitator_mode with
			| Translation JPG	-> "jpg"
			| Translation PDF	-> "pdf"
			| Translation PNG	-> "png"
			| _					-> raise (InternalError ("Impossible situation: No graphic extension found although JPG/PDF/PNG was expected"))
		in
		Graphics.dot extension (options#files_prefix ^ "-pta") translated_model;
		print_message Verbose_standard ("File successfully created."); (*** TODO: add file name in a proper manner ***)
		
		(* Create a file with some statistics on the origina model if requested *)
		ResultProcessor.process_result Translation_result "translation to graphics" None;

		terminate_program()

	(************************************************************)
	(* Computation of the whole state space *)
	(************************************************************)
	| State_space_computation ->
	
		(*** NOTE: this is static subclass coercition; see https://ocaml.org/learn/tutorials/objects.html ***)
		let concrete_algorithm :> AlgoGeneric.algoGeneric = new AlgoPostStar.algoPostStar in
		
		(*** NOTE: duplicate code with what follows ***)
		
		let result = concrete_algorithm#run() in

		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		(* Process *)
		ResultProcessor.process_result result concrete_algorithm#algorithm_name None;
	
		()

	(************************************************************)
	(* Some algorithm *)
	(************************************************************)
	(** Synthesis algorithm *)
	| Algorithm ->
	begin
		(* Retrieve the algorithm *)
		(*** NOTE: at this stage, we are sure to have defined a property ***)
		let abstract_property = Input.get_property() in
		
		let emptiness_only =
			match abstract_property.synthesis_type with
			| Witness   -> true
			| Synthesis -> false
		in
		
(*		(* Determine the right algorithm depending on the property *)
		let abstract_algorithm = match abstract_property.property with
			(* Reachability *)
			| EF state_predicate -> EFsynth state_predicate
			(* Safety *)
			| AGnot state_predicate -> EFsynth state_predicate
			
			| _ -> raise (NotImplemented ("algorithm_of_property"))
		in*)



(*** TODO: check if needed in the new version (probably not) ***)
		(*(************************************************************)
		(* Preliminary checks *)
		(************************************************************)

		if options#imitator_mode = EF_synthesis || options#imitator_mode = EFunsafe_synthesis || options#imitator_mode = EF_min  || options#imitator_mode = EF_max || options#imitator_mode = EF_synth_min  || options#imitator_mode = EF_synth_max || options#imitator_mode = EF_synth_min_priority_queue || options#imitator_mode = AF_synthesis then(
			match model.correctness_condition with
				(* Synthesis only works w.r.t. (un)reachability *)
				| Some (Unreachable _) -> ()
				| _ -> print_error ("Parametric reachability algorithms can only be run if an unreachability property is defined in the model.");
					abort_program();
		);

		if options#imitator_mode = EF_min then(
			match model.optimized_parameter with
				| Minimize _ -> ()
				| _ ->
					print_error ("A minimized parameter must be defined in the model to run EFmin.");
					abort_program();
		);

		if options#imitator_mode = EF_max then(
			match model.optimized_parameter with
				| Maximize _ -> ()
				| _ ->
					print_error ("A maximized parameter must be defined in the model to run EFmax.");
					abort_program();
		);

		if options#imitator_mode = EF_synth_min then(
			match model.optimized_parameter with
				| Minimize _ -> ()
				| _ ->
					print_error ("A minimized parameter must be defined in the model to run EFsynthmin.");
					abort_program();
		);

		if options#imitator_mode = EF_synth_max then(
			match model.optimized_parameter with
				| Maximize _ -> ()
				| _ ->
					print_error ("A maximized parameter must be defined in the model to run EFsynthmax.");
					abort_program();
		);

		if options#imitator_mode = EFexemplify then(
			match model.global_time_clock with
				| Some _ -> ()
				| _ ->
					print_error ("An absolute time clock must be defined in the model to run EFexemplify.");
					abort_program();
		);




		if (options#imitator_mode = Border_cartography && model.correctness_condition = None) then(
			print_error ("In border cartography mode, a correctness property must be defined.");
			abort_program();
		);*)



		(*** TODO: reintroduce
		(************************************************************)
		(* Dynamic clock elimination *)
		(************************************************************)
		(* Need to be called before initial state is created! *)
		if options#dynamic_clock_elimination then (
			ClocksElimination.prepare_clocks_elimination ()
		);
	***)


(*(************************************************************)
(* EXPERIMENTAL: branch and bound *)
(************************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound model pi0 init_state_after_time_elapsing;
	terminate_program();
);*)




		(************************************************************)
		(************************************************************)
		(* Run IMITATOR *)
		(************************************************************)
		(************************************************************)

(*		(* Generic method for the cartography to create either a new IM instance, or a new PRP instance *)
		(*** TODO: also add IMK, etc., if needed ***)
		let new_im_or_prp =
			if options#efim then
				fun () ->
				let myalgo :> AlgoStateBased.algoStateBased = new AlgoPRP.algoPRP in myalgo
			else
				fun () -> let myalgo :> AlgoStateBased.algoStateBased = new AlgoIM.algoIM in myalgo
		in*)

			(* Find the correct concrete algorithm to execute *)
			let concrete_algorithm : AlgoGeneric.algoGeneric = match abstract_property.property with

			
		(*------------------------------------------------------------*)
		(* Non-nested CTL *)
		(*------------------------------------------------------------*)
			(************************************************************)
			(* Reachability *)
			(************************************************************)
			| EF state_predicate ->
				
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFunsafeSynth.algoEFunsafeSynth state_predicate in myalgo
			
			
			(************************************************************)
			(* Safety *)
			(************************************************************)
			| AGnot state_predicate ->
			
				(*** NOTE: witness not supported (we need to compute everything to make sure the system is safe) ***)
				if abstract_property.synthesis_type = Witness then(
					print_warning "Exhibition of a subset of parameter valuations is not yet supported by this algorithm; either the whole set of valuations will be computed, or an over-approximation of this set.";
				);
			
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoAGsafeSynth.algoAGsafeSynth state_predicate in myalgo


		(*------------------------------------------------------------*)
		(* Reachability and specification illustration *)
		(*------------------------------------------------------------*)
			| EFexemplify state_predicate ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFexemplify.algoEFexemplify state_predicate in myalgo


		(*------------------------------------------------------------*)
		(* Optimized reachability *)
		(*------------------------------------------------------------*)
			(************************************************************)
			(* Reachability with minimization of a parameter valuation *)
			(************************************************************)
			| EFpmin (state_predicate , parameter_index) ->
				let efopt_algo = new AlgoEFmin.algoEFmin state_predicate parameter_index in
				(*** NOTE: very important: must set NOW the parameters ***)
				efopt_algo#set_synthesize_valuations (not emptiness_only); (* Synthesis of valuations out of the desired parameter *)
				let myalgo :> AlgoGeneric.algoGeneric = efopt_algo in
				myalgo

		
			(************************************************************)
			(* Reachability with maximization of a parameter valuation *)
			(************************************************************)
			| EFpmax (state_predicate , parameter_index) ->
				let efopt_algo = new AlgoEFmax.algoEFmax state_predicate parameter_index  in
				(*** NOTE: very important: must set NOW the parameters ***)
				efopt_algo#set_synthesize_valuations (not emptiness_only); (* Synthesis of valuations out of the desired parameter *)
				let myalgo :> AlgoGeneric.algoGeneric = efopt_algo in
				myalgo

		
			(************************************************************)
			(* Reachability with minimal-time *)
			(************************************************************)
			| EFtmin state_predicate ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFtminQueue.algoEFtminQueue state_predicate in myalgo

		
		
		(*------------------------------------------------------------*)
		(* Cycles *)
		(*------------------------------------------------------------*)
		
			(************************************************************)
			(* Parametric loop synthesis *)
			(************************************************************)
			| Cycle ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoLoopSynth.algoLoopSynth in myalgo

			| Accepting_cycle _ ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoAccLoopSynth.algoAccLoopSynth in myalgo

		(*------------------------------------------------------------*)
		(* Deadlock-freeness *)
		(*------------------------------------------------------------*)

			(************************************************************)
			(* Parametric deadlock checking *)
			(************************************************************)
			| Deadlock_Freeness ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoDeadlockFree.algoDeadlockFree in myalgo

		(*------------------------------------------------------------*)
		(* Inverse method, trace preservation, robustness *)
		(*------------------------------------------------------------*)
		
			(************************************************************)
			(* Inverse method *)
			(************************************************************)
			
			(* Inverse method with complete, non-convex result *)
			| IM pval ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMcomplete.algoIMcomplete pval in myalgo

			(* Non-complete, non-deterministic inverse method with convex result *)
			| ConvexIM pval ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIM.algoIM pval in myalgo

			(* Parametric reachability preservation *)
			| PRP (state_predicate, pval) ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoPRP.algoPRP pval state_predicate in myalgo

			(* Variant IMK of the Inverse method *)
			| IMK pval ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMK.algoIMK pval in myalgo

			(* Variant IMunion of the Inverse method *)
			| IMunion pval ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMunion.algoIMunion pval in myalgo


			(*** TODO: allow for old version with list of constraints ***)
(*			(* Old version (with list of constraints) *)
			| EF_synthesis (*when not options#new_ef_mode*) ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFsynthOld.algoEFsynth in myalgo
				*)

		(*------------------------------------------------------------*)
		(* Cartography algorithms *)
		(*------------------------------------------------------------*)
	
			(* Cartography *)
			| Cover_cartography hyper_rectangle ->
				let bc_algo = new AlgoBCCover.algoBCCover hyper_rectangle (fun pval -> let myalgo :> AlgoStateBased.algoStateBased = new AlgoIM.algoIM pval in myalgo) AlgoCartoGeneric.Tiles_list in
(*				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function ;
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;*)
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
	
	(*

			(************************************************************)
			(* EF-exemplification *)
			(************************************************************)
			| EFexemplify ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoEFexemplify.algoEFexemplify in myalgo


				
			(************************************************************)
			(* AF-synthesis *)
			(************************************************************)
			| AF_synthesis ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoAF.algoAFsynth in myalgo
*)
				(*
			| Acc_loop_synthesis_NDFS ->
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoNDFS.algoNDFS in myalgo


			(************************************************************)
			(* Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA) *)
			(************************************************************)
			| Parametric_NZ_CUBcheck ->
				(* Computing a constraint for which the PTA is CUB *)
				print_message Verbose_standard ("Checking whether the PTA is CUB for some parameter valuations…");

				let cub_constraint = CUBchecker.check_cub model in

				if verbose_mode_greater Verbose_low then(
					(* Computing a constraint for which the PTA is CUB *)
					print_message Verbose_low ("Computed CUB constraint");
					print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_constraint);
					print_message Verbose_low ("Comparing the computed constraint with the initial constraint:");
					print_message Verbose_low (LinearConstraint.string_of_p_linear_constraint model.variable_names model.initial_p_constraint);

				);

				(* Compare if the model is CUB for *all* valuations *)
				let is_universally_cub = LinearConstraint.p_is_equal cub_constraint model.initial_p_constraint in

				if is_universally_cub then(
					print_message Verbose_standard ("The model is a CUB-PTA for all defined parameter valuations, i.e.:");
				)else(
					print_message Verbose_standard ("The model is a CUB-PTA for the following parameter valuations:");
				);
				print_message Verbose_standard (LinearConstraint.string_of_p_linear_constraint model.variable_names cub_constraint);

				(*** TODO: check if the constraint is stricter than the original constraint; if yes, the result can only be an under-approximation ***)

				(* Update the model *)
				LinearConstraint.px_intersection_assign_p model.initial_constraint [cub_constraint];
				(* Update the initial p constraint too *)
				LinearConstraint.p_intersection_assign model.initial_p_constraint [cub_constraint];

				(* Call the NZ emptiness check *)
				let nz_algo = new AlgoNZCUB.algoNZCUB in

				(* Force under-approximation if not universally CUB *)
				if not is_universally_cub then(
					nz_algo#force_underapproximation;
				);

				let myalgo :> AlgoGeneric.algoGeneric = nz_algo in myalgo


			| Parametric_NZ_CUBtransform ->
				print_message Verbose_standard ("Generating the transformed model…");

				let cub_model = CUBchecker.cubpta_of_pta model in
				(*** HACK: set the model in the input module too ***)
				Input.set_model cub_model;

				print_message Verbose_standard ("Transformation completed");

				(* Only export to file in graphics for >= Verbose_low *)
				if verbose_mode_greater Verbose_low then(
					(* Export the model to a file *)
					(*** TODO: not necessary? (but so far useful to test) ***)

					let translated_model = ModelPrinter.string_of_model cub_model in

					let imi_file = options#files_prefix ^ "-cub.imi" in
					if verbose_mode_greater Verbose_total then(
						print_message Verbose_total ("\n" ^ translated_model ^ "\n");
					);

					(* Write *)
					write_to_file imi_file translated_model;
					print_message Verbose_low ("File '" ^ imi_file ^ "' successfully created.");


					(* Then transform to a graphics *)
					(*** TODO: not necessary? (but so far useful to test) ***)

					let translated_model = PTA2JPG.string_of_model cub_model in
					if verbose_mode_greater Verbose_high then(
						print_message Verbose_high ("\n" ^ translated_model ^ "\n");
					);

					Graphics.dot Constants.pta_default_image_format (options#files_prefix ^ "-cubpta") translated_model;

					print_message Verbose_low ("Graphic export successfully created."); (*** TODO: add file name in a proper manner ***)
				); (* end export *)

				(* Call the NZ emptiness check *)
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoNZCUB.algoNZCUB in myalgo


			| Parametric_NZ_CUB ->
				(* Just call the NZ emptiness check *)
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoNZCUB.algoNZCUB in myalgo


				*)

				(*
			(************************************************************)
			(* Inverse method and variants *)
			(************************************************************)
			(* IMK *)
			(*** TODO: use four different modes ***)
			| Inverse_method when options#pi_compatible ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMK.algoIMK in myalgo

			(* PRP *)
			(*** NOTE: deprecated ***)
			| Inverse_method when options#efim ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoPRP.algoPRP in myalgo

			(* IMunion *)
			| Inverse_method when options#union ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMunion.algoIMunion in myalgo

			(* Inverse Method *)
			| Inverse_method ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIM.algoIM in myalgo

			(* Inverse Method *)
			| Inverse_method_complete ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoIMcomplete.algoIMcomplete in myalgo


			(************************************************************)
			(* PRP *)
			(************************************************************)
			| PRP ->
					let myalgo :> AlgoGeneric.algoGeneric = new AlgoPRP.algoPRP in myalgo
*)

			(************************************************************)
			(* Begin distributed cartography *)
			(************************************************************)

			(*** WARNING:  Do not modify the following lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)
			(*(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)

			(** TODO: add conditions to CUB
			| Parametric_NZ_CUBtransformDistributed ->
				print_message Verbose_standard ("Generating the transformed model…");

				let cub_model = CUBchecker.cubpta_of_pta model in
				(*** HACK: set the model in the input module too ***)
				Input.set_model cub_model;

				print_message Verbose_standard ("Transformation completed");

				(* Only export to file in graphics for >= Verbose_low *)
				if verbose_mode_greater Verbose_low then(
					(* Export the model to a file *)
					(*** TODO: not necessary? (but so far useful to test) ***)

					let translated_model = ModelPrinter.string_of_model cub_model in

					let imi_file = options#files_prefix ^ "-cub.imi" in
					if verbose_mode_greater Verbose_total then(
						print_message Verbose_total ("\n" ^ translated_model ^ "\n");
					);

					(* Write *)
					write_to_file imi_file translated_model;
					print_message Verbose_low ("File '" ^ imi_file ^ "' successfully created.");


					(* Then transform to a graphics *)
					(*** TODO: not necessary? (but so far useful to test) ***)

					let translated_model = PTA2JPG.string_of_model cub_model in
					if verbose_mode_greater Verbose_high then(
						print_message Verbose_high ("\n" ^ translated_model ^ "\n");
					);

					Graphics.dot Constants.pta_default_image_format (options#files_prefix ^ "-cubpta") translated_model;

					print_message Verbose_low ("Graphic export successfully created."); (*** TODO: add file name in a proper manner ***)
				); (* end export *)

				(* Call the NZ emptiness check *)
				let myalgo :> AlgoGeneric.algoGeneric = new AlgoNZCUBdist.algoNZCUBdist in myalgo
			*)

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
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo
					else
						let bc_algo = new AlgoBCCoverDistributedMSSeqWorker.algoBCCoverDistributedMSSeqWorker in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo

				(** Distributed mode: Master worker with sequential pi0 shuffled *)
				| Distributed_ms_shuffle ->
					(* Branch between master and worker *)
					if DistributedUtilities.is_master() then
						let bc_algo = new AlgoBCCoverDistributedMSShuffleMaster.algoBCCoverDistributedMSShuffleMaster in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo
					else
						let bc_algo = new AlgoBCCoverDistributedMSShuffleWorker.algoBCCoverDistributedMSShuffleWorker in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
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
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo
					else
						let bc_algo = new AlgoBCCoverDistributedMSRandomSeqWorker.algoBCCoverDistributedMSRandomSeqWorker in
						(*** NOTE: very important: must set NOW the parameters ***)
		(* 				bc_algo#set_max_tries nb_tries; *)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo

				(** Distributed mode: Master worker with subdomain distribution *)
				| Distributed_ms_subpart ->
					(* Branch between master and worker *)
					if DistributedUtilities.is_master() then
						let bc_algo = new AlgoBCCoverDistributedSubdomainDynamicCoordinator.algoBCCoverDistributedSubdomainDynamicCoordinator in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo
					else
						let bc_algo = new AlgoBCCoverDistributedSubdomainDynamicCollaborator.algoBCCoverDistributedSubdomainDynamicCollaborator in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo

				(** Distributed mode: static distribution mode (each node has its own subdomain with no communication) *)
				| Distributed_static ->
					(* Branch between collaborator and coordinator *)
					if DistributedUtilities.is_coordinator() then
						let bc_algo = new AlgoBCCoverDistributedSubdomainStaticCoordinator.algoBCCoverDistributedSubdomainStaticCoordinator in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo
					else
						let bc_algo = new AlgoBCCoverDistributedSubdomainStaticCollaborator.algoBCCoverDistributedSubdomainStaticCollaborator in
						(*** NOTE: very important: must set NOW the parameters ***)
						bc_algo#set_algo_instance_function new_im_or_prp;
						bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
						let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
						myalgo


				| _ -> raise (InternalError("Other distribution modes not yet implemented"))

				in algo


			(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)*)
			(*** WARNING:  Do not modify the previous lines! (used by an external script to compile the non-distributed version of IMITATOR) ***)

			(************************************************************)
			(* End distributed cartography *)
			(************************************************************)


		(*	(************************************************************)
			(* Non-distributed cartography *)
			(************************************************************)

			(* BC with full coverage *)
			| Cover_cartography ->
				let bc_algo = new AlgoBCCover.algoBCCover in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

			(* BC with full coverage and learning-based abstractions *)
			| Learning_cartography ->
				let bc_algo = new AlgoBCCoverLearning.algoBCCoverLearning in
				(*** NOTE: very important: the algo instance function should NOT be set for this algorithm (as it always uses EFsynth or PRP anyway) ***)
		(* 		bc_algo#set_algo_instance_function new_im_or_prp; *)
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_good_bad_constraint;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

			(* BC with full coverage (shuffled version) *)
			| Shuffle_cartography ->
				let bc_algo = new AlgoBCShuffle.algoBCShuffle in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function new_im_or_prp;
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

			| Border_cartography ->
				raise (NotImplemented("Border cartography is disabled"))

			(* BC with random coverage *)
			| Random_cartography nb ->
				let bc_algo = new AlgoBCRandom.algoBCRandom in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_max_tries nb;
				bc_algo#set_algo_instance_function new_im_or_prp;
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo


			(* BC with random coverage followed by sequential coverage *)
			| RandomSeq_cartography nb ->
				let bc_algo = new AlgoBCRandomSeq.algoBCRandomSeq in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_max_tries nb;
				bc_algo#set_algo_instance_function new_im_or_prp;
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_list;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo

			(* Iterative calls to PRP *)
			| PRPC ->
				let bc_algo = new AlgoBCCover.algoBCCover in
				(*** NOTE: very important: must set NOW the parameters ***)
				bc_algo#set_algo_instance_function (fun () -> let myalgo :> AlgoStateBased.algoStateBased = new AlgoPRP.algoPRP in myalgo);
				(*** NOTE: for PRPC, we use a constraint manager! ***)
				bc_algo#set_tiles_manager_type AlgoCartoGeneric.Tiles_good_bad_constraint;
				let myalgo :> AlgoGeneric.algoGeneric = bc_algo in
				myalgo
*)

		| _ ->
			(*** TODO: temporary end ***)
			raise (NotImplemented ("IMITATOR says temporarily bye!"));
			exit 1;


		in



		(* Run! *)
		let result = concrete_algorithm#run() in

		(* Stop the main algorithm counters *)
		counter_algorithm_and_parsing#stop;
		counter_main_algorithm#stop;

		(* Process *)
		ResultProcessor.process_result result concrete_algorithm#algorithm_name None;

	end; (* match type of abstract algorithm *)

(************************************************************)
(************************************************************)
(* End branching depending on the algorithm *)
(************************************************************)
(************************************************************)
end;



(************************************************************)
(* END EXCEPTION MECHANISM *)
(************************************************************)
) with
	e ->(
	let error_message = match e with
		| InternalError msg -> "Fatal internal error: " ^ msg ^ ""
		| NotImplemented msg -> "A non-implemented feature has been called: " ^ msg ^ ""
		| Division_by_0 msg -> "Division by 0! " ^ msg ^ ""
		| Failure msg -> "'Failure' exception: '" ^ msg ^ "'"
		| Invalid_argument msg -> "'Invalid_argument' exception: '" ^ msg ^ "'"
		| SerializationError msg -> "Serialization error: " ^ msg ^ ""
		| InterfacingError msg -> "Interfacing error: " ^ msg ^ ""
		| Not_found -> "'Not_found' exception!"
		| Random_generator_initialization_exception-> "A fatal error occurred during the random generator initialization."
		| e -> "Fatal exception '" ^ (Printexc.to_string e) ^ "'."
	in

	(* Stop the main algorithm counters *)
	counter_algorithm_and_parsing#stop;
	counter_main_algorithm#stop;

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

(* Only print counters if statistics are required, or experiments verbose mode *)
if (try (Input.get_options())#statistics with _ -> false) || verbose_mode_greater Verbose_experiments then
	print_message Verbose_standard (string_of_all_counters());


(************************************************************)
(* Bye bye! *)
(************************************************************)

terminate_program()

