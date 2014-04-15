(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Ulrich Kuehne, Etienne Andre
 * 
 * Created:       2009/09/07
 * Last modified: 2014/03/15
 *
 ****************************************************************)


(**************************************************)
(* Modules *)
(**************************************************)
open Global
open AbstractModel
(* open Arg *)
open ModelPrinter
open Options
open Reachability
open Gc

open Mpi


(**************************************************

TAGS POUR CHOSES A FAIRE
- (**** TO DO ****)
- (**** BAD PROG ****)
- (**** TO OPTIMIZE ****)
- (**** OPTIMIZED ****)

<>

**************************************************)

;;


(**************************************************)
(**************************************************)
(* STARTING PROGRAM *)
(**************************************************)
(**************************************************)


(* TEST !! *)
(*LinearConstraint.test_PDBMs();
terminate_program();*)



(**************************************************)
(* Get the arguments *)
(**************************************************)
(* object with command line options *)
let options = new imitator_options in

options#parse;

(* Set the options (for other modules) *)
Input.set_options options;


(**************************************************)
(**************************************************)
(* Print startup message *)
(**************************************************)
(**************************************************)
  
begin
  match options#distribution_mode with
  | Distributed -> 
    let rank = Mpi.comm_rank comm_world in
    if rank = 0 then
      begin
	(* Recall the arguments *)
	options#recall(); 
	(* Print stuff about the authors *)
	print_message Debug_standard header_string;
	(* Print date *)
	print_message Debug_standard ("Analysis time: " ^ (now()) ^ "\n")
      end;
  | _ -> 
    (* Recall the arguments *)
    options#recall(); 
    (* Print stuff about the authors *)
    print_message Debug_standard header_string;
    (* Print date *)
    print_message Debug_standard ("Analysis time: " ^ (now()) ^ "\n")  
end;

    

(**************************************************)
(* Get input *)
(**************************************************)
let model, pi0, v0 = ParsingUtility.compile options in

Input.set_model model;
Input.set_pi0 pi0;


(**************************************************)
(* Debug print: model *)
(**************************************************)
if debug_mode_greater Debug_total then
	print_message Debug_total ("\nModel:\n" ^ (ModelPrinter.string_of_model model) ^ "\n");


(**************************************************)
(* Case distributed *)
(**************************************************)
begin
match options#distribution_mode with
	(* Fork if distributed *)
	| Distributed -> PaTATOR.run()
	| _ -> ()
end;



(**************************************************)
(* Case translation *)
(**************************************************)

(* Translation to CLP (work in progress) *)
if options#pta2clp then(
	print_message Debug_standard ("Translating model to CLP.");
	print_warning ("Work in progress!!!!");
	print_message Debug_standard ("\nmodel in CLP:\n" ^ (PTA2CLP.string_of_model model) ^ "\n");
	terminate_program()
);

(* Translation to GrML (experimental) *)
if options#pta2gml then(
	print_message Debug_standard ("Translating model to GrML.");
	let translated_model = PTA2GrML.string_of_model model in
	let gml_file = options#files_prefix ^ ".grml" in
	if debug_mode_greater Debug_total then(
		print_message Debug_total ("\n" ^ translated_model ^ "\n");
	);
	(* Write *)
	write_to_file gml_file translated_model;
	terminate_program()
);

(* Translation to JPG *)
if options#pta2jpg then(
	print_message Debug_standard ("Translating model to a graphics.");
	let translated_model = PTA2JPG.string_of_model model in
	if debug_mode_greater Debug_high then(
		print_message Debug_high ("\n" ^ translated_model ^ "\n");
	);
	Graphics.dot model options#files_prefix translated_model;
	terminate_program()
);

(* Direct cartography output *)
if options#cartonly then(
	print_message Debug_standard ("Direct output of a cartography (no analysis will be run).");
	(* Get the parameters *)
	let constraints , (p1_min , p1_max) , (p2_min , p2_max) = model.carto in
	(* Transform the constraint for cartography *)
	let constraints = List.map (fun (linear_constraint , tile_nature) ->
		Convex_constraint (linear_constraint , tile_nature)
	) constraints in
	(* Call the cartography *)
	Graphics.cartography model [| (p1_min , p1_max); (p2_min , p2_max) |] constraints options#files_prefix;
	(* The end *)
	terminate_program()
);
(* 		| End_of_file -> print_error ("Parsing error in file " ^ file_name ^ ": unexpected end of file."); abort_program (); exit(1) *)



(**************************************************)
(* Preliminary checks *)
(**************************************************)

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



(**************************************************)
(* EXPERIMENTAL: dynamic clock elimination *)
(**************************************************)
(* Need to be called before initial state is created! *)
if options#dynamic_clock_elimination then (
	Reachability.prepare_clocks_elimination model
);







(*(* TESTS *) 
print_message Debug_standard ("\nInitial constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");

(*let n = ref 1 in

List.iter (fun parameter_id ->
	LinearConstraint.time_elapse_assign [parameter_id] (list_diff model.parameters [parameter_id]) initial_constraint_after_time_elapsing;
	
	print_message Debug_standard ("\nAfter time elapsing #" ^ (string_of_int !n) ^ " on parameter '" ^ (model.variable_names parameter_id) ^ "' :\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
	
	Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto" ^ (string_of_int !n));

	n := !n + 1;

) model.parameters;
(* Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-carto"); *)
terminate_program();*)


LinearConstraint.grow_to_zero_assign model.parameters model.clocks_and_discrete initial_constraint_after_time_elapsing;
print_message Debug_standard ("\nFinal constraint:\n" ^ (LinearConstraint.string_of_linear_constraint model.variable_names initial_constraint_after_time_elapsing) ^ "\n");
Graphics.cartography model v0 [Convex_constraint initial_constraint_after_time_elapsing] (options#file ^ "-cartoz");
terminate_program();*)



(*(**************************************************)
(* EXPERIMENTAL: branch and bound *)
(**************************************************)

if options#imitator_mode = Inverse_method && options#branch_and_bound then(
	Reachability.branch_and_bound model pi0 init_state_after_time_elapsing;
	terminate_program();
);*)




(**************************************************)
(* Execute IMITATOR *)
(**************************************************)

begin
try(
	let zones =
	match options#imitator_mode with
		| Translation -> raise (InternalError "Translation cannot be executed here; program should already have terminated at this point.");

		
		(* Exploration *)
		| State_space_exploration
			-> Reachability.full_state_space_exploration model;
			[]
			
		(* Synthesis *)
		| EF_synthesis 
			->
			[Reachability.ef_synthesis model]

			
		(* Inverse Method *)
		| Inverse_method ->
			if options#efim then
				(
					(*** WARNING!!! Why a dedicated function here, whereas for BC+EFIM this function is not (?) called? ***)
				Reachability.efim model;
				[]
				)
			else(
				Reachability.inverse_method model;
				[]
			)


		| Cover_cartography | Border_cartography ->
		(* Behavioral cartography algorithm with full coverage *)
			Cartography.cover_behavioral_cartography model v0
			
			
		| Random_cartography nb ->
		(* Behavioral cartography algorithm with random iterations *)
			Cartography.random_behavioral_cartography model v0 nb;

			
	in

	(* Computation of the cartography *)
	if options#cart then (
			(* No cartography if no zone *)
			if zones = [] then(
				print_message Debug_standard ("\nNo cartography can be generated since the list of constraints is empty.\n");
			)else(
				print_message Debug_standard ("\nGeneration of the graphical cartography...");
				Graphics.cartography model v0 zones (options#files_prefix ^ "_cart")
			)
		) else (
			print_message Debug_high "Graphical cartography not asked: graph not generated."
		)
	;
) with
	| InternalError e -> (print_error ("Internal error: " ^ e ^ "\nPlease (kindly) insult the developers."); abort_program (); exit 1);
end;


(**************************************************)
(* Bye bye! *)
(**************************************************)

(* Reachability.print_stats (); *)

terminate_program()
