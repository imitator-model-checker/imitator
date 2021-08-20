(************************************************************
 *
 *                       IMITATOR
 * 
 * Université Paris 13, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: EFexemplify algorithm [work in progress]. Structurally identical to EFsynth (at the beginning), so the code processes with simple add-ons
 * 
 * File contributors : Étienne André
 * Created           : 2019/07/08
 * Last modified     : 2021/08/20
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
open AlgoEFsynth
open Statistics
open State
open StateSpace




(************************************************************)
(************************************************************)
(* Class-independent methods *)
(************************************************************)
(************************************************************)



(************************************************************)
(************************************************************)
(* Class definition *)
(************************************************************)
(************************************************************)
class algoEFexemplify (state_predicate : AbstractProperty.state_predicate) =
	object (self) inherit algoEFsynth state_predicate as super
	
	(************************************************************)
	(* Class variables *)
	(************************************************************)
	
	(* Positive examples spotted (positive examples: concrete runs to the target state) *)
	val mutable positive_examples : Result.valuation_and_concrete_run list = []
	
	(* Negative examples spotted (negative examples: *impossible* concrete runs to the target state) *)
	val mutable negative_examples : Result.valuation_and_concrete_run list = []
	
	val nb_POSITIVE_EXAMPLES_MAX = 6
	val nb_NEGATIVE_EXAMPLES_MAX = 6


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Name of the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method algorithm_name = "EFexemplify"
	
	
	
	(************************************************************)
	(* Class methods *)
	(************************************************************)
	
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Variable initialization *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method initialize_variables =
		super#initialize_variables;

		positive_examples <- [];
		negative_examples <- [];

		(* The end *)
		()


	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Generate counter-example(s) if required by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)

	method process_counterexample (target_state_index : State.state_index) =
		(* Print some information *)
		let nb_positive_examples = List.length positive_examples + 1 in
		self#print_algo_message Verbose_standard ("Target state #" ^ (string_of_int nb_positive_examples) ^ " found!");
		
		(*------------------------------------------------------------*)
		(* Call generic function *)
		(*------------------------------------------------------------*)
		let positive_valuation_and_concrete_run, negative_valuation_and_concrete_run_option_otherpval, negative_valuation_and_concrete_run_option_samepval = self#exhibit_3_counterexamples target_state_index in

		(*------------------------------------------------------------*)
		(* Update the lists *)
		(*------------------------------------------------------------*)
		
		(* Update the positive counterexample processed *)
		positive_examples <- positive_valuation_and_concrete_run :: positive_examples;

		begin
		match negative_valuation_and_concrete_run_option_otherpval with 
		| None ->
			print_message Verbose_standard "\n\nFound no parameter valuation allowing a negative counterexample for this run";
		| Some negative_valuation_and_concrete_run ->
				negative_examples <- negative_valuation_and_concrete_run :: negative_examples;
		end;
		
		begin
		match negative_valuation_and_concrete_run_option_samepval with
		| None ->
			print_message Verbose_standard "\n\nFound no clock valuation allowing a negative counterexample for the same parameter valuaton for this run";
		| Some valuation_and_concrete_run ->
			(* Update the counterexamples processed *)
			negative_examples <- valuation_and_concrete_run :: negative_examples;
		end;
		

		(*------------------------------------------------------------*)
		(* Check termination *)
		(*------------------------------------------------------------*)
		
		(* If maximum number of counterexamples processed: stop *)
		if List.length positive_examples >= nb_POSITIVE_EXAMPLES_MAX then(
			(* Update termination status *)
			self#print_algo_message Verbose_standard ("Target state #" ^ (string_of_int (List.length positive_examples)) ^ " is the maximum number sought. Terminating…");
			(*** NOTE/HACK: the number of unexplored states is not known, therefore we do not add it… ***)
			termination_status <- Some Target_found;
		
			raise TerminateAnalysis;
		)else(
			(* Add the target state to the set of states to explore (a bit a hack); indeed, for exemplification, we may be interested in exploring beyond bad states, as we may find more! *)
			new_states_indexes <- target_state_index :: new_states_indexes;
		);

		(* The end *)
		()
	

	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	(* Method packaging the result output by the algorithm *)
	(*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*)
	method compute_result =
		(* Print some information *)
		self#print_algo_message_newline Verbose_standard (
			"Algorithm completed " ^ (after_seconds ()) ^ "."
		);
		
		(* Get the termination status *)
		 let termination_status = match termination_status with
			| None -> raise (InternalError ("Termination status not set in " ^ (self#algorithm_name) ^ ".compute_result"))
			| Some status -> status
		in
		
		(* Return the result *)
		Runs_exhibition_result
		{
			(* Non-necessarily convex constraint guaranteeing the reachability of the bad location *)
			(*** NOTE: use rev since we added the runs by reversed order ***)
			runs				= List.rev_append positive_examples (List.rev negative_examples);
			
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
