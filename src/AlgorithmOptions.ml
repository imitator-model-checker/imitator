(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Describes some of the options associated with algorithms
 *
 * File contributors : Étienne André
 * Created           : 2020/08/25
 * Last modified     : 2020/09/14
 *
 ************************************************************)


(************************************************************)
(* Internal modules *)
(************************************************************)
open AbstractAlgorithm
open AbstractProperty


(************************************************************)
(* Default options depending on the property *)
(************************************************************)

(*------------------------------------------------------------*)
(* Inclusion *)
(*------------------------------------------------------------*)

let inclusion_needed property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
	
	
	(*------------------------------------------------------------*)
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify _
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter valuation *)
	| EFpmin _
	
	(* Reachability with maximization of a parameter valuation *)
	| EFpmax _
	
	(* Reachability with minimal-time *)
	| EFtmin _
	
		-> true


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(** Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
	| NZCycle_check
	
	(** Infinite-run (cycle) with non-Zeno assumption: method by transforming the PTA into a CUB-PTA *)
	| NZCycle_transform
	
	(** Infinite-run (cycle) with non-Zeno assumption: method assuming the PTA is already a CUB-PTA *)
	| NZCycle_CUB
	
		-> false
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness
		-> true

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM _

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM _
		-> false

	(* Parametric reachability preservation *)
	| PRP _
		-> true

	(* Variant IMK of the Inverse method *)
	| IMK _

	(* Variant IMunion of the Inverse method *)
	| IMunion _
		-> false

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography _
		-> false

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		-> true
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(** Look for the border using the cartography*)
	| Border_cartography _
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		-> false

	(* Parametric reachability preservation *)
	| PRPC _
		-> true


(*------------------------------------------------------------*)
(* Merge *)
(*------------------------------------------------------------*)

(*** NOTE: Shortcut! ***)
let merge_needed = inclusion_needed



(*------------------------------------------------------------*)
(* Exploration order *)
(*------------------------------------------------------------*)

(*let default_exploration_order property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
	
	
	(*------------------------------------------------------------*)
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify _
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter valuation *)
	| EFpmin _
	
	(* Reachability with maximization of a parameter valuation *)
	| EFpmax _
	
	(* Reachability with minimal-time *)
	| EFtmin _
	
		-> Exploration_layer_BFS


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(** Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
	| NZCycle_check
	
	(** Infinite-run (cycle) with non-Zeno assumption: method by transforming the PTA into a CUB-PTA *)
	| NZCycle_transform
	
	(** Infinite-run (cycle) with non-Zeno assumption: method assuming the PTA is already a CUB-PTA *)
	| NZCycle_CUB
	
		-> Exploration_layer_BFS
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness
	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM _

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM _

	(* Parametric reachability preservation *)
	| PRP _

	(* Variant IMK of the Inverse method *)
	| IMK _

	(* Variant IMunion of the Inverse method *)
	| IMunion _

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography _

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(** Look for the border using the cartography*)
	| Border_cartography _
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _

	(* Parametric reachability preservation *)
	| PRPC _
		-> Exploration_layer_BFS
*)



(************************************************************)
(* Predicates on properties *)
(************************************************************)

let is_cartography property =
	match property.property with
	| Cover_cartography _
	| Learning_cartography _
	| Shuffle_cartography _
	| Border_cartography _
	| Random_cartography _
	| RandomSeq_cartography _
	| PRPC _
		-> true

	| _ -> false


(************************************************************)
(* Textual description of properties *)
(************************************************************)

(* Gives a textual description of a property *)
let text_of_property property =
	let synthesis_or_witness = match property.synthesis_type with
		| Synthesis -> "synthesis"
		| Witness -> "witness"
	in
	
	match property.property with
	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _ -> "reachability " ^ synthesis_or_witness
	
	(* Safety *)
	| AGnot _ -> "safety " ^ synthesis_or_witness
	
	
	(*------------------------------------------------------------*)
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify _ -> "reachability counterexample exemplification"
	
	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	
	(* Reachability with minimization of a parameter valuation *)
	| EFpmin _ -> "reachability parameter minimization " ^ synthesis_or_witness
	
	(* Reachability with maximization of a parameter valuation *)
	| EFpmax _ -> "reachability parameter maximization " ^ synthesis_or_witness
	
	(* Reachability with minimal-time *)
	| EFtmin _ -> "reachability time minimization " ^ synthesis_or_witness
	

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _ -> "infinite accepting run " ^ synthesis_or_witness

	(** Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
	| NZCycle_check -> "non-Zeno infinite accepting run " ^ synthesis_or_witness ^ " (without CUB transformation)"
	
	(** Infinite-run (cycle) with non-Zeno assumption: method by transforming the PTA into a CUB-PTA *)
	| NZCycle_transform -> "non-Zeno infinite accepting run " ^ synthesis_or_witness ^ " (with CUB transformation)"
	
	(** Infinite-run (cycle) with non-Zeno assumption: method assuming the PTA is already a CUB-PTA *)
	| NZCycle_CUB -> "non-Zeno infinite accepting run " ^ synthesis_or_witness ^ " (for a CUB PTA)"
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness -> "parametric deadlock " ^ synthesis_or_witness

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM _ -> "inverse method"

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM _ -> "convex inverse method"

	(* Parametric reachability preservation *)
	| PRP _ -> "PRP"

	(* Variant IMK of the Inverse method *)
	| IMK _ -> "IMK [AS13]"

	(* Variant IMunion of the Inverse method *)
	| IMunion _ -> "IMunion [AS13]"

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography (_, step) -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst step)

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography (_, _, step) -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst step) ^ " and using learning-based abstractions"
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography (_, step) -> "behavioral cartography algorithm with full coverage (shuffled version) and step " ^ (NumConst.string_of_numconst step)
	
	(** Look for the border using the cartography*)
	| Border_cartography (_, step) -> "behavioral cartography algorithm with border detection and step " ^ (NumConst.string_of_numconst step)
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography (_, nb, step) -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst step)
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography (_, nb, step) -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations + sequential phase and step " ^ (NumConst.string_of_numconst step)

	(* Parametric reachability preservation *)
	| PRPC _ -> "parametric reachability preservation cartography"

(*
				| State_space_computation -> "parametric state space exploration"
				| EF_synthesis -> "EF-synthesis"
				| EFunsafe_synthesis -> "EFunsafe-synthesis"
				| EF_min -> "EF-minimization"
				| EF_max -> "EF-maximization"
				| EF_synth_min -> "EF-synth with minimization"
				| EF_synth_max -> "EF-synth with maximization"
				| EF_synth_min_priority_queue -> "EF-synth with minimal reachability"
				| EFexemplify -> "EF-exemplify"
				| AF_synthesis -> "AF-synthesis"
				| Loop_synthesis -> "infinite run synthesis"
				| Acc_loop_synthesis -> "accepting infinite run synthesis"
				| Acc_loop_synthesis_NDFS -> "accepting infinite run synthesis with NDFS exploration"
				| Parametric_NZ_CUBcheck -> "parametric non-Zeno emptiness checking (CUB checking)"
				| Parametric_NZ_CUBtransform -> "parametric non-Zeno emptiness checking (CUB transformation)"
				| Parametric_NZ_CUBtransformDistributed -> "parametric non-Zeno emptiness checking (CUB transformation), distributed version"
				| Parametric_NZ_CUB -> "parametric non-Zeno emptiness checking [testing mode without transformation]"
				| Parametric_deadlock_checking -> "Parametric deadlock-checking"
				| Inverse_method -> "inverse method"
				| Inverse_method_complete -> "inverse method with complete result"
				| PRP -> "parametric reachability preservation"
				| Cover_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst step)
				| Learning_cartography -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst step) ^ " and using learning-based abstractions"
				| Shuffle_cartography -> "behavioral cartography algorithm with full coverage (shuffled version) and step " ^ (NumConst.string_of_numconst step)
				| Border_cartography -> "behavioral cartography algorithm with border detection (experimental) and step " ^ (NumConst.string_of_numconst step)
				| Random_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst step)
				| RandomSeq_cartography nb -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations + sequential phase and step " ^ (NumConst.string_of_numconst step)
				| PRPC -> "parametric reachability preservation cartography"
				*)
