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
 * Last modified     : 2021/09/16
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

let default_state_comparison property : AbstractAlgorithm.state_comparison_operator =
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
	
		-> Inclusion_check


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle
	
		-> Equality_check
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness
		-> Inclusion_check

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM _

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM _
		-> Equality_check

	(* Parametric reachability preservation *)
	| PRP _
		-> Inclusion_check

	(* Variant IMK of the Inverse method *)
	| IMK _

	(* Variant IMunion of the Inverse method *)
	| IMunion _
		-> Equality_check

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography _
		-> Equality_check

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		-> Inclusion_check
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(** Look for the border using the cartography*)
	| Border_cartography _
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		-> Equality_check

	(* Parametric reachability preservation *)
	| PRPC _
		-> Inclusion_check



(* Does the use of a given state_comparison_operator for a given abstract_property preserve the result correctness? *)
let is_state_comparison_correct (abstract_property : AbstractProperty.abstract_property) (state_comparison_operator : AbstractAlgorithm.state_comparison_operator) : bool =
	match abstract_property.property with
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

		(* All comparison operators preserve correctness *)
		-> true


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _
	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle
		(* No inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness
		(* All comparison operators preserve correctness *)
		(*** WARNING: not sure…? ***)
		-> true

	
	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	
	(* Inverse method with complete, non-convex result *)
	| IM _

	(* Non-complete, non-deterministic inverse method with convex result *)
	| ConvexIM _

	(* Variant IMK of the Inverse method *)
	| IMK _

	(* Variant IMunion of the Inverse method *)
	| IMunion _
		(* No inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check

	(* Parametric reachability preservation *)
	| PRP _
		(* All comparison operators preserve correctness *)
		-> true

	
	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)
	
	(* Cartography *)
	| Cover_cartography _

	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(** Look for the border using the cartography*)
	| Border_cartography _
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		(* No inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check

	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		(* All comparison operators preserve correctness *)
		(*** WARNING: not sure…? ***)
		-> true
	
	(* Parametric reachability preservation *)
	| PRPC _
		(* All comparison operators preserve correctness *)
		-> true



(*------------------------------------------------------------*)
(* Merge *)
(*------------------------------------------------------------*)

let merge_needed property =
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
	
	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle
	
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

(*------------------------------------------------------------*)
(* Does the property support the #witness mode? *)
(*------------------------------------------------------------*)

let supports_witness property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
		-> true
	
	
	(*------------------------------------------------------------*)
	(* Reachability and specification illustration *)
	(*------------------------------------------------------------*)
	
	(** EF-synthesis with examples of (un)safe words *)
	| EFexemplify _
		(*** TODO ***)
		-> false
	
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
	
	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle
	
		-> true
	

	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)
	
	(* Deadlock-free synthesis *)
	| Deadlock_Freeness
		-> false

	
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
		-> false

	
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
		-> false


(*------------------------------------------------------------*)
(* Does the property support the #exemplification mode? *)
(*------------------------------------------------------------*)

let supports_exemplification property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
		-> true
	(*------------------------------------------------------------*)
	(* Any other *)
	(*------------------------------------------------------------*)
	| _ -> false


(*------------------------------------------------------------*)
let is_cartography property =
(*------------------------------------------------------------*)
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
		| Exemplification	-> "exemplification"
		| Synthesis			-> "synthesis"
		| Witness			-> "witness"
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

	(** Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _ -> "infinite generalized accepting run " ^ synthesis_or_witness

	(** Infinite-run (cycle) with non-Zeno assumption *)
	| NZ_Cycle -> "non-Zeno infinite accepting run " ^ synthesis_or_witness ^ ""
	

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
