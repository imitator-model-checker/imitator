(************************************************************
 *
 *                       IMITATOR
 *
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Describes some of the options associated with algorithms
 *
 * File contributors : Étienne André
 * Created           : 2020/08/25
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
(** Inclusion *)
(*------------------------------------------------------------*)

let default_state_comparison property : AbstractAlgorithm.state_comparison_operator =
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	(*** NOTE: irrelevant! ***)
	| Valid -> No_check

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
	
	(* Global invariant *)
	| AG _

	(* Exists until *)
	| EU _

		-> Inclusion_check

	(*** TODO: decide heuristics ***)
	(* Exists release *)
	| ER _

	(* Exists weak until *)
	| EW _

		-> Equality_check

	(*** TODO: decide heuristics ***)
	(* Unavoidability *)
	| AF _

	(* Always release *)
	| AR _

	(* Always until *)
	| AU _

	(* Always weak until *)
	| AW _

		-> Equality_check

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		-> Inclusion_check

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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(* Infinite-run (cycle) with non-Zeno assumption *)
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

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		-> Inclusion_check
	
	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(* Look for the border using the cartography*)
	| Border_cartography _
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		-> Equality_check

	(* Parametric reachability preservation *)
	| PRPC _
		-> Inclusion_check

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Win _ -> Inclusion_check



(* Does the use of a given state_comparison_operator for a given abstract_property preserve the result correctness? *)
let is_state_comparison_correct (abstract_property : AbstractProperty.abstract_property) (state_comparison_operator : AbstractAlgorithm.state_comparison_operator) : bool =
	match abstract_property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	(*** NOTE: irrelevant! ***)
	| Valid -> true

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _

	(* Global invariant *)
	| AG _

	(* Exists until *)
	| EU _
		(* All comparison operators preserve correctness *)
		-> true

	(*** TODO: decide heuristics ***)
	(* Exists release *)
	| ER _

	(* Exists weak until *)
	| EW _

		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check


		(*** TODO: decide heuristics ***)
	(* Unavoidability *)
	| AF _

	(* Always release *)
	| AR _

	(* Always until *)
	| AU _

	(* Always weak until *)
	| AW _
		(* No inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		(* All comparison operators preserve correctness *)
		-> true


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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _
	(* Infinite-run (cycle) with non-Zeno assumption *)
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

	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(* Look for the border using the cartography*)
	| Border_cartography _
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		(* No inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		(* All comparison operators preserve correctness *)
		(*** WARNING: not sure…? ***)
		-> true
	
	(* Parametric reachability preservation *)
	| PRPC _
		(* All comparison operators preserve correctness *)
		-> true

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Win _
		(* No reversed inclusion allowed *)
		-> state_comparison_operator = Equality_check || state_comparison_operator = No_check || state_comparison_operator = Inclusion_check



(*------------------------------------------------------------*)
(** Merge *)
(*------------------------------------------------------------*)

let merge_needed property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	(*** NOTE: irrelevant! ***)
	| Valid -> false


	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _
	
	(* Global invariant *)
	| AG _

	(* Exists until *)
	| EU _
		-> true

	(*** TODO: decide heuristics ***)
	(* Exists release *)
	| ER _

	(* Exists weak until *)
	| EW _
		-> false

	(*** TODO: decide heuristics ***)
	(* Unavoidability *)
	| AF _

	(* Always release *)
	| AR _

	(* Always until *)
	| AU _

	(* Always weak until *)
	| AW _
		-> false

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		-> true

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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(* Infinite-run (cycle) with non-Zeno assumption *)
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

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
		-> true
	
	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(* Look for the border using the cartography*)
	| Border_cartography _
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _
		-> false

	(* Parametric reachability preservation *)
	| PRPC _
		-> true

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Win _ -> false


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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(* Infinite-run (cycle) with non-Zeno assumption: method by checking whether the PTA is already a CUB-PTA for some valuation *)
	| NZCycle_check

	(* Infinite-run (cycle) with non-Zeno assumption: method by transforming the PTA into a CUB-PTA *)
	| NZCycle_transform
	
	(* Infinite-run (cycle) with non-Zeno assumption: method assuming the PTA is already a CUB-PTA *)
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

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
	
	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(* Look for the border using the cartography*)
	| Border_cartography _
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _

	(* Parametric reachability preservation *)
	| PRPC _
		-> Exploration_layer_BFS
*)



(************************************************************)
(* Predicates on properties *)
(************************************************************)

(*------------------------------------------------------------*)
(** Does the property support the #witness mode? *)
(*------------------------------------------------------------*)

let supports_witness property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	| Valid -> false


	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
	(* Safety *)
	| AGnot _

	(* Global invariant *)
	| AG _

	(* Exists until *)
	| EU _

		-> true
	
	(*** TODO (2024/02): should probably be easy! ***)
	(* Exists release *)
	| ER _
	(* Exists weak until *)
	| EW _
		-> false


	(* Unavoidability *)
	| AF _
	(* Always release *)
	| AR _
	(* Always until *)
	| AU _
	(* Always weak until *)
	| AW _
		-> false

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		-> true

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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

	(* Infinite-run (cycle) with non-Zeno assumption *)
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

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography _
	
	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography _
	
	(* Look for the border using the cartography*)
	| Border_cartography _
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography _
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography _

	(* Parametric reachability preservation *)
	| PRPC _
		-> false

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Win _ -> true


(*------------------------------------------------------------*)
(** Is the "cumulative pruning" option relevant for this property? *)
(*------------------------------------------------------------*)
let supports_cumulative_pruning property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	| Valid -> false


	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)
	| EF _
	| AGnot _
	| AG _
	| EU _
		-> true

	(*** TODO (2024/02): should probably be easy! ***)
	| ER _
	| EW _
		-> false

	| AF _
	| AR _
	| AU _
	| AW _
		-> false

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		-> true

	(*------------------------------------------------------------*)
	(* Optimized reachability *)
	(*------------------------------------------------------------*)
	| EFpmin _
	| EFpmax _
	| EFtmin _
		-> true


	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	| Cycle_through _
	| Cycle_through_generalized _
	| NZ_Cycle
		-> true


	(*------------------------------------------------------------*)
	(* Deadlock-freeness *)
	(*------------------------------------------------------------*)

	| Deadlock_Freeness
		-> false


	(*------------------------------------------------------------*)
	(* Inverse method, trace preservation, robustness *)
	(*------------------------------------------------------------*)
	| IM _
	| ConvexIM _
	| IMK _
	| IMunion _
	| PRP _
		-> true

	(*------------------------------------------------------------*)
	(* Cartography algorithms *)
	(*------------------------------------------------------------*)

	| Cover_cartography _
	| Learning_cartography _
	| Shuffle_cartography _
	| Border_cartography _
	| Random_cartography _
	| RandomSeq_cartography _
	| PRPC _
		-> false

	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	(*** TODO: double check ***)
	| Win _ -> true


(*------------------------------------------------------------*)
(** Does the property support the #exemplification mode? *)
(*------------------------------------------------------------*)

let supports_exemplification property =
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	| Valid -> false

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _
	
(*	(* Safety *)
	| AGnot _*)

(*	(* Global invariant *)
	| AG _*)

	(* Until *)
	| EU _
		-> true
	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _
		-> true

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _
	
	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _

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

(** Gives a textual description of a property *)
let text_of_property property =
	let synthesis_or_witness = match property.synthesis_type with
		| Exemplification	-> "exemplification"
		| Synthesis			-> "synthesis"
		| Witness			-> "witness"
	in
	
	match property.property with
	(*------------------------------------------------------------*)
	(* Basic properties *)
	(*------------------------------------------------------------*)
	| Valid -> "validity " ^ synthesis_or_witness

	(*------------------------------------------------------------*)
	(* Non-nested CTL *)
	(*------------------------------------------------------------*)

	(* Reachability *)
	| EF _ -> "reachability " ^ synthesis_or_witness
	
	(* Safety *)
	| AGnot _ -> "safety " ^ synthesis_or_witness
	
	(* Global invariant *)
	| AG _ -> "global invariant " ^ synthesis_or_witness

	(* Exists release *)
	| ER _ -> "exists release " ^ synthesis_or_witness

	(* Exists until *)
	| EU _ -> "exists until " ^ synthesis_or_witness

	(* Exists weak until *)
	| EW _ -> "exists weak until " ^ synthesis_or_witness

	(* Unavoidability *)
	| AF _ -> "unavoidability " ^ synthesis_or_witness

	(* Always release *)
	| AR _ -> "always release " ^ synthesis_or_witness

	(* Always until *)
	| AU _ -> "always until " ^ synthesis_or_witness

	(* Always weak until *)
	| AW _ -> "always weak until " ^ synthesis_or_witness

	(*------------------------------------------------------------*)
	(* Non-nested CTL (timed version) *)
	(*------------------------------------------------------------*)
	(* Reachability *)
	| EF_timed _ -> "timed reachability " ^ synthesis_or_witness

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
	
	(* Accepting infinite-run (cycle) through a state predicate *)
	| Cycle_through _ -> "infinite accepting run " ^ synthesis_or_witness

	(* Accepting infinite-run (cycle) through a generalized condition (list of state predicates, and one of them must hold on at least one state in a given cycle) *)
	| Cycle_through_generalized _ -> "infinite generalized accepting run " ^ synthesis_or_witness

	(* Infinite-run (cycle) with non-Zeno assumption *)
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

	(* Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography (_, _, step) -> "behavioral cartography algorithm with full coverage and step " ^ (NumConst.string_of_numconst step) ^ " and using learning-based abstractions"
	
	(* Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography (_, step) -> "behavioral cartography algorithm with full coverage (shuffled version) and step " ^ (NumConst.string_of_numconst step)
	
	(* Look for the border using the cartography*)
	| Border_cartography (_, step) -> "behavioral cartography algorithm with border detection and step " ^ (NumConst.string_of_numconst step)
	
	(* Randomly pick up values for a given number of iterations *)
	| Random_cartography (_, nb, step) -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations and step " ^ (NumConst.string_of_numconst step)
	
	(* Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography (_, nb, step) -> "behavioral cartography algorithm with " ^ (string_of_int nb) ^ " random iterations + sequential phase and step " ^ (NumConst.string_of_numconst step)

	(* Parametric reachability preservation *)
	| PRPC _ -> "parametric reachability preservation cartography"


	(*------------------------------------------------------------*)
	(* Games *)
	(*------------------------------------------------------------*)

	(* Parametric timed game: reachability condition *)
	| Win _ -> "parametric timed game with reachability condition (" ^ synthesis_or_witness ^ ")"


(************************************************************)
(** Get the v0 of a property, if any *)
(************************************************************)
let v0_option_of_property property : AbstractModel.v0 option =
	match property.property with
	| Valid

	| EF _
	| AGnot _
	| AG _
	| ER _
	| EU _
	| EW _
	| AF _
	| AR _
	| AU _
	| AW _
	| EF_timed _
	| EFpmin _
	| EFpmax _
	| EFtmin _
	| Cycle_through _
	| Cycle_through_generalized _
	| PRP _

	| NZ_Cycle

	| Deadlock_Freeness
	| IM _
	| ConvexIM _
	| IMK _
	| IMunion _

	| Win _
		-> None

	| Cover_cartography (v0, _)
	| Learning_cartography (_ , v0, _)
	| Shuffle_cartography (v0, _)
	| Border_cartography (v0, _)
	| Random_cartography (v0, _, _)
	| RandomSeq_cartography (v0, _, _)
	| PRPC (_, v0, _)

		-> Some v0


(************************************************************)
(** Get the list of state predicates defined in a property (or [] if none) *)
(************************************************************)
let state_predicates_of_property property : AbstractProperty.state_predicate list =
	match property.property with
	| EF state_predicate
	| AGnot state_predicate
	| AG state_predicate
	| AF state_predicate
	| EFpmin (state_predicate , _)
	| EFpmax (state_predicate , _)
	| EFtmin state_predicate
	| EF_timed (_, state_predicate)
	| Cycle_through state_predicate
	| PRP (state_predicate, _)
	| Win (state_predicate)
		-> [state_predicate]

	| ER (state_predicate_1, state_predicate_2)
	| EU (state_predicate_1, state_predicate_2)
	| EW (state_predicate_1, state_predicate_2)
	| AR (state_predicate_1, state_predicate_2)
	| AU (state_predicate_1, state_predicate_2)
	| AW (state_predicate_1, state_predicate_2)
		-> [state_predicate_1; state_predicate_2]

	| Cycle_through_generalized state_predicate_list
		-> state_predicate_list

	| Valid

	| NZ_Cycle
	| Deadlock_Freeness
	| IM _
	| ConvexIM _
	| IMK _
	| IMunion _

	| Cover_cartography _
	| Learning_cartography _
	| Shuffle_cartography _
	| Border_cartography _
	| Random_cartography _
	| RandomSeq_cartography _
	| PRPC _

		-> []