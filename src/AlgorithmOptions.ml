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
 * Last modified     : 2020/08/25
 *
 ************************************************************)


(************************************************************)
(* Internal modules *)
(************************************************************)
open AbstractProperty


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
	

	(*------------------------------------------------------------*)
	(* Cycles *)
	(*------------------------------------------------------------*)
	
	(** Infinite-run (cycle) *)
	| Cycle

	(** Accepting infinite-run (cycle) *)
	| Accepting_cycle _
	
		-> true

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

