open Global
open LinearConstraint
open Automaton
open AbstractImitatorFile


let string_of_signature =
	List.fold_left (fun s p -> 
		s ^ (if p then "1" else "0")
	) ""

let string_of_abstract_state (loc, b) =
	let program = Program.get_program () in
	(string_of_location program.automata_names program.location_names program.variable_names loc) ^ " {" ^
	(string_of_signature b) ^ "}"
	

(** build all possible predicate signatures *)
let build_signatures preds = 
	let n = List.length preds in
	let rec combis n =
		match n with
			| 0 -> []
			| 1 -> [[true]; [false]]
			| _ -> 
					let tail = combis (n - 1) in
					List.rev_append 
						(List.map (fun b -> true  :: b ) tail)
						(List.map (fun b -> false :: b ) tail) in		
  combis n	
	

(** converts the abstract signature (list of bool) into a linear constraint,
	 	given the list of predicates *)
let instantiate_predicates preds b =
	let ineqs = List.map2 (fun p v -> 
		if v then	p else LinearConstraint.negate_inequality p			 
	) preds b in
	LinearConstraint.make ineqs
	
	
(** build all consistent predicate signatures *)
let build_consistent_signatures preds =
	(* build all signatures *)
	let combis = build_signatures preds in
	(* discard all unsatisfiable combinations *)
	List.filter (fun b -> 
		LinearConstraint.is_satisfiable (instantiate_predicates preds b)
	) combis


(** converts an abstract state into a concrete state *)
let concretize preds (loc, b) =
	let program = Program.get_program () in
	(* build invariant *)
	let invariants =
		List.map (fun aut_index ->
			let loc_index = Automaton.get_location loc aut_index in  
			program.invariants aut_index loc_index		
		) program.automata in
	let inv = LinearConstraint.intersection invariants in	
	(* build predicate instantiation *)
	let p = instantiate_predicates preds b in
	LinearConstraint.intersection_assign inv [p];
	(* return location with constraint *)
	(loc, inv)
	
	
(** get all consistent abstract states for a location,
    given a list of predicates *)
let get_abstract_states preds loc = 
	let combis = build_consistent_signatures preds in
	(* combine signatures with invariant *)
	let invs = List.map (fun b -> 
		let _, inv = concretize preds (loc, b) in
		(b, inv)
	) combis in
	(* discard inconsistent states *)
	List.map (fun (b, _) -> (loc, b)) (
		List.filter (fun (b, inv) -> 
			LinearConstraint.is_satisfiable inv
		) invs
	) 

		
(** compute continuous successors of an abstract state *)
let continuous_successors preds astate =
	let cstate = concretize	preds astate in
	let elapse = Reachability.time_elapse cstate in
	let loc, b = astate in
	let astates = get_abstract_states preds loc in
	List.filter (fun s -> 
		let _, c = concretize preds s in
		LinearConstraint.is_satisfiable (LinearConstraint.intersection [elapse; c])
	) astates

	



