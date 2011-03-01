open Global
open LinearConstraint
open Automaton
open AbstractImitatorFile
open ImitatorPrinter


let string_of_signature =
	List.fold_left (fun s p -> 
		s ^ (if p then "1" else "0")
	) ""
	
exception Different	
	
let signatures_equal b b' = 
	if List.length b <> List.length b' then false else 
	begin
		try (
			List.iter2 (fun p p' -> 
				if p <> p' then raise Different 
			) b b';
			(* reached here, so all entries equal *)
			true
		) with Different -> false
	end 

let string_of_abstract_state (loc, b) =
	let program = Program.get_program () in
	(string_of_location program.automata_names program.location_names program.variable_names loc) ^ " {" ^
	(string_of_signature b) ^ "}"
	

(** converts the abstract signature (list of bool) into a linear constraint,
	 	given the list of predicates *)
let instantiate_predicates preds b =
	let ineqs = List.map2 (fun p v -> 
		if v then	p else LinearConstraint.negate_inequality p			 
	) preds b in
	LinearConstraint.make ineqs
	

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
	(* intersect with invariant and global domain *)
	LinearConstraint.intersection_assign inv [p; program.domain];
	(* return location with constraint *)
	(loc, inv)
	
	
let add_predicate p constr phase =
	let p = if phase then p else LinearConstraint.negate_inequality p in
	let pconstr = LinearConstraint.make [p] in
	LinearConstraint.intersection [constr; pconstr]
	
(* split state space recursively, discarding inconsistent regions *)
let rec split_rec preds constr prefix =
	if not (LinearConstraint.is_satisfiable constr) then [] else
	match preds with
		| [] -> [List.rev prefix]
		| p :: tail -> (
			let tsigs = split_rec tail (add_predicate p constr true ) (true  :: prefix) in
			let nsigs = split_rec tail (add_predicate p constr false) (false :: prefix) in
			tsigs @ nsigs
		)	

let split preds constr =
	split_rec preds constr [] 
	
	
(** converts a concrete state into a list of abstract states *)
let abstract preds (loc, c) =
	let program = Program.get_program () in
	print_message Debug_total ("abstracting state: " ^ string_of_state (loc, c));
	(* get all consistent signatures *)
	let signatures = split preds (LinearConstraint.intersection [c; program.domain]) in
	(* combine with location *)
	List.map (fun b -> (loc, b)) signatures
		
	
(** get all consistent abstract states for a location,
    given a list of predicates *)
let get_abstract_states preds loc =
	let program = Program.get_program () in 
	(* build invariant *)
	let invariants =
		List.map (fun aut_index ->
			let loc_index = Automaton.get_location loc aut_index in  
			program.invariants aut_index loc_index		
		) program.automata in
	let inv = LinearConstraint.intersection invariants in
	(* get consistent abstract states *)
	abstract preds (loc, inv)
	

(** get all feasible regions with respect to a list of predicates *)
let get_feasible_regions preds =
	let program = Program.get_program () in
	(* get all consistent signatures *)
	let signatures = split preds program.domain in
	(* concretize signatures *)
	List.map (instantiate_predicates preds) signatures

