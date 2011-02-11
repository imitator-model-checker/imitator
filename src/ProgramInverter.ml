open Global
open AbstractImitatorFile
open Ppl_ocaml
open ImitatorPrinter
open Gmp
open Gmp.Q.Infixes
open LinearConstraint


(* Functions for inverting discrete updates *)
let len = Array.length and aini = Array.init

(* gloabl reference to the offset for the first considered discrete variable *)
let var_offset = ref 0

let one = Q.from_int 1
let zero = Q.zero
let abs q = if (Q.sgn q < 0) then Q.neg q else q 

let print mat vec =
	let i = len mat and j = len mat.(0) in
	print_newline ();
  for i = 0 to i-1 do
		for j = 0 to j-1 do
			let x = mat.(i).(j) in
			if x =/ one then
				print_string "1"
			else if x =/ zero then
				print_string "0"
			else ( 
				print_string (Gmp.Q.to_string mat.(i).(j));
			);
			print_char ' '			
		done;
		print_string "    | ";
		print_string (Gmp.Q.to_string vec.(i));
  	print_newline ()
	done

let zero_vector = 
	fun _ -> NumConst.zero 

let unit_matrix n = 
	let mat = Array.make_matrix n n zero in
	for i = 0 to (n-1) do
		mat.(i).(i) <- one
	done;
	mat

let negate_vector =	Array.map Q.neg


(* build the row of a nxn matrix from a linear term *)
let make_row n term =
	let const_coeff = LinearConstraint.evaluate_linear_term zero_vector term in
	(* function for term evaluation, considers the offset to the first variable *)
  let unit_vector i = 
		fun j -> if j = (i + !var_offset) then NumConst.one else NumConst.zero in
	let row = Array.make n zero in
	for i = 0 to (n-1) do
		let a = LinearConstraint.evaluate_linear_term (unit_vector i) term in
		let a_minus_const = NumConst.sub a const_coeff in
		let q = NumConst.mpq_of_numconst a_minus_const in
		row.(i) <- q
	done;
	row


(* construct a (square) nxn matrix and a vector from a list of linear assignments,  *)
(* where an assignment is a pair of a variable index and a linear term *)
let make n assignments =	
	let mat = unit_matrix n in
	let vec = Array.make n Gmp.Q.zero in
	let insert_row = fun assignment -> (
		(* build row *)
		let v, term = assignment in
		let row = make_row n term in
		mat.(v - !var_offset) <- row;
		(* get constant offset *)
		let coef = NumConst.mpq_of_numconst (LinearConstraint.evaluate_linear_term zero_vector term) in
		vec.(v - !var_offset) <- coef
	) in
	List.iter insert_row assignments;	
	(mat, vec) 


(* construct a linear term from a row and a constant *)
let make_term row coef =
	(* build a list of coefficients and variables for non-zero entries *)
	let coefs = ref [] in	
	Array.iteri (fun i a -> 	
		if a <>/ zero then
			let coef = (NumConst.numconst_of_mpq a, i + !var_offset) in
			coefs := coef :: !coefs
	) row;
	LinearConstraint.make_linear_term !coefs (NumConst.numconst_of_mpq coef)


(* convert an affine mapping to a list of variable updates *)
let make_updates mat vec =
	let updates = ref [] in
	Array.iteri (fun i row ->
		let update = make_term row vec.(i) in
		(* drop identities *)
		if not (update = Pl (Var (i + !var_offset), Coef NumConst.zero)) then
		  updates := (i + !var_offset, update) :: !updates
	) mat;
	!updates


(* multiply a nxn matrix with a vector *)
let mult mat vec =
	let n = Array.length mat in
	let new_vec = Array.make n Gmp.Q.zero in 
	for i = 0 to n-1 do
		let row = mat.(i) in
		let sum = ref Gmp.Q.zero in
		for j = 0 to n-1 do
			sum := !sum +/ vec.(j) */ row.(j)
		done;
		new_vec.(i) <- !sum
	done;
	new_vec	
			
			
(* invert a matrix *)
exception Singular
let invert mat =
	let l = len mat in
	let am = Array.mapi (fun m row -> (Array.append row
	      (aini l (fun n -> if m=n then one else zero)))) mat in
	for i = 0 to l-1 do (
		let im = ref 0 and mv = ref (abs am.(i).(i)) in
	  for j = i+1 to l-1 do (
			let ae = abs am.(j).(i) in
	  	if (!mv < ae) then (mv := ae; im := j)
		) done;
	  if !mv = zero then raise Singular;
	  if !im > i then (
			for n = i to (2*l - 1) do
	      (let s = am.(i).(n) in am.(i).(n) <- am.(!im).(n); am.(!im).(n) <- s)
			done
		);
	  let r = one // am.(i).(i) in
	  for j = i to 2*l - 1 do (
			am.(i).(j) <- r */ am.(i).(j)
		) done;
	  for k = i+1 to l-1 do (
			let f = am.(k).(i) in
	    for j = i+1 to 2*l - 1 do (
				am.(k).(j) <- am.(k).(j) -/ f */ am.(i).(j))
	    done);
		done)
	done;
	for i = 0 to l-1 do (
		for j = i+1 to l-1 do (
			let p = am.(i).(j) in
	    for k = i+1 to 2*l - 1 do
	    	(am.(i).(k) <- am.(i).(k) -/ am.(j).(k) */ p)
			done)
		done)
	done;
	Array.map (fun row -> Array.sub row l l) am


(* invert a list of discrete variable updates *)
let invert_discrete_updates variables assignments =
	(* get the number of variables *)
	let n = List.length variables in
	(* if no variables present, return empty list *)
	if n = 0 then [] else (
		(* set the offset to the first variable *)
		var_offset := List.hd variables;
		print_message Debug_total ("first discrete variable index: " ^ (string_of_int !var_offset));
		print_message Debug_total ("number of discrete variables : " ^ (string_of_int n));
		(* convert the assignments to an affine mapping (A,B) *)
		print_message Debug_total "build affine mapping from update:";
		let mat, vec = make n assignments in
		if debug_mode_greater Debug_total then print mat vec;
		(* invert the matrix A *)
		print_message Debug_total "invert matrix";
		let inv_mat = invert mat in
		(* negate the vector B *)
		let neg_vec = negate_vector vec in
		(* multiply negated B with A^-1 *)
		let inv_vec = mult inv_mat neg_vec in 		
		(* convert the affine mapping (A',B') back to variable updates *)
		if debug_mode_greater Debug_total then (
			print_message Debug_total "inverse affine mapping:";
			print inv_mat inv_vec
		);
		make_updates inv_mat inv_vec
	)


(* Invert a flow constraint *)
let invert_flow vars_to_invert flow =
	let negate_vars = fun v -> (
		if (List.mem v vars_to_invert) then
			Unary_Minus (Variable v)
		else
			Variable v
	) in
	LinearConstraint.substitute negate_vars flow 	


(* Invert the standard flow for clocks *)
let invert_standard_flow program =
	let flow = program.standard_flow in
	let clocks = program.clocks in
	invert_flow program.renamed_clocks flow


(* Invert the flows for analog variables *)
let invert_flows program = 
	(* get the variables to invert *)
	let analogs = List.filter (program.is_analog) program.clocks in
	let analogs_prime = List.map program.prime_of_variable analogs in
	if (analogs = []) then (
		(* always return the empty flow *)
		fun _ _ -> Undefined
	) else (
		(* create an empty array *)
		let new_flow_array = Array.create program.nb_automata (Array.create 0 Undefined) in
		(* For each automaton *) 
		for aut_index = 0 to (program.nb_automata - 1) do		
			let locations = program.locations_per_automaton aut_index in
			let nb_locations = List.length locations in
			let flows = Array.create nb_locations Undefined in
			(* For each location *)
			List.iter (fun loc_index -> 
				let flow = program.analog_flows aut_index loc_index in
				let inv_flow = match flow with 
					| Rectangular constr -> Rectangular (invert_flow analogs_prime constr) 
					| Affine constr -> Affine (invert_flow analogs_prime constr) 
					| Undefined -> Undefined in
				flows.(loc_index) <- inv_flow;
			) locations;
			(* store array *)
			new_flow_array.(aut_index) <- flows
		done;
		(* functional representation *)
	  fun aut_index loc_index -> new_flow_array.(aut_index).(loc_index)
	)

		
(* invert a constraint and compute its update set *)
let invert_constraint program constr =
	(* swap X and X' in constraint *)
	LinearConstraint.rename_variables program.renamed_clocks_couples constr	

(* compute update set for a constraint *)
let get_update_set program constr =
	let support = LinearConstraint.support constr in
	let primes = VariableSet.filter program.is_renamed_clock support in
	let unprimed = VariableSet.fold (fun v vars -> 
		VariableSet.add (program.variable_of_prime v) vars
	) primes VariableSet.empty in
	unprimed			 		
	
	
(* invert the update for a transition *)
let invert_guard_and_update program local_vars guard update =
	(* get all local clocks *)
	let clocks = VariableSet.filter program.is_clock local_vars in 
	(* build normalised constraint and update set *)
	let mu = match update with
		| All_free -> LinearConstraint.true_constraint ()
		| All_stable -> (
				let stable_pairs = List.map (fun v -> (v, program.prime_of_variable v)) (VariableSet.elements local_vars) in
				LinearConstraint.make_equalities stable_pairs
			)
		| Clocks_stable -> (				
				let stable_pairs = List.map (fun v -> (v, program.prime_of_variable v)) (VariableSet.elements clocks) in
				LinearConstraint.make_equalities stable_pairs			
			)
		| Update mu -> mu in
	(* combine with guard *)
	let mu_and_guard = LinearConstraint.intersection [mu; guard] in
	(* invert constraint *)
	let inv_mu = invert_constraint program mu_and_guard in
	(* compute new guard *)
	let inv_guard = LinearConstraint.hide program.renamed_clocks inv_mu in
	(* build new update *)
	if LinearConstraint.is_true inv_mu then
		(inv_guard, All_free)
	else
		(inv_guard, Update inv_mu)
	
	
(* invert one transition, returns a pair (loc, trans) with the new source location *)
(* and the converted transition  *)
let invert_transition program local_vars new_dest transition =
	(* get components, drop destination location *)
	let guard, update, discr_updates, old_dest = transition in
	(* invert discrete updates *)
	print_message Debug_total "invert discrete updates";
	let inv_discr_updates =
		try (
			invert_discrete_updates program.discrete discr_updates
		) with Singular -> (
			raise (InternalError ("discrete updates not reversible (" ^	(string_of_discrete_updates discr_updates) ^ ")"))
		) in 		
	(* invert updates *)
	print_message Debug_total "invert clock updates";
	let inv_guard, inv_update = invert_guard_and_update program local_vars guard update in
	(* assemble new transition *)
	let inv_trans = (inv_guard, inv_update, inv_discr_updates, new_dest) in
	(* return new source location and inverted transition *)
	(old_dest, inv_trans)
	
	
(* invert all transitions and create the index for actions *)
let invert_transitions program =	
	(* create empty array for transitions *)
	let trans_array = Array.make program.nb_automata (Array.make 0 (Array.make 0 [])) in
	let action_array = Array.make program.nb_automata (Array.make 0 []) in
	(* for each automaton *)
	let automata = program.automata in 
	List.iter (fun aut_index -> 
		(* create arrays for this automaton *)
		let locations = program.locations_per_automaton aut_index in
		let nb_locations = List.length locations in
		let nb_actions = program.nb_actions in	
		trans_array.(aut_index) <- Array.make_matrix nb_locations nb_actions [];
		action_array.(aut_index) <- Array.make nb_locations [];
		(* get local variables for this automaton *)
		let local_vars = program.continuous_per_automaton aut_index in
		(* for each location in this automaton *)
		List.iter (fun loc_index ->
			(* for each possible action *)
			let actions = program.actions_per_location aut_index loc_index in
			List.iter (fun act_index -> 			
				let transitions = program.transitions aut_index loc_index act_index in
				(* invert transitions *)
				List.iter (fun transition -> 
					let new_source, inv_trans = invert_transition program local_vars loc_index transition in
					(* store new transition *)
					trans_array.(aut_index).(new_source).(act_index) <- inv_trans :: trans_array.(aut_index).(new_source).(act_index);
					(* register action *)
					action_array.(aut_index).(new_source) <- list_union [act_index] action_array.(aut_index).(new_source);
				) transitions;
			) actions;
		) locations;
	) automata;
	(* return functional representations *)
	let trans_fun = fun aut_i loc_i act_i -> trans_array.(aut_i).(loc_i).(act_i) in
	let action_fun = fun aut_i loc_i -> action_array.(aut_i).(loc_i) in
	(trans_fun, action_fun)
	

(* debug dump *)
let dump program = 
	for aut_i = 0 to (program.nb_automata - 1) do (
		let locations = program.locations_per_automaton aut_i in
		List.iter (fun loc_i -> 
			let actions = program.actions_per_location aut_i loc_i in
			let nb_actions = List.length actions in
			print_message Debug_standard (
				"location " ^ (program.automata_names aut_i) ^
				"." ^ (program.location_names aut_i loc_i) ^ 
				" has " ^ (string_of_int nb_actions) ^
				" registered actions:");
			List.iter (fun act_i -> 
				print_message Debug_standard (
					"action " ^ (program.action_names act_i));
				let transitions = program.transitions aut_i loc_i act_i in				
				List.iter (fun trans -> 
					let guard, update, discr_update, dest_i = trans in
					print_message Debug_standard (
						"  -> " ^ (program.location_names aut_i dest_i));
					print_message Debug_standard (
						"     guard: " ^ (LinearConstraint.string_of_linear_constraint program.variable_names guard));
					let upstr = match update with
						| All_free -> "all free"
						| All_stable -> "all stable"
						| Clocks_stable -> "clocks stable"
						| Update constr -> LinearConstraint.string_of_linear_constraint program.variable_names constr	in
					print_message Debug_standard (
						"     updated variables: " ^ upstr);
						
				) transitions
			) actions   
		) locations
	) done


(* invert all automata in a program *)
let invert program =
	print_message Debug_high "invert standard flow";
	let inv_standard_flow = invert_standard_flow program in
	(* invert flow conditions *)
	print_message Debug_high "invert analog flows";
	let inv_analog_flows = invert_flows program in
	(* invert transitions *)
	print_message Debug_high "invert transitions"; 
	let inv_transitions, new_actions_per_location = invert_transitions program in
	
	(* construct new structure *)
	let inv_program = { program with 
		standard_flow = inv_standard_flow;
		analog_flows = inv_analog_flows;
		transitions = inv_transitions;
		actions_per_location = new_actions_per_location
	} in
	inv_program
