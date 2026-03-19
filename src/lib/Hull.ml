(************************************************************
 *                       IMITATOR
 * Hull heuristic policy — see Hull.mli
 ************************************************************)

open ImitatorUtilities
open LinearConstraint
open AbstractAlgorithm

type t = {
	counts            : (int, int) Hashtbl.t;  (* location_index -> hull_count *)
	last_coarse_hull  : (int, int) Hashtbl.t;  (* location_index -> hull_count at last coarse hull *)
	coarse_thresholds : (int, int) Hashtbl.t;  (* location_index -> current per-location coarse threshold *)
}

let create () = {
	counts            = Hashtbl.create 16;
	last_coarse_hull  = Hashtbl.create 16;
	coarse_thresholds = Hashtbl.create 16;
}

let apply policy ~options ~location_index ~representative ~incoming =
	let count = try Hashtbl.find policy.counts location_index with Not_found -> 0 in
	Hashtbl.replace policy.counts location_index (count + 1);

	let simplify_mode_name = match options#hull_simplify_mode with
		| Hull_simplify_none        -> "none"
		| Hull_simplify_constraints -> "constraints"
		| Hull_simplify_generators  -> "generators"
	in

	(* Periodic simplification: apply every hull_simplify_period operations.
	   Disabled when hull_simplify_period = 0 or mode = none. *)
	let should_simplify =
		options#hull_simplify_mode <> Hull_simplify_none
		&& options#hull_simplify_period > 0
		&& (count + 1) mod options#hull_simplify_period = 0
	in

	let do_simplify c =
		print_message Verbose_low ("[Hull] location=" ^ string_of_int location_index
			^ " hull_count=" ^ string_of_int (count + 1)
			^ " (period=" ^ string_of_int options#hull_simplify_period ^ ")"
			^ ": simplifying via " ^ simplify_mode_name);
		match options#hull_simplify_mode with
		| Hull_simplify_none        -> c
		| Hull_simplify_constraints -> px_simplify_via_constraints c
		| Hull_simplify_generators  -> px_simplify_via_generators c
	in

	(* Apply coarse hull (box or octagonal) with per-location oscillation tracking.
	   Always called for hybrid modes; the threshold check is inside. *)
	let oscillation_threshold = 5 in
	let apply_coarse_hull c coarse_fn method_name =
		let n = px_nb_constraints c in
		let threshold =
			try Hashtbl.find policy.coarse_thresholds location_index
			with Not_found -> options#hull_abstraction_threshold
		in
		if n > threshold then (
			(match Hashtbl.find_opt policy.last_coarse_hull location_index with
			| Some prev_count when (count + 1) - prev_count <= oscillation_threshold ->
				let new_threshold = threshold * 2 in
				Hashtbl.replace policy.coarse_thresholds location_index new_threshold;
				print_message Verbose_low ("[Hull] OSCILLATION at location=" ^ string_of_int location_index
					^ ": " ^ method_name ^ " applied at hull_count=" ^ string_of_int prev_count
					^ " and again at hull_count=" ^ string_of_int (count + 1)
					^ " (gap=" ^ string_of_int ((count + 1) - prev_count)
					^ " <= oscillation_threshold=" ^ string_of_int oscillation_threshold ^ ")"
					^ " — raising coarse_threshold " ^ string_of_int threshold
					^ " -> " ^ string_of_int new_threshold)
			| _ -> ()
			);
			Hashtbl.replace policy.last_coarse_hull location_index (count + 1);
			print_message Verbose_low ("[Hull] location=" ^ string_of_int location_index
				^ " hull_count=" ^ string_of_int (count + 1)
				^ " constraints=" ^ string_of_int n
				^ " > coarse_threshold=" ^ string_of_int threshold
				^ ": applying " ^ method_name);
			coarse_fn [c]
		) else
			c
	in

	match options#hull_method with

	(* Pure modes: no convex hull; compute coarse hull of both inputs directly *)
	| Hull_box_only ->
		print_message Verbose_low ("[Hull] location=" ^ string_of_int location_index
			^ " hull_count=" ^ string_of_int (count + 1)
			^ ": box only");
		px_box_hull [representative; incoming]

	| Hull_octagonal_only ->
		print_message Verbose_low ("[Hull] location=" ^ string_of_int location_index
			^ " hull_count=" ^ string_of_int (count + 1)
			^ ": octagonal only");
		px_octagonal_hull [representative; incoming]

	(* Convex-only: exact hull, then periodic simplification, no coarse hull *)
	| Hull_convex_only ->
		px_convex_hull_assign representative incoming;
		if should_simplify then do_simplify representative
		else representative

	(* Hybrid modes: exact hull, periodic simplification, then always check coarse threshold *)
	| Hull_box_hybrid ->
		px_convex_hull_assign representative incoming;
		let c = if should_simplify then do_simplify representative else representative in
		apply_coarse_hull c px_box_hull "box hull"

	| Hull_octagonal_hybrid ->
		px_convex_hull_assign representative incoming;
		let c = if should_simplify then do_simplify representative else representative in
		apply_coarse_hull c px_octagonal_hull "octagonal hull"
