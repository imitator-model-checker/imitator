(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Cache
 * 
 * File contributors : Ulrich Kühne, Étienne André
 * Created           : 2010 (?)
 *
 ************************************************************)

 

(************************************************************)
(* Internal modules *)
(************************************************************)
open ImitatorUtilities
open Statistics


(************************************************************)
(** Statistics *)
(************************************************************)
let counter_hits = create_discrete_counter_and_register "cache hits" Cache_counter Verbose_low
let counter_misses = create_discrete_counter_and_register "cache misses" Cache_counter Verbose_low
let counter_collisions = create_discrete_counter_and_register "cache collisions" Cache_counter Verbose_low


type ('a, 'b) t = {
	mutable size  : int;
	mutable table : (int, 'a * 'b) Hashtbl.t;
	mutable hash  : 'a -> int;
	
	(* statistics *)
(*	mutable hits : int;
	mutable misses : int;
	mutable collisions : int;*)
}


let make hash_fun max_size =
	{
		size = max_size;
		table = Hashtbl.create 0;
		hash = hash_fun;
(*		hits = 0;
		misses = 0;
		collisions = 0;*)
	}
	
	
let flush cache =
	Hashtbl.clear cache.table
(*	cache.hits <- 0;
	cache.misses <- 0;
	cache.collisions <- 0*)


let resize cache new_size =
	flush cache;
	cache.size <- new_size
		
		
let find cache key =
	if cache.size = 0 then None else
	let h = (cache.hash key) mod cache.size in
	try (                                                                    
		let stored_key, item = Hashtbl.find cache.table h in                    
		if key = stored_key then (
			(* Statistics *)
			counter_hits#increment;
(* 			cache.hits <- cache.hits + 1;  *)
			Some item 
		) else (
			(* Statistics *)
			counter_collisions#increment;
(* 			cache.collisions <- cache.collisions + 1;  *)
			None  
		)          
	) with Not_found -> (
			(* Statistics *)
			counter_misses#increment;
(* 			cache.misses <- cache.misses + 1;  *)
			None
		) 
		

let store cache key item =
	if cache.size = 0 then () else
	let h = (cache.hash key) mod cache.size in
	Hashtbl.replace cache.table h (key, item)
	
	
(*let print_stats cache =
	let fill_rate = 100.0 *. (float_of_int (Hashtbl.length cache.table)) /. (float_of_int cache.size) in
	print_message Verbose_standard ("filled: " ^ (string_of_float fill_rate) ^ "%");
	print_message Verbose_standard ("hits  : " ^ (string_of_int cache.hits));
	print_message Verbose_standard ("misses: " ^ (string_of_int cache.misses));
	print_message Verbose_standard ("coll. : " ^ (string_of_int cache.collisions))*)
	