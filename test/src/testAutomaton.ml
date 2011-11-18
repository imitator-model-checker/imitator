open OUnit
open Automaton

let nb_automata = 2;;
let min_discrete = 4;;
let max_discrete = 6;;
let nb_discrete = max_discrete - min_discrete + 1;;

let of_int = NumConst.numconst_of_int;;

let automata_names = fun i -> "aut" ^ (string_of_int i);;
let location_names = fun auto loc -> "loc" ^ (string_of_int loc);;
let discrete_names = fun i -> if i = 0 then "i" else if i = 1 then "j" else ("k" ^ (string_of_int i));;

Automaton.initialize nb_automata min_discrete max_discrete;;

let location1 = Automaton.make_location [0, 0; 1, 2] [4, of_int 2];;
let location2 = Automaton.update_location [0, 3] [4, of_int 1; 6, of_int 1] location1;;

let location1_name = "aut0: loc0, aut1: loc2, k4 = 2, k5 = 0, k6 = 0"
let location2_name = "aut0: loc3, aut1: loc2, k4 = 1, k5 = 0, k6 = 1"


let test_locations _ =
	let build_location_name1 = (Automaton.string_of_location automata_names location_names discrete_names location1) in
	let build_location_name2 = (Automaton.string_of_location automata_names location_names discrete_names location2) in
	
	print_string build_location_name1;
	print_string build_location_name2;
		  
	assert_equal 
		build_location_name1 location1_name;
	assert_equal 
		build_location_name2 location2_name

(** Build testsuite **)

let suite = "testAutomaton" >::: [
		"test_locations" >:: test_locations]


