(*****************************************************************
 *
 *                     IMITATOR II
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Author:        Etienne Andre
 * Created:       2009/09/08
 * Last modified: 2010/05/07
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
(* open ExtList *)


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string
exception Found

(** Parsing exception: starting position of the error symbol, ending position of the error symbol *)
exception ParsingError of (int * int)



(****************************************************************)
(** Debug modes *)
(****************************************************************)

type debug_mode =
	| Debug_error (* c'est quoi ca ? *)
	| Debug_nodebug
	| Debug_standard
	| Debug_low
	| Debug_medium
	| Debug_high
	| Debug_total

(* Associate an integer to each debug mode *)
let level_of_debug = function
	| Debug_error -> 0
	| Debug_nodebug -> 0
	| Debug_standard -> 1
	| Debug_low -> 2
	| Debug_medium -> 3
	| Debug_high -> 4
	| Debug_total -> 5

(* The global debug mode *)
type global_debug_mode_type =
	| Debug_mode_not_set
	| Debug_mode_set of debug_mode

let global_debug_mode = ref Debug_mode_not_set

let timed_mode = ref false


(*(* Return true if debug1 >= debug2, false otherwise *)
let debug_mode_greater debug1 debug2 =
	(level_of_debug debug1) >= (level_of_debug debug2)*)

(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
let debug_mode_greater debug_mode =
	(* Get the global debug mode *)
	let global_debug_mode =
	match !global_debug_mode with
	| Debug_mode_not_set -> raise (InternalError ("The debug mode has already been set, impossible to access it."))
	| Debug_mode_set global_debug_mode -> global_debug_mode
	in
	(* Compare *)
	(level_of_debug global_debug_mode) >= (level_of_debug debug_mode)


(* Convert a string into a debug_mode; raise Not_found if not found *)
let debug_mode_of_string debug_mode =
	if debug_mode = "nodebug" then Debug_nodebug
	else if debug_mode = "standard" then Debug_standard
	else if debug_mode = "low" then Debug_low
	else if debug_mode = "medium" then Debug_medium
	else if debug_mode = "high" then Debug_high
	else if debug_mode = "total" then Debug_total
	else raise Not_found

(* Set the debug mode *)
let set_debug_mode debug_mode =
	(*match !global_debug_mode with
	| Debug_mode_not_set -> global_debug_mode := Debug_mode_set debug_mode
	| Debug_mode_set debug_mode -> raise (InternalError ("The debug mode has already been set, impossible to set it again."))*)
	global_debug_mode := Debug_mode_set debug_mode

(* Get the debug mode *)
let get_debug_mode () =
	match !global_debug_mode with
	| Debug_mode_not_set -> raise (InternalError ("The debug mode has not yet been set."))
	| Debug_mode_set debug_mode -> debug_mode


(****************************************************************)
(** Global time counter *)
(****************************************************************)
let counter = ref (Unix.gettimeofday())

(* Compute a duration in ms *)
let duration_of_float d =
	((float_of_int) (int_of_float (d *. 1000.0))) /. 1000.0

(** Get the value of the counter *)
let get_time() =
	(Unix.gettimeofday()) -. (!counter)

(* Compute the duration since time t *)
let time_from t =
	(Unix.gettimeofday()) -. t

(* Print a number of seconds *)
let string_of_seconds nb_seconds =
	let duration = duration_of_float nb_seconds in
	let plural = (if duration <= 1.0 then "" else "s") in
	(string_of_float duration) ^ " second" ^ plural


(* Create a string of the form 'after x seconds', where x is the time since the program started *)
let after_seconds () =
	"after " ^ (string_of_seconds (get_time()))

(** Set the timed mode *)
let set_timed_mode () =
	timed_mode := true


(****************************************************************)
(** Useful functions on lists *)
(****************************************************************)

(* Check if a list is empty *)
let list_empty = function
	| [] -> true
	| _ -> false
	
(* Return a random element in a list *)
let random_element l =
	Random.self_init();
	let nth = Random.int (List.length l) in
	List.nth l nth

(* list_of_interval a b Create a fresh new list filled with elements [a, a+1, ..., b-1, b] *)
let list_of_interval a b =
	(* Check if the interval is valid *)
	if a > b then [] else(
		(* Create an array (more efficient?) *)
		let ar = Array.make (b - a + 1) a in
		(* Fill it (no need to update index 0) *)
		for index = 1 to b - a do
			ar.(index) <- index + a
		done;
		(* Return a list *)
		Array.to_list ar
	)

(* Intersection of 2 lists (keeps the order of the elements as in l1) *)
let list_inter l1 l2 =
	List.filter (fun e -> List.mem e l2) l1

(* Union of 2 lists: add elements of l2 in tail of l1; order is preserved *)
let list_union l1 l2 =
	List.rev (List.fold_left
		(fun l e -> if List.mem e l1 then l else e::l)
		(List.rev l1)
		l2
	)

(* Tail-recursive function for 'append' *)
let list_append l1 l2 =
	ExtList.(@) l1 l2
	

(* Return a list where every element only appears once *)
let list_only_once l =
	List.rev (List.fold_left
		(fun current_list e -> if List.mem e current_list then current_list else e::current_list)
		[]
		l
	)


(* Return a sublist of a list with only the elements existing several times *)
let elements_existing_several_times l =
	let rec elements_existing_several_times_rec elements = function
		| [] -> elements
		| first :: rest ->
			(* Take the elements equal to "first" away from "rest" *)
			let elements_equal_to_first, sub_rest = List.partition (fun a -> a = first) rest in
			match elements_equal_to_first with
			(* Empty list: keep searching *)
			| [] -> elements_existing_several_times_rec elements sub_rest
			(* Non-empty list: add "first" to the list of elements existing several times, and keep searching *)
			| _ -> elements_existing_several_times_rec (first :: elements) sub_rest
	in
	List.rev (elements_existing_several_times_rec [] l)


(****************************************************************)
(** Useful functions on arrays *)
(****************************************************************)

(* Check if an element belongs to an array *)
let in_array e a =
	List.mem e (Array.to_list a)

(* Returns the (first) index of an element in an array, or raise Not_found if not found *)
let index_of e a =
	let length = Array.length a in
	let found = ref false in
	let i = ref 0 in
	while not !found && !i < length do
		if a.(!i) = e then found := true;
		i := !i + 1;
	done;
	if not !found then raise Not_found;
	(* Return the index *)
	(!i - 1)
	
(* Return the list of the indexes whose value is true *)
let true_indexes a =
	let list_of_indexes = ref [] in
	Array.iteri (fun index value ->
		if value then (list_of_indexes := index :: !list_of_indexes);
	) a;
	List.rev (!list_of_indexes)


(* Shuffle an array *)
(* let shuffle_array = Array.sort (fun _ _ -> (Random.int 3) - 1) *)

(** exists p {a1; ...; an} checks if at least one element of the Array satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
let array_exists p a =
	try(
		Array.iter (fun 
			elem -> if p elem then (raise Found);
		) a;
		(* Not found *)
		false
	) with Found -> true


(****************************************************************)
(** Useful functions on dynamic arrays *)
(****************************************************************)

(* exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
let dynArray_exists p a =
	try(
		DynArray.iter (fun 
			elem -> if p elem then (raise Found);
		) a;
		(* Not found *)
		false
	) with Found -> true


(****************************************************************)
(** Useful functions on string *)
(****************************************************************)
(* Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""

(* Convert a list of string into a string *)
let string_of_list_of_string =
	List.fold_left (fun the_string s -> the_string ^ s) ""

(* Convert an array of string into a string with separators *)
let string_of_array_of_string_with_sep sep a =
	let length = Array.length a in
	if length = 0 then "" else(
		let the_string = ref "" in
		for i = 0 to length - 2 do
			the_string := (!the_string) ^ a.(i) ^ sep
		done;
		!the_string ^ a.(length - 1)
	)

(* Convert a list of string into a string with separators *)
let string_of_list_of_string_with_sep sep l =
	string_of_array_of_string_with_sep sep (Array.of_list l)


(* 's_of_int i' Return "s" if i > 1, "" otherwise *)
let s_of_int i =
	if i > 1 then "s" else ""

(****************************************************************)
(** Useful functions on booleans *)
(****************************************************************)
(* Evaluate both part of an 'and' comparison and return the conjunction *)
let evaluate_and a b =
(*	let computed_a = a in
	let computed_b = b in
	computed_a && computed_b*)
	a && b

(* Evaluate both part of an 'or' comparison and return the disjunction *)
let evaluate_or a b =
	a || b


(****************************************************************)
(** Messages *)
(****************************************************************)
(* Print a string *)
let print_message_generic message =
	(* Timed mode *)
	let time_info =
		if !timed_mode then (" (at t = " ^ (string_of_seconds (get_time())) ^ ")")
		else "" in
	(* Print message *)
	print_string (message ^ time_info ^ "\n");
	(* Flush! *)
	flush stdout


(* Print a message if global_debug_mode >= message_debug_mode *)
let print_message message_debug_mode message =
	(* Only print the message if its message_debug_mode is smaller or equal to the global_debug_mode *)
	if debug_mode_greater message_debug_mode then
		(* Compute the debug level *)
		let debug_level = level_of_debug message_debug_mode in
		(* Find number of blanks for indentation *)
		let nb_spaces = if debug_level-1 > 0 then debug_level-1 else 0 in
		(* Create blanks proportionnally to the debug_level (at least one space) *)
		let spaces = " " ^ string_of_array_of_string (Array.make nb_spaces "   ") in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
		(* Print *)
		print_message_generic formatted_message



(* Print a warning *)
let print_warning message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** Warning: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print *)
	print_message_generic formatted_message


(* Print an error *)
let print_error message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** ERROR: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print *)
	print_message_generic formatted_message


(****************************************************************)
(** Terminating functions *)
(****************************************************************)

(* Abort program *)
let abort_program () =
	print_error ("Program aborted (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush stdout;
	exit(0)

(* Terminate program *)
let terminate_program () =
	print_newline();
	print_message Debug_standard ("IMITATOR II successfully terminated (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush stdout;
	exit(0)

