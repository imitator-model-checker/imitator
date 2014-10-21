(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2009/09/08
 * Last modified: 2014/10/21
 *
 ****************************************************************)
 
 
(****************************************************************)
(** Modules *)
(****************************************************************)

open Gc
open Unix
(*  open Printf  *)
open DynArray
 

(****************************************************************)
(** Constants *)
(****************************************************************)

let program_name = "IMITATOR"
let version_string = "2.6.2"

let print_version_string () = 
	print_string (program_name ^ " " ^ version_string ^ "\n")


(* Extension for input model files *)
let model_extension = ".imi"



let print_contributors()  = 
	print_version_string();
	print_string " This tool has been developed by:\n";
	print_string " * Etienne Andre       (2008 -     )\n";
	print_string " * Camille Coti        (2014 -     )\n";
	print_string " * Daphne Dussaud      (2010)\n";
	print_string " * Sami Evangelista    (2014 -     )\n";
	print_string " * Ulrich Kuehne       (2010 - 2011)\n";
	print_string " * Nguyen Hoang Gia    (2014 -     )\n";
	print_string " * Romain Soulat       (2010 - 2013)\n";
	print_string "\n";
	print_string " Moral support and suggestions by:\n";
	print_string " * Emmanuelle Encrenaz\n";
	print_string " * Laurent Fribourg\n";
	print_string " * Giuseppe Lipari\n";
	()


(****************************************************************)
(** Exceptions *)
(****************************************************************)
exception InternalError of string
exception SerializationError of string
exception Found
exception InvalidModel
exception UnexpectedToken of char

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

(* set to standard by default *)
let global_debug_mode = ref (Debug_mode_set Debug_standard)

let timed_mode = ref false


(* Get the debug mode *)
let get_debug_mode () =
	match !global_debug_mode with
	| Debug_mode_not_set -> raise (InternalError ("The debug mode has not yet been set."))
	| Debug_mode_set debug_mode -> debug_mode


(* Set the debug mode *)
let set_debug_mode debug_mode =
	(*match !global_debug_mode with
	| Debug_mode_not_set -> global_debug_mode := Debug_mode_set debug_mode
	| Debug_mode_set debug_mode -> raise (InternalError ("The debug mode has already been set, impossible to set it again."))*)
	global_debug_mode := Debug_mode_set debug_mode


(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
let debug_mode_greater debug_mode =
	(* Get the global debug mode *)
	let global_debug_mode = get_debug_mode() in
(*	match !global_debug_mode with
	| Debug_mode_not_set -> raise (InternalError ("The debug mode has not been set, impossible to access it."))
	| Debug_mode_set global_debug_mode -> global_debug_mode
	in*)
	(* Compare *)
	(level_of_debug global_debug_mode) >= (level_of_debug debug_mode)


(* Convert a string into a debug_mode; raise Not_found if not found *)
let debug_mode_of_string debug_mode =
	if debug_mode = "mute" then Debug_nodebug
	else if debug_mode = "standard" then Debug_standard
	else if debug_mode = "low" then Debug_low
	else if debug_mode = "medium" then Debug_medium
	else if debug_mode = "high" then Debug_high
	else if debug_mode = "total" then Debug_total
	else raise Not_found



(****************************************************************)
(** Global types *)
(****************************************************************)

(** Mode for IMITATOR *)
type imitator_mode =
	(** Translation to another language: no analysis *)
	| Translation
	(** Classical state space exploration *)
	| State_space_exploration
	(** EF-synthesis *)
	| EF_synthesis
	(** Classical inverse method *)
	| Inverse_method
	(** Cover the whole cartography *)
	| Cover_cartography
	(** Look for the border using the cartography*)
	| Border_cartography
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of int



(****************************************************************)
(** Global time counter *)
(****************************************************************)
let counter = ref (Unix.gettimeofday())





(****************************************************************)
(** Useful functions on lists *)
(****************************************************************)

(* Check if a list is empty *)
let list_empty l =
	l = []

(* Return a random element in a list *)
let random_element l =
	Random.self_init();
	let nth = Random.int (List.length l) in
	List.nth l nth

(** list_of_interval l u Create a fresh new list filled with elements [l, l+1, ..., u-1, u] *)
let rec list_of_interval l u =
	if ( l > u )
		then []
	else l :: (list_of_interval ( l + 1 ) u)
(*	(* Check if the interval is valid *)
	if a > b then [] else(
		(* Create an array (more efficient?) *)
		let ar = Array.make (b - a + 1) a in
		(* Fill it (no need to update index 0) *)
		for index = 1 to b - a do
			ar.(index) <- index + a
		done;
		(* Return a list *)
		Array.to_list ar
	)*)

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


(** Returns l1 minus l2, with assumption that all elements of l1 are different *)
let list_diff (l1 : 'a list) (l2 : 'a list) : 'a list =
(* 	print_message Debug_standard ("List diff : [" ^ (string_of_int (List.length l1)) ^ "] \ [" ^ (string_of_int (List.length l2)) ^ "]"); *)
	(* Optimize a little *)
	if l2 = [] then l1
	else (if l1 = [] then []
	else
		List.filter (fun elt -> not (List.mem elt l2)) l1
		(* NOTE: surprisingly much less efficient (some times 4 times slower!) to do the n log(n) solution below rather than the n2 solution above *)
(*		let set_of_list l =
			List.fold_left (fun set elt -> IntSet.add elt set) IntSet.empty l
		in
		(* Convert l1 *)
		let s1 = set_of_list l1 in
	(*	(* Convert l2 *)
		let s2 = set_of_list l2 in
		(* Performs set difference *)
		let set_diff = IntSet.diff s1 s2 in*)
		(* Remove elements from l2 *)
		let set_diff =
			List.fold_left (fun set elt -> IntSet.remove elt set) s1 l2
		in
		(* Return elements *)
		IntSet.elements set_diff
*)
	)


(* Tail-recursive function for 'append' *)
let list_append l1 l2 =
	ExtList.(@) l1 l2
	

(* Return a list where every element only appears once *)
(** WARNING: exponential here *)
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


(* Remove the first occurence of element e in list l ; returns the list unchanged if not found *)
let rec list_remove_first_occurence e = function
	| [] -> []
	| first :: rest -> if e = first then rest
		else first :: (list_remove_first_occurence e rest)


(** Remove the ith element of a list *)
let list_delete_at i al =
	(* First check the arguments *)
	if i < 0 then raise (Invalid_argument "list_delete_at");
	if i >= List.length al  then raise (Failure "list_delete_at");
	let rec del i = function
		| [] -> []
		| h::t when i = 0 -> t
		| h::t -> h :: del (i - 1) t
	in
	del i al

(** Replace the ith element of a list *)
let list_set_nth i elem l =
	(* First check the arguments *)
	if i < 0 then raise (Invalid_argument "list_set_nth");
	if i >= List.length l  then raise (Failure "list_set_nth");
	let rec set i elem = function
		| [] -> []
		| h::t when i = 0 -> elem::t
		| h::t -> h :: set (i - 1) elem t
	in
	set i elem l


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


(** Shuffles the values of an array *)
(*** NOTE: important, otherwise always the same "random" ! ***)
;;Random.self_init();;
(*** NOTE: Found online at http://www.codecodex.com/wiki/index.php?title=Shuffle_an_array ***)
(*** WARNING: not a real shuffle! the first element is always at the end... ***)
let array_shuffle a = Array.sort (fun _ _ -> (Random.int 3) - 1) a



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

(* Returns a fresh string made of 'n' times 's' *)
let string_n_times n s =
	string_of_array_of_string (Array.make n s)

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


(* Returns a list of substrings splitted using sep *)
let split sep = Str.split (Str.regexp ("[" ^ sep ^ "]"))


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
(** Useful functions on floats *)
(****************************************************************)
(*
(** round_n n f rounds float f with n decimal digits *)
let round_n n f =
	let factor = 10. ** (float_of_int n) in
(*	print_float factor;
	exit 0;*)
	(float_of_int (int_of_float (floor  (f *. factor)))) /. factor*)


(****************************************************************)
(** Printing time functions *)
(****************************************************************)
let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
				"Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(* 'add_digits n i' adds (m-n) '0' in front of 'i', if 'i' is an integer with only 'm' digits; result is always a string *)
let add_digits n i =
	(* Convert to string *)
	let str_i = string_of_int i in
	(* Count the number of digits *)
	let size_i = String.length str_i in
	(
		(* Add more *)
		if size_i <= n then
			(string_n_times (n - size_i) "0")
		(* Otherwise keep unchanged *)
		else ""
	) ^ str_i


(* Adds a zero if a number has only 1 digit *)
let two_digits = add_digits 2
(*	(* Add a 0 if needed *)
	(if i <= 9 then "0" else "")
	^ (string_of_int i)*)

let format_time time =
  let tm = localtime time in
(*  sprintf "%s %s %2d %02d:%02d:%02d %04d"
    days.(tm.tm_wday)
    months.(tm.tm_mon)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
    (tm.tm_year + 1900)*)
    (days.(tm.tm_wday))
    ^ " " ^ (months.(tm.tm_mon))
    ^ " " ^ (string_of_int tm.tm_mday)
    ^ ", " ^ (string_of_int (tm.tm_year + 1900))
    ^ " " ^ (two_digits tm.tm_hour)
    ^ ":" ^ (two_digits tm.tm_min)
    ^ ":" ^ (two_digits tm.tm_sec)
 
(*let time = fst (Unix.mktime {tm_sec=50; tm_min=45; tm_hour=3;
		tm_mday=18; tm_mon=0; tm_year=73;
		tm_wday=0; tm_yday=0; tm_isdst=false})*)

(* Print the current date and time under the form of a string *)
let now () = "" ^ (format_time (Unix.gettimeofday ()))
(* printf "format_time gives: %s\n" (format_time time) *)


(* Round a float with 3 digits after comma, and convert to string *)
let round3_float d =
(* 	((float_of_int) (int_of_float (d *. 1000.0))) /. 1000.0 *)
	(* Integer part *)
	let int_part = string_of_int (int_of_float (floor d)) in
	(* Floating part on 3 digits *)
	let real_part = add_digits 3 ((int_of_float (d *. 1000.0)) mod 1000) in
	(* Concatenate both *)
	int_part ^ "." ^ real_part

(** Get the value of the counter *)
let get_time() =
	(Unix.gettimeofday()) -. (!counter)

(* Compute the duration since time t *)
let time_from t =
	(Unix.gettimeofday()) -. t

(* Print a number of seconds *)
let string_of_seconds nb_seconds =
	let duration = round3_float nb_seconds in
	let plural = (if nb_seconds <= 1.0 then "" else "s") in
	duration ^ " second" ^ plural


(* Create a string of the form 'after x seconds', where x is the time since the program started *)
let after_seconds () =
	"after " ^ (string_of_seconds (get_time()))

(** Set the timed mode *)
let set_timed_mode () =
	timed_mode := true



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
	flush Pervasives.stdout


(* Print a message if global_debug_mode >= message_debug_mode *)
let print_message message_debug_mode message =
	(* Only print the message if its message_debug_mode is smaller or equal to the global_debug_mode *)
	if debug_mode_greater message_debug_mode then
		(* Compute the debug level *)
		let debug_level = level_of_debug message_debug_mode in
		(* Find number of blanks for indentation *)
		let nb_spaces = if debug_level-1 > 0 then debug_level-1 else 0 in
		(* Create blanks proportionnally to the debug_level (at least one space) *)
		let spaces = " " ^ (string_n_times nb_spaces "   ") in
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






let header_string =
	(* Build info *)
	let build_info = "Build: " ^ BuildInfo.build_number ^ " (" ^ BuildInfo.build_time ^ ")" in
	
	"************************************************************\n"
	^ "*  " ^ program_name ^ " " ^ version_string ^ (string_n_times (46 - (String.length version_string)) " ") ^ " *\n"
	^ "*                                                          *\n"
	^ "*                     Etienne Andre, Ulrich Kuehne et al.  *\n"
	^ "*                                             2009 - " ^ (BuildInfo.build_year) ^ "  *\n"
	^ "*                       LSV, ENS de Cachan & CNRS, France  *\n"
	^ "*  Universite Paris 13, Sorbonne Paris Cite, LIPN, France  *\n"
	^ "*                                                          *\n"
	^ "*  " ^ (string_n_times (55 - (String.length build_info)) " ") ^ build_info ^ " *\n"
	^ "************************************************************"



(**************************************************)
(** System functions *)
(**************************************************)

let write_to_file file_name file_content =
	let oc = open_out file_name in
	(* Write file *)
	output_string oc file_content;
	(* Close channel *)
	close_out oc;
	()


(* Delete a file, and print a message if not found *)
let delete_file file_name =
	try (
		(* Delete the file *)
		Sys.remove file_name;
		(* Confirm *)
		print_message Debug_total ("Removed file " ^ file_name ^ " successfully.");
	)
	with Sys_error e ->
		print_error ("File " ^ file_name ^ " could not be removed. System says: '" ^ e ^ "'.")


(** Print info on the memory used *)
let print_memory_used debug_level =
	(* Print memory information *)
	let gc_stat = Gc.stat () in
	let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
	(* Compute the word size in bytes *)
	let word_size = (*4.0*)Sys.word_size / 8 in
	let nb_ko = nb_words *. (float_of_int word_size) /. 1024.0 in
		print_message debug_level ("Estimated memory used: " ^ (round3_float nb_ko) ^ " KiB (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words of size " ^ (string_of_int word_size) ^ ")")



(****************************************************************)
(** Terminating functions *)
(****************************************************************)

(* Abort program *)
let abort_program () =
	print_error (program_name ^ " aborted (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush Pervasives.stdout;
	exit(1)

(* Terminate program *)
let terminate_program () =
	print_newline();
	print_message Debug_standard (program_name ^ " successfully terminated (" ^ (after_seconds ()) ^ ")");
	(* Print memory info *)
	print_memory_used Debug_low;
	(* The end *)
	print_newline();
	flush Pervasives.stdout;
	exit(0)

