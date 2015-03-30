(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        Etienne Andre
 * 
 * Created:       2014/10/24
 * Last modified: 2015/03/30
 *
 ****************************************************************)
 
 
(****************************************************************)
(** External modules *)
(****************************************************************)
open Gc

(****************************************************************)
(** Internal modules *)
(****************************************************************)
open CamlUtilities


(****************************************************************)
(** Global time counter *)
(****************************************************************)
let counter = ref (Unix.gettimeofday())




(****************************************************************)
(** Debug modes *)
(****************************************************************)

type debug_mode =
	| Verbose_mute
	| Verbose_standard
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total

(* Associate an integer to each debug mode *)
let level_of_debug = function
	| Verbose_mute -> 0
	| Verbose_standard -> 1
	| Verbose_low -> 2
	| Verbose_medium -> 3
	| Verbose_high -> 4
	| Verbose_total -> 5

(* The global debug mode *)
type global_debug_mode_type =
	| Verbose_mode_not_set
	| Verbose_mode_set of debug_mode

(* set to standard by default *)
let global_debug_mode = ref (Verbose_mode_set Verbose_standard)

let timed_mode = ref false


(* Get the debug mode *)
let get_debug_mode () =
	match !global_debug_mode with
	| Verbose_mode_not_set -> raise (Exceptions.InternalError ("The debug mode has not yet been set."))
	| Verbose_mode_set debug_mode -> debug_mode


(* Set the debug mode *)
let set_debug_mode debug_mode =
	(*match !global_debug_mode with
	| Verbose_mode_not_set -> global_debug_mode := Verbose_mode_set debug_mode
	| Verbose_mode_set debug_mode -> raise (InternalError ("The debug mode has already been set, impossible to set it again."))*)
	global_debug_mode := Verbose_mode_set debug_mode


(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
let debug_mode_greater debug_mode =
	(* Get the global debug mode *)
	let global_debug_mode = get_debug_mode() in
(*	match !global_debug_mode with
	| Verbose_mode_not_set -> raise (InternalError ("The debug mode has not been set, impossible to access it."))
	| Verbose_mode_set global_debug_mode -> global_debug_mode
	in*)
	(* Compare *)
	(level_of_debug global_debug_mode) >= (level_of_debug debug_mode)


(* Convert a string into a debug_mode; raise Not_found if not found *)
let debug_mode_of_string debug_mode =
	if debug_mode = "mute" then Verbose_mute
	else if debug_mode = "standard" then Verbose_standard
	else if debug_mode = "low" then Verbose_low
	else if debug_mode = "medium" then Verbose_medium
	else if debug_mode = "high" then Verbose_high
	else if debug_mode = "total" then Verbose_total
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





(** Get the value of the counter *)
let get_time() =
	(Unix.gettimeofday()) -. (!counter)

(* Compute the duration since time t *)
let time_from t =
	(Unix.gettimeofday()) -. t

(** Convert a % to a nice string *)
let string_of_percent percent =
	let percent = round3_float (percent *. 100.0) in
	percent ^ " %"


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
	if debug_mode_greater message_debug_mode then(
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
	)



(* Print a warning *)
let print_warning message =
	(* Do not print warnings in mute mode *)
	if debug_mode_greater Verbose_standard then(
		let spaces = " " in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ "*** Warning: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
		(* Print *)
		print_message_generic formatted_message
	)


(* Print an error *)
let print_error message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ "*** ERROR: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) in
	(* Print *)
	print_message_generic formatted_message



(****************************************************************)
(** Information printing *)
(****************************************************************)
 
let print_version_string () = 
	print_string (Constants.program_name ^ " " ^ Constants.version_string ^ "\n")


let print_header_string () =

	let header_string = 
	
	(* Build info *)
	let build_info = "Build: " ^ BuildInfo.build_number ^ " (" ^ BuildInfo.build_time ^ ")" in
	let length_header = 55 in
	
	"************************************************************\n"
	^ "*  " ^ Constants.program_name ^ " " ^ Constants.version_string ^ (string_n_times (length_header - (String.length Constants.program_name) - 1 - (String.length Constants.version_string)) " ") ^ " *\n"
	^ "*                                                          *\n"
	^ "*                     Etienne Andre, Ulrich Kuehne et al.  *\n"
	^ "*                                             2009 - " ^ (BuildInfo.build_year) ^ "  *\n"
	^ "*                       LSV, ENS de Cachan & CNRS, France  *\n"
	^ "*  LIPN, Universite Paris 13, Sorbonne Paris Cite, France  *\n"
	^ "*                                                          *\n"
	^ "*  " ^ (string_n_times (length_header - (String.length build_info)) " ") ^ build_info ^ " *\n"
	^ "************************************************************"
	
	in print_message Verbose_standard header_string


(* Print the name of the contributors *)
let print_contributors()  = 
	print_version_string();
	print_string (" " ^ Constants.program_name ^ " has been developed by:\n");
	print_string (" * Etienne Andre       (2008 - " ^ (BuildInfo.build_year) ^ ")\n");
	print_string " * Camille Coti        (2014)\n";
	print_string " * Daphne Dussaud      (2010)\n";
	print_string " * Sami Evangelista    (2014)\n";
	print_string " * Ulrich Kuehne       (2010 - 2011)\n";
	print_string " * Nguyen Hoang Gia    (2014 - 2015)\n";
	print_string " * Romain Soulat       (2010 - 2013)\n";
	print_string "\n";
	print_string " Moral support and suggestions by:\n";
	print_string " * Emmanuelle Encrenaz\n";
	print_string " * Laurent Fribourg\n";
	print_string " * Giuseppe Lipari\n";
	()






(**************************************************)
(** System functions *)
(**************************************************)


(* Delete a file, and print a message if not found *)
let delete_file file_name =
	try (
		(* Delete the file *)
		Sys.remove file_name;
		(* Confirm *)
		print_message Verbose_total ("Removed file " ^ file_name ^ " successfully.");
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
	print_error (Constants.program_name ^ " aborted (" ^ (after_seconds ()) ^ ")");
	print_newline();
	flush Pervasives.stdout;
	exit(1)

(* Terminate program *)
let terminate_program () =
	print_newline();
	print_message Verbose_standard (Constants.program_name ^ " successfully terminated (" ^ (after_seconds ()) ^ ")");
	(* Print memory info *)
	if debug_mode_greater Verbose_low then(
		print_memory_used Verbose_low;
	);
	(* The end *)
	print_newline();
	flush Pervasives.stdout;
	exit(0)
