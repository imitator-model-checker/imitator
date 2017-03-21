(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * LIPN, Université Paris 13, Sorbonne Paris Cité (France)
 * 
 * Module description: Useful and general functions for IMITATOR
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2017/03/21
 *
 ************************************************************)


 
(************************************************************)
(** External modules *)
(************************************************************)
open Gc

(************************************************************)
(** Internal modules *)
(************************************************************)
open OCamlUtilities


(************************************************************)
(** Global time counter *)
(************************************************************)
let counter = ref (Unix.gettimeofday())





(************************************************************)
(** Versioning *)
(************************************************************)

(* Name + version *)
let program_name_and_version () =
	Constants.program_name
	^ " "
	^ Constants.version_string


(* Name + version + nickname *)
let program_name_and_version_and_nickname () =
	Constants.program_name
	^ " "
	^ Constants.version_string
	^ " \""
	^ Constants.version_name
	^ "\""


(* Name + version + build number *)
let program_name_and_version_and_build () =
	Constants.program_name
	^ " "
	^ Constants.version_string
	^ " (build "
	^ BuildInfo.build_number
	^ ")"


(* Name + version + nickname + build number *)
let program_name_and_version_and_nickname_and_build () =
	Constants.program_name
	^ " "
	^ Constants.version_string
	^ " \""
	^ Constants.version_name
	^ "\" (build "
	^ BuildInfo.build_number
	^ ")"


(* Name + version + nickname + build number + build time *)
let program_name_and_version_and_nickname_and_build_time () =
	Constants.program_name
	^ " "
	^ Constants.version_string
	^ " \""
	^ Constants.version_name
	^ "\" build "
	^ BuildInfo.build_number
	^ "\" ("
	^ BuildInfo.build_time
	^ ")"


(* Shorten the hash to 7 characters *)
let shorten_hash_7 hash =
	if String.length hash < 7 then hash
	else String.sub hash 0 7

(** GitHub branch and first 7 characters of git hash (if applicable) *)
let git_branch_and_hash =
	match BuildInfo.git_branch, BuildInfo.git_hash with
	| None, None -> "unknown git info"
	| Some branch, None -> branch ^ "/unknown hash"
	| None, Some hash -> "unknown/" ^ (shorten_hash_7 hash)
	| Some branch, Some hash -> branch ^ "/" ^ (shorten_hash_7 hash)


(** GitHub branch and full git hash (if applicable) *)
let git_branch_and_full_hash =
	match BuildInfo.git_branch, BuildInfo.git_hash with
	| None, None -> "unknown git info"
	| Some branch, None -> branch ^ "/unknown hash"
	| None, Some hash -> "unknown/" ^ hash
	| Some branch, Some hash -> branch ^ "/" ^ hash


(* URL of IMITATOR without http:// *)
let imitator_url = "www.imitator.fr"


(************************************************************)
(** Verbosity modes *)
(************************************************************)

type verbose_mode =
	| Verbose_mute
	| Verbose_warnings
	| Verbose_standard
	| Verbose_low
	| Verbose_medium
	| Verbose_high
	| Verbose_total

(* Associate an integer to each verbose mode *)
let level_of_verbose = function
	| Verbose_mute		-> 0
	| Verbose_warnings	-> 1
	| Verbose_standard	-> 2
	| Verbose_low		-> 3
	| Verbose_medium	-> 4
	| Verbose_high		-> 5
	| Verbose_total		-> 6

(* The global verbose mode *)
type global_verbose_mode_type =
	| Verbose_mode_not_set
	| Verbose_mode_set of verbose_mode

(* set to standard by default *)
let global_verbose_mode = ref (Verbose_mode_set Verbose_standard)

let timed_mode = ref false


(* Get the verbose mode *)
let get_verbose_mode () =
	match !global_verbose_mode with
	| Verbose_mode_not_set -> raise (Exceptions.InternalError ("The verbose mode has not yet been set."))
	| Verbose_mode_set verbose_mode -> verbose_mode


(* Set the verbose mode *)
let set_verbose_mode verbose_mode =
	(*match !global_verbose_mode with
	| Verbose_mode_not_set -> global_verbose_mode := Verbose_mode_set verbose_mode
	| Verbose_mode_set verbose_mode -> raise (InternalError ("The verbose mode has already been set, impossible to set it again."))*)
	global_verbose_mode := Verbose_mode_set verbose_mode


(* Return true if the global verbose mode is greater than 'verbose_mode', false otherwise *)
let verbose_mode_greater verbose_mode =
	(* Get the global verbose mode *)
	let global_verbose_mode = get_verbose_mode() in
	(* Compare *)
	(level_of_verbose global_verbose_mode) >= (level_of_verbose verbose_mode)


(* Convert a string into a verbose_mode; raise Not_found if not found *)
let verbose_mode_of_string verbose_mode =
	if verbose_mode = "mute" then Verbose_mute
	else if verbose_mode = "warnings" then Verbose_warnings
	else if verbose_mode = "standard" then Verbose_standard
	else if verbose_mode = "low" then Verbose_low
	else if verbose_mode = "medium" then Verbose_medium
	else if verbose_mode = "high" then Verbose_high
	else if verbose_mode = "total" then Verbose_total
	else raise Not_found



(************************************************************)
(** Global types *)
(************************************************************)

(** Mode for IMITATOR *)
type imitator_mode =
	(** Translation to another language: no analysis *)
	| Translation
	
	(** Classical state space exploration *)
	| State_space_exploration
	
	(** EF-synthesis *)
	| EF_synthesis
	
	(** EF-synthesis w.r.t. unsafe locations *)
	| EFunsafe_synthesis
	
	(** Parametric loop synthesis *)
	| Loop_synthesis
	
	(** Parametric Büchi-emptiness checking with non-Zenoness (method: check whether the PTA is CUB) *)
	| Parametric_NZ_CUBcheck
	
	(** Parametric Büchi-emptiness checking with non-Zenoness (method: transformation into a CUB-PTA) *)
	| Parametric_NZ_CUBtransform
	
	(** Parametric Büchi-emptiness checking with non-Zenoness on a CUB-PTA: hidden option (mainly for testing) *)
	| Parametric_NZ_CUB
	
	(** Parametric deadlock-checking *)
	| Parametric_deadlock_checking
	
	(** Inverse method *)
	| Inverse_method
	
	(** Parametric reachability preservation *)
	| PRP
	
	(** Cover the whole cartography *)
	| Cover_cartography
	
	(** Cover the whole cartography using learning-based abstractions *)
	| Learning_cartography
	
	(** Cover the whole cartography after shuffling point (mostly useful for the distributed IMITATOR) *)
	| Shuffle_cartography
	
	(** Look for the border using the cartography*)
	| Border_cartography
	
	(** Randomly pick up values for a given number of iterations *)
	| Random_cartography of int
	
	(** Randomly pick up values for a given number of iterations, then switch to sequential algorithm once no more point has been found after a given max number of attempts (mostly useful for the distributed IMITATOR) *)
	| RandomSeq_cartography of int

	(** Synthesis using iterative calls to PRP *)
	| PRPC




type distribution_mode =
	(** Normal mode *)
	| Non_distributed
	
	(** Distributed mode: static distribution mode (each node has its own part with no communication) *)
	| Distributed_static
	
	(** Distributed mode: Master slave with sequential pi0 *)
	| Distributed_ms_sequential
	(** Distributed mode: Master slave with sequential pi0 shuffled *)
	| Distributed_ms_shuffle
	(** Distributed mode: Master slave with random pi0 and n retries before switching to sequential mode *)
	| Distributed_ms_random of int
	(** Distributed mode: Master slave with subpart distribution *)
	| Distributed_ms_subpart

	(**  Distributed mode: Workers live their own lives and communicate results to the coordinator  **)
	| Distributed_unsupervised
	(**  Distributed mode: multi-threaded version of Distributed_unsupervised  **)
	| Distributed_unsupervised_multi_threaded



type exploration_order =
	(** Layer-BFS: all states at depth i are computed, and then their successors at depth i+1 *)
	| Exploration_layer_BFS
	(** Queue-BFS: basic queue, independent of the depth *)
	| Exploration_queue_BFS
	

(************************************************************)
(** Time functions *)
(************************************************************)

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



(************************************************************)
(** Messages *)
(************************************************************)

type shell_highlighting_type =
	| Shell_bold
	| Shell_error
	| Shell_normal
	| Shell_result
	| Shell_soundness
	| Shell_warning

let shell_code_of_shell_highlighting_type = function
	| Shell_bold -> "\027[1m"
	| Shell_error -> "\027[91m"
	| Shell_normal -> "\027[0m"
	| Shell_result -> "\027[92m"
	| Shell_soundness -> "\027[94m"
	| Shell_warning -> "\027[93m"

(*    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'*)




(* Print a string *)
let print_message_generic printing_function channel message =
	(* Timed mode *)
	let time_info =
		if !timed_mode then (" (at t = " ^ (string_of_seconds (get_time())) ^ ")")
		else "" in
	(* Print message *)
	printing_function (message ^ time_info ^ "\n");
	(* Flush! *)
	flush channel


(* Print a message if global_verbose_mode >= message_verbose_mode *)
let print_highlighted_message shell_highlighting_type message_verbose_mode message =
	(* Only print the message if its message_verbose_mode is smaller or equal to the global_verbose_mode *)
	if verbose_mode_greater message_verbose_mode then(
		(* Compute the verbose level *)
		let verbose_level = level_of_verbose message_verbose_mode in
		(* Find number of blanks for indentation *)
		let nb_spaces = if verbose_level-1 > 0 then verbose_level-1 else 0 in
		(* Create blanks proportionnally to the verbose_level (at least one space) *)
		let spaces = " " ^ (string_n_times nb_spaces "   ") in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (shell_code_of_shell_highlighting_type shell_highlighting_type) ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) ^ (shell_code_of_shell_highlighting_type Shell_normal) in
		(* Print *)
		print_message_generic print_string Pervasives.stdout formatted_message
	)


(* Print a message if global_verbose_mode >= message_verbose_mode *)
let print_message = print_highlighted_message Shell_normal



(* Print a warning *)
let print_warning message =
	(* Do not print warnings in mute mode *)
	if verbose_mode_greater Verbose_warnings then(
		let spaces = " " in
		(* Add new lines and blanks everywhere *)
		let formatted_message = spaces ^ (shell_code_of_shell_highlighting_type Shell_warning) ^ "*** Warning: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) ^ (shell_code_of_shell_highlighting_type Shell_normal) in
		(* Print *)
		(*** NOTE: warnings are displaied to stderr (hence the OCaml function 'prerr_string') ***)
		print_message_generic prerr_string Pervasives.stderr formatted_message
	)


(* Print an error *)
let print_error message =
	let spaces = " " in
	(* Add new lines and blanks everywhere *)
	let formatted_message = spaces ^ (shell_code_of_shell_highlighting_type Shell_error) ^ "*** ERROR: " ^ (Str.global_replace (Str.regexp "\n") ("\n" ^ spaces) message) ^ (shell_code_of_shell_highlighting_type Shell_normal) in
	(* Print *)
	print_message_generic prerr_string Pervasives.stderr formatted_message



(************************************************************)
(** Information printing *)
(************************************************************)
 
let print_version_string () = 
	print_string (Constants.program_name ^ " " ^ Constants.version_string ^ "\n")


let print_header_string () =

	let header_string = 
	
	(* Build info *)
	let build_info = "Build: " ^ BuildInfo.build_number ^ " (" ^ BuildInfo.build_time ^ ")" in
	let length_header = 54 in
	
	let imi_name = program_name_and_version_and_nickname() in
	
	"************************************************************\n"
	^ "*  " ^ (shell_code_of_shell_highlighting_type Shell_bold) ^ imi_name ^ (shell_code_of_shell_highlighting_type Shell_normal) ^  (string_n_times (length_header - (String.length imi_name)) " ") ^ "  *\n"
	^ "*                                                          *\n"
	^ "*                     Etienne Andre, Ulrich Kuehne et al.  *\n"
	^ "*                                             2009 - " ^ (BuildInfo.build_year) ^ "  *\n"
	^ "*                       LSV, ENS de Cachan & CNRS, France  *\n"
	^ "*  LIPN, Universite Paris 13, Sorbonne Paris Cite, France  *\n"
	^ "*  " ^ (string_n_times (length_header - (String.length imitator_url)) " ") ^ imitator_url ^ "  *\n"
	^ "*                                                          *\n"
	^ "*  " ^ (string_n_times (length_header - (String.length build_info)) " ") ^ build_info ^ "  *\n"
	^ "*  " ^ (string_n_times (length_header - (String.length git_branch_and_hash)) " ") ^ git_branch_and_hash ^ "  *\n"
	^ "************************************************************"
	
	in print_message Verbose_standard header_string


(* Print the name of the contributors *)
let print_contributors()  = 
	print_string ("    " ^ Constants.program_name ^ " has been developed by:\n");
	print_string ("    * Etienne Andre       (2008 - " ^ (BuildInfo.build_year) ^ ")\n");
	print_string "    * Camille Coti        (2014)\n";
	print_string "    * Daphne Dussaud      (2010)\n";
	print_string "    * Sami Evangelista    (2014)\n";
	print_string "    * Ulrich Kuehne       (2010 - 2011)\n";
	print_string "    * Nguyen Hoang Gia    (2014 - 2016)\n";
	print_string "    * Romain Soulat       (2010 - 2013)\n";
	print_string "\n";
	print_string "    Compiling, testing and packaging:\n";
	print_string "    * Corentin Guillevic  (2015)\n";
	print_string "    * Sarah Hadbi         (2015)\n";
	print_string "    * Fabrice Kordon      (2015)\n";
	print_string "    * Alban Linard        (2014 - 2015)\n";
	print_string "    * Stéphane Rosse      (2017)\n";
	print_string "\n";
	print_string "    Moral support and suggestions by:\n";
	print_string "    * Emmanuelle Encrenaz\n";
	print_string "    * Laurent Fribourg\n";
	print_string "    * Giuseppe Lipari\n";
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


(** Convert a number of KiB (float) into KiB/MiB/GiB/TiB *)
let kiB_MiB_GiB_TiB_of_KiB nb_kib =
	(* Case KiB *)
	if nb_kib <= 1024.0 then (round3_float nb_kib) ^ " KiB"
	else
	if nb_kib <= 1024.0 ** 2.0 then (round3_float (nb_kib /. 1024.0)) ^ " MiB"
	else
	if nb_kib <= 1024.0 ** 3.0 then (round3_float (nb_kib /. (1024.0 ** 2.0))) ^ " GiB"
	else (round3_float (nb_kib /. (1024.0 ** 3.0))) ^ " TiB"


(** Convert a number of words into a memory information *)
let memory_info_of_words nb_words word_size = 
	let nb_kib = nb_words *. (float_of_int word_size) /. 1024.0 in
	 (kiB_MiB_GiB_TiB_of_KiB nb_kib) ^ " (i.e., " ^ (string_of_int (int_of_float nb_words)) ^ " words of size " ^ (string_of_int word_size) ^ ")"


(** Obtain a string giving information on the memory used *)
let memory_used () =
	(* Print memory information *)
	let gc_stat = Gc.stat () in
	let nb_words = gc_stat.minor_words +. gc_stat.major_words -. gc_stat.promoted_words in
	(* Compute the word size in bytes *)
	let word_size = (*4.0*)Sys.word_size / 8 in
	memory_info_of_words nb_words word_size
	

(************************************************************)
(** Terminating functions *)
(************************************************************)

(* Abort program *)
let abort_program () =
	print_error (Constants.program_name ^ " aborted (" ^ (after_seconds ()) ^ ")");
	(*** NOTE: print new line to stderr ***)
	prerr_newline();
	flush Pervasives.stderr;
	flush Pervasives.stdout;
	exit(1)


(* Terminate program *)
let terminate_program () =
	print_newline();
	print_message Verbose_standard ((shell_code_of_shell_highlighting_type Shell_bold) ^ Constants.program_name ^ " successfully terminated" ^ (shell_code_of_shell_highlighting_type Shell_normal) ^ " (" ^ (after_seconds ()) ^ ")");
	(* Print memory info *)
	if verbose_mode_greater Verbose_standard then(
		print_message Verbose_standard ("Estimated memory used: " ^ (memory_used ()));
	);
	(* The end *)
	print_newline();
	flush Pervasives.stderr;
	flush Pervasives.stdout;
	exit(0)
