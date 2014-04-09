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
 * Last modified: 2014/04/09
 *
 ****************************************************************)


(****************************************************************)
(** Modules *)
(****************************************************************)
open DynArray


(****************************************************************)
(** Constants *)
(****************************************************************)

(*** TODO: separate global wrt Ocaml from global wrt IMITATOR ***)


val program_name: string

val version_string: string

val header_string: string

(* val print_version_string: unit -> unit *)

(* Extension for input model files *)
val model_extension: string

val print_contributors : unit -> unit

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


(* Return true if the global debug mode is greater than 'debug_mode', false otherwise *)
val debug_mode_greater : debug_mode -> bool

(* Convert a string into a debug_mode; raise Not_found if not found *)
val debug_mode_of_string : string -> debug_mode

(* Set the debug mode *)
val set_debug_mode : debug_mode -> unit

(* Get the debug mode *)
val get_debug_mode : unit -> debug_mode


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

(* Compute the duration in ms between 2 times *)
(* val duration : float -> float -> float *)

(** Get the value of the counter *)
val get_time : unit -> float

(** Compute the duration since time t *)
val time_from : float -> float

(** Print a number of seconds *)
val string_of_seconds : float -> string

(** Create a string of the form 'after x seconds', where x is the time since the program started *)
val after_seconds : unit -> string

(** Set the timed mode *)
val set_timed_mode : unit -> unit

(****************************************************************)
(** Useful functions on float *)
(****************************************************************)
(** Round a float with 3 digits after comma, and convert to string *)
val round3_float : float -> string


(****************************************************************)
(** Date functions *)
(****************************************************************)

(** Print the current date and time under the form of a string *)
val now : unit -> string

(****************************************************************)
(** Useful functions on lists *)
(****************************************************************)
(** Check if a list is empty *)
val list_empty : 'a list -> bool

(** Return a random element in a list *)
val random_element : 'a list -> 'a

(** list_of_interval a b Create a fresh new list filled with elements [a, a+1, ..., b-1, b] *)
val list_of_interval : int -> int -> int list

(** Intersection of 2 lists (keeps the order of the elements as in l1) *)
val list_inter : 'a list -> 'a list -> 'a list

(** Union of 2 lists *)
val list_union : 'a list -> 'a list -> 'a list


(** Difference of 2 lists *)
val list_diff : 'a list	 -> 'a list-> 'a list


(** Tail-recursive function for 'append' *)
val list_append : 'a list -> 'a list -> 'a list

(** Return a list where every element only appears once *)
val list_only_once : 'a list -> 'a list

(** Filter the elements appearing several times in the list *)
val elements_existing_several_times : 'a list -> 'a list

(** Remove the first occurence of element e in list l; return the list unchanged if not found *)
val list_remove_first_occurence : 'a -> 'a list -> 'a list

(** Remove the ith element of a list *)
val list_delete_at : int -> 'a list -> 'a list

(** Replace the ith element of a list *)
val list_set_nth : int -> 'a -> 'a list -> 'a list


(****************************************************************)
(** Useful functions on arrays *)
(****************************************************************)

(* Check if an element belongs to an array *)
val in_array : 'a -> 'a array -> bool

(* Returns the (first) index of an element in an array, or raise Not_found if not found *)
val index_of : 'a -> 'a array -> int

(* Return the list of the indexes whose value is true *)
val true_indexes : bool array -> int list

(* Shuffle an array *)
(* val shuffle_array : 'a array -> unit *)

(** exists p {a1; ...; an} checks if at least one element of the Array satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val array_exists : ('a -> bool) -> 'a array -> bool


(****************************************************************)
(** Useful functions on dynamic arrays *)
(****************************************************************)

(* exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val dynArray_exists : ('a -> bool) -> 'a DynArray.t -> bool


(****************************************************************)
(** Useful functions on string *)
(****************************************************************)
(** Returns a fresh string made of 'n' times 's' *)
val string_n_times : int -> string -> string

(* Convert an array of string into a string *)
val string_of_array_of_string : string array -> string

(* Convert a list of string into a string *)
val string_of_list_of_string : string list -> string

(* Convert an array of string into a string with separators *)
val string_of_array_of_string_with_sep : string -> string array -> string

(* Convert a list of string into a string with separators *)
val string_of_list_of_string_with_sep : string -> string list -> string

(* Returns a list of substrings splitted using sep *)
val split : string -> string -> string list

(* 's_of_int i' Return "s" if i > 1, "" otherwise *)
val s_of_int : int -> string

(****************************************************************)
(** Useful functions on booleans *)
(****************************************************************)
(* Evaluate both part of an 'and' comparison and return the conjunction *)
val evaluate_and : bool -> bool -> bool

(* Evaluate both part of an 'or' comparison and return the disjunction *)
val evaluate_or : bool -> bool -> bool




(**************************************************)
(** System functions *)
(**************************************************)

val write_to_file : string -> string -> unit


val delete_file : string -> unit

(** Print info on the memory used *)
val print_memory_used : debug_mode -> unit


(****************************************************************)
(** Messages *)
(****************************************************************)
(*(* Print a message if global_debug_mode >= message_debug_mode *)
val print_debug_message : debug_mode -> debug_mode -> string -> unit*)

(* Print a message if global_debug_mode >= message_debug_mode *)
val print_message : debug_mode -> string -> unit

(* Print a warning *)
val print_warning : string -> unit

(* Print an error *)
val print_error : string -> unit


(****************************************************************)
(** Terminating functions *)
(****************************************************************)
(* Abort program *)
val abort_program : unit -> unit

(* Terminate program *)
val terminate_program : unit -> unit
