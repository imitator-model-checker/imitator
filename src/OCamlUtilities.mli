(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Paris 13, LIPN, CNRS, France
 * 
 * Module description: Useful OCaml functions
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 * Last modified     : 2019/08/09
 *
 ************************************************************)

 
(************************************************************)
(** Useful functions on integers *)
(************************************************************)
(** Check if an integer is a power of two, i.e., n = 2^m, with m >= 1 *)
val is_a_power_of_2 : int -> bool



(************************************************************)
(** Useful functions on float *)
(************************************************************)
(** Round a float with 1 digit after comma, and convert to string *)
val round1_float : float -> string

(** Round a float with 3 digits after comma, and convert to string *)
val round3_float : float -> string


(************************************************************)
(** Useful functions on options *)
(************************************************************)
(** Get the value of an 'a option that is assumed to be different from None, or raise NoneException otherwise *)
val a_of_a_option : 'a option -> 'a


(************************************************************)
(** Useful functions on lists *)
(************************************************************)
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

(**  Returns the last element of the list, or raise Empty_list if the list is empty. This function takes linear time *)
val list_last : 'a list -> 'a

(**  Returns the list without its last element; raises Empty_list if the list is empty *)
val list_without_last  : 'a list -> 'a list

(**  Returns a pair (the list without its last element, the last element of the list), or raise Empty_list if the list is empty. *)
val list_split_last : 'a list -> 'a list * 'a

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

(** Select the sublist of a list from position i to position j *)
val sublist : int -> int -> 'a list -> 'a list


(************************************************************)
(** Useful functions on arrays *)
(************************************************************)

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

(** Shuffles the values of an array *)
val array_shuffle : 'a array -> unit


(************************************************************)
(** Useful functions on dynamic arrays *)
(************************************************************)

(* exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val dynArray_exists : ('a -> bool) -> 'a DynArray.t -> bool


(************************************************************)
(** Useful functions on hash tables *)
(************************************************************)
(** Get all bound keys in an hash table; multiple bindings yield multiple (identical) keys *)
(*** NOTE: indeed, in our setting, we only use hashtbl with a single binding ***)
val hashtbl_get_all_keys : ('a , 'b) Hashtbl.t -> 'a list

(** Get the binding associated to a key, or the default binding if key is not associated to any binding *)
val hashtbl_get_or_default : ('a , 'b) Hashtbl.t -> 'a -> 'b -> 'b

(** function to filter hash table with a predicate on keys *)
val hashtbl_filter : ('a -> bool) -> ('a,'b) Hashtbl.t -> unit 

(************************************************************)
(** Useful functions on string *)
(************************************************************)
(** Returns a fresh string made of 'n' times 's' *)
val string_n_times : int -> string -> string

(* Convert an array of string into a string *)
val string_of_array_of_string : string array -> string

(* Convert a list of string into a string *)
val string_of_list_of_string : string list -> string

(* Convert an array of string into a string with separators *)
val string_of_array_of_string_with_sep : string -> string array -> string

(** Convert a list of string into a string with separators (uses an internal conversion to array) *)
val string_of_list_of_string_with_sep : string -> string list -> string

(** Convert a list of int into a string with , separator *)
val string_of_list_of_int : int list -> string

(* Returns a list of substrings splitted using sep *)
(*** WARNING: the behavior of this function is odd (when sep=";;" or "£"; bug hidden here? ***)
val split : string -> string -> string list

(** 's_of_int i' Return "s" if i > 1, "" otherwise *)
val s_of_int : int -> string

(** 'waswere_of_int i' Return "were" if i > 1, "was" otherwise *)
val waswere_of_int  : int -> string

(** Escape \n & > for use in dot *)
val escape_string_for_dot : string -> string


(************************************************************)
(** Useful functions on booleans *)
(************************************************************)
(* Evaluate both part of an 'and' comparison and return the conjunction *)
val evaluate_and : bool -> bool -> bool

(* Evaluate both part of an 'or' comparison and return the disjunction *)
val evaluate_or : bool -> bool -> bool

(* XOR: returns true if both are different *)
val xor : bool -> bool -> bool

(* XNOR: returns true if both are true or both are false, i.e., when both are equal to each other *)
val xnor : bool -> bool -> bool


(************************************************************)
(** Date functions *)
(************************************************************)

(** Print the current date and time under the form of a string *)
val now : unit -> string



(**************************************************)
(** System functions *)
(**************************************************)

(** Read the first line of a file and convert to string *)
val read_first_line_from_file : string -> string

(** Read a file and convert to string *)
val read_from_file : string -> string

(** `write_to_file file_name file_content` will create a file `file_name` with content `file_content` *)
val write_to_file : string -> string -> unit

