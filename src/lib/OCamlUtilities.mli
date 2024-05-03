(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 *
 * Module description: Useful OCaml functions
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 *
 ************************************************************)

 
(************************************************************)
(* Useful functions on integers *)
(************************************************************)
(** Check if an integer is a power of two, i.e., n = 2^m, with m >= 1 *)
val is_a_power_of_2 : int -> bool



(************************************************************)
(* Useful functions on float *)
(************************************************************)
(** Round a float with 1 digit after comma, and convert to string *)
val round1_float : float -> string

(** Round a float with 3 digits after comma, and convert to string *)
val round3_float : float -> string


(************************************************************)
(* Useful functions on options *)
(************************************************************)
(** Get the value of an 'a option that is assumed to be different from None, or raise NoneException otherwise *)
val a_of_a_option : 'a option -> 'a

(************************************************************)
(* Useful lambda functions *)
(************************************************************)
val identity : 'a -> 'a

(************************************************************)
(* Useful functions on tuples *)
(************************************************************)

(** Get first element of tuple *)
val first_of_tuple : 'a * 'b -> 'a

(** Get second element of tuple *)
val second_of_tuple : 'a * 'b -> 'b

(** Apply a tuple as argument to a function *)
val apply_tuple : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

(** Map first element of tuple *)
val map_first_of_tuple : ('a -> 'c) -> 'a * 'b -> 'c * 'b

(** Get first element of triplet *)
val first_of_triplet : 'a * 'b * 'c -> 'a

(** Get second element of triplet *)
val second_of_triplet : 'a * 'b * 'c -> 'b

(** Get third element of triplet *)
val third_of_triplet : 'a * 'b * 'c -> 'c


(************************************************************)
(* Useful functions on lists *)
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

(* Remove all empty strings in list *)
val list_without_empty_string : string list -> string list

(** Remove the ith element of a list *)
val list_delete_at : int -> 'a list -> 'a list

(** Replace the ith element of a list *)
val list_set_nth : int -> 'a -> 'a list -> 'a list

(** Get combination of two list **)
val list_combination : 'a list -> 'a list -> ('a * 'a) list
val list_combination_2 : 'a list -> 'a list -> ('a * 'a) list

(* Check if predicate is true for all arrangement of list *)
val for_all_in_arrangement : ('a -> 'a -> bool) -> 'a list -> bool

(** Select the sublist of a list from position i to position j *)
val sublist : int -> int -> 'a list -> 'a list

(* Partition list by grouping elements by keys in a list of tuples *)
val group_by : ('a -> 'b) -> 'a list -> ('b * 'a list) list

(* Partition list by grouping elements by keys in a list of tuples *)
(* and map values associated by keys according to valueSelector function *)
val group_by_and_map : ('a -> 'b) -> ('a -> 'c) -> 'a list -> ('b * 'c list) list

(* Type used for partition map *)
type ('a, 'b) my_either = My_left of 'a | My_right of 'b

(* Partition and map list *)
val partition_map : ('a -> ('b, 'c) my_either) -> 'a list -> ('b list * 'c list)

(* Partition list by grouping elements by keys in a hashtable *)
val hashtbl_group_by : ('a -> 'b) -> 'a list -> ('b, 'a list) Hashtbl.t

(* Create an hashtbl from a list of tuples *)
(* Raise an error if two pairs have the same key *)
val hashtbl_of_tuples : ('a * 'b) list -> ('a, 'b) Hashtbl.t

(************************************************************)
(* Useful functions on arrays *)
(************************************************************)

(** Check if an element belongs to an array *)
val in_array : 'a -> 'a array -> bool

(** Returns the (first) index of an element in an array, or raise Not_found if not found *)
val index_of : 'a -> 'a array -> int

(** Return the list of the indexes whose value is true *)
val true_indexes : bool array -> int list

(* Shuffle an array *)
(* val shuffle_array : 'a array -> unit *)

(** exists p {a1; ...; an} checks if at least one element of the Array satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val array_exists : ('a -> bool) -> 'a array -> bool

(** Shuffles the values of an array *)
val array_shuffle : 'a array -> unit

(** Perform the substraction of 2 NumConst array of same size **)
val sub_array : NumConst.t array -> NumConst.t array -> NumConst.t array


(************************************************************)
(* Useful functions on dynamic arrays *)
(************************************************************)

(** exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
val dynArray_exists : ('a -> bool) -> 'a DynArray.t -> bool

(************************************************************)
(* Useful functions on queues *)
(************************************************************)

(** Transform a list into a queue *)
val queue_of_list : 'a list -> 'a Queue.t


(** Transform a queue into a list *)
val list_of_queue : 'a Queue.t -> 'a list


(************************************************************)
(* Useful functions on hash tables *)
(************************************************************)
(** Get all bound keys in an hash table; multiple bindings yield multiple (identical) keys *)
(*** NOTE: indeed, in our setting, we only use hashtbl with a single binding ***)
val hashtbl_get_all_keys : ('a , 'b) Hashtbl.t -> 'a list

(** Get the binding associated to a key, or the default binding if key is not associated to any binding *)
val hashtbl_get_or_default : ('a , 'b) Hashtbl.t -> 'a -> 'b -> 'b

(** function to filter hash table with a predicate on keys *)
val hashtbl_filter : ('a -> bool) -> ('a,'b) Hashtbl.t -> unit 

(************************************************************)
(* Useful functions on string *)
(************************************************************)
(** Returns a fresh string made of 'n' times 's' *)
val string_n_times : int -> string -> string

(** Convert an array of string into a string *)
val string_of_array_of_string : string array -> string

(** Convert a list of string into a string *)
val string_of_list_of_string : string list -> string

(** Convert an array of string into a string with separators *)
val string_of_array_of_string_with_sep : string -> string array -> string

(** Convert an array of string into a string with separators removing empty strings *)
val string_of_array_of_string_with_sep_without_empty_strings : string -> string array -> string

(** Convert a list of string into a string with separators (uses an internal conversion to array) *)
val string_of_list_of_string_with_sep : string -> string list -> string

(** Convert a list of string into a string with separators removing empty strings *)
val string_of_list_of_string_with_sep_without_empty_strings : string -> string list -> string

(** Add \t identation of string according to the given level *)
val indent_paragraph : int -> string -> string

(** Convert a list of int into a string with , separator *)
val string_of_list_of_int : int list -> string

(** Returns a list of substrings split using sep *)
(*** WARNING: the behavior of this function is odd (when sep=";;" or "£"; bug hidden here? ***)
val split : string -> string -> string list

(** 's_of_int i' Return "s" if i > 1, "" otherwise *)
val s_of_int : int -> string

(** 'waswere_of_int i' Return "were" if i > 1, "was" otherwise *)
val waswere_of_int  : int -> string

(** Escape \n & > for use in dot *)
val escape_string_for_dot : string -> string

(************************************************************)
(* Useful functions on Booleans *)
(************************************************************)
(** Evaluate both part of an 'and' comparison and return the conjunction *)
val evaluate_and : bool -> bool -> bool

(** Evaluate both part of an 'or' comparison and return the disjunction *)
val evaluate_or : bool -> bool -> bool

(** Evaluate all parts of a list and return the conjunction *)
(*** WARNING: this is NOT a List.exists! We WANT all elements to be evaluated without laziness ***)
val evaluate_all : bool list -> bool

(** XOR: returns true if both are different *)
val xor : bool -> bool -> bool

(** XNOR: returns true if both are true or both are false, i.e., when both are equal to each other *)
val xnor : bool -> bool -> bool


(************************************************************)
(* Date functions *)
(************************************************************)

(** Print the current date and time under the form of a string *)
val now : unit -> string



(**************************************************)
(* System functions *)
(**************************************************)

(** Read the first line of a file and convert to string *)
val read_first_line_from_file : string -> string

(** Read a file and convert to string *)
val read_from_file : string -> string

(** `write_to_file file_name file_content` will create a file `file_name` with content `file_content` *)
val write_to_file : string -> string -> unit

(** pow of x by e *)
val pow : Int32.t -> Int32.t -> Int32.t

(** pow of int of x by e *)
val pow_int : int -> int -> int

val modulo : Int32.t -> Int32.t -> Int32.t

(** Render a beautiful and cute json from an ugly horrible json *)
val prettify_json : string -> string

(** equivalent to List.filter_map of OCaml 4.08, but reverse the list *)
val rev_filter_map : ('a -> 'b option) -> 'a list -> 'b list

val list_to_string_set : string list -> CustomModules.StringSet.t
val string_set_to_list : CustomModules.StringSet.t -> string list
val var_set_to_list : CustomModules.VarSet.t -> (string * int) list

(** Convert list to array *)
val array_of_list : 'a list -> 'a array

(** Convert array to list *)
val list_of_array : 'a array -> 'a list

(** Unit as binary operator *)
val bin_unit : unit -> unit -> unit