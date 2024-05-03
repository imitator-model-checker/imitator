(************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 * 
 * Module description: Useful OCaml functions
 * 
 * File contributors : Étienne André
 * Created           : 2014/10/24
 *
 ************************************************************)
 

(************************************************************)
(* Modules *)
(************************************************************)
open Unix


(************************************************************)
(* Useful functions on integers *)
(************************************************************)
let rec is_a_power_of_2_rec n =
	if n mod 2 <> 0 then false
	else if n = 2 then true
	else is_a_power_of_2_rec (n/2)


(** Check if an integer is a power of two, i.e., n = 2^m, with m >= 1 *)
let is_a_power_of_2 n =
	if n < 2 then false
	else is_a_power_of_2_rec n

(*;;
let tests = [1; 2; 3; 4; -5; 0; 7; 8; 4; 232323; 2048; 76] in
List.iter (fun n ->
	print_string ("\nIs " ^ (string_of_int n) ^ " a power of 2? " ^ (string_of_bool (is_a_power_of_2 n)))
	) tests;
exit(1)*)


(************************************************************)
(* Useful functions on options *)
(************************************************************)

(** Get the value of an 'a option that is assumed to be different from None, or raise NoneException otherwise *)
let a_of_a_option = function
	| Some thing -> thing
	| None -> raise Exceptions.NoneException

(*(** Apply function to an element if Some, else return None *)
let apply_or_none f = function
    | Some x -> Some (f x)
    | None -> None*)

(************************************************************)
(* Useful lambda functions *)
(************************************************************)
let identity x = x

(************************************************************)
(* Useful functions on tuples *)
(************************************************************)

(* Get first element of tuple *)
let first_of_tuple (x, _) = x
(* Get second element of tuple *)
let second_of_tuple (_, x) = x
(* Apply a tuple as argument to a function *)
let apply_tuple f (a, b) = f a b
(* Map first element of tuple *)
let map_first_of_tuple f (x, y) = f x, y
(* Get first element of triplet *)
let first_of_triplet (x, _, _) = x
(* Get second element of triplet *)
let second_of_triplet (_, x, _) = x
(* Get third element of triplet *)
let third_of_triplet (_, _, x) = x

(************************************************************)
(* Useful functions on lists *)
(************************************************************)

(** Check if a list is empty *)
let list_empty l =
	l = []

(** Return a random element in a list *)
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


(**  Returns the last element of the list, or raise Empty_list if the list is empty. This function takes linear time *)
let list_last = ExtList.List.last


(**  Returns the list without its last element; raises Empty_list if the list is empty *)
let list_without_last l =
	(* Check non-empty list *)
	if List.length l = 0 then raise ExtList.List.Empty_list;
	(* Split at position l-1 *)
	let result, _ = ExtList.List.split_nth (List.length l - 1) l in
	(* Return *)
	result


(**  Returns a pair (the list without its last element, the last element of the list), or raise Empty_list if the list is empty. *)
let list_split_last l =
	(* Check non-empty list *)
	if List.length l = 0 then raise ExtList.List.Empty_list;
	(* Split at position l-1 *)
	let rest, last = ExtList.List.split_nth (List.length l - 1) l in
	(* Return *)
	match last with
	| [last] -> rest, last
	| _ -> raise (Exceptions.InternalError("Wrong split in list_split_last"))




(* Return a list where every element only appears once *)
(*** WARNING: exponential here ***)
(*let list_only_once l =
	List.rev (List.fold_left
		(fun current_list e -> if List.mem e current_list then current_list else e::current_list)
		[]
		l
	)*)
(*** WARNING: not tested ***)
let list_only_once l = ExtList.List.unique ~cmp:(=) l


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
(*let rec list_remove_first_occurence e = function
	| [] -> []
	| first :: rest -> if e = first then rest
		else first :: (list_remove_first_occurence e rest)*)
(*** WARNING: not tested ***)
let list_remove_first_occurence e l = ExtList.List.remove l e

(* Remove all empty strings in list *)
let list_without_empty_string = List.filter (fun s -> s <> "")

(** Remove the ith element of a list *)
let list_delete_at i al =
	(* First check the arguments *)
	if i < 0 then raise (Invalid_argument "list_delete_at");
	if i >= List.length al  then raise (Failure "list_delete_at");
	let rec del i = function
		| [] -> []
		| _::t when i = 0 -> t
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
		| _::t when i = 0 -> elem::t
		| h::t -> h :: set (i - 1) elem t
	in
	set i elem l

let list_combination l1 l2 =
    let acc = [] in
    let l = ref acc in
    for i = 0 to (List.length l1) - 1 do
        for j = 0 to (List.length l2) - 1 do
            let e1 = List.nth l1 i in
            let e2 = List.nth l2 j in
            if e1 <> e2 then
                l := (e1, e2)::!l;
        done;
    done;
    !l

let list_combination_2 l1 l2 =
    let acc = [] in
    let l = ref acc in
    for i = 0 to (List.length l1) - 1 do
        for j = 0 to (List.length l2) - 1 do
            let e1 = List.nth l1 i in
            let e2 = List.nth l2 j in
            l := (e1, e2)::!l;
        done;
    done;
    !l

(** Select the sublist of a list from position i to position j *)
let sublist minb maxb l =
	if minb < 0 || maxb >= (List.length l) || minb > maxb then(
		raise (Invalid_argument "sublist")
	);
	let rec sublist_rec minb maxb l =
	match l with
		| [] -> raise (Invalid_argument "sublist")
		| h :: t -> 
			let tail = if maxb = 0 then [] else sublist_rec (minb - 1) (maxb - 1) t in
			if minb > 0 then tail else h :: tail
	in sublist_rec minb maxb l

(* Partition list by grouping elements by keys in a list of tuples *)
let group_by keySelector l =
    let keys = List.map keySelector l in
    let uniq_keys = list_only_once keys in
    let group_by_keys = List.map (fun key -> (key, List.filter (fun x -> keySelector x = key) l)) uniq_keys in
    group_by_keys

(* Partition list by grouping elements by keys in a list of tuples *)
(* and map values associated by keys according to valueSelector function *)
let group_by_and_map keySelector valueSelector l =
    let keys = List.map keySelector l in
    let uniq_keys = list_only_once keys in
    let group_by_keys = List.map (fun key -> (key, List.map valueSelector (List.filter (fun x -> keySelector x = key) l))) uniq_keys in
    group_by_keys

(* Type used for partition map *)
type ('a, 'b) my_either = My_left of 'a | My_right of 'b

(* Partition and map list *)
let partition_map f l =
    let lm = List.map f l in
    let a, b = List.partition (function My_left _ -> true | My_right _ -> false) lm in
    List.map (function My_left x -> x | My_right _ -> raise (Exceptions.InternalError "impossible")) a,
    List.map (function My_left _ ->  raise (Exceptions.InternalError "impossible") | My_right x -> x) b


(* Partition list by grouping elements by keys in a hashtable *)
let hashtbl_group_by keySelector l =
    let group_by_keys = group_by keySelector l in
    let table = Hashtbl.create (List.length group_by_keys) in

    for i = 0 to (List.length group_by_keys) - 1 do
        let key, group = List.nth group_by_keys i in
        Hashtbl.add table key group
    done;
    table

(* Check if predicate is true for all arrangement of list *)
let for_all_in_arrangement predicate l =
    let result = ref true in
    for i = 0 to (List.length l) - 1 do
        for j = i to (List.length l) - 1 do
            result := !result && predicate (List.nth l i) (List.nth l j)
        done
    done;
    !result

(************************************************************)
(** Useful functions on arrays *)
(************************************************************)

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
		incr i;
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
			elem -> if p elem then (raise Exceptions.Found);
		) a;
		(* Not found *)
		false
	) with Exceptions.Found -> true


(*** NOTE: important, otherwise always the same "random" ! ***)
;;Random.self_init();;
(** Shuffles the values of an array *)
(*** NOTE: Found online at http://www.codecodex.com/wiki/index.php?title=Shuffle_an_array ***)
(*** WARNING: not a real shuffle! the first element is always at the end... ***)
let array_shuffle a = Array.sort (fun _ _ -> (Random.int 3) - 1) a

(** Perform the substraction of 2 NumConst array of same size **)
let sub_array array1 array2 =
  (* Create the result *)
  let result = Array.make (Array.length array1) NumConst.zero in
  (* Iterate on both arrays *)
  for i = 0 to (Array.length array1) - 1 do
    (* Perform array1 - array2 *)
    result.(i) <- NumConst.sub array1.(i) array2.(i);
  done;
  (* Return the result *)
  result


(************************************************************)
(* Useful functions on dynamic arrays *)
(************************************************************)

(** exists p {a1; ...; an} checks if at least one element of the DynArray satisfies the predicate p. That is, it returns (p a1) || (p a2) || ... || (p an). *)
let dynArray_exists p a =
	try(
		DynArray.iter (fun 
			elem -> if p elem then (raise Exceptions.Found);
		) a;
		(* Not found *)
		false
	) with Exceptions.Found -> true

(************************************************************)
(* Useful functions on queues *)
(************************************************************)

(** Transform a list into a queue *)
let queue_of_list l =
	let queue = Queue.create() in
	(* Add all elements from l *)
	List.iter (fun elem -> Queue.add elem queue) l;
	(* Return the queue *)
	queue

(** Transform a queue into a list *)
let list_of_queue queue =
	List.rev (Queue.fold (fun current_list elem -> elem :: current_list) [] queue)


(************************************************************)
(* Useful functions on hash tables *)
(************************************************************)
(** Get all bound keys in an hash table; multiple bindings yield multiple (identical) keys *)
(*** NOTE: indeed, in our setting, we only use hashtbl with a single binding ***)
let hashtbl_get_all_keys hashtbl =
	Hashtbl.fold
		(fun key _ current_list ->
			key :: current_list)
		hashtbl []

(** Get the binding associated to a key, or the default binding if key is not associated to any binding *)
let hashtbl_get_or_default hashtbl key default_value =
	if not (Hashtbl.mem hashtbl key) then default_value
	else Hashtbl.find hashtbl key

(** function to filter hash table with a predicate on keys *)
let hashtbl_filter pred =
	Hashtbl.filter_map_inplace (fun k v -> if pred k then Some v else None)

let hashtbl_of_tuples tuples =
    let table = Hashtbl.create (List.length tuples) in
    List.iter (fun (a, b) ->
        Hashtbl.add table a b
    ) tuples;
    table

(************************************************************)
(* Useful functions on string *)
(************************************************************)
(** Convert an array of string into a string *)
let string_of_array_of_string =
	Array.fold_left (fun the_string s -> the_string ^ s) ""

(** Returns a fresh string made of 'n' times 's' *)
let string_n_times n s =
	string_of_array_of_string (Array.make n s)

(** Convert a list of string into a string *)
let string_of_list_of_string =
	List.fold_left (fun the_string s -> the_string ^ s) ""

(** Convert an array of string into a string with separators *)
let string_of_array_of_string_with_sep sep a =
	let length = Array.length a in
	if length = 0 then "" else(
		let the_string = ref "" in
		for i = 0 to length - 2 do
			the_string := (!the_string) ^ a.(i) ^ sep
		done;
		!the_string ^ a.(length - 1)
	)

(* Compute the number of non-empty elements in an array *)
let length_of_array_without_empty_elements a =
	let classic_array_length = Array.length a in
	let number_non_empty_elements = ref 0 in
	for i = 0 to classic_array_length - 1 do
		if a.(i) <> "" then number_non_empty_elements := (!number_non_empty_elements) + 1;
	done;
	!number_non_empty_elements

(* Convert an array of string into a string with separators removing empty strings *)
let string_of_array_of_string_with_sep_without_empty_strings sep a =
	let length = Array.length a in
	let number_elements_non_empty = length_of_array_without_empty_elements a in
	if number_elements_non_empty = 0 then "" else (
		if number_elements_non_empty = 1 then a.(0) else (
		let the_string = ref "" in
		for i = 0 to length - 2 do
		    let s = a.(i) in
		    if s <> "" then
			    the_string := (!the_string) ^ s ^ sep;
		done;
		!the_string ^ a.(length - 1)
	))

(** Convert a list of string into a string with separators *)
let rec string_of_list_of_string_with_sep sep = function
	| [] -> ""
	| [elem] -> elem
	| head :: tail -> head ^ sep ^ (string_of_list_of_string_with_sep sep tail)

(** Convert a list of string into a string with separators removing empty strings *)
let string_of_list_of_string_with_sep_without_empty_strings sep list =
    string_of_list_of_string_with_sep sep (List.filter (fun string -> string<>"") list)

(** Convert a list of int into a string with , separator *)
let string_of_list_of_int l =
        "[" ^ (string_of_list_of_string_with_sep ", " (List.map string_of_int l)) ^ "]"

(* Returns a list of substrings splitted using sep *)
(*** WARNING: the behavior of this function is odd (when sep=";;" or "£"; bug hidden here? ***)
let split sep = Str.split (Str.regexp ("[" ^ sep ^ "]"))

(* Add \t identation of string according to the given level *)
let indent_paragraph indent_level s =
    let str_tabulations = string_n_times indent_level "\t" in
    let lines = split "\n" s in
    List.fold_left (fun the_string line -> the_string ^ str_tabulations ^ line) "" lines

(** 's_of_int i' Return "s" if i > 1, "" otherwise *)
let s_of_int i =
	if i > 1 then "s" else ""


(** 'waswere_of_int i' Return "were" if i > 1, "was" otherwise *)
let waswere_of_int  i =
	if i > 1 then "were" else "was"


(** Escape \n & > & < for use in dot *)
let escape_string_for_dot str =
	(*** BUG: cannot work with global replace ***)
(*		Str.global_substitute (Str.regexp ">\\|&") (fun s -> if s = ">" then "\\>" else if s = "&" then "\\&" else s)
			str*)
(* 		Str.global_replace (Str.regexp "\\(>\\|&\\)") ("\\" ^ "\\(\\1\\)") *)
	Str.global_replace (Str.regexp "\"") ("\\\"")
		(Str.global_replace (Str.regexp "\n") (" \\n ")
			(Str.global_replace (Str.regexp ">") ("\\>")
				(Str.global_replace (Str.regexp "<") ("\\<")
					(Str.global_replace (Str.regexp "&") ("\\&") str)
				)
			)
		)



(************************************************************)
(* Useful functions on booleans *)
(************************************************************)

(** Evaluate both parts of an 'and' comparison and return the conjunction *)
let evaluate_and a b =
(*	let computed_a = a in
	let computed_b = b in
	computed_a && computed_b*)
	a && b

(** Evaluate all parts of a list and return the conjunction *)
(*** WARNING: this is NOT a List.exists! We WANT all elements to be evaluated without laziness ***)
let evaluate_all =
	List.fold_left (fun current_result element -> evaluate_and current_result element) true

(** Evaluate both part of an 'or' comparison and return the disjunction *)
let evaluate_or a b =
	a || b


(** XOR: returns true if both are different *)
let xor (a : bool) (b : bool) : bool =
	a <> b

(** XNOR: returns true if both are true or both are false, i.e., when both are equal to each other *)
let xnor (a : bool) (b : bool) : bool =
	a = b


(************************************************************)
(** Useful functions on floats *)
(************************************************************)
(*
(** round_n n f rounds float f with n decimal digits *)
let round_n n f =
	let factor = 10. ** (float_of_int n) in
(*	print_float factor;
	exit 0;*)
	(float_of_int (int_of_float (floor  (f *. factor)))) /. factor*)


(************************************************************)
(* Printing time functions *)
(************************************************************)
let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
				"Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

(** 'add_digits n i' adds (m-n) '0' in front of 'i', if 'i' is an integer with only 'm' digits; result is always a string *)
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


(** Adds a zero if a number has only 1 digit *)
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


(*** TODO: factor the 2 following functions ***)

(** Round a float with 1 digit after comma, and convert to string *)
let round1_float d =
	(* Integer part *)
	let int_part = string_of_int (int_of_float (floor d)) in
	(* Floating part on 3 digits *)
	let real_part = add_digits 1 ((int_of_float (d *. 10.0)) mod 10) in
	(* Concatenate both *)
	int_part ^ "." ^ real_part

(* Round a float with 3 digits after comma, and convert to string *)
let round3_float d =
(* 	((float_of_int) (int_of_float (d *. 1000.0))) /. 1000.0 *)
	(* Integer part *)
	let int_part = string_of_int (int_of_float (floor d)) in
	(* Floating part on 3 digits *)
	let real_part = add_digits 3 ((int_of_float (d *. 1000.0)) mod 1000) in
	(* Concatenate both *)
	int_part ^ "." ^ real_part


(**************************************************)
(* System functions *)
(**************************************************)

(** Read the first line of a file and convert to string *)
let read_first_line_from_file file_name =
	(*** TODO: test for file existence! ***)
	let ic = open_in file_name in
	input_line ic

(** Read a file and convert to string *)
let read_from_file file_name =
	(*** TODO: test for file existence! ***)
	let ic = open_in file_name in
	Std.input_all ic


(** `write_to_file file_name file_content` will create a file `file_name` with content `file_content` *)
let write_to_file file_name file_content =
	(*** TODO: test for file existence! ***)
	let oc = open_out file_name in
	(* Write file *)
	output_string oc file_content;
	(* Close channel *)
	close_out oc;
	()

(* pow of x by e *)
let pow x e =
    let rec pow_rec r = function
        | e when Int32.equal e Int32.zero -> Int32.one
        | e when Int32.equal e Int32.one -> r
        | e -> pow_rec (Int32.mul x r) (Int32.sub e Int32.one)
    in pow_rec x e

let pow_int x e =
    let rec pow_rec r = function
        | e when e = 0 -> 1
        | e when e = 1 -> r
        | e -> pow_rec (x * r) (e - 1)
    in pow_rec x e

let modulo x y = Int32.sub x (Int32.mul (Int32.div x y) y)

(* Render a beautiful and cute json from an ugly horrible json *)
let prettify_json json =

    (* Set start indent level *)
    let indent_level_ref = ref 0 in

    let prettify_json_char c (* char *) pc (* previous char *) =

        let indent_level = !indent_level_ref in

        (* Generate tabulations at different levels *)
        let tabs = string_n_times indent_level "\t" in
        let tabs_0 = if indent_level >= 1 then string_n_times (indent_level - 1) "\t" else "" in
        let tabs_1 = string_n_times (indent_level + 1) "\t" in

        (* Check current char *)
        match c with
        | '{'
        | '[' as c ->
            indent_level_ref := indent_level + 1;
            "\n" ^ tabs ^ String.make 1 c ^ "\n" ^ tabs_1
        | '}'
        | ']' as c ->
            indent_level_ref := indent_level - 1;
            "\n" ^ tabs_0 ^ String.make 1 c ^ "\n"
        | ',' ->
            begin
            match pc with
            | '}'
            | ']' ->
                tabs ^ ",\n" ^ tabs
            | _ ->
                ",\n" ^ tabs
            end
        | c ->
            String.make 1 c
    in

    (* *)
    let pretty_json = ref (prettify_json_char (String.get json 0) ' ') in

    (* Generate pretty json iterating through ugly json *)
    for i = 1 to (String.length json) - 1 do
        pretty_json := !pretty_json ^ prettify_json_char (String.get json i) (String.get json (i - 1))
    done;

    (* Return pretty json *)
    !pretty_json

let rev_filter_map f l =
    List.map f l |>
    List.filter (fun x -> match x with | None -> false | Some _ -> true) |>
    List.fold_left (fun acc x -> match x with | None -> acc | Some x -> x :: acc) []

let list_to_string_set x = x |> List.to_seq |> CustomModules.StringSet.of_seq
let string_set_to_list x = x |> CustomModules.StringSet.to_seq |> List.of_seq
let var_set_to_list x = x |> CustomModules.VarSet.to_seq |> List.of_seq

(** Convert list to array *)
let array_of_list x = x |> List.to_seq |> Array.of_seq

(** Convert array to list *)
let list_of_array x = x |> Array.to_seq |> List.of_seq

(** Unit as binary operator *)
let bin_unit (a : unit) (b : unit) = a; b; ()
