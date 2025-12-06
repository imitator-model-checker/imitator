open Automaton
open Exceptions
open ImitatorUtilities

(* A partial view consists of the automaton taking the action and the list of locations visible to the coalition *)
type partial_view = automaton_index * (location_index list)

(* A strategy is a sorted array of (partial_view, action_index) pairs *)
type strategy = (partial_view * action_index) array

type strategy_index = int

(* Pools for strategy storage and indexing *)
let strategy_pool : (strategy_index, strategy) Hashtbl.t = Hashtbl.create 100
let strategy_hash_map : (int, strategy_index list) Hashtbl.t = Hashtbl.create 100

(* Memoization tables for strategy inclusion and killing *)
let inclusion_pool : (strategy_index, strategy_index list) Hashtbl.t = Hashtbl.create 100
let killed_strategies : strategy_index list ref = ref []

let next_strategy_index = ref 0

(* Generate a fresh strategy index and initialize its inclusion list *)
let fresh_strategy_index () =
  let idx = !next_strategy_index in
  incr next_strategy_index;
  Hashtbl.add inclusion_pool idx [];
  idx

(* Hash a strategy based on its array content *)
let hash (s : strategy) : int =
  Hashtbl.hash (Array.to_list s)

(* Check structural equality between two strategies *)
let equal (s1 : strategy) (s2 : strategy) : bool =
  let len1 = Array.length s1 in
  let len2 = Array.length s2 in
  if len1 <> len2 then false
  else
    let rec aux i =
      if i = len1 then true
      else
        let (pv1, a1) = s1.(i) in
        let (pv2, a2) = s2.(i) in
        if pv1 <> pv2 || a1 <> a2 then false else aux (i + 1)
    in
    aux 0

(* Binary search for a partial_view in a sorted strategy *)
let binary_search (s : strategy) (pv : partial_view) : action_index option =
  let rec aux low high =
    if low > high then None
    else
      let mid = (low + high) / 2 in
      let (mpv, ma) = s.(mid) in
      match compare pv mpv with
      | 0 -> Some ma
      | n when n < 0 -> aux low (mid - 1)
      | _ -> aux (mid + 1) high
  in
  aux 0 (Array.length s - 1)

(* Insert a new (partial_view, action_index) in order into a sorted strategy *)
let add_in_order (s : strategy) (pv : partial_view) (a : action_index) : strategy =
  let len = Array.length s in
  let rec find_pos i =
    if i = len then i
    else
      let (cur_pv, _) = s.(i) in
      if compare pv cur_pv < 0 then i else find_pos (i + 1)
  in
  let pos = find_pos 0 in
  Array.init (len + 1) (fun i ->
    if i < pos then s.(i)
    else if i = pos then (pv, a)
    else s.(i - 1))

(* Check whether strategy s1 is included in strategy s2 *)
let included (s1 : strategy) (s2 : strategy) : bool =
  let len1 = Array.length s1 in
  let len2 = Array.length s2 in
  let rec loop i j =
    if i = len1 then true (* All elements of s1 are found in s2 *)
    else if j = len2 then false (* s2 exhausted before matching s1 *)
    else
      let (pv1, a1) = s1.(i) in
      let (pv2, a2) = s2.(j) in
      if a1 = a2 && pv1 = pv2 then
        loop (i + 1) (j + 1)
      else if compare pv1 pv2 < 0 then
        false
      else
        loop i (j + 1)
  in
  loop 0 0

(* Memoized public inclusion function *)
(* inclusion_pool[super] contains all strategies strictly included in it *)
let is_included (idx1 : strategy_index) (idx2 : strategy_index) : bool =
  List.mem idx1 (Hashtbl.find inclusion_pool idx2)

(* Efficiently fill the inclusion table by checking and propagating subset relations *)
let fill_inclusion_table new_idx new_strategy =
  let options = Input.get_options() in 
  if not options#memoized_strategies_inclusion then () else
  Hashtbl.iter (fun existing_idx existing_strategy ->
    if new_idx <> existing_idx then (
      (* Check if new ⊆ existing *)
      if Array.length new_strategy <= Array.length existing_strategy then (
        let incl = included new_strategy existing_strategy in
        if incl then (
          let current = Hashtbl.find inclusion_pool existing_idx in
          if not (List.mem new_idx current) then
            Hashtbl.replace inclusion_pool existing_idx (new_idx :: current);
        )
      );
      (* Check if existing ⊆ new *)
      if Array.length new_strategy >= Array.length existing_strategy then (
        let incl = included existing_strategy new_strategy in
        if incl then (
          let current = Hashtbl.find inclusion_pool new_idx in
          if not (List.mem existing_idx current) then
            Hashtbl.replace inclusion_pool new_idx (existing_idx :: current);
        )
      )
    )
  ) strategy_pool

(* Main insertion and retrieval function with memoization and inclusion update *)
let get_or_add_strategy (s : strategy) : strategy_index =
  let h = hash s in
  match Hashtbl.find_opt strategy_hash_map h with
  | Some candidates ->
      (match List.find_opt (fun idx -> equal s (Hashtbl.find strategy_pool idx)) candidates with
       | Some idx -> idx
       | None ->
           let new_idx = fresh_strategy_index () in
           Hashtbl.add strategy_pool new_idx s;
           Hashtbl.replace strategy_hash_map h (new_idx :: candidates);
           fill_inclusion_table new_idx s; 
           new_idx)
  | None ->
      let new_idx = fresh_strategy_index () in
      Hashtbl.add strategy_pool new_idx s;
      Hashtbl.add strategy_hash_map h [new_idx];
      fill_inclusion_table new_idx s; 
      new_idx

exception Conflicting_strategy

(* Extend a strategy by adding a new (partial_view, action_index), handling conflicts *)
let create_strategy (strat_index_source : strategy_index)
                    (pv : partial_view)
                    (a : action_index) : strategy_index =
  let s = Hashtbl.find strategy_pool strat_index_source in
  match binary_search s pv with
  | Some existing ->
      if existing = a then strat_index_source
      else raise Conflicting_strategy
  | None ->
      let new_s = add_in_order s pv a in
      let new_idx = get_or_add_strategy new_s in
      new_idx 

let initialize_strategies (entries : (partial_view * action_index) list) : unit =
  (* Ensure the strategy pool is empty before initialization *)
  if Hashtbl.length strategy_pool <> 0 then
    raise (InternalError "Strategy pool must be empty before initialization");

  (* Sort the entries by partial_view to maintain internal order *)
  let sorted_entries = List.sort (fun (pv1, _) (pv2, _) -> compare pv1 pv2) entries in
  let strat = Array.of_list sorted_entries in

  (* Register the strategy *)
  let new_idx = fresh_strategy_index () in
  let h = hash strat in

  Hashtbl.add strategy_pool new_idx strat;
  Hashtbl.add strategy_hash_map h [new_idx];
  Hashtbl.replace inclusion_pool new_idx [];

  (* No need to compute inclusion table — pool has only one element *)
  ()

(* Retrieve a strategy by its index *)
let get_strategy (idx : strategy_index) : strategy =
  Hashtbl.find strategy_pool idx

(* Mark a strategy as killed *)
let kill_strategy (idx : strategy_index) : unit =
  if not (List.mem idx !killed_strategies) then(
    print_message Verbose_experiments ("The strategy of index : "^ (string_of_int idx) ^" has been killed");
    killed_strategies := idx :: !killed_strategies)

(* List all strategies that have not been killed *)
let find_all_alive_strategies () : strategy_index list =
  let alive_strategies = ref [] in
  Hashtbl.iter (fun strategy_index _ ->
    if not (List.mem strategy_index !killed_strategies) then(
      alive_strategies := strategy_index :: !alive_strategies);
  ) strategy_pool;
  !alive_strategies

let is_dead (idx : strategy_index) : bool =
  List.mem idx !killed_strategies

(* Propagate the effect of killed strategies to all strategies they include *)
let propagate_killed_strategy () : unit =
  List.iter (fun strategy_index ->
    let included_in = Hashtbl.find inclusion_pool strategy_index in
    List.iter (fun strategy_index_included ->
      kill_strategy strategy_index_included
    ) included_in
  ) !killed_strategies
