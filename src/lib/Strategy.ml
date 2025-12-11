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

(* Track parent-child derivation relationships *)
let derivation_parent : (strategy_index, strategy_index) Hashtbl.t = Hashtbl.create 100
let derivation_children : (strategy_index, strategy_index list) Hashtbl.t = Hashtbl.create 100

(* Memoization tables for strategy inclusion and killing *)
let killed_strategies : strategy_index list ref = ref []

let next_strategy_index = ref 0

(* Generate a fresh strategy index and initialize its inclusion list *)
let fresh_strategy_index () =
  let idx = !next_strategy_index in
  incr next_strategy_index;
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


(* Main insertion and retrieval function with memoization *)
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
           new_idx)
  | None ->
      let new_idx = fresh_strategy_index () in
      Hashtbl.add strategy_pool new_idx s;
      Hashtbl.add strategy_hash_map h [new_idx];
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
      
      (* Track parent→child relationship (bidirectional) *)
      if new_idx <> strat_index_source then (
        Hashtbl.add derivation_parent new_idx strat_index_source;
        let children = try Hashtbl.find derivation_children strat_index_source with Not_found -> [] in
        Hashtbl.replace derivation_children strat_index_source (new_idx :: children)
      );
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
  ()

(* Retrieve a strategy by its index *)
let get_strategy (idx : strategy_index) : strategy =
  Hashtbl.find strategy_pool idx

(* Kill strategy with control over propagation direction *)
let rec kill_strategy (idx : strategy_index) (from_propagate : bool) : unit =
  if not (List.mem idx !killed_strategies) then (
    print_message Verbose_experiments (Printf.sprintf "Killing strategy %d (from_propagate=%b)" idx from_propagate);
    killed_strategies := idx :: !killed_strategies;
    
    (* If killed naturally (not from propagate), kill ALL descendants *)
    if not from_propagate then (
      print_message Verbose_experiments (Printf.sprintf "  -> Natural kill: propagating DOWN to descendants");
      let children = try Hashtbl.find derivation_children idx with Not_found -> [] in
      List.iter (fun child_idx ->
        print_message Verbose_experiments (Printf.sprintf "     -> Killing descendant %d" child_idx);
        kill_strategy child_idx false  (* Recursively kill descendants *)
      ) children
    )
    (* If from propagate, we're going UP, don't touch children *)
  )

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

(* Propagate UP from killed strategies to ancestors *)
let propagate_killed_strategy () : unit =
  
  let rec kill_ancestors idx =
    kill_strategy idx true;  (* Mark as killed from propagation *)
    (* Recursively kill parent (going UP) *)
    match Hashtbl.find_opt derivation_parent idx with
    | Some parent_idx -> 
        print_message Verbose_experiments (Printf.sprintf "  -> Propagating UP to parent %d" parent_idx);
        kill_ancestors parent_idx
    | None -> ()
  in

  (* Apply to all initially killed strategies *)
  (* Note: we make a copy because killed_strategies will grow during iteration *)
  let initial_killed = !killed_strategies in
  List.iter kill_ancestors initial_killed

