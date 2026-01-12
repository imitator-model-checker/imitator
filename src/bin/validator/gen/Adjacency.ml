open Validator_config
open QCheck2.Gen 

module IntSet = Set.Make(Int)

let gen_of_dist : Spec.dist -> int t = function 
  | Exact n -> pure n
  | Range (min, max) -> int_range min max

let spanning_tree ~nb_locations ~current_edge_count ~ancestors ~max_edge_count = 
  let eligible_parents parents =
    parents
    |> List.filter (fun i -> current_edge_count.(i) < max_edge_count)
  in

  let m = Array.make_matrix nb_locations nb_locations 0 in

  let rec loop i parents  =
    if i >= nb_locations then return ()
    else
      let eligible = eligible_parents parents in
      let* parent = oneofl eligible in

      m.(parent).(i) <- m.(parent).(i) + 1;
      current_edge_count.(parent) <- current_edge_count.(parent) + 1;
      ancestors.(i) <- IntSet.add i ancestors.(parent);
      loop (i + 1) (i :: parents)
  in

  let* () = loop 1 [0] in
  return m


let reachable_nodes matrix src =
  let n = Array.length matrix in

  let neighbors u =
    let acc = ref IntSet.empty in
    for v = 0 to n - 1 do
      if matrix.(u).(v) > 0 then acc := IntSet.add v !acc
    done;
    !acc
  in

  let rec dfs stack visited =
    match stack with
    | [] -> visited
    | u :: rest ->
      if IntSet.mem u visited then
        dfs rest visited
      else
        let visited' = IntSet.add u visited in
        let stack' = IntSet.fold (fun v st -> v :: st) (neighbors u) rest in
        dfs stack' visited'
  in
  dfs [src] IntSet.empty

let generate ~nb_locations ~(spec : Spec.t)  =

  let current_edge_count = Array.make nb_locations 0 in 
  let max_edge_count = match spec.transitions_per_location with 
  | Range (_,max) -> max
  | Exact n -> n
  in

  let ancestors = Array.make nb_locations IntSet.empty in
  ancestors.(0) <- IntSet.singleton 0;

  let* matrix = match spec.all_reachable with
    | true -> spanning_tree ~nb_locations ~current_edge_count ~ancestors ~max_edge_count
    | false -> return (Array.make_matrix nb_locations nb_locations 0)
  in


  let legal_edge i j  = if spec.cycles then true else 
    not (IntSet.mem i (reachable_nodes matrix j)) in

  let add_edge_to_row row i =
    Array.mapi (fun j curr -> if i = j then curr + 1 else curr) row
  in

  let add_random_edges_to_row = fun i row ->
    let* nb_transitions = gen_of_dist spec.transitions_per_location in 
    let nb_edges_to_add = nb_transitions - current_edge_count.(i) in 
    if nb_edges_to_add > 0 then
      (let all_nodes = (List.init nb_locations (fun x -> x)) in 

      let rec select_edges edges_left remaining_nodes acc = 
         let available_nodes = List.filter (fun j -> legal_edge i j) remaining_nodes in 
         if available_nodes = [] || edges_left = 0 then return acc
         else (
           let* j = oneofl available_nodes in
           select_edges (edges_left - 1) available_nodes (j::acc)
           )
      in

      let+ selected_edges = select_edges nb_edges_to_add all_nodes [] in
      List.fold_left (fun acc -> add_edge_to_row acc) row selected_edges)
    else
      return row
  in

  let rec loop i =
    if i >= nb_locations then return ()
    else
      (let* row = add_random_edges_to_row i matrix.(i) in
      matrix.(i) <- row;
      loop (i + 1))
  in

  let+ () = loop 0 in
  matrix