open Validator_spec

module IntSet = Set.Make(Int)

let generate ~sampler ~nodes ~(spec : Spec.t)  =
  let matrix : int array array = Array.make_matrix nodes nodes 0 in 

  let edge_count = Array.make nodes 0 in 
  let max_edges_count = match spec.transitions_per_location with 
  | Range (min,range) -> min + range - 1
  | Exact n -> n
  in

  let ancestors = Array.make nodes IntSet.empty in
  ancestors.(0) <- IntSet.singleton 0;

  let eligible_parents parents =
    parents
    |> List.filter (fun i -> edge_count.(i) < max_edges_count)
  in

  (* Spanning tree *)
  if spec.all_reachable then 
    (let parents = ref [0] in
    for i = 1 to nodes - 1 do 
      let eligible_parents = eligible_parents !parents in 
      let parent = Sampler.sample_uniform sampler ~from:eligible_parents in
      matrix.(parent).(i) <- 1;
      edge_count.(parent) <- edge_count.(parent) + 1;
      ancestors.(i) <- IntSet.add i (ancestors.(parent));
      parents := i::!parents
    done);

  let legal_edge i j  = if spec.cycles then
      true
    else
      not (IntSet.mem j (ancestors.(i)))
  in
  let add_edge_to_row row i =
    Array.mapi (fun j curr -> if i = j then curr + 1 else curr) row
  in

  let add_random_edges_to_row = fun i row ->
    let nb_edges_to_add = Sampler.sample_dist sampler spec.transitions_per_location - edge_count.(i) in 
    if nb_edges_to_add > 0 then
      let all_nodes = (List.init nodes (fun x -> x)) in 
      let selected_edge_indices = List.init nb_edges_to_add (fun _ -> 
        let available_nodes = (List.filter (fun j -> legal_edge i j) all_nodes) in
        match available_nodes with
        | [] -> None
        | nodes ->
          let j = Sampler.sample_uniform sampler ~from:nodes in
          ancestors.(j) <- IntSet.union ancestors.(j) ancestors.(i);
          Some j) 
      in
      selected_edge_indices
      |> List.filter_map (fun x -> x)
      |> List.fold_left (fun acc -> add_edge_to_row acc) row 
    else
      row
  in
  Array.mapi add_random_edges_to_row matrix