open QCheck2
open SimpleModel
open Validator_config

let gen_of_dist : Spec.dist -> int Gen.t = function 
  | Exact n -> Gen.pure n
  | Range (min, max) -> Gen.int_range ~origin:min min max


  
module Transition = struct 
  open Gen
  let resets nb_clocks prob =
    let* add_reset = BasicGens.bool_of_ratio prob in
    if not add_reset then return []
    else 
      let* reset_amount = BasicGens.bounded_geo ~bound:nb_clocks prob in
      let* reset_clocks = BasicGens.choose_k_array reset_amount (Array.init nb_clocks Fun.id) in
      return (Array.to_list reset_clocks)

  let transition ~nb_clocks ~nb_parameters ~invariant ~(spec : Spec.t) =

    let* guard =  
      let* is_guard = BasicGens.bool_of_ratio spec.guard_probability in 
      if not is_guard then 
        return @@ PZone.top ~nb_clocks ~nb_parameters
      else
        PZoneGen.gen ~nb_clocks ~nb_parameters ~seed:invariant in 
    let* controllable = BasicGens.bool_of_ratio spec.controllability_ratio in 
    let+ resets = resets nb_clocks spec.reset_probability in
    {controllable; guard; resets}
end




module Automaton = struct
  open Gen
  (* 
    Probability to be accepting = (i/(n-1))^2, scaled down such that the
    expected number of accepting locations per automaton = 1  
  *)
  let prob_accepting n i = 
    if n <= 1 then 1.0 else 

    let num = float_of_int (6 * i * i) in 
    let denom = float_of_int ((n - 1) * n * (2 * n - 1)) in

    num /. denom  


  let accepting_locations ~nb_auto ~nb_loc_of_automaton ~guarantee_accepting =
    flatten_a @@
    Array.init nb_auto (fun i ->
      let accepting_loc_exists = ref false in 
      flatten_a @@
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let prob = prob_accepting (nb_loc_of_automaton.(i)) j in
        let+ accepting = BasicGens.bool_of_ratio prob in 
        if accepting then accepting_loc_exists := true;
        let forced_accepting =  j = nb_loc_of_automaton.(i) - 1 && not !accepting_loc_exists && guarantee_accepting in
        accepting || forced_accepting 
      ))

  let invariants ~nb_auto ~nb_loc_of_automaton ~nb_clocks ~nb_parameters ~(spec : Spec.t) = 
    flatten_a @@
    Array.init nb_auto (fun i -> 
      flatten_a @@
      Array.init nb_loc_of_automaton.(i) (fun j ->
        let* has_invariant = BasicGens.bool_of_ratio spec.invariant_probability in 
        if not has_invariant then 
          pure @@ PZone.top ~nb_clocks ~nb_parameters
        else
          let seed = if j = 0 then 
            PZone.zero_clock ~nb_clocks ~nb_parameters
          else 
            PZone.top ~nb_clocks ~nb_parameters 
          in
          PZoneGen.gen ~nb_clocks ~nb_parameters ~seed
      ))

  let transitions_of_edge nb_clocks nb_parameters invariant spec nb_edges =
    flatten_l @@ List.init nb_edges (fun _ -> Transition.transition ~nb_clocks ~nb_parameters ~invariant ~spec)

  let transition_matrix_of_adjacency_matrix nb_clocks nb_parameters invariants spec adj = 
    adj
    |> Array.mapi (fun i row -> 
      row 
      |> Array.map @@ transitions_of_edge nb_clocks nb_parameters invariants.(i) spec
      |> flatten_a
    )
    |> flatten_a

  let transition_matrix ~spec ~nb_clocks ~nb_parameters ~nb_locations ~(invariants : PZone.t array) =
    Adjacency.generate ~nb_locations ~spec
    >>= transition_matrix_of_adjacency_matrix nb_clocks nb_parameters invariants spec

end

module Generator = struct 
  open Gen
  let gen (spec : Spec.t)  =
    let* nb_auto = (gen_of_dist spec.nb_automata) in

    let* nb_loc_per_automaton = array_repeat nb_auto (gen_of_dist spec.nb_locations)
    and* nb_clocks = (gen_of_dist spec.nb_clocks)
    and* nb_parameters = (gen_of_dist spec.nb_parameters) in 

    let* invariants =
      Automaton.invariants
        ~nb_auto ~nb_loc_of_automaton:nb_loc_per_automaton
        ~nb_clocks ~nb_parameters ~spec
    in
    let+ automata =
      flatten_l @@
      List.init nb_auto (fun i ->
        Automaton.transition_matrix
          ~nb_locations:nb_loc_per_automaton.(i) ~spec
          ~invariants:invariants.(i)
          ~nb_clocks ~nb_parameters)
    and+ accepting =
      Automaton.accepting_locations
        ~nb_auto ~nb_loc_of_automaton:nb_loc_per_automaton
        ~guarantee_accepting:true
    in
    { automata; accepting; nb_clocks; nb_parameters; invariants }
end

let gen = Generator.gen