open QCheck2
open Gen
open PZone


let box_around valuation max_constant =
  flatten_a @@
  Array.map (fun v -> 
    let+ l = int_range 0 v
    and+ u = int_range v max_constant in 
    let lb = ConstBound.make Strictness.NonStrict l in 
    let ub = ConstBound.make Strictness.NonStrict u in 
    ({lb; ub} : Interval.t)
    ) 
  valuation 

let couple_around c_valuation p_valuation =
  let nb_parameters = Array.length p_valuation in 
  flatten_a @@
  Array.map (fun v_c ->
    let open ParamBound in
    let open Strictness in 
    let* p = int_range 0 (nb_parameters - 1) in 
    let candidates = 
      if v_c < p_valuation.(p) then 
        [Upper (NonStrict, p); Upper (Strict, p)]
      else if v_c > p_valuation.(p) then 
        [Lower (NonStrict, p); Lower (Strict, p)]
      else
        [Upper (NonStrict, p); Lower (NonStrict, p)]  
    in
    oneofl candidates
    )
  c_valuation

type atom = Clock of int | Param of int | Coupling of int


let thin k pz = 
  let nb_clocks = Array.length pz.clocks in 
  let nb_parameters = Array.length pz.params in 
  let nb_coupling = Array.length pz.coupling in 
  let nb_atoms = nb_clocks + nb_parameters + nb_coupling in 
  let atom_indices = Array.init nb_atoms Fun.id in 
  let+ chosen_indices = BasicGens.choose_k_array k atom_indices in 

  let atom_type_of_index id = 
    if id < nb_clocks then Clock id
    else if id < nb_clocks + nb_parameters then Param (id - nb_clocks)
    else Coupling (id - (nb_clocks + nb_parameters))
  in

  let thinned = PZone.top ~nb_clocks ~nb_parameters in 
  Array.iter 
    (fun i -> 
      match atom_type_of_index i with
      | Clock x -> thinned.clocks.(x) <- pz.clocks.(x)
      | Param p -> thinned.params.(p) <- pz.params.(p)
      | Coupling c -> thinned.coupling.(c) <- pz.coupling.(c)
      )
  chosen_indices;
  thinned

let gen ~nb_clocks ~nb_parameters ~max_constant ~seed = 
  let n = nb_clocks + nb_parameters in 
  let witness = PZone.witness seed ~max_constant in

  let* atom_amount = BasicGens.bounded_geo ~bound:n 0.5
  and* clocks : Box.t = box_around witness.clocks max_constant
  and* params : Box.t = box_around witness.params max_constant
  and* coupling : Coupling.t = couple_around witness.clocks witness.params in 

  let max = {
    clocks;
    params;
    coupling
  } in

  let max_prop = PZone.propagate max in 

  assert (PZone.membership witness max_prop);

  let* thinned = thin atom_amount max_prop in 
  assert (PZone.membership witness thinned);

  pure thinned