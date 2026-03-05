open QCheck2
open Gen
open PZone


let box_around valuation max_constant =
  flatten_a @@
  Array.map (fun v ->
    let* l = int_range 0 v
    and* u = int_range v max_constant
    and* flip_lb = bool
    and* flip_ub = bool in
    let lb_s = if l < v && flip_lb then Strictness.Strict else Strictness.NonStrict in
    let ub_s = if u > v && flip_ub then Strictness.Strict else Strictness.NonStrict in
    let lb = ConstBound.make lb_s l in
    let ub = ConstBound.make ub_s u in
    pure ({lb; ub} : Interval.t)
  )
  valuation

let couple_around c_valuation p_valuation =
  let nb_parameters = Array.length p_valuation in 
  let nb_clocks = Array.length c_valuation in
  if nb_parameters = 0 then pure (Array.init  nb_clocks (fun _ -> ParamBound.NoBound)) else
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

type atom =
  | Clock_lb of int | Clock_ub of int
  | Param_lb of int | Param_ub of int
  | Coupling of int


let thin k pz =
  let nb_clocks    = Array.length pz.clocks in
  let nb_parameters = Array.length pz.params in
  let nb_coupling  = Array.length pz.coupling in
  let nb_atoms = 2 * nb_clocks + 2 * nb_parameters + nb_coupling in
  let atom_indices = Array.init nb_atoms Fun.id in
  let+ chosen_indices = BasicGens.choose_k_array k atom_indices in

  let atom_type_of_index id =
    if id < nb_clocks then Clock_lb id
    else if id < 2 * nb_clocks then Clock_ub (id - nb_clocks)
    else if id < 2 * nb_clocks + nb_parameters then Param_lb (id - 2 * nb_clocks)
    else if id < 2 * nb_clocks + 2 * nb_parameters then Param_ub (id - (2 * nb_clocks + nb_parameters))
    else Coupling (id - (2 * nb_clocks + 2 * nb_parameters))
  in

  let thinned = PZone.top ~nb_clocks ~nb_parameters in
  Array.iter
    (fun i ->
      match atom_type_of_index i with
      | Clock_lb x ->
          thinned.clocks.(x) <- { thinned.clocks.(x) with lb = pz.clocks.(x).lb }
      | Clock_ub x ->
          thinned.clocks.(x) <- { thinned.clocks.(x) with ub = pz.clocks.(x).ub }
      | Param_lb p ->
          thinned.params.(p) <- { thinned.params.(p) with lb = pz.params.(p).lb }
      | Param_ub p ->
          thinned.params.(p) <- { thinned.params.(p) with ub = pz.params.(p).ub }
      | Coupling c ->
          thinned.coupling.(c) <- pz.coupling.(c)
    )
    chosen_indices;
  thinned

let gen ~nb_clocks ~nb_parameters ~max_constant ~seed = 
  let n = 3 * nb_clocks + 2 * nb_parameters in (* 2*clocks + 2*params + nb_coupling (=nb_clocks) *)
  let* witness = PZone.witness seed ~max_constant in

  let* atom_amount = BasicGens.bounded_geo ~bound:n 0.5
  and* clocks : Box.t = box_around witness.clocks max_constant
  and* params : Box.t = box_around witness.params max_constant
  and* coupling : Coupling.t = couple_around witness.clocks witness.params in 

  let max = {
    clocks;
    params;
    coupling
  } in

  let max_prop = PZone.propagate max ~max_constant in 

  assert (PZone.membership witness max_prop);

  let* thinned = thin atom_amount max_prop in 
  assert (PZone.membership witness thinned);

  pure thinned