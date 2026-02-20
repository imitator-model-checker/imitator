open QCheck2
open Gen


let bool_of_ratio ratio = 
  Gen.(
    let+ roll = float_bound_inclusive 1. in 
    roll <= ratio
  )

let bounded_geo ?bound p = 
  if bound = Some 0 then pure 0 else
  let rec aux r =
    let* roll = float_bound_inclusive 1. in 
    let hit_bound = match bound with Some k -> r >= k | None -> false in 
    if roll > p || hit_bound then pure r else aux (r + 1) 
  in
  aux 1

let choose_k_array (k:int) (a:'a array) : 'a array Gen.t =
  let n = Array.length a in
  if k < 0 || k > n then invalid_arg "choose_k_array_gen: k out of range";

  let b = Array.copy a in
  let acc = Array.make k b.(0) in

  let rec loop i =
    if i = k then
      pure acc
    else
      let* j = int_range i (n - 1) in
      let tmp = b.(i) in
      b.(i) <- b.(j);
      b.(j) <- tmp;
      acc.(i) <- b.(i);
      loop (i + 1)
  in
  if k = 0 then pure [||] else loop 0
