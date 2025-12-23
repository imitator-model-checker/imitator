open Validator_spec

type t = { seed : string; mutable i : int }

let create ~seed = {seed; i = 0}

let next_byte br =
  let c = String.get br.seed (br.i mod String.length br.seed) in
  br.i <- br.i + 1;
  int_of_char c

let next_bool br ~prob =
  let threshold = int_of_float (prob *. 255.) in 
  next_byte br < threshold

let next_int br ?(min = 0) range =
  let max = min + range in 
  assert (max <= 255);
  (next_byte br mod max) + min

let sample_uniform br ~from =
  let idx = next_int br (List.length from) in 
  List.nth from idx

let sample_dist t : Spec.dist -> int = function
  | Exact n -> n
  | Range (min, n) -> next_int t ~min n
