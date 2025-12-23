open Validator_spec

type t 

val create : seed:string -> t

val next_bool : t -> prob:float -> bool

val next_int : t -> ?min:int -> int -> int

val sample_uniform : t -> from:('a list) -> 'a

val sample_dist : t -> Spec.dist -> int