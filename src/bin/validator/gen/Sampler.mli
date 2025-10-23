type t 

val create : seed:string -> t

val next_bool : t -> prob:float -> bool

val next_int : t -> ?min:int -> int -> int

val sample_uniform : t -> from:('a list) -> 'a