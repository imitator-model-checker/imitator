open Format

type verbosity =
  | Silent | Experiments | Normal | Debug

type t 

val create : ?verbosity:verbosity -> ?fmt:formatter -> unit -> t

val info : t -> ('a, formatter, unit, unit) format4 -> 'a 
val debug : t -> ('a, formatter, unit, unit) format4 -> 'a 
val warn : t -> ('a, formatter, unit, unit) format4 -> 'a 
