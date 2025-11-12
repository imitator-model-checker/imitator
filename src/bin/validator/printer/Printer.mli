open Format

type verbosity =
  | Silent | Experiments | Normal | Debug

type t 

val create : ?verbosity:verbosity -> ?formatter:formatter -> unit -> t

val info : t -> ('a, formatter, unit, unit) format4 -> 'a 

val debug : t -> ('a, formatter, unit, unit) format4 -> 'a 

val warn : t -> ('a, formatter, unit, unit) format4 -> 'a 

val error : t -> ('a, formatter, unit, unit) format4 -> 'a

val start_section : t -> ('a, formatter, unit, unit) format4 -> 'a

val end_section : t -> unit

val with_section : t -> (unit, formatter, unit, unit) format4 -> (unit -> 'a) -> 'a

val flush : unit -> unit