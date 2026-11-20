module Const : sig type t = int end
module Clock : sig type t = int end
module Param : sig type t = int end


module Strictness : sig  
  type t = Strict | NonStrict
  val compare : t -> t -> int
  val min : t -> t -> t
end

type zval = Const.t array

module Valuation : sig 
  type t = {
    params : zval;
    clocks : zval;
  }
end

module ConstBound : sig 
  type t = (Strictness.t * Const.t) option

  val make : Strictness.t -> Const.t -> t

  val compare : t -> t -> int

  val min : t -> t -> t
  val max : t -> t -> t
end

module Interval : sig
  type t = { lb : ConstBound.t; ub : ConstBound.t }
  val top : t
  val intersect : t -> t -> t
end

module ParamBound : sig 
  type t = 
  | Upper of (Strictness.t * Param.t)
  | Lower of (Strictness.t * Param.t)
  | NoBound

  val instantiate : zval -> t -> Interval.t
  val to_string : t -> string
end

module Box : sig 
  type t = Interval.t array
  val top : int -> t
  val intersect : t -> t -> t
end

module Coupling : sig 
  type t = ParamBound.t array
  val top : int -> t
end


(* Semi-PDBM style zone representation *)
type t = {
  clocks : Box.t;
  params : Box.t;
  coupling : Coupling.t
}

val top : nb_clocks:int -> nb_parameters:int -> t

val zero_clock : nb_clocks:int -> nb_parameters:int -> t

val to_string : t -> string

val instantiate : zval -> t -> Box.t

val propagate : t -> max_constant:int -> t

val witness : t -> max_constant:int -> Valuation.t QCheck2.Gen.t

val membership : Valuation.t -> t -> bool