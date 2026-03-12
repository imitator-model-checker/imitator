(************************************************************
 *
 *                       IMITATOR
 * 
 * Aarhus University
 * 
 * Module description: A polymorphic hash table object that supports default elements.
                       Expects a function that generates a default element.
                       Values can be objects/references and it works as expected.
 * 
 * File contributors : Mikael Bisgaard Dahlsen-Jensen
 * Created           : 2024/05/23
 *
 ************************************************************)

class ['a, 'b] defaultHashTable : 
 ('a -> 'b) ->
  object 
    val mutable internal_tbl : ('a, 'b) Hashtbl.t
    method replace : 'a -> 'b -> unit
    method find : 'a -> 'b    
    method iter : ('a -> 'b -> unit) -> unit
    method fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c
    method is_empty : bool
    method mem : 'a -> bool
    (* Merges keys according to a mapping (mergee -> merger) 
        along with a function describing how to merge their value (for example list concatenation if values are lists) *)
    method merge_keys : ('a -> 'a) -> ('b -> 'b -> 'b) -> unit
  end