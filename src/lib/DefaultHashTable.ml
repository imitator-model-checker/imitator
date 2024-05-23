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

class ['a, 'b] defaultHashTable (default : unit -> 'b) = object
  val mutable internal_tbl : ('a, 'b) Hashtbl.t = Hashtbl.create 100
	method replace key value = Hashtbl.replace internal_tbl key value
	method find key =  try Hashtbl.find internal_tbl key with
                      Not_found -> 
                        let x = default () in Hashtbl.replace internal_tbl key x; x 
	method iter f = Hashtbl.iter f internal_tbl
  method fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c = 
    fun f init -> Hashtbl.fold f internal_tbl init
  method is_empty = Hashtbl.length internal_tbl = 0
end 