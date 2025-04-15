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

class ['a, 'b] defaultHashTable (default : 'a -> 'b) = object
  val mutable internal_tbl : ('a, 'b) Hashtbl.t = Hashtbl.create 100
	method replace key value = Hashtbl.replace internal_tbl key value
	method find key =  try Hashtbl.find internal_tbl key with
                      Not_found -> 
                        let x = default key in Hashtbl.replace internal_tbl key x; x 
	method iter f = Hashtbl.iter f internal_tbl
  method fold : 'c. ('a -> 'b -> 'c -> 'c) -> 'c -> 'c = 
    fun f init -> Hashtbl.fold f internal_tbl init
  method is_empty = Hashtbl.length internal_tbl = 0
  (* Merges keys according to a mapping (mergee -> merger) 
      along with a function describing how to merge their value (for example list concatenation if values are lists) *)
  method merge_keys (key_mapping : 'a -> 'a) (value_merger : 'b -> 'b -> 'b) = 
    let new_tbl = Hashtbl.create 100 in 
    let rec lookup_key_mapping elem = 
      let res = key_mapping elem  in if res = elem then elem else lookup_key_mapping res in 
    Hashtbl.iter (
      fun key value -> 
        let merger = lookup_key_mapping key in 
        if merger = key then
          Hashtbl.add new_tbl key value
        else 
          Hashtbl.add new_tbl merger (value_merger (Hashtbl.find internal_tbl merger) (Hashtbl.find internal_tbl key))
    )
    internal_tbl
end