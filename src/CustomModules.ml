(* Define a string set structure to gather sets of variables *)
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module VarRef = struct
   type t = string * int
   let compare = compare
end

module VarSet = Set.Make(VarRef)
