(************************************************************
 *
 *                       IMITATOR
 * 
 * Aarhus University
 * 
 * Module description: Priority Queue (Smallest element has highest priority!)
 *                     Implemented using a Leftist Heap
 * 
 * File contributors : Mikael Bisgaard Dahlsen-Jensen
 * Created           : 2023/07/12
 *
 ************************************************************)

type comparison = Equal | GreaterThan | LessThan

module type OrderedSig =
  sig
    type t
    val compare: t -> t -> comparison
  end

module type PriorityQueueSig =
  sig
    exception Queue_is_empty
    type t
    type queue
    type rank
    val empty : queue
    (* Time complexity: O (log n) *)
    val merge : queue -> queue -> queue

      (* Time complexity: O (1) *)
    val is_empty : queue  -> bool

    (* Time complexity: O (log n) *)
    val insert : queue  -> t -> queue

    (* Time complexity: O (log n) *)
    val extract : queue -> t * queue

    (* Time complexity: O (1) *)
    val peek : queue -> t

    (* Time complexity: O (1) *)
    val length : queue -> int

    (* Time complexity: O (n) *)
    val to_list : queue -> t list

    (* Time complexity: O (n) *)
    val of_list : t list -> queue
  end

module Make (Elt : OrderedSig) 
  : (PriorityQueueSig with type t = Elt.t) =
  struct
		exception Queue_is_empty
		type rank = int
		type t = Elt.t
		type inner_queue = Leaf | Node of inner_queue * t * inner_queue * rank
    type queue = inner_queue * int
		let empty = Leaf, 0
		let singleton elt = Node (Leaf, elt, Leaf, 1), 1
		let rank = function Leaf -> 0 | Node (_,_,_,r) -> r  
		let rec inner_merge t1 t2 =  
			match t1,t2 with
				| Leaf, t | t, Leaf -> t
				| Node (l, elt1, r, _), Node (_, elt2, _, _) ->
					match Elt.compare elt1 elt2 with
          | GreaterThan ->  inner_merge t2 t1 
          | LessThan | Equal -> 
            begin	
              let merged = inner_merge r t2 in 
              let rank_left = rank l and rank_right = rank merged in
              if rank_left >= rank_right then Node (l, elt1, merged, rank_right+1)
              else Node (merged, elt1, l, rank_left+1) 
            end
    let merge (q1, length1) (q2, length2) = inner_merge q1 q2, length1 + length2
		let insert t x = merge (singleton x) t
    let inner_extract = function
      |	Leaf -> raise Queue_is_empty
      | Node(left, elt, right, _)  -> elt, (inner_merge left right)
		let extract (q, length) = let (elt, inner_q) = inner_extract q in elt, (inner_q, length-1)
    let peek = function 
      | (Leaf, _) -> raise Queue_is_empty
      | (Node(_, elt, _, _), _)  -> elt
		let is_empty (queue, _) = queue = Leaf
    let length (_, length) = length
    let to_list (q, _) = 
      let rec unfold q = 
        match q with 
          | Leaf -> []
          | Node (_, _, _, _) -> 
            let elt, q' = inner_extract q in 
            elt :: unfold q' in
      unfold q
    let rec of_list l = 
        match l with 
          | [] -> empty
          | x::xs -> 
            insert (of_list xs) x
  end