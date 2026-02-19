val bool_of_ratio : float -> bool QCheck2.Gen.t
val bounded_geo : ?bound:int -> float -> int QCheck2.Gen.t
val choose_k_array : int -> 'a array -> 'a array QCheck2.Gen.t