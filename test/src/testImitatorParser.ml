open OUnit
open ImitatorParser

let test_nothing _ = 
	assert_equal 1 1

(** Build testsuite **)

let suite = "testImitatorParser" >::: 
	["test_nothing" >:: test_nothing]
			

