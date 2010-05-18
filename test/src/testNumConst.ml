open OUnit

let a = NumConst.numconst_of_float 0.4
let b = NumConst.numconst_of_frac 1 3
let c = NumConst.numconst_of_int 2
let d = NumConst.numconst_of_float 2.00
let e = NumConst.numconst_of_frac 306 153

let test1 _ =
		assert_bool "a > b" (NumConst.g a b)
		
let test2 _ =
		assert_bool "b <= c" (NumConst.le b c)
		
let test3 _ = 
		assert_bool "c = d" (NumConst.equal c d);
		assert_bool "d = e" (NumConst.equal d e)
		
(** Build testsuite **)

let suite = "testImitatorParser" >::: 
	["test1" >:: test1;
	 "test2" >:: test2;
	 "test3" >:: test3]
			
