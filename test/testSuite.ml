open OUnit

(* Collect the tests of different modules into one test suite *)
let suite = "IMITATOR2 test suite" >::: 
  [TestNumConst.suite;
	 TestImitatorParser.suite;
	 TestAutomaton.suite]

let _ =
  run_test_tt_main suite
