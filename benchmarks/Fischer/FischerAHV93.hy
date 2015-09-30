--************************************************************--
--************************************************************--
--
--   Fischer protocol for mutual exclusion (parametric timed version)
--
--   Model described in
--   "Parametric Real-Time Reasoning" (fig. 2)
--   Alur, Henzinger, Vardi (STOC 1993)
--
--   Etienne ANDRE
--   Laboratoire d'Informatique de Paris Nord
--
--                                Created : 2011/11/25
--                          Last modified : 2011/11/25
--************************************************************--
--************************************************************--


var
 	x, x_prime
		: clock;

	a, b, c, d
			: parameter;


--************************************************************--
  automaton lock
--************************************************************--
synclabs: is_0, is_0_prime, is_1, is_1_prime, is_2, is_2_prime,
	set_0, set_0_prime, set_1, set_2_prime
;
initially lock0;

loc lock0: while True wait {}
	when True sync is_0 do {} goto lock0;
	when True sync is_0_prime do {} goto lock0;
	when True sync set_1 do {} goto lock1;
	when True sync set_2_prime do {} goto lock2;

loc lock1: while True wait {}
	when True sync is_1 do {} goto lock1;
	when True sync is_1_prime do {} goto lock1;
	when True sync set_0 do {} goto lock0;
	when True sync set_0_prime do {} goto lock0;
	when True sync set_2_prime do {} goto lock2;

loc lock2: while True wait {}
	when True sync is_2 do {} goto lock2;
	when True sync is_2_prime do {} goto lock2;
	when True sync set_1 do {} goto lock1;
	when True sync set_0 do {} goto lock0;
	when True sync set_0_prime do {} goto lock0;

end -- lock



--************************************************************--
  automaton P1
--************************************************************--
synclabs: is_0, is_1, is_2, set_0, set_1;
initially P1_0;

loc P1_0: while True wait {}
	when True do {} goto P1_0;
	when True do {} goto P1_1;

loc P1_1: while True wait {}
	when True sync is_2 do {} goto P1_1;
	when True sync is_0 do {x' = 0} goto P1_2;

loc P1_2: while True wait {}
	when c < x & x < d sync set_1 do {x' = 0} goto P1_3;

loc P1_3: while True wait {}
	when a < x & x < b sync is_2 do {} goto P1_1;
	when a < x & x < b sync is_1 do {} goto P1_4;

loc P1_4: while True wait {}
	when True do {} goto P1_4;
	when True sync set_0 do {} goto P1_0;

end -- P1



--************************************************************--
  automaton P2
--************************************************************--
synclabs: is_0_prime, is_1_prime, is_2_prime, set_0_prime, set_2_prime;
initially P2_0;

loc P2_0: while True wait {}
	when True do {} goto P2_0;
	when True do {} goto P2_1;

loc P2_1: while True wait {}
	when True sync is_1_prime do {} goto P2_1;
	when True sync is_0_prime do {x_prime' = 0} goto P2_2;

loc P2_2: while True wait {}
	when c < x_prime & x_prime < d sync set_2_prime do {x_prime' = 0} goto P2_3;

loc P2_3: while True wait {}
	when a < x_prime & x_prime < b sync is_2_prime do {} goto P2_1;
	when a < x_prime & x_prime < b sync is_1_prime do {} goto P2_4;

loc P2_4: while True wait {}
	when True do {} goto P2_4;
	when True sync set_0_prime do {} goto P2_0;

end -- P2


--************************************************************--
--************************************************************--
-- ANALYSIS
--************************************************************--
--************************************************************--

var init, bad, postreg : region;

init := True
	----------------------
	-- Initial locations
	----------------------
	& loc[lock] = lock0
	& loc[P1]   = P1_0
	& loc[P2]   = P2_0
	
	----------------------
	-- Clocks
	----------------------

	----------------------
	-- Given constraints
	----------------------

	----------------------
	-- Instantiations
	----------------------

;

postreg := reach forward from init endreach;

print(hide non_parameters in postreg endhide);

bad := loc[P1] = P1_4 & loc[P2] = P2_4
;

prints "INTERSECTION";

print(hide non_parameters in (postreg & bad) endhide);


