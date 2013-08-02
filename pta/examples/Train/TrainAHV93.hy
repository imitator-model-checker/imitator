--************************************************************--
--************************************************************--
--
--   Railroad Gate Controller
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
 	x, y, z
		: clock;

	a, b, c, d, e, f
			: parameter;


--************************************************************--
  automaton train
--************************************************************--
synclabs: approach, inn, out, exit;
initially train0;

loc train0: while True wait {}
	when True sync approach do {x' = 0} goto train1;

loc train1: while True wait {}
	when x > a sync inn do {} goto train2;

loc train2: while True wait {}
	when True sync out do {} goto train3;

loc train3: while True wait {}
	when x < b sync exit do {} goto train0;

end -- train


--************************************************************--
  automaton gate
--************************************************************--
synclabs: lower, up, down, raise;
initially gate0;

loc gate0: while True wait {}
	when True sync lower do {y' = 0} goto gate1;

loc gate1: while True wait {}
	when c < y & y < d sync down do {} goto gate2;

loc gate2: while True wait {}
	when True sync raise do {y' = 0} goto gate3;

loc gate3: while True wait {}
	when c < y & y < d sync up do {} goto gate0;

end -- gate



--************************************************************--
  automaton controller
--************************************************************--
synclabs: approach, lower, exit, raise;
initially controller0;

loc controller0: while True wait {}
	when True sync approach do {z' = 0} goto controller1;

loc controller1: while True wait {}
	when e < z & z < f sync lower do {} goto controller2;

loc controller2: while True wait {}
	when True sync exit do {z' = 0} goto controller3;

loc controller3: while True wait {}
	when e < z & z < f sync raise do {} goto controller0;

end -- controller


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
	& loc[train]      = train0
	& loc[gate]       = gate0
	& loc[controller] = controller0

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

bad := loc[train] = train2
	& (loc[gate] = gate0 | loc[gate] = gate1)
;

prints "INTERSECTION";

print(hide non_parameters in (postreg & bad) endhide);


