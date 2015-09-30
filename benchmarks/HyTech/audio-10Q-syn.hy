-- Audio control protocol
--
-- synthesizing error tolerance for modified protocol

define(lowrate,1)
define(hirate,1)


var
	x,      -- (* sender skewed clock *)
	y       -- (* receiver skewed clock *)
	: analog;

	c, 	-- (* use for input sequence, represented in binary *)
		--	(* stores value of bits yet to be acknowledged    *)
	leng,	--
	k, 	-- (* for parity of input string                     *)
	m       -- (* for parity in receiver                         *)
	: discrete;
	eps:
	parameter;

 -- -------------------------------------------------------------- *)

automaton sender
synclabs : list_in, head0, head1, heade, in0, up, in1;
initially  idle & True;

loc idle: while True wait {dx in [lowrate,hirate]}
	when True sync list_in do {x' = 0} goto s_start;

loc s_start: while x=0 wait {dx in [lowrate,hirate]}
	when True sync in1 goto rise_1;
	

loc rise_1: while x=0 wait {dx in[lowrate,hirate]}
	when True sync up goto transhigh;
	
loc transhigh: while x=0 wait {dx in [lowrate,hirate]}
	when True sync heade goto transhigh1;
	when True sync head1 goto transhigh1;
	when True sync head0 goto transhigh0;
	
loc transhigh1: while x<=2+2eps wait {dx in [lowrate,hirate]}
	when 2-2eps<=x & x <=2+2eps sync head1 do {x' = 0} goto translow1_2Q;
	when 2-2eps<=x & x<=2+2eps sync heade goto idle;
	
loc translow1: while x<=4+4eps wait {dx in [lowrate,hirate]}
	when 4-4eps<=x & x<=4+4eps do {x' = 0} sync in1 goto rise_1;


loc translow1_2Q: while x<=2+2eps wait {dx in [lowrate,hirate]}
	when 2-2eps<=x & x<=2+2eps do {x' = 0} sync in1 goto rise_1;

loc transhigh0: while x <= 4+4eps wait {dx in [lowrate,hirate]}
	when 4-4eps<=x & x<=4+4eps do {x' = 0} sync in0 goto translow;
	
loc transhigh0_2Q: while x <=2+2eps wait {dx in [lowrate,hirate]}
	when 2-2eps<=x & x<=2+2eps do {x' = 0} sync in0 goto translow;

loc translow: while x=0 wait {dx in [lowrate,hirate]}
	when True sync heade goto idle;
	when True sync head0 goto translow0;
	when True sync head1 goto translow1;

loc translow0: while x <=2+2eps wait {dx in [lowrate,hirate]}
	when 2-2eps<=x & x<=2+2eps sync head0 do {x' = 0} goto rise_0;

loc rise_0: while x=0 wait {dx in [lowrate,hirate]}
	when True sync up goto transhigh0_2Q;


end

-- receiver can "sync" on list_in, and merely ignore anythign that happens
-- if the list comes in before it is finished receiving.
-- can model this by blocking the list_in signal whenever we ddont care about
-- its effect, i.e. if all non-r_idle locations.
automaton receiver
synclabs: up, out0, out1,outneq0, outneq1, list_in, outoverflow;
				-- "watch" list_in, if premature
				-- i.e. not (r_idle v error)
				-- then go to safe state

initially r_idle & m=0;

loc r_idle: while True wait {dy in [lowrate,hirate]}
	when True sync up do {y' = 0} goto up_1;
	when True sync list_in goto r_idle;


loc up_1: while y=0 wait {dy in [lowrate,hirate]}
	when True do { m' = 1-m} sync out1 goto last_is_1;
	when True sync outneq1 goto error;
	when True sync outoverflow goto overflowerror;

--	when True sync list_in goto safe;

loc last_is_1: while y<=10+10eps wait {dy in [lowrate,hirate]}
	-- need to add a trailing 0 if even and last is 1.
	when 10-10eps<=y & y<=10+10eps & m=0 do {y' = 0,m' = 0} sync out0 goto r_final;
	when 10-10eps<=y & y<=10+10eps & m=1 do {y' = 0,m' = 0} goto r_final;

	when        y<=3+3eps sync up goto error;
	when y>=3-3eps & y<=5+5eps do {y' = 0} 
			sync up goto up_1;
	when y>=5-5eps & y<=7+7eps do {y' = 0} 
			sync up goto up_0;
	when y>=7-7eps 	do {y' = 0} 
			sync up goto next_is_01;


--		when True sync list_in goto safe;


loc next_is_01: while y=0  wait {dy in [lowrate,hirate]}
	when True do {m' = 1-m} sync out0 goto up_1;
	when True sync outneq0 goto error;
	when True sync outoverflow goto overflowerror;

--	when True sync list_in goto safe;

loc up_0: while y=0 wait {dy in [lowrate,hirate]}
	when True do {m' = 1-m} sync out0  goto last_is_0;
	when True sync outneq0 goto error;
	when True sync outoverflow goto overflowerror;
--	when True sync list_in goto safe;

loc last_is_0: while y<=7+7eps wait {dy in [lowrate,hirate]}
	when        y<=3+3eps sync up goto error;
	when y>=3-3eps & y<=5+5eps do {y' = 0} sync up goto up_0;
	when y>=5-5eps        do {y' = 0} sync up goto next_is_01;
	when y>=7-7eps & y<=7+7eps do {y' = 0,m' = 0} sync out0 goto r_final;

--	when True sync list_in goto safe;

loc r_final: while y = 0 wait {dy in[lowrate,hirate]}
	when leng >= 1 goto error;
	when True goto r_idle;


loc error: while True wait {dy in [lowrate,hirate]}

-- loc safe: while True wait {dy in  [lowrate,hirate]}


loc overflowerror: while True wait {dy in [lowrate,hirate]}

end



automaton input
synclabs: in1, in0, head0, head1, heade, list_in;
initially i_stop & c=1 & k=0 & leng=1;

loc head_is_1: while True wait {}
	-- acknowledge reads of head1;
	when True sync head1 goto head_is_1;
	-- respond to writes
	when True do {k' = 1-k,c' = 2c+1, 
		leng' = leng+1} sync in1  goto head_is_1;
	when True do {k' = 1-k, c' = 2c, 
			leng' = leng+1} sync in1 goto head_is_0;
	-- can always terminate when even so far
	when k=0 sync in1  goto i_stop;
	

loc head_is_0:  while True wait {}
	-- acknowledge reads of head0
	when True sync head0 goto head_is_0;
	when True do {k' = 1-k, c' = 2c+1, 
			leng' = leng+1} sync in0 goto head_is_1;
	when True do {k' = 1-k, c' = 2c, 
			leng' = leng+1} sync in0 goto head_is_0;
	
	-- can always terminate when even so far
	when k=0 sync in0  goto i_stop;

	-- can always choose to end with trailing 00
	when  True do { c' = 2c, 
			leng' = leng+1} sync in0 goto  endeven00;

loc endeven00: while True wait {}
	-- acknowledge reads of head0
	when True sync head0 goto endeven00;
	when True sync in0 goto i_stop;

loc i_stop:  while True wait {}
	-- ack reads of head empty
	when True sync heade goto i_stop;
	
	-- initialize list
	when True  do {c' = 1,k' = 0,leng' = 1} sync list_in goto head_is_1;
end

automaton output
initially ack & True;  -- no initial region info
synclabs: out1, out0, outneq0, outneq1, outoverflow;

loc ack: while True wait {}
	-- out 1
	when leng=1 & c=1 do {c' = 0,leng' = leng-1}
		sync out1 goto ack;
	when leng=2 & c>=2 do {c' = c-2,leng' = leng-1}
		sync out1 goto ack;
	when leng=3 & c>=4 do {c' = c-4,leng' = leng-1}
		sync out1 goto ack;
	
	
	-- out neq 0
	when leng=0 		sync outneq0 goto ack;
	when leng=1 & c=1 	sync outneq0 goto ack;
	when leng=2 & c>=2 	sync outneq0 goto ack;
	when leng=3 & c>=4 	sync outneq0 goto ack;

	-- out overflow
	when leng>=4	 	sync outoverflow goto ack;

	-- out 0
	when leng=1 & c=0 do {leng' = leng-1}
		sync out0 goto ack;
	when leng=2 & c<=1 do {leng' = leng-1}
		sync out0 goto ack;
	when leng=3 & c<=3 do {leng' = leng-1}
		sync out0 goto ack;
	
	
	-- out neq 1
	when leng=0 		sync outneq1 goto ack;
	when leng=1 & c=0	sync outneq1 goto ack;
	when leng=2 & c<=1 	sync outneq1 goto ack;
	when leng=3 & c<=3 	sync outneq1 goto ack;

	-- out overflow


end


-- analysis commands

var
  init_reg, final_reg, reached : region;

init_reg := loc[sender] = idle & loc[receiver] = r_idle & m=0 &
	loc[input] = i_stop & c=0 & k=0 & leng = 0 & loc[output] = ack;

final_reg := loc[receiver] = error | loc[receiver] = overflowerror;

reached := reach forward from init_reg endreach;

prints "Audio control protocol not correct for the following error tolerance";

print omit sender, input, output  locations 
	hide non_parameters in reached & final_reg endhide;




