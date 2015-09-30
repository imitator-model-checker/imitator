-- spsmall_taaw.hy
-- passage d'un 1 seulement.

-- modelisation de wel, sur deux cycles, avec delai_up et delai_down.

var 	x25, x26,
	x1, 
	z0, s,
	z1,z5, z7,z8, zD
		: clock;
	t, q0, q1, q2, q7, q8, q26, d,e, d1, e1, o5, o15, o16, qDdec, oDdec, o8
		: discrete;

	tHI, tLO, b3up, b3down, b2up, b2down, tDdec,
	tupc1, tdownc1, tupE, tdownc2, tdownE, tupD1,
	tdownwen, toutQZ1, toutQA0, toutQA1, toutDdec, toutQE1,
	tout25, tout26, tout8, tout7, tout5,
	down_d15,
	tsetupwen,
	lbl13, ubl13, lbl14,ubl14, lbl7,ubl7, lbl8,ubl8, lbl5,ubl5,
	lbl16,ubl16,lbl1, ubl1 		
			: parameter;




automaton clock1 
synclabs: upc1, downc1, 
		downc2, upE, downE;
initially Highc1pre;

loc Highc1pre: while x1 <=tHI+b3up & x1>=0 wait {}			-- enable latch WEN
	when x1=tHI+b3up sync upc1 do {tupc1'=s,e'=s} goto LowE; 

loc LowE: while x1 <= tHI+b3up+b2up & x1>=0 wait {}
	when x1=tHI+b3up+b2up sync upE do {tupE'=s,e1'=s} goto Lowc2;  

loc Lowc2: while x1 <= tHI+tLO+b3down & x1>=0 wait {}
	when x1= b3down+tHI+tLO sync downc1
		do {tdownc1'=s} goto Highc1bis;

loc Highc1bis: while x1 <=tHI+tLO+b3down+down_d15 & x1>=0 wait {}
	when x1 = tHI+tLO+b3down+down_d15 sync downc2 do {tdownc2'=s,o15'=s} goto YY;

loc YY: while x1 <= tHI+tLO+b3down+b2down & x1>=0 wait{}
	when x1=tHI+tLO+b3down+b2down sync downE do {tdownE'=s} goto PreEndc1; -- entree CK de gate25

loc PreEndc1: while x1 <= 2tHI+2tLO & x1>=0 wait {}
	when x1=2tHI+2tLO -- sync upc1 
----	& tout7=tHI+tLO+b3down+down_d15+lbl7+lbl8  -- 03/11/06

--	& tupc1 < tdownwen 
--	& tupc1<tupE
--	& tdownwen < toutQZ1
--	& tupE<toutQZ1 
--	& toutQZ1<toutQA1 
--	& toutQA1<toutQE1 
--	& toutQE1<tupD1
--	& tupD1<toutDdec
--	& tupD1<tdownc1
--	& toutDdec<tdownc2
--	& tdownc1<tdownc2
--	& tdownc2<tdownE 
--	& tdownc2<tout5
--	& tdownE<tout8
--	& tout5<tout8 
--	& tout8<tout7 

--	do {e'=s} 
	goto Endc1;  -- 18/10/06


loc Endc1: while x1>=0 wait {}
	when True do {} goto Endc1;

end -- clock1


automaton wen
synclabs: downwen ;--, upwen;
initially Highwen;

loc Highwen: while s <= tHI+tLO-tsetupwen & s>=0 wait {}
	when s=tHI+tLO-tsetupwen sync downwen do {tdownwen'=s,q0'=1} goto End_d1;  

loc End_d1: while s>=0 wait {}
	when True do {} goto End_d1;

end -- wen


automaton D
synclabs: upD1 ;
initially Ddec_0;

loc Ddec_0: while s <= tDdec & s>=0 wait {}
	when s=tDdec sync upD1 do {tupD1'=s,qDdec'=1, d1'=s} goto End_Ddec;  

loc End_Ddec: while s>=0 wait {}
	when True do {} goto End_Ddec;

end -- wen


automaton delay13
synclabs: downwen, -- upwen, 
	outQZ1; --outQZ0;
initially D0Z;

loc D0Z: while z0>=0 wait {} 
	when True sync downwen do {z0'=0} goto D1Z;

loc D1Z: while z0 <= ubl13 & z0>=0 wait {}
--	when True sync upwen do {} goto end0; -- delai inertiel de wire
	when z0 >= lbl13 sync outQZ1 do {toutQZ1'=s,q1'=1,d'=s} goto end0; -- D1bisZ;

loc end0: while z0>=0  wait {}
--	when True sync upwen do {} goto end0;
	when True do {} goto end0;

end -- delay13

automaton latchW
synclabs: upc1, downc1, --outQZ0, 
	outQZ1, outQA0, outQA1;
initially D2c0A;

loc D2c1A:while z1>=0 wait {}
	when True sync downc1 do {} goto D2c0A;
	when True sync outQZ1 do {z1'=0} goto D1c1A;

loc D2c0A: while z1>=0 wait{}
	when True sync upc1 do{} goto D2c1A;
	when True sync outQZ1 do {} goto D1c0A;

loc D0c0A: while z1>=0 wait {}
 	when True sync upc1 do {z1'=0} goto D0c1A;
 	when True sync outQZ1 do {} goto D1c0A;
--	when True sync outQZ0 do {} goto D0c0A; -- 30/01/06 

loc D1c0A: while z1>=0 wait {}
 	when True sync upc1  do {z1'=0} goto D1c1A;
--	when True sync outQZ0 do {} goto D0c0A;
 	when True sync outQZ1 do {} goto D1c0A;

loc D0c1A: while z1 <= ubl14 & z1>=0 wait {}
 	when True sync downc1 do {} goto D0c0A;
 	when True sync outQZ1 do {z1'=0} goto D1c1A;
--	when True sync outQZ0 do {} goto D0c1A;  
 	when z1 >= lbl14 sync outQA0 do {toutQA0'=s,q1'=0} 
         goto D0c1bisA; -- D2c1A;


loc D1c1A: while z1 <= ubl14  & z1>=0 wait {}
 	when True sync downc1 do {} goto D1c0A;
--	when True sync outQZ0 do {z1'=0} goto D0c1A; -- 30/01/06
 	when True sync outQZ1 do {} goto D1c1A;  
 	when z1 >= lbl14 &  d>e 
	sync outQA1 do {toutQA1'=s,q1'=1}  -- d+ubl14>=e
         goto D1c1bisA; -- D2c1A; 

 loc D0c1bisA: while z1>=0 wait {}
 	when True sync downc1 do {} goto D0c0A;
    	when True sync outQZ1 do {z1'=0} goto D1c1A;

 loc D1c1bisA: while z1>=0  wait {}	-- simplification (14/03/06)
--	when True sync downc1 do {} goto D1c1bisA;
--	when True sync upc1 do {} goto D1c1bisA;
--	when True do {} goto D1c1bisA;

 	when True sync downc1 do {} goto D1c0A; -- D1c1bisA; 30/01/06
--	when True sync outQZ0 do {z1'=0} goto D0c1A; -- 31/01/06
	when True sync outQZ1 do {} goto D1c1bisA; -- 31/01/06

end -- latchW

automaton latchD
synclabs: upD1, upE, downE, outDdec;

initially D2E0;

loc D2E1: while zD>=0 wait {}
	when True sync downE do {} goto D2E0;
	when True sync upD1 do {zD'=0} goto D1E1;

loc D2E0: while zD>=0 wait{}
	when True sync upD1 do{} goto D1E0;
	when True sync upE do {} goto D2E1;

loc D1E0: while zD>=0 wait {}
 	when True sync upE do {zD'=0} goto D1E1;


loc D1E1: while zD <= ubl1  wait {}
 	when True sync downE do {} goto D1E0;
-- 	when True sync outQZ1 do {} goto D1c1A;  
 	when zD >= lbl1 &  d1>e1  -- 20/10/06
----	do {zD'=0}  -- d+ubl14>=e  -- supprimé le 02/11/06
----         goto Wire5; -- D2c1A; -- supprimé le 02/11/06
	sync outDdec do {toutDdec'=s,oDdec'=s} goto EndlatchD;  --02//11/06

---- supprimé le 02/11/06
---- loc Wire5: while zD <= ubl5 & zD>=0 wait {}
----	when zD >=lbl5 sync outDdec do {toutDdec'=s,oDdec'=s} goto EndlatchD;
---- 	when True sync downE do {} goto Wire5;

 loc EndlatchD: while zD>=0  wait {}
 	when True do {} goto EndlatchD;
 	when True sync downE do {} goto EndlatchD;

end -- latchD


-- ajout du 02/11/06 
automaton delay5
 synclabs: outDdec, out5 ;
 initially E5_0;

 loc E5_0: while z5>=0 wait {} 
 	when True sync outDdec do {z5'=0} goto E5_1;

 loc E5_1: while z5 <= ubl5 & z5>=0 wait {}
 	when z5 >= lbl5 sync out5 do {tout5'=s,o5'=s} goto end5; --E1bisZ;

 loc end5: while z5>=0  wait {}
 	when True do {} goto end5;

 end -- delay5

 
 automaton delay16
 synclabs: outQA1, outQE1 ;
 initially E0Z;

 loc E0Z: while z5>=0 wait {} 
 	when True sync outQA1 do {z5'=0} goto E1Z;

 loc E1Z: while z5 <= ubl16 & z5>=0 wait {}
 	when z5 >= lbl16 sync outQE1 do {toutQE1'=s,q2'=1, o16'=s} goto endE; --E1bisZ;

 loc endE: while z5>=0  wait {}
 	when True do {} goto endE;

 end -- delay16

 automaton delay8
 synclabs: out25, out8 ;
 initially E8_0;

 loc E8_0: while z8>=0 wait {} 
 	when True sync out25 do {z8'=0} goto E8_1;

 loc E8_1: while z8 <= ubl8 & z8>=0 wait {}
 	when z8 >= lbl8 sync out8 do {q8'=1, tout8'=s, o8'=s} goto end8; --E1bisZ;

 loc end8: while z8>=0  wait {}
 	when True do {} goto end8;

 end -- delay8


 automaton delay7
 synclabs: out26, outQ ;
 initially E7_0;

 loc E7_0: while z7>=0 wait {} 
 	when True sync out26 do {z7'=0} goto E7_1;

 loc E7_1: while z7 <= ubl7 & z7>=0 wait {}
 	when z7 >= lbl7 sync outQ do {q7'=1, tout7'=s} goto end7; --E1bisZ;

 loc end7: while z7>=0  wait {}
 	when True do {} goto end7;

 end -- delay7


automaton gate_25
synclabs:outQE1,out25,downc2; 
initially init_or;

loc init_or : while x25>=0 wait {}
	when True sync outQE1 do{} goto A;
	when True sync downc2 do{} goto B;

loc A: while x25>=0 wait {}
	when True sync downc2 do{q2'=1,x25'=0} goto D;

loc B: while x25>=0 wait {}
	when True sync outQE1 do{q2'=1,x25'=0} goto D;


loc D: while x25>=0 wait {}
	when o15>o16 & x25=0 sync out25 do {tout25'=s} goto End_or;

loc End_or: while x25>=0 wait {}
	when True do{} goto End_or;

end -- gate_25


automaton gate_26
synclabs:out8,out5,out26;   -- out5 le 02/11/06
initially init26;

loc init26 : while x26>=0 wait {}
	when True sync out8 do{} goto A26;
	when True sync out5 do{} goto B26;

loc A26: while x26>=0 wait {}
	when True sync out5 do{q26'=1,x26'=0} goto D26;

loc B26: while x26>=0 wait {}
	when True sync out8 do{q26'=1,x26'=0} goto D26;

loc D26: while x26>=0 wait {}
	when o8>oDdec & x26=0 sync out26 do {tout26'=s} goto End26;

loc End26: while x26>=0 wait {}
	when True do {} goto End26;

end -- gate_26

-- analysis commands

var init_reg, reached1,
reached2, reached3,reached4,reached5,reached6,reached7,reached8,reached9,reached10, 
reached11,
reached12, reached13,reached14,reached15,reached16,reached17,reached18,reached19,reached20, 
pre_reg, post_reg, bad_reg1, bad_reg2, nZ, interEnd , approx,
reg1, reg2, loc_final, goal_reg, ult_reg : region;

init_reg :=  
	loc[clock1]=Highc1pre 
	& loc[wen]=Highwen 
	& loc[latchW]=D2c0A  
	& loc[delay5]=E5_0
	& loc[delay7]=E7_0
	& loc[delay8]=E8_0
	& loc[delay13]=D0Z
	& loc[delay16]=E0Z
 	& loc[gate_25]=init_or
	& loc[gate_26]=init26
	& loc[latchD]=D2E0
	& loc[D]=Ddec_0
 
	&  x1=0 & x25=0 & x26=0 
	& z0=0 & z1=0 & z5=0 & z7=0 & z8=0 & zD=0
	& s=0 

	& b3down>0 & b3up>0  
	& b2down>0 & b2up>0

        & 0<lbl13 & lbl13=ubl13
        & 0<lbl14 & lbl14=ubl14  
	& 0<lbl16 & lbl16=ubl16
	& 0<lbl1 & lbl1=ubl1
	& 0<lbl5 & lbl5=ubl5
	& 0<lbl7 & lbl7=ubl7
	& 0<lbl8 & lbl8=ubl8
	& 0<down_d15  

	& t=0 & q0=2 & q1=2 & q2=2  & q7=2 & q8=2 & q26=2 & qDdec=2
	& d=0 & e=0 & d1=0 & e1=0 & o15=0 & o16=0 & o8=0 & oDdec=0
--	& tupc1=0 & tdownc1=0 & tupE=0 & tdownc2=0 & tdownE=0 
--	& tdownwen=0 &  tupD1=0
--	& tdownwen=0 & toutQZ1=0 & toutQA0=0 & toutQA1=0 
--	& toutDdec=0 & tout5=0
--	& toutQE1=0 & tout25=0 & tout26=0 & tout8=0 & tout7=0 & tout5=0


	& 0<tsetupwen

        & tHI>0 & tLO>0 & tDdec>0 & down_d15>0 & tsetupwen>0

-- FORMATS --

----	& tHI + b2down + b3down  < tDdec 
----	& b2down+b3down+lbl1<tLO
----	& b3down+tsetupwen<tLO+lbl13
----	& lbl13+lbl14<tsetupwen+b3down
----	& lbl14<tHI
----	& lbl13+lbl14+lbl16<tsetupwen+b3down+down_d15
----	& tHI+tLO+b3down+down_d15<lbl5+lbl1+tDdec
----	& tDdec+lbl5+lbl1<lbl8+b3down+down_d15+tHI+tLO

-- SP1 obtenu par Pre* avec ordre total --
----  &   tHI + tLO + lbl13 + lbl14 + lbl16 < tDdec + tsetupwen  

--& tDdec + ubl1 < tHI + tLO + b3down   
	-- retrait donne: tsetupD in ]99,107] and tsetupW in ]46,54[
--& tLO < b3up + b2up + tsetupwen  
	-- retrait supplementaire permet de trouver des valeurs tsetupW<46

----& tHI + tLO + b3down + down_d15 < tDdec + ubl5 + ubl1  
--& tDdec + ubl5 + ubl1 < tHI + tLO + b3down + b2down  
----& b3down + down_d15 + lbl7 + lbl8 < tHI + tLO  
----& b3up + b2up + tsetupwen < tLO + lbl13 
----& b3up + tsetupwen < tLO  
----& b2down < down_d15 + lbl8;

-- SPSMALL 1 generique

-- & tHI + tLO + lbl13 + lbl14 + lbl16 < tDdec + tsetupwen   --difficile à retirer
& tHI + tLO + b3down + down_d15 < tDdec + ubl5 + ubl1   
--& tDdec + ubl1 < tHI + tLO + b3down   ---- LIMITATIF
& tDdec + ubl5 + ubl1 < tHI + tLO + b3down + b2down   
& b3down + down_d15 + lbl7 + lbl8 < tHI + tLO   
& b3up + b2up + tsetupwen < tLO + lbl13   -- pourraitt etre retiré en fait !
& tLO < b3up + b2up + tsetupwen   	
& b3up + tsetupwen < tLO   
& b2down < down_d15 + lbl8
;

-- SPSMALL 2 generique

----& tHI + tLO + ubl13 + ubl14 + ubl16 < tDdec + tsetupwen   
----& tHI + tLO + b3down + b2down < tDdec + lbl5 + ubl1   
----& tHI + tLO + b3down < tDdec + ubl1   
----& tDdec < tHI + tLO + b3down   
----& tDdec + ubl1 < tHI + tLO + b3down + down_d15   
----& tDdec + lbl5 + ubl1 < tHI + tLO + b3down + down_d15 + ubl8   
----& b3down + down_d15 + ubl7 + ubl8 < tHI + tLO   
----& b3up + b2up + tsetupwen < tLO 
----& down_d15 < b2down   ;

-- ----------------------------------------------------------------------

-- SPSMALL 1 (rising edge)

--	& tLO = 74
--	& tHI = 36
--	& tsetupwen=54 -- (34,108) -- (46,96) OK --- orglmt:48; 28 est la valeur limite 
			--- (28,109) OK; (28,65) OK aussi !
			--- 76 est la valeur limite maximale
----
--	& tDdec=97 -- orglmt:96; (48,110)(28,110)NOK ; (28,109)(48,109)OK
			--- 65 est la valeur limite minimale (tsetupDmax=140)
--	& lbl1 = 14 
--	& b2up = 23 & b2down=30
--	& b3up=5 & b3down=2
--	& lbl5=22 & lbl7=21 & lbl8=22
--	& lbl13 = 8 & lbl14 = 22 & down_d15=11 
--	& lbl16 = 1;
------------------------------------------------------------------------
------------------------------------------------------------------------
-- SPSMALL 2 (rising edge)

--	& tLO = 170
--	& tHI = 72
--	& tsetupwen = 109 --- 28 est la valeur limite
----
--	& tDdec= 198
--	& lbl1 = 60
--	& b2up = 43 & b2down=66
--	& b3up=8 & b3down=4
--	& lbl5=61 & lbl7=47 & lbl8=42
--	& lbl13 = 23 & lbl14 = 36 & down_d15=43 & lbl16 = 1;
------------------------------------------------------------------------
prints "hi there: initial region";
print init_reg;
prints "";


----prints "iterated succ";
----post_reg := reach forward from init_reg endreach;
----interEnd := post_reg & loc[clock1]=Endc1
----	& loc[wen]=End_d1 
----	& loc[delay13]=end0
----	& loc[latchW]=D1c0A
----	& loc[delay16]=endE
---- 	& loc[gate_25]=End_or 
----	& loc[delay7]=end7
----	& loc[delay8]=end8
----	& loc[delay5]=end5
----	& loc[gate_26]=End26
----	& loc[latchD]=EndlatchD
----	& loc[D]=End_Ddec;

----approx := hull(interEnd);

----prints "approx";
----print(hide d,e,d1,e1,o15,o16, s,t,
----	o8,oDdec,x1,x25,x26,z0,z1,z5,z7,z8,zD,o5
----	,tupc1,tdownc1,tupE,tdownc2,tdownE,tdownwen,tupD1,tdownwen,toutQZ1,toutQA0,toutQA1,toutDdec,toutQE1,tout25,tout26, tout5
----,tout8
----,tout7
----,q2,q26,q0,q1,qDdec,q8,q7 
----in approx -- interEnd --post_reg 
----endhide);

--print(hide d,x1 in post_reg endhide);

---- A REMETTRE POUR POST* ----
prints "iterated succ";
post_reg := reach forward from init_reg endreach;

loc_final := post_reg 
	& loc[clock1]=Endc1
	& loc[wen]=End_d1 
	& loc[delay13]=end0
	& loc[latchW]=D1c0A
	& loc[delay16]=endE
 	& loc[gate_25]=End_or 
	& loc[delay7]=end7
	& loc[delay8]=end8
	& loc[delay5]=end5
	& loc[gate_26]=End26
	& loc[latchD]=EndlatchD
	& loc[D]=End_Ddec
	& q1=1 & q2=1 & q0=1 & q2=1 & q7=1 & q8=1 & q26=1
	&  x1>=0 & x25>=0 & x26>=0 
	& z0>=0 & z1>=0 & z5>=0 & z7>=0 & z8>=0 & zD>=0
	& tHI>0 & tLO>0
	& s=2tHI+2tLO 
        & 0<lbl13 & lbl13=ubl13
        & 0<lbl14 & lbl14=ubl14  
	& 0<lbl16 & lbl16=ubl16
	& 0<lbl1 & lbl1=ubl1
	& 0<lbl5 & lbl5=ubl5
	& 0<lbl7 & lbl7=ubl7
	& 0<lbl8 & lbl8=ubl8
	& 0<down_d15 & b2up>0 & b2down>0 
	& tsetupwen>0 & tDdec>0
	& tupc1>0 & tdownc1>0 & tupE>0 & tdownc2>0 & tdownE>0 
	& tdownwen>0 &  tupD1>0
	& tdownwen>0 & toutQZ1>0 & toutQA0>0 & toutQA1>0 
	& toutDdec>0 
	& toutQE1>0 & tout25>0 & tout26>0 & tout8>0 & tout7>0 & tout5>0

-- ordonnancement total  des evenements sur SP1

--	& tupc1 < tdownwen & tdownwen < tupE & tupE<toutQZ1 & toutQZ1<toutQA1 
--	& toutQA1<toutQE1 & toutQE1<tupD1 & tupD1<toutDdec
--	& toutDdec<tdownc1 & tdownc1<tdownc2
--	& tdownc2<tout5 & tout5<tdownE & tdownE<tout8 & tout8<tout7 
	& tout7>tHI+tLO+b3down+down_d15+lbl7+lbl8 -- <2tHI+2tLO;
	;

goal_reg := hide x25, x26, x1, z0, z1,z5, z7,z8, zD,
	t, q0, q1, q2, q7, q8, q26, d,e, d1, e1, o5, o15, o16, qDdec, oDdec,o8,
	tsetupwen, tDdec,
	tupc1, tdownc1, tupE, tdownc2, tdownE,
	tdownwen,  tupD1,
	tdownwen, toutQZ1, toutQA0, toutQA1,
	toutDdec,
	toutQE1, tout25, tout26, tout8, tout5,

--	tout7,
	s
--,a,b,c,d,e,f,g,h
in 
loc_final
endhide
;

print(goal_reg);

ult_reg := hide tout7 in goal_reg endhide;
prints " le hull";
print(hull (ult_reg));

----& tHI + tLO + lbl13 + lbl14 + lbl16 < tDdec + tsetupwen  -- (a supprimer)
----& tHI + tLO + b3down + down_d15 < tDdec + ubl5 + ubl1  
----& tDdec + ubl1 < tHI + tLO + b3down  
----& tDdec + ubl5 + ubl1 < tHI + tLO + b3down + b2down  
----& b3down + down_d15 + lbl7 + lbl8 < tHI + tLO  
----& b3up + b2up + tsetupwen < tLO + lbl13 
----& tLO < b3up + b2up + tsetupwen  
----& b3up + tsetupwen < tLO  
----& b2down < down_d15 + lbl8;

----& b3down + down_d15 + lbl7 + lbl8 <= tHI + tLO   
----& down_d15 <= b2down   
----& tHI + tLO + b3down + down_d15 <= tDdec + lbl5 + lbl1
----& b2down <= down_d15 + lbl8   
----& b3up + b2up + tsetupwen <= tLO + ubl13 
----& tHI + tLO + ubl13 + ubl14 + lbl16 <= tDdec + tsetupwen 
----& tDdec + lbl1 <= tHI + tLO + b3down + b2down   
----& ubl13 + ubl14 + lbl16 <= b3down + down_d15 + tsetupwen   
----& ubl13 + ubl14 <= b3down + tsetupwen   
----& b3up + tsetupwen <= tLO

----	& tLO = 74
-----	& tHI = 36
--	& tsetupwen = 48
--	& tDdec = 96
----	& lbl1 = 14 & b2up = 23 & b2down= 30 & b3up=5 & b3down=2 
----	& lbl5=22 & lbl7=21 & lbl8=22

----	& lbl13 = 8 & lbl14 = 22 & down_d15=11 & lbl16 = 1;

--prints "hi there again: final region";
--print loc_final;
--prints "";

--prints "iterated pred";
--pre_reg := reach backward from loc_final endreach;
--prints "general";
--print(hide d,e,d1,e1,o15,o16, s,t,
--o8,oDdec,x1,x25,x26,z0,z1,z5,z7,z8,zD,o5
--,tupc1,tdownc1,tupE,tdownc2,tdownE,tdownwen,tupD1,tdownwen,toutQZ1,toutQA0,toutQA1,toutDdec,toutQE1,tout25,tout26, tout5
--,tout8
--,tout7
--,q2,q26,q0,q1,qDdec,q8,q7 
--in pre_reg
--endhide);



--prints " intersection";
--nZ := pre_reg & init_reg &s=0;
--if empty(nZ)
--then prints "NOK";
--else print(nZ);
--endif;






