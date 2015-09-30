-- reader-writer

var
	a ,b ,c ,d ,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
        z0,z1,z2,z3,z4,mu,nu
	: discrete;


 -- -------------------------------------------------------------- *)

automaton reader
synclabs : r1,r2,r3,r21r34r37,r1r14r17r21r34r37,r12131721223137  ;
initially far;

loc far: while e<=1 & f<=1 & g<=1 & i<=1 & j<=1 & l<=1 & n<=1 & p<=1 &
	q<=1 & r<=1 & t<=1 & u<=1 & v<=1 & x<=1 & y<=1 & z<=1 & z1<=1 & 
	z3<=1 & z4<=1 &
	a>=0 & b>=0 & c>=0 & d>=0 & e>=0 & f>=0 & g>=0 & h>=0 & i>=0 & 
j>=0 & k>=0 & l>=0 & m>=0 & n>=0 & o>=0 & p>=0 & q>=0 & r>=0 & s>=0 & 
t>=0 & u>=0 & v>=0 & w>=0 & x>=0 & y>=0 & z>=0 & 
        z0>=0 & z1>=0 & z2>=0 & z3>=0 & z4>=0 	
  wait {} 

--	when i>=1 	 --sync r38 
		-- [t38,[[b, 1], [i, 0]], [[i,0]]]])
--		do {b'=b+1} goto far;
--		do {mu'>=1} goto Upr38;

	when e>=1  sync r1
		--[t1 ,[[e,-1],[h,1],[j,1]],         [[e,0]]],
	do {e'=e-1, h'=h+1, j'=j+1} goto far;
--		do {mu'>=1} goto Upr1;


	when j>=1 & k>=1  sync r2
		--[t2 ,[[j,-1],[k,-1],[m,1],[n,1]],  [[j,0],[k,0]]],
		do {j'=j-1, k'=k-1, m'=m+1, n'=n+1} goto far;
--		do {mu'>=1} goto Upr2;


	when n>=1 & o>=1  sync r3
		--[t3 ,[[n,-1],[o,-1],[p,1],[r,1]],  [[n,0],[o,0]]]
		do {n'=n-1, o'=o-1, p'=p+1, r'=r+1} goto far;
--		do {mu'>=1} goto Upr3;


	when r>=1 & v>=1  --sync r4
		--[t4 ,[[r,-1],[v,-1],[y,1]],        [[r,0],[v,0]]],
		do {r'=r-1, v'=v-1, y'=y+1} goto far;
--		do {mu'>=1} goto Upr4;

	when y>=1  --sync r5 
		--[t5 ,[[y,-1],[z0,1],[z1,1]],       [[y,0]]],
		do {y'=y-1, z0'=z0+1, z1'=z1+1} goto far;
--		do {mu'>=1} goto Upr5;

	when z1>=1 --sync r6 
		--[t6 ,[[t, 1],[z1,-1]],             [[z1,0]]],
		do {z1'=z1-1, t'=t+1} goto far;
--		do {mu'>=1} goto Upr6;

	when b>=1 & t>=1 --sync r7
             	--[t7 , [[b,-1],[c, 1],[e, 1],[t,-1]],[[b,0],[t,0]]],
		do {b'=b-1, t'=t-1, c'=c+1, e'=e+1} goto far;
--		do {mu'>=1} goto Upr7;

	when z1>=1 & z2>=1 --sync r8
		--[t8 ,[[y, 1],[z1,-1],[z2,-1]],     [[z1,0],[z2,0]]]
		do {z1'=z1-1, z2'=z2-1, y'=y+1} goto far;
--		do {mu'>=1} goto Upr8;


	when y>=1 --sync r9
		--[t9 ,[[u, 1],[w, 1],[y,-1]],       [[y,0]]],
		do {y'=y-1, u'=u+1, w'=w+1} goto far;
--		do {mu'>=1} goto Upr9;


	when u>=1 --sync r10
		--[t10,[[t, 1],[u,-1]],              [[u,0]]],
		do {u'=u-1, t'=t+1} goto far;
--		do {mu'>=1} goto Upr10;

	when t>=1 --sync r11
             	--[t11,[[a, 1],[g, 1],[t,-1]],       [[t,0]]],
		do {t'=t-1, a'=a+1, g'=g+1} goto far;
--		do {mu'>=1} goto Upr11;

	when s>=1 & u>=1 --sync r12
		--[t12, [[a, 1], [g, 1],[s,-1],[u,-1]],[[s,0],[u,0]]],
		do {s'=s-1, u'=u-1, a'=a+1, g'=g+1} goto far;
--		do {mu'>=1} goto Upr12;


	when n>=1 	 --sync r13
		--[t13, [[a, 1], [g, 1],[n,-1]],       [[n,0]]],
		do {n'=n-1, a'=a+1, g'=g+1} goto far;
--		do {mu'>=1} goto Upr13;

	when j>=1 	 --sync r14
		--[t14, [[a, 1],[g, 1],[j,-1]],       [[j,0]]
		do {j'=j-1, a'=a+1, g'=g+1} goto far;
--		do {mu'>=1} goto Upr14;

	when d>=1 & g>=1 	 --sync r15
		--[t15,[[d,-1],[e, 1],[g,-1]],       [[d,0],[g,0]]],
		do {d'=d-1, g'=g-1, e'=e+1} goto far;
--		do {mu'>=1} goto Upr15;

	when b>=1 & g>=1 	 --sync r16
		--[t16, [[b,-1], [e, 1],[g,-1]],       [[b,0],[g,0]]],
		do {b'=b-1, g'=g-1, e'=e+1} goto far;
--		do {mu'>=1} goto Upr16;

	when g>=1 	 --sync r17     
		--[t17,[[e, 1],[g,-1]],              [[g,0]]],
		do {g'=g-1, e'=e+1} goto far;
--		do {mu'>=1} goto Upr17;

--	when g>=1 	 --sync r18
		--[t18,[[a, 1], [g, 0]],              [[g,0]]],
--		do {a'=a+1} goto far;
--		do {mu'>=1} goto Upr18;

	when f>=1 & h>=1 	 --sync r21
		--[t21,[[f,-1],[h,-1],[k, 1],[l, 1]],[[f,0],[h,0]]],
		do {f'=f-1, h'=h-1, k'=k+1, l'=l+1} goto far;
--		do {mu'>=1} goto Upr21;

	when l>=1 & m>=1 	 --sync r22
--		--[t22,[[l,-1],[m,-1],[o, 1],[q, 1]],[[l,0],[m,0]]],
		do {l'=l-1, m'=m-1, o'=o+1, q'=q+1} goto far;
--		do {mu'>=1} goto Upr22;

	when p>=1 & q>=1 	 --sync r23
		--[t23,[[p,-1],[q,-1],[v, 1],[z, 1]],[[p,0],[q,0]]],
		do {p'=p-1, q'=q-1, v'=v+1, z'=z+1} goto far;
--		do {mu'>=1} goto Upr23;


	when z>=1 & z0>=1 	 --sync r24
		--[t24,[[z,-1],[z0,-1],[z3, 1]],     [[z,0],[z0,0]]],
		do {z'=z-1, z0'=z0-1, z3'=z3+1} goto far;
--	do {mu'>=1} goto Upr24;



	when z3>=1	 --sync r25
		--[t25,[[z3,-1],[z4, 1]],            [[z3,0]]],
		do {z3'=z3-1, z4'=z4+1} goto far;
--		do {mu'>=1} goto Upr25;


	when z4>=1	 --sync r26
		--[t26,[[z4,-1],[x, 1]],             [[z4,0]]],
		do {z4'=z4-1, x'=x+1} goto far;
--		do {mu'>=1} goto Upr26;

	when a>=1 & x>=1	 --sync r27
		--[t27,[[a,-1], [d, 1],[f, 1],[x,-1]],[[a,0],[x,0]]],
		do {a'=a-1, x'=x-1, d'=d+1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr27;

	when z3>=1 	 --sync r28
		--[t28,[[z, 1],[z2, 1],[z3,-1]],     [[z3,0]]],
		do {z3'=z3-1, z'=z+1, z2'=z2+1} goto far;
--		do {mu'>=1} goto Upr28;

	when z>=1 	 --sync r29     
		--[t29,[[z,-1],[x, 1]],              [[z,0]]],
		do {z'=z-1, x'=x+1} goto far;
--		do {mu'>=1} goto Upr29;

	when w>=1 & z>=1 	 --sync r30  
		--[t30,[[s, 1],[w,-1],[x, 1],[z,-1]],[[w,0],[z,0]]],
		do {w'=w-1, z'=z-1, s'=s+1, x'=x+1} goto far;
--		do {mu'>=1} goto Upr30;

	when q>=1 	 --sync r31
		--[t31,[[b, 1],[i, 1],[q,-1]],[[q,0]]],
		do {q'=q-1, b'=b+1, i'=i+1} goto far;
--		do {mu'>=1} goto Upr31;

--	when q>=1 	 --sync r3138n37
		--[t31,[[b, 1],[i, 1],[q,-1]],[[q,0]]],
--		do {q'=q-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=0} goto Upr3138n37;

--	when x>=1 	 --sync r3238n37
		--[t31,[[b, 1],[i, 1],[q,-1]],[[q,0]]],
--		do {q'=q-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=0} goto Upr3238n37;

--	when z4>=1 	 --sync r3338n37
		--[t31,[[b, 1],[i, 1],[q,-1]],[[q,0]]],
--		do {q'=q-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=0} goto Upr3338n37;

--	when l>=1 	 --sync r3438n37
		--[t31,[[b, 1],[i, 1],[q,-1]],[[q,0]]],
--		do {q'=q-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=0} goto Upr3438n37;

	when x>=1 	 --sync r32
		--[t32,[[b, 1], [i, 1],[x,-1]],       [[x,0]]],
		do {x'=x-1, b'=b+1, i'=i+1} goto far;
--		do {mu'>=1} goto Upr32;

--	when x>=1 	 --sync r32r37
		--[t32,[[b, 1], [i, 1],[x,-1]],       [[x,0]]],
--		do {x'=x-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr32;

	when z4>=1 	 --sync r33
		--[t33,[[b, 1], [i, 1],[z4,-1]], [[z4,0]]],
		do {z4'=z4-1, b'=b+1, i'=i+1} goto far;
--		do {mu'>=1} goto Upr33;

--	when z4>=1 	 --sync r33r37
		--[t33,[[b, 1], [i, 1],[z4,-1]], [[z4,0]]],
--		do {z4'=z4-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr33;

	when l>=1 	 --sync r34
		--[t34,[[b, 1],[i, 1],[l,-1]], [[l,0]]],
		do {l'=l-1, b'=b+1, i'=i+1} goto far;
--		do {mu'>=1} goto Upr34;

--	when l>=1 	 --sync r34r37
		--[t34,[[b, 1],[i, 1],[l,-1]], [[l,0]]],
--		do {l'=l-1, b'=b+1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr34;

	when c>=1 & i>=1 	 --sync r35
		--[t35,[[c,-1],[f, 1],[i,-1]],       [[c,0],[i,0]]],
		do {c'=c-1, i'=i-1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr35;

	when a>=1 & i>=1 	 --sync r36     ABORTING!!!
		--[t36,[[a,-1],[f, 1],[i,-1]], [[a,0],[i,0]]],
		do {a'=a-1, i'=i-1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr36;

	when i>=1 	 --sync r37 ABORTING!!!
		--[t37,[[f, 1],[i,-1]], [[i,0], %[a,0]]],
		do {i'=i-1, f'=f+1} goto far;
--		do {mu'>=1} goto Upr37;

--	when i>=1 	 --sync r38n37 
		--[t37,[[f, 1],[i,-1]], [[i,0], %[a,0]]],
--		do {i'=i-1, f'=f+1, b'=b+mu} goto far;
--		do {mu'>=0} goto Upr38n37;


--	when d>=0 & g>=0 --sync r15r1
		--['t15.t1',[[d,-1],[g,-1],[h,1],[j,1]],[[d,0],[g,0]]],
--		do {mu'>=1} goto Upr15r1;

--	when b>=0 & g>=0 --sync r16r1
		--['t16.t1',[[b,-1],[g,-1],[h,1],[j,1]],[[b,0],[g,0]]],
--		do {mu'>=1} goto Upr16r1;

--	when g>=0 --sync r17r1   -- LOOPS!!!
		--['t17.t1',[[g,-1],[h,1],[j,1]],[[g,0]]],
--		do {mu'>=1} goto Upr17r1;
--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--  	when e>=1 	 --sync r1r14r17   
		--[t1 ,[[e,-1],[h,1],[j,1]],         [[e,0]]],
		--[t14, [[a, 1],[g, 1],[j,-1]],       [[j,0]]
		--[t17,[[e, 1],[g,-1]],              [[g,0]]],
--		do {mu'>=1} goto Upr1r14r17;                    -- a',h'

--	when e>=1 	 --sync r1r14r18n   
		--[t1 ,[[e,-1],[h,1],[j,1]],         [[e,0]]],
		--[t14, [[a, 1],[g, 1],[j,-1]],       [[j,0]]
		--[t18,[[a, 1]],              [[g,0]]],
--		do {mu'>=1} goto Upr1r14r18n;                    -- a',h'

--	when f>=1 & h>=1	--sync r21r34r38n37star   
		--[t21 ,[[f,-1],[h,-],[k,1],[l,1]     [[f,0],[h,0]
		--[t34, [[b, 1],[i, 1],[l,-1]],       [[l,0]]
		--[t38,[[b, 1]],                     [[i,0]]],
--		do {mu'>=0, nu'>=1} goto Upr21r34r38n37star;           -- b',k'

	when f>=1 & h>=1 	 sync r21r34r37  
		--[t21 ,[[f,-1],[h,-1],[l,1],[k,1]],         [[f,0],[h,0]]],
		--[t34, [[b, 1],[i, 1],[l,-1]],       [[l,0]]
		--[t37,[[f, 1],[i,-1]],              [[i,0]]],
		do {mu'>=1} goto Upr21r34r37;   --!!!mu'=1     -- b'+1,k'+1,h'-1

--	when z>=1 & z0>=1 	 --sync r24r28
		--[t24,[[z,-1],[z0,-1],[z3, 1]],     [[z,0],[z0,0]]],
		--[t28,[[z, 1],[z2, 1],[z3,-1]],     [[z3,0]]],
--		do {mu'>=1} goto Upr24r28;


--	when z1>=1 & z2>=1 --sync r8r5
		--[t8 ,[[y, 1],[z1,-1],[z2,-1]],     [[z1,0],[z2,0]]]
		--[t5 ,[[y,-1],[z0,1],[z1,1]],       [[y,0]]],
--		do {mu'>=1} goto Upr8r5;

	when e>=1 & f>=1   sync r1r14r17r21r34r37
		do {mu'>=1} goto Upr1r14r17r21r34r37; --!!!mu'=1    -- a',b',k'


--	when e>=1 & f>=1    -- syncr12345672122232425263237
--		do {mu'>=1, nu'=0} goto Upr12345672122232425263237;    -- c'


--	when e>=1 & f>=1    -- syncr1234912172122232730
--		do {mu'>=1} goto Upr1234912172122232730;        -- d'

--	when e>=1 & f>=1    -- syncr12347910212223293237
--		do {mu'>=1, nu'=0} goto Upr12347910212223293237;      -- c',w'

--	when e>=1 & f>=1    -- syncr12347910212223303237
--		do {mu'>=1, nu'=0} goto Upr12347910212223303237;      -- c',s'

--	when e>=1 & f>=1    -- syncr123456111221222324253337
--		do {mu'>=1} goto Upr123456111221222324253337;   -- a',b'

--	when e>=1 & f>=1 & k>=1    -- syncr121317213437
--		do {mu'>=1} goto Upr121317213437;   -- a',b',m'

	when e>=1 & f>=1 & k>=1    sync r12131721223137
		do {mu'>=1} goto Upr12131721223137;   --!!! mu'=1 -- a',b',o'

--	when e>=1 & f>=1   -- syncr12345672122232924283237
--		do {mu'>=1, nu'=0} goto Upr12345672122232924283237;  -- c',z0'

--	when e>=1 & f>=1   -- syncr12345624282924283237
--		do {mu'>=1,nu'=0} goto Upr12345624282924283237;     -- c',z2'

--	when e>=1 & f>=1   -- syncr12345611172122232729
--		do {mu'>=1} goto Upr12345611172122232729;     -- d',z0'

--	when e>=1 & f>=1   -- syncr123456111721222327292428
--		do {mu'>=1} goto Upr123456111721222327292428;     -- d',z2'


loc Upr123456111721222327292428: while True  wait {} 
	when True 
	do {d'=d+mu,z2'=z2+mu} goto far;

loc Upr12345611172122232729: while True  wait {} 
	when True 
	do {d'=d+mu,z0'=z0+mu} goto far;

loc Upr12345624282924283237: while True  wait {} 
	when True 
	do {c'=c+mu,z2'=z2+mu, b'=b+nu} goto far;

loc Upr12345672122232924283237: while True  wait {} 
	when True 
	do {c'=c+mu,z0'=z0+mu, b'=b+nu} goto far;

loc Upr121317213437: while True  wait {} 
	when True 
	do {a'=a+mu,b'=b+mu,m'=m+mu} goto far;

loc Upr12131721223137: while True  wait {} 
	when True 
	do {a'=a+mu,b'=b+mu,o'=o+mu} goto far;

loc Upr123456111221222324253337: while True  wait {} 
	when True 
	do {a'=a+mu,b'=b+mu} goto far;

loc Upr12347910212223303237: while True  wait {} 
	when True 
		do {c'=c+mu,s'=s+mu,b'=b+nu} goto far;

loc Upr12347910212223293237: while True  wait {} 
	when True 
		do {c'=c+mu,w'=w+mu,b'=b+nu} goto far;

loc Upr1234912172122232730: while True  wait {} 
	when True 
		do {d'=d+mu} goto far;

loc Upr12345672122232425263237: while True  wait {} 
	when True 
		do {c'=c+mu, b'=b+nu} goto far;

loc Upr1r14r17r21r34r37: while True  wait {} 
	when True 
		do {a'=a+mu, b'=b+mu, k'=k+mu} goto far;

loc Upr1r14r17: while True  wait {} 
	when True 
		do {a'=a+mu, h'=h+mu} goto far;


loc Upr38n37: while True  wait {} 
	when True 
		do {i'=i-1, f'=f+1, b'=b+mu} goto far;

loc Upr3138n37: while True  wait {} 
	when True 
		do {q'=q-1, f'=f+1, b'=b+mu+1} goto far;

loc Upr3238n37: while True  wait {} 
	when True 
		do {x'=x-1, f'=f+1, b'=b+mu+1} goto far;

loc Upr3338n37: while True  wait {} 
	when True 
		do {z4'=z4-1, f'=f+1, b'=b+mu+1} goto far;

loc Upr3438n37: while True  wait {} 
	when True 
		do {l'=l-1, f'=f+1, b'=b+mu+1} goto far;

loc Upr1r14r18n: while True  wait {} 
	when True 
		do {e'=e-1, g'=g+1, h'=h+1, a'=a+1+mu} goto far;


loc Upr21r34r38n37star: while True  wait {} 
	when h>=nu
		do {h'=h-nu, k'=k+nu, b'=b+nu+mu} goto far;


loc Upr21r34r37: while True  wait {} 
	when h>=mu 
		do {b'=b+mu, k'=k+mu, h'=h-mu} goto far;

loc Upr8r5: while True  wait {} 
	when z2>=mu 
		do {z2'=z2-mu, z0'=z0+mu} goto far;

loc Upr24r28: while True  wait {} 
	when z0>=mu 
		do {z0'=z0-mu, z2'=z2+mu} goto far;

loc Upr15r1: while True  wait {} 
	when d>=mu & g>=mu
		do {d'=d-mu, g'=g-mu, h'=h+mu, j'=j+mu} goto far;

loc Upr16r1: while True  wait {} 
	when b>=mu & g>=mu
		do {b'=b-mu, g'=g-mu, h'=h+mu, j'=j+mu} goto far;

loc Upr17r1: while True  wait {} 
	when g>=mu
		do {g'=g-mu, h'=h+mu, j'=j+mu} goto far;


loc Upr18: while True  wait {} 
	when True
		do {a'=a+mu} goto far;

loc Upr38: while True  wait {} 
	when True
		do {b'=b+mu} goto far;




	

end -- reader

-- analysis commands

var init_reg, final_reg, reached, inter: region;

 init_reg := loc[reader]=far  
		& a=0 & b=0 & c=0 & d=0 & e=1 & f=1 & g=0 & h=0 & 
		i=0 & j=0 & k=0 & l=0 & m=0 & n=0 & o=0 & p=0 & q=0 & 
		r=0 & s=0 & t=0 & u=0 & v=0 & w=0 & x=0 & y=0 & z=0 & 
		z0=0 & z1=0 & z2=0 & z3=0 & z4=0 & mu=1 & nu=1;

 final_reg := loc[reader]=far & z=0 & q=0 & l=0 & g=0 & i=0 & e=0 
			& j=0 & n=0 & y=0 & z1=0 & t=0 & u=0 & x=0 & z4=0
                    	& z3=0  
			-- & (r=0 | (r=1 & v=0)) 
			& r=1 & v=0
			-- & (r=0 | v=0)
			--& (f=0 | (f=1 & h=0)) 
			& f=1 & h=0
			-- & (f=0 | h=0);
			-- & (p=0 | p=1) & (v=0 | v=1)
			& p=1
			& a>=0 & b>=0 & c>=0 & d>=0 -- & h>=0 
			& k>=0 & m>=0 
			& o>=0 & s>=0 & w>=0 & z0>=0 & z2>=0 & mu>=0 & nu>=0; 


-- final_reg := loc[reader]=far & z=0 & q=0 & l=0 & g=0 & i=0 & e=0 
--			& j=0 & n=0 & y=0 & z1=0 & t=0 & u=0 & x=0 & z4=0
--                    	& z3=0  
--			-- & (r=0 | (r=1 & v=0)) 
--			& r=1 & v=0
--			-- & (r=0 | v=0)
--			--& (f=0 | (f=1 & h=0)) 
--			& f=1 & h=0
--			-- & (f=0 | h=0);
--			-- & (p=0 | p=1) & (v=0 | v=1)
--			& p=1
--			& a=2 & b=3 & c=0 & d=0 -- & h>=0 
--			& k=1 & m=1 
--			& o=0 & s=0 & w=0 & z0=0 & z2=0; 

prints "backward";
reached := reach backward from final_reg endreach & loc[reader]=far;
printsize reached;
inter := reached & init_reg;
if empty(inter) then prints "OK";
		else prints "NOK";
--		print trace to init_reg using reached;
endif;
print(reached);

--prints "forward";
--reached := reach forward from init_reg endreach & loc[reader]=far;
--printsize reached;
--inter := reached & final_reg;
--if empty(inter) then prints "OK";
--		else prints "NOK";
--		print trace to final_reg using reached;
--endif;
--print(reached);













