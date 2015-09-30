-- reader-writer

var
	x1,
        x2,
        x3,x4,
        x5,x6,x7,n,m      -- 
 	: discrete;
	y,z,t: clock;
	q,r: parameter;

 -- -------------------------------------------------------------- *)

automaton reader
synclabs : ;
initially vide;

loc vide: while x1>=0 & x2>=0 
	& x4>=0 & x5>=0 & x6>=0 & x7>=0  
		wait {} 


	when x6>=1  --sync r1 
		do {n'>=1, z'=0
			} goto Upr1vide;

--	when x1>=1 & x7>=1 & z>5  --sync r2r3
--		do {n'=1 , z'=0} goto Upr2r3vide;


	when x6>=1 & x7>=1 & x3<=19 & z>5   --sync r1r2r3
		do {n'=1, z'=0
			} goto Upr1r2r3vide;

	when x6>=1 & x7>=1 & x3=20 & z>5  --sync r1r2r3
		do {n'=1,z'=0, y'=0
			} goto Upr1r2r3videfull;

	when x3>=1 & x6>=1 --sync r4r5r6
		do {n'>=1 --, y'=0
			} goto Upr4r5r6vide;


loc full: while x1>=0 & x2>=0 
	& x4>=0 & x5>=0 & x6>=0 & x7>=0 & y<=5
		wait {} 


	when x6>=1  --sync r1 
		do {n'>=1, z'=0
		} goto Upr1full;


--	when x1>=1 & x7>=1 & z>5 --sync r2r3
--		do {n'=1,z'=0} goto Upr2r3full;


	when x6>=1 & x7>=1 & x3>=21 & z>5   --sync r1r2r3
		do {n'>=1 ,z'=0, y'=0
			} goto Upr1r2r3full;


	when x3>=22 & x6>=1 --sync r4r5r6
		do {n'=1, y'=0
			} goto Upr4r5r6full;

	when x3=21 & x6>=1 --sync r4r5r6
		do {n'=1
			} goto Upr4r5r6fullvide;


loc Upr1full: while True  wait {} 
	when x6>=n & asap
		do {x6'=x6-n, x1'=x1+n} goto full;

loc Upr2r3full: while True  wait {} 
	when x1>=n & x7>=n & asap
		do {x3'=x3+n, x6'=x6+n, x1'=x1-n, x7'=x7-n} goto full;

loc Upr1r2r3full: while True  wait {} 
	when x7>=n & z>5n & asap
		do {x3'=x3+n, x7'=x7-n} goto full;

loc Upr4r5r6full: while True  wait {} 
	when x3>=n & asap
		do {x7'=x7+n, x3'=x3-n} goto full;

loc Upr4r5r6vide: while True  wait {} 
	when x3>=n & asap
		do {x7'=x7+n, x3'=x3-n} goto vide;


loc Upr1vide: while True  wait {} 
	when x6>=n & asap
		do {x6'=x6-n, x1'=x1+n} goto vide;

loc Upr2r3vide: while True  wait {} 
	when x1>=n & x7>=n & asap
		do {x3'=x3+n, x6'=x6+n, x1'=x1-n, x7'=x7-n} goto full;

loc Upr1r2r3vide: while True  wait {} 
	when x7>=n & asap
		do {x3'=x3+n, x7'=x7-n} goto vide;

loc Upr1r2r3videfull: while True  wait {} 
	when x7>=n & asap
		do {x3'=x3+n, x7'=x7-n} goto full;

loc Upr4r5r6fullvide: while True  wait {} 
	when x3>=n & asap
		do {x7'=x7+n, x3'=x3-n} goto vide;

-------------------------------------------------------------------



	

end -- reader

-- analysis commands

var init_reg, final_reg, reached1, reached2, reached3, reached4, reached5, reached6, reached7, reached,old,
inter: region;

 init_reg := loc[reader]=vide & x1=0
                        & x2=0 
			& x3=0 & x4=0 
			& x5=0 & x6=q & x7=r & y=z  & z=0 & q>=1 & r>=1;
			-- & x6=15 & x7=15 ;

 final_reg := (loc[reader]=vide  | loc[reader]=full )
                 --& (x1=0 | x7=0)  
		-- loc[reader]=full
		 & x7=0
		 & x2=0 
		 & x4=0 & x5=0 & x6=0 & x1>=0 & x3>=0
		 & q>=1 & r>=1 ; 



prints "forward";
reached := reach forward from init_reg endreach & 
		(loc[reader]=vide | loc[reader]=full);
printsize reached;
inter := reached & final_reg;
if empty(inter) then prints "OK";
		else prints "NOK";
--		print trace to init_reg using reached;
		print inter;
endif;
print(reached);

--prints "backward";
--reached := reach backward from final_reg endreach & 
--		(loc[reader]=vide | loc[reader]=full);
--printsize reached;
--inter := reached & init_reg;
--if empty(inter) then prints "OK";
--		else prints "NOK";
--		print trace to init_reg using reached;
--		print inter;
--endif;
--print(reached);














