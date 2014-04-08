-- START OF TEMPORARY HYTECH FILE

var 
	var1, var0, var2, var3
		: clock;
	var4
		: parameter;
automaton aa
synclabs: ;
initially A1;
loc A1: while True wait {}
  when True do {} goto A1;
end -- aa
var region_toto1, region_toto2 : region;
prints "---INTERESTING ZONE DELIMITER---";
region_toto1 := True
& 
 -- Constraint 1
var1 - var0 -32 = 0
;
region_toto2 := True
& 
 -- Constraint 2
var1 - var0 -25 = 0
;
if region_toto1 = region_toto2 then
prints "---TRUE :-)---";
else prints "---FALSE :-)---"; endif;
prints "---INTERESTING ZONE DELIMITER---";
-- END OF TEMPORARY HYTECH FILE