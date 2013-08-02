var 
	
	A_Pn,	B_Pn,
	A_no,	B_no,
	A_msg, B_msg,
	A_eof, B_eof,
	A_start, B_start,
	A_stop, B_stop,
	A_T4,	B_T4,
	A_frame, B_frame,
	A_new_Pn, B_new_Pn,
	A_reset, B_reset,
	A_check, B_check,
	A_err, B_err,
	A_res, B_res,
	A_diff, B_diff,
	A_observe, B_observe,
	A_S1, B_S1,
	A_S2, B_S2,
	A_Pf, B_Pf,
	one, zero
	: discrete;

	A_c, B_c
	: clock;

	t781, t40, t50000, t25000
	: parameter;


automaton bus
	synclabs : ;
	
	initially c_analyze & True ;
	
	loc c_analyze : while True wait {}
						 when True do {A_Pn' = 1 , B_Pn' = 1} goto active;

	loc active :	while True wait {}
						when A_Pn = 0 do {zero'=1, one'=0} goto active;
						when B_Pn = 0 do {zero'=1, one'=0} goto active;
						when A_Pn = 1 & B_Pn = 1 do {one'=1, zero'=0} goto active;

end --bus

automaton Frame_Generator_A 
	synclabs : ;

	initially start ;

	loc start : 		while True wait {}
							when A_frame=1 do {A_no' = 1 , A_msg' = 10 , A_eof' = 0 , A_stop' = 0 , A_T4' = 0} goto first;


	loc first :			while True wait {}
							when A_new_Pn=1 do {A_Pn' = 0} goto msg;


	loc msg :		 	while True wait {}
							when A_T4 =0 & A_msg = 0 & A_new_Pn=1 & A_reset = 0 do {A_no' = A_no +1, A_Pn' = 0, A_start' = 1, A_Pn'= 0} goto c_set_msg;
							when A_msg > 0 & A_new_Pn = 1 & A_reset = 0 do {A_Pn' = 1, A_msg' = A_msg - 1} goto c_set_stop;
							when A_reset = 1 do { A_reset' = 0 } goto start;
							when A_T4 = 1 & A_msg = 0 & A_new_Pn = 0 & A_reset = 0 do {A_Pn'=0} goto last;


	loc last : 			while True wait {}
							when A_new_Pn = 1 do { A_Pn' = 1 , A_eof' = 1 , A_start' = 0 } goto start;
	

	loc c_set_stop : 	while True wait {}
							when A_T4 = 0 goto c_continue;
							when A_T4=1 & A_msg >=7 goto c_continue;
							when A_T4=1 & A_msg <7 do {A_stop' = 4} goto c_continue;
	

	loc c_continue : 	while True wait {}
							when True goto msg;
 

	loc c_set_msg :	while True wait {}
							when A_no < 2 do {A_msg' = 2} goto msg;
							when A_no < 2 do {A_msg' = 4} goto msg;
							when A_no < 2 do {A_msg' = 6} goto msg;
							when A_no <= 2 & A_no > 1 do {A_msg' = 8, A_T4'=1} goto msg;
end

automaton Detector_A
	synclabs : ;

	initially wait_call;

	loc wait_call : 	while True wait {}
							when A_check = 1 do {A_check'=0 , A_res'=0} goto c_ex_Pf;

	loc c_ex_Pf :		while True wait {}
							when A_Pf=1 goto c_ex_S1;
							when A_Pf=0 goto c_ex1_S1;

	loc c_ex_S1 :		while True wait {}
							when A_S1=1 goto c_ex_Pn;
							when A_S1=0 & A_err > 3 goto c_ex_Pn;
							when A_S1=0 & A_err <= 3 do {A_err' = A_err + 1} goto c_ex_Pn;

	loc c_ex_Pn :		while True wait {}
							when A_Pn=1 goto c_ex_S2;
							when A_Pn=0 goto c_ex1_S1;

	loc c_ex_S2 : 		while True wait {}
							when A_S2=0 & A_err>3 goto wait_call;
							when A_S2=0 & A_err<=3 do {A_err'=A_err +1} goto wait_call;
							when A_S2=1 goto c_ex_S1;

	loc c_ex1_S1 : 	while True wait {}
							when A_S1=0 goto c_ex1_S2;
							when A_S1=1 goto c_calc_res;

	loc c_ex1_S2 :		while True wait {}
							when A_S2=0 goto wait_call;
							when A_S2=1 goto c_calc_res;

	loc c_calc_res :	while True wait {}
							when A_err=0 & A_res=0 goto wait_call;
							when A_err > 0 & A_err<=3 do {A_res'=1} goto wait_call;
							when A_err > 3 do {A_res'=2} goto wait_call;

end

automaton Sender_A
	synclabs : ;

	initially start;
	
	loc start : 			while True wait {}
								when True do {A_c'=0} goto idle;
	
	loc idle :				while A_c<=t781 wait {}
								when A_c=t781 goto c_ex_start;
	
	loc c_ex_start : 		while True wait {}
								when B_start=1 do {A_c'=0} goto other_started;
								when B_start=0 do {A_c'=0} goto ex_silence1;

	loc other_started :	while A_c<=4*t781 wait {}
								when A_c=4*t781 & B_start=1 do {A_c'=0} goto other_started;
								when A_c=4*t781 & B_start=0 do {A_c'=0} goto ex_silence1;

	loc ex_silence1 :		while A_c <= 3*t781 wait {}
								when A_c=3*t781 & one=1 do {A_c'=0} goto ex_silence2;
								when A_c=3*t781 & zero=1 goto c_goto_idle;

	loc c_goto_idle :		while True wait {}
								when True do {A_c'=0} goto idle;

	loc ex_silence2 :		while A_c<=t781 wait {}
								when A_c=t781 & zero=1 goto c_goto_idle;
								when A_c=t781 & one=1 do {A_c'=0} goto transmit;

	loc transmit :			while A_c<=t781 wait {}
								when A_c=t781 do {A_err'=0, A_diff'=0, A_Pf'=1, A_frame'=1} goto c_check_eof;

	loc c_check_eof :		while True wait {}
								when A_eof=1 do {A_c'=0} goto stop;
								when A_eof=0 & one=1	do {A_S1'=1, A_c'=0} goto newPn;
								when A_eof=0 & zero=1 do {A_S1'=0, A_c'=0} goto newPn;

	loc stop :				while A_c<=t50000 wait {}
								when A_c=t50000 do {A_c'=0}  goto idle;

	loc newPn :				while A_c<=t40 wait {}
								when A_c=t40 & A_err=0 do {A_new_Pn'=1} goto sample;
								when A_c=t40 & A_err>0 do {A_Pn'=1} goto sample;

	loc sample :			while A_c<=t781 wait {}
								when A_c=t781 & zero=1 do {A_S2'=0} goto c_call_observe;
								when A_c=t781 & one=1 do {A_S2'=1} goto c_call_observe;

	loc c_call_observe :	while True wait {}
								when True do {A_observe'=1} goto c_call_check;

	loc c_call_check :	while True wait {}
								when A_stop=0 & A_check=1 do {A_c'=0} goto ex_jam;
								when A_stop=1 & A_res=1 do {A_c'=0} goto ex_jam;

	loc ex_jam : 			while A_c<=t781 wait {}
								when A_c=t781 & A_res=0 goto c_nPf;
								when A_c=t781 & A_res=1 do {A_Pn'=0, A_reset'=1, A_c'=0} goto jam;
								when A_c=t781 & A_res=2 do {A_Pn'=1, A_reset'=1, A_start'=0, A_c'=0} goto until_silence;

	loc c_nPf : 			while True wait {}
								when A_Pn = 0 do {A_Pf'=0} goto c_check_eof;
								when A_Pn = 1 do {A_Pf'=1} goto c_check_eof;

	loc jam :				while A_c<=t25000 wait {}
								when A_c=t25000 do {A_Pn'=1, A_start'=0, A_res'=0, A_c'=0} goto transmit;

	loc until_silence : 	while A_c<=t781 wait {}
								when A_c=t781 & zero=1 do {A_c'=0} goto until_silence;
								when A_c=t781 & one=1 do {A_c'=0} goto hold;

	loc hold :				while A_c<=36*t781 wait {}
								when A_c=36*t781 do {A_res'=0} goto c_goto_idle;

end -- Sender_A

automaton Frame_Generator_B 
	synclabs : ;

	initially start ;

	loc start : 		while True wait {}
							when B_frame=1 do {B_no' = 1 , B_msg' = 10 , B_eof' = 0 , B_stop' = 0 , B_T4' = 0} goto first;


	loc first :			while True wait {}
							when B_new_Pn=1 do {B_Pn' = 0} goto msg;


	loc msg :		 	while True wait {}
							when B_T4 =0 & B_msg = 0 & B_new_Pn=1 & B_reset = 0 do {B_no' = B_no +1, B_Pn' = 0, B_start' = 1, B_Pn'= 0} goto c_set_msg;
							when B_msg > 0 & B_new_Pn = 1 & B_reset = 0 do {B_Pn' = 1, B_msg' = B_msg - 1} goto c_set_stop;
							when B_reset = 1 do { B_reset' = 0 } goto start;
							when B_T4 = 1 & B_msg = 0 & B_new_Pn = 0 & B_reset = 0 do {B_Pn'=0} goto last;


	loc last : 			while True wait {}
							when B_new_Pn = 1 do { B_Pn' = 1 , B_eof' = 1 , B_start' = 0 } goto start;
	

	loc c_set_stop : 	while True wait {}
							when B_T4 = 0 goto c_continue;
							when B_T4=1 & B_msg >=7 goto c_continue;
							when B_T4=1 & B_msg <7 do {B_stop' = 4} goto c_continue;
	

	loc c_continue : 	while True wait {}
							when True goto msg;
 

	loc c_set_msg :	while True wait {}
							when B_no < 2 do {B_msg' = 2} goto msg;
							when B_no < 2 do {B_msg' = 4} goto msg;
							when B_no < 2 do {B_msg' = 6} goto msg;
							when B_no <= 2 & B_no > 1 do {B_msg' = 8, B_T4'=1} goto msg;
end

automaton Detector_B
	synclabs : ;

	initially wait_call;

	loc wait_call : 	while True wait {}
							when B_check = 1 do {B_check'=0 , B_res'=0} goto c_ex_Pf;

	loc c_ex_Pf :		while True wait {}
							when B_Pf=1 goto c_ex_S1;
							when B_Pf=0 goto c_ex1_S1;

	loc c_ex_S1 :		while True wait {}
							when B_S1=1 goto c_ex_Pn;
							when B_S1=0 & B_err > 3 goto c_ex_Pn;
							when B_S1=0 & B_err <= 3 do {B_err' = B_err + 1} goto c_ex_Pn;

	loc c_ex_Pn :		while True wait {}
							when B_Pn=1 goto c_ex_S2;
							when B_Pn=0 goto c_ex1_S1;

	loc c_ex_S2 : 		while True wait {}
							when B_S2=0 & B_err>3 goto wait_call;
							when B_S2=0 & B_err<=3 do {B_err'=B_err +1} goto wait_call;
							when B_S2=1 goto c_ex_S1;

	loc c_ex1_S1 : 	while True wait {}
							when B_S1=0 goto c_ex1_S2;
							when B_S1=1 goto c_calc_res;

	loc c_ex1_S2 :		while True wait {}
							when B_S2=0 goto wait_call;
							when B_S2=1 goto c_calc_res;

	loc c_calc_res :	while True wait {}
							when B_err=0 & B_res=0 goto wait_call;
							when B_err > 0 & B_err<=3 do {B_res'=1} goto wait_call;
							when B_err > 3 do {B_res'=2} goto wait_call;

end

automaton Sender_B
	synclabs : ;

	initially start;
	
	loc start : 			while True wait {}
								when True do {B_c'=0} goto idle;
	
	loc idle :				while B_c<=t781 wait {}
								when B_c=t781 goto c_ex_start;
	
	loc c_ex_start : 		while True wait {}
								when A_start=1 do {B_c'=0} goto other_started;
								when A_start=0 do {B_c'=0} goto ex_silence1;

	loc other_started :	while B_c<=4*t781 wait {}
								when B_c=4*t781 & A_start=1 do {B_c'=0} goto other_started;
								when B_c=4*t781 & A_start=0 do {B_c'=0} goto ex_silence1;

	loc ex_silence1 :		while B_c <= 3*t781 wait {}
								when B_c=3*t781 & one=1 do {B_c'=0} goto ex_silence2;
								when B_c=3*t781 & zero=1 goto c_goto_idle;

	loc c_goto_idle :		while True wait {}
								when True do {B_c'=0} goto idle;

	loc ex_silence2 :		while B_c<=t781 wait {}
								when B_c=t781 & zero=1 goto c_goto_idle;
								when B_c=t781 & one=1 do {B_c'=0} goto transmit;

	loc transmit :			while B_c<=t781 wait {}
								when B_c=t781 do {B_err'=0, B_diff'=0, B_Pf'=1, B_frame'=1} goto c_check_eof;

	loc c_check_eof :		while True wait {}
								when B_eof=1 do {B_c'=0} goto stop;
								when B_eof=0 & one=1	do {B_S1'=1, B_c'=0} goto newPn;
								when B_eof=0 & zero=1 do {B_S1'=0, B_c'=0} goto newPn;

	loc stop :				while B_c<=t50000 wait {}
								when B_c=t50000 do {B_c'=0}  goto idle;

	loc newPn :				while B_c<=t40 wait {}
								when B_c=t40 & B_err=0 do {B_new_Pn'=1} goto sample;
								when B_c=t40 & B_err>0 do {B_Pn'=1} goto sample;

	loc sample :			while B_c<=t781 wait {}
								when B_c=t781 & zero=1 do {B_S2'=0} goto c_call_observe;
								when B_c=t781 & one=1 do {B_S2'=1} goto c_call_observe;

	loc c_call_observe :	while True wait {}
								when True do {B_observe'=1} goto c_call_check;

	loc c_call_check :	while True wait {}
								when B_stop=0 & B_check=1 do {B_c'=0} goto ex_jam;
								when B_stop=1 & B_res=1 do {B_c'=0} goto ex_jam;

	loc ex_jam : 			while B_c<=t781 wait {}
								when B_c=t781 & B_res=0 goto c_nPf;
								when B_c=t781 & B_res=1 do {B_Pn'=0, B_reset'=1, B_c'=0} goto jam;
								when B_c=t781 & B_res=2 do {B_Pn'=1, B_reset'=1, B_start'=0, B_c'=0} goto until_silence;

	loc c_nPf : 			while True wait {}
								when B_Pn = 0 do {B_Pf'=0} goto c_check_eof;
								when B_Pn = 1 do {B_Pf'=1} goto c_check_eof;

	loc jam :				while B_c<=t25000 wait {}
								when B_c=t25000 do {B_Pn'=1, B_start'=0, B_res'=0, B_c'=0} goto transmit;

	loc until_silence : 	while B_c<=t781 wait {}
								when B_c=t781 & zero=1 do {B_c'=0} goto until_silence;
								when B_c=t781 & one=1 do {B_c'=0} goto hold;

	loc hold :				while B_c<=36*t781 wait {}
								when B_c=36*t781 do {B_res'=0} goto c_goto_idle;

end -- Sender_B

automaton Observer_A

	synclabs : ;

	initially compare;
	
	loc compare : 	while True wait {}
						when A_observe=1 & A_Pf=A_S1 & A_Pn=A_S2 do {A_observe'=0} goto compare;
						when A_observe=1 & A_Pn=1 & A_S2=0 do {A_observe'=0, A_diff'=1} goto compare;
						when A_observe=1 & A_Pf=1 & A_S1=0 do {A_observe'=0, A_diff'=1} goto compare;
end

automaton Observer_B

	synclabs : ;

	initially compare;
	
	loc compare : 	while True wait {}
						when B_observe=1 & B_Pf=B_S1 & B_Pn=B_S2 do {B_observe'=0} goto compare;
						when B_observe=1 & B_Pn=1 & B_S2=0 do {B_observe'=0, B_diff'=1} goto compare;
						when B_observe=1 & B_Pf=1 & B_S1=0 do {B_observe'=0, B_diff'=1} goto compare;
end

automaton SUPER_OBSERVER

	synclabs: ERREUR_DECOUVERTE;

	initially No_Error;

	loc No_Error : while True wait {}
						when  A_eof=1 & A_diff=1  sync ERREUR_DECOUVERTE goto Error;
						when  A_eof=1 & B_res=0   sync ERREUR_DECOUVERTE goto Error;
						when  B_eof=1 & B_diff=1  sync ERREUR_DECOUVERTE goto Error;
						when  B_eof=1 & A_res=0   sync ERREUR_DECOUVERTE goto Error;

	loc Error : 	while True wait {}

end

var
  init_reg, final_reg, reached : region;

init_reg := loc[bus]=c_analyze & loc[Frame_Generator_A]=start & loc[Frame_Generator_B]=start & loc[Detector_A]=wait_call & loc[Detector_B]=wait_call  & loc[Sender_A]=start & loc[Sender_B]=start & loc[Observer_A] = compare & loc[Observer_B]=compare & loc[SUPER_OBSERVER]=No_error;
final_reg := ( A_eof=1 & (A_diff=1 | B_res=0) ) | ( B_eof=1 & (B_diff=1 | A_res=0) )  ;
reached := reach forward from init_reg endreach;
print trace to final_reg using reached;
