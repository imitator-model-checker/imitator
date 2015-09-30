------------------------------ Description du systeme -------------------------------
----------                                                                 ----------
----------                                                                 ----------
-------------------------------------------------------------------------------------

--      Definition des variables du modele: horloges, discretes, parametres        --
--                                                                                 --
-------------------------------------------------------------------------------------

var
	t,
	x_q_2,
	x_q_1	: clock;

	s,
	r,
	q_2,
	q_1
	: discrete;

	delta0_l_q_2, delta0_u_q_2, delta1_l_q_2, delta1_u_q_2,
	delta0_l_q_1, delta0_u_q_1, delta1_l_q_1, delta1_u_q_1	: parameter;

-- 		les automates de l'environnement qui definent les signaux d'entree		  --
--                                                                                 --
-------------------------------------------------------------------------------------

automaton env_nd_signals

synclabs: r_up, r_down,
          s_up, s_down ;
initially env_nd_signals_0 ;

loc env_nd_signals_0 : while True wait{}
	when True sync r_down do {r' = 0} goto env_nd_signals_1;
	when True sync s_down do {s' = 0} goto env_nd_signals_2;

loc env_nd_signals_1 : while True wait{}
	when True sync s_down do {s' = 0} goto env_nd_signals_end;

loc env_nd_signals_2 : while True wait{}
	when True sync r_down do {r' = 0} goto env_nd_signals_end;

-- loc env_nd_signals_0 : while t <= 10 wait{}
-- 	when t = 10 sync r_down do {r'=0} goto env_nd_signals_1;
-- 
-- loc env_nd_signals_1 : while t <= 10 wait{}
-- -- 	when t = 10 sync s_down do {s'=0} goto env_nd_signals_2;
-- 	when t = 10 sync s_down do {s'=0} goto env_nd_signals_end;
-- 
-- loc env_nd_signals_2 : while t <= 30 wait{}
-- 	when t = 30 sync r_up do {r'=1} goto env_nd_signals_3;
-- 
-- loc env_nd_signals_3 : while t <= 40 wait{}
-- 	when t = 40 sync s_up do {s'=1} goto env_nd_signals_4;
-- 
-- loc env_nd_signals_4 : while t <= 50 wait{}
-- 	when t = 50 sync s_down do {s'=0} goto env_nd_signals_5;
-- 
-- loc env_nd_signals_5 : while t <= 60 wait{}
-- 	when t = 60 sync r_down do {r'=0} goto env_nd_signals_6;
-- 
-- loc env_nd_signals_6 : while t <= 70 wait{}
-- 	when t = 70 sync s_up do {s'=1} goto env_nd_signals_7;
-- 
-- loc env_nd_signals_7 : while t <= 80 wait{}
-- 	when t = 80 sync r_up do {r'=1} goto env_nd_signals_end;

loc env_nd_signals_end : while True wait{}

end

-- les automates qui definent tous les signaux auxilieres et les signaux de sortie --
--                                                                                 --
-------------------------------------------------------------------------------------

-- les automates qui definent le signal q_2:
--------------------------------------------

automaton Ass_q_2

synclabs: s_up, s_down, q_1_up, q_1_down, q_2_up, q_2_down;
initially l_f_q_2 ;

loc l_x0_q_2 : while x_q_2 <= delta0_u_q_2 wait {}

	when  s = 0  sync s_down goto l_x0_q_2;
	when  s = 1  sync s_up goto l_x0_q_2;
	when  s = 1-0   & q_1 = 1 sync s_down goto l_x0_q_2;
	when  s = 1-1   & q_1 = 1 sync s_up goto l_x0_q_2;
	when s = 1-0   & q_1 = 0 sync s_down do{ x_q_2'=0 } goto l_x1_q_2;
	when  q_1 = 0  sync q_1_down goto l_x0_q_2;
	when  q_1 = 1  sync q_1_up goto l_x0_q_2;
	when  q_1 = 1-0   & s = 1 sync q_1_down goto l_x0_q_2;
	when  q_1 = 1-1   & s = 1 sync q_1_up goto l_x0_q_2;
	when q_1 = 1-0   & s = 0 sync q_1_down do{ x_q_2'=0 } goto l_x1_q_2;

	when  x_q_2 >= delta0_l_q_2 sync q_2_down do {q_2' = 0} goto l_f_q_2;

loc l_x1_q_2 : while x_q_2 <= delta1_u_q_2 wait {}

	when  s = 0  sync s_down goto l_x1_q_2;
	when s = 1-1   & q_1 = 0 sync s_up do{ x_q_2'=0 } goto l_x0_q_2;
	when  q_1 = 0  sync q_1_down goto l_x1_q_2;
	when q_1 = 1-1   & s = 0 sync q_1_up do{ x_q_2'=0 } goto l_x0_q_2;

	when  x_q_2 >= delta1_l_q_2 sync q_2_up do {q_2' = 1} goto l_f_q_2;

loc l_f_q_2 : while True wait {}

	when  s = 0  sync s_down goto l_f_q_2;
	when  s = 1  sync s_up goto l_f_q_2;
	when  s = 1-0   & q_1 = 1 sync s_down goto l_f_q_2;
	when  s = 1-1   & q_1 = 1 sync s_up goto l_f_q_2;
	when s = 1-0   & q_1 = 0 sync s_down do{ x_q_2'=0 } goto l_x1_q_2;
	when s = 1-1   & q_1 = 0 sync s_up do{ x_q_2'=0 } goto l_x0_q_2;
	when  q_1 = 0  sync q_1_down goto l_f_q_2;
	when  q_1 = 1  sync q_1_up goto l_f_q_2;
	when  q_1 = 1-0   & s = 1 sync q_1_down goto l_f_q_2;
	when  q_1 = 1-1   & s = 1 sync q_1_up goto l_f_q_2;
	when q_1 = 1-0   & s = 0 sync q_1_down do{ x_q_2'=0 } goto l_x1_q_2;
	when q_1 = 1-1   & s = 0 sync q_1_up do{ x_q_2'=0 } goto l_x0_q_2;

end

-- les automates qui definent le signal q_1:
--------------------------------------------

automaton Ass_q_1

synclabs: r_up, r_down, q_2_up, q_2_down, q_1_up, q_1_down;
initially l_f_q_1 ;

loc l_x0_q_1 : while x_q_1 <= delta0_u_q_1 wait {}

	when  r = 0  sync r_down goto l_x0_q_1;
	when  r = 1  sync r_up goto l_x0_q_1;
	when  r = 1-0   & q_2 = 1 sync r_down goto l_x0_q_1;
	when  r = 1-1   & q_2 = 1 sync r_up goto l_x0_q_1;
	when r = 1-0   & q_2 = 0 sync r_down do{ x_q_1'=0 } goto l_x1_q_1;

	when  q_2 = 0  sync q_2_down goto l_x0_q_1;
	when  q_2 = 1  sync q_2_up goto l_x0_q_1;
	when  q_2 = 1-0   & r = 1 sync q_2_down goto l_x0_q_1;
	when  q_2 = 1-1   & r = 1 sync q_2_up goto l_x0_q_1;
	when q_2 = 1-0   & r = 0 sync q_2_down do{ x_q_1'=0 } goto l_x1_q_1;

	when  x_q_1 >= delta0_l_q_1 sync q_1_down do {q_1' = 0} goto l_f_q_1;

loc l_x1_q_1 : while x_q_1 <= delta1_u_q_1 wait {}

	when  r = 0  sync r_down goto l_x1_q_1;
	when r = 1-1   & q_2 = 0 sync r_up do{ x_q_1'=0 } goto l_x0_q_1;

	when  q_2 = 0  sync q_2_down goto l_x1_q_1;
	when q_2 = 1-1   & r = 0 sync q_2_up do{ x_q_1'=0 } goto l_x0_q_1;

	when  x_q_1 >= delta1_l_q_1 sync q_1_up do {q_1' = 1} goto l_f_q_1;

loc l_f_q_1 : while True wait {}

	when  r = 0  sync r_down goto l_f_q_1;
	when  r = 1  sync r_up goto l_f_q_1;

	when  r = 1-0   & q_2 = 1 sync r_down goto l_f_q_1;
	when  r = 1-1   & q_2 = 1 sync r_up goto l_f_q_1;

	when r = 1-0   & q_2 = 0 sync r_down do{ x_q_1'=0 } goto l_x1_q_1;
	when r = 1-1   & q_2 = 0 sync r_up do{ x_q_1'=0 } goto l_x0_q_1;

	when  q_2 = 0  sync q_2_down goto l_f_q_1;
	when  q_2 = 1  sync q_2_up goto l_f_q_1;
	when  q_2 = 1-0   & r = 1 sync q_2_down goto l_f_q_1;
	when  q_2 = 1-1   & r = 1 sync q_2_up goto l_f_q_1;
	when q_2 = 1-0   & r = 0 sync q_2_down do{ x_q_1'=0 } goto l_x1_q_1;
	when q_2 = 1-1   & r = 0 sync q_2_up do{ x_q_1'=0 } goto l_x0_q_1;

end

-------------------------------- Commandes d'analyse --------------------------------:
----------                                                                 ----------:
----------                                                                 ----------:
-------------------------------------------------------------------------------------:

var init : region;

init :=
	loc[env_nd_signals] = env_nd_signals_0 &
	loc[Ass_q_2] = l_f_q_2 &
	loc[Ass_q_1] = l_f_q_1 &

	t = 0 &
	x_q_2 = 0 &
	x_q_1 = 0 &

	s = 1 &
	r = 1 &
	q_2 = 0 &
	q_1 = 0

-- 	& delta0_l_q_2 = 2 & delta0_u_q_2 = 4 & delta1_l_q_2 = 2 & delta1_u_q_2 = 4 &
-- 	delta0_l_q_1 = 1 & delta0_u_q_1 = 3 & delta1_l_q_1 = 1 & delta1_u_q_1 = 3
	
	;

-- reach_reg := reach forward from init_reg endreach;

