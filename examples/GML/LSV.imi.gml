<!-- ************************************************************
 * Program examples/Valmem/LSV.imi
 * Converted by IMITATOR 2.42
 ************************************************************ -->

<!-- ************************************************************
 * !! Experimental translation !!
 *   - All automata are defined into one file (but in independent GML structures)
 *   - All variables are declared in all automata.
 *   - Initial constraint (on all variables) is added to each automaton.
 *   - We suppose that automata synchronize on variables and actions sharing the same names (common behavior).
 * This translation will be improved by the definition of synchronization rules conform with FML. Work in progress.
 ************************************************************ -->

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton env_nd_signals
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">env_nd_signals_0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">env_nd_signals_1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">13</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">env_nd_signals_2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">72</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">env_nd_signals_3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">188</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">env_nd_signals_4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">242</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">env_nd_signals_5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">255</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">env_nd_signals_6</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">314</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">env_nd_signals_7</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">412</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">env_nd_signals_8</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">484</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">env_nd_signals_9</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">497</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">env_nd_signals_10</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">556</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">env_nd_signals_11</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">654</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">env_nd_signals_12</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">672</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">env_nd_signals_13</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">726</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">env_nd_signals_14</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">798</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">env_nd_signals_15</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">896</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="16" nodeType="state">
		<attribute name="name">env_nd_signals_16</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">968</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="17" nodeType="state">
		<attribute name="name">env_nd_signals_17</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">1040</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="18" nodeType="state">
		<attribute name="name">env_nd_signals_end</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">13</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_0</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="2" target="3">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">72</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="3" target="4">
		<!-- Nosync wen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">188</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="5" arcType="transition" source="4" target="5">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">242</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="6" arcType="transition" source="5" target="6">
		<!-- Nosync d_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">255</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="6" target="7">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">314</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="8" arcType="transition" source="7" target="8">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">412</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">a_0</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="9" arcType="transition" source="8" target="9">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">484</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="10" arcType="transition" source="9" target="10">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">497</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_0</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="10" target="11">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">556</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="11" target="12">
		<!-- Nosync a_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">654</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">a_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="13" arcType="transition" source="12" target="13">
		<!-- Nosync wen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">672</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wen</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="14" arcType="transition" source="13" target="14">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">726</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="14" target="15">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">798</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="16" arcType="transition" source="15" target="16">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">896</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">a_0</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="17" arcType="transition" source="16" target="17">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">968</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="18" arcType="transition" source="17" target="18">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1040</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ck</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="19" arcType="transition" source="18" target="18">
		<attribute name="label">nosync_1</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton B1_reg_mux_output
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_s0_B1_reg_mux_output</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_s1_B1_reg_mux_output</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_B1_reg_mux_output</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="20" arcType="transition" source="0" target="1">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="21" arcType="transition" source="0" target="0">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="22" arcType="transition" source="0" target="0">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="0" target="1">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="0" target="0">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="25" arcType="transition" source="0" target="0">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="26" arcType="transition" source="0" target="1">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="27" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="29" arcType="transition" source="0" target="1">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="30" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="32" arcType="transition" source="1" target="0">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">x_B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="*">
							<attribute name="const">-1</attribute>
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="33" arcType="transition" source="1" target="0">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">x_B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B1_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="*">
							<attribute name="const">-1</attribute>
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton B2_reg_mux_output
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_s0_B2_reg_mux_output</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_s1_B2_reg_mux_output</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_B2_reg_mux_output</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="34" arcType="transition" source="0" target="1">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="0" target="0">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="36" arcType="transition" source="0" target="0">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="37" arcType="transition" source="0" target="0">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="38" arcType="transition" source="0" target="1">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="39" arcType="transition" source="0" target="0">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="40" arcType="transition" source="0" target="0">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="41" arcType="transition" source="0" target="0">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="42" arcType="transition" source="0" target="1">
		<!-- Nosync rowa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="43" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="44" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="46" arcType="transition" source="0" target="1">
		<!-- Nosync rowa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="47" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="48" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="49" arcType="transition" source="0" target="0">
		<!-- Nosync rowa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="50" arcType="transition" source="0" target="1">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="51" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="52" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="53" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="54" arcType="transition" source="0" target="1">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">net13d</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">wela</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rowa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">i_B_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="55" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13d</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">wela</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rowa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="56" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wela</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="57" arcType="transition" source="0" target="0">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="58" arcType="transition" source="1" target="0">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">x_B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="*">
							<attribute name="const">-1</attribute>
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="59" arcType="transition" source="1" target="0">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">x_B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B2_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="*">
							<attribute name="const">-1</attribute>
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i_B_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton P_reg_mux_output
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_P_reg_mux_output</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_mux_output</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_P_reg_mux_output</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_mux_output</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_P_reg_mux_output</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">l_s_P_reg_mux_output</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="60" arcType="transition" source="0" target="0">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="61" arcType="transition" source="0" target="0">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="62" arcType="transition" source="0" target="1">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="63" arcType="transition" source="0" target="0">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="64" arcType="transition" source="0" target="0">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="65" arcType="transition" source="0" target="0">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="66" arcType="transition" source="0" target="0">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="67" arcType="transition" source="0" target="1">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="68" arcType="transition" source="0" target="2">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B1_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mux_output</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net27</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="69" arcType="transition" source="0" target="2">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mux_output</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net27</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="70" arcType="transition" source="0" target="0">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="71" arcType="transition" source="0" target="3">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="72" arcType="transition" source="0" target="1">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="73" arcType="transition" source="0" target="0">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="74" arcType="transition" source="0" target="1">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="75" arcType="transition" source="0" target="0">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="76" arcType="transition" source="0" target="3">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="77" arcType="transition" source="0" target="0">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="78" arcType="transition" source="1" target="1">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="79" arcType="transition" source="1" target="1">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="80" arcType="transition" source="1" target="0">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="81" arcType="transition" source="1" target="1">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="82" arcType="transition" source="1" target="1">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="83" arcType="transition" source="1" target="0">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="84" arcType="transition" source="1" target="1">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="85" arcType="transition" source="1" target="1">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="86" arcType="transition" source="1" target="2">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B1_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mux_output</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net27</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="87" arcType="transition" source="1" target="2">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mux_output</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net27</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="88" arcType="transition" source="1" target="1">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="89" arcType="transition" source="1" target="3">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="90" arcType="transition" source="1" target="0">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="91" arcType="transition" source="1" target="1">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="92" arcType="transition" source="1" target="0">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="93" arcType="transition" source="1" target="1">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="94" arcType="transition" source="1" target="3">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="95" arcType="transition" source="1" target="1">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="96" arcType="transition" source="2" target="0">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="97" arcType="transition" source="2" target="2">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="98" arcType="transition" source="2" target="2">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="99" arcType="transition" source="2" target="1">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="100" arcType="transition" source="2" target="2">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">passa</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="101" arcType="transition" source="2" target="2">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="102" arcType="transition" source="2" target="2">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="103" arcType="transition" source="2" target="0">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="104" arcType="transition" source="2" target="2">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="105" arcType="transition" source="2" target="2">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="106" arcType="transition" source="2" target="1">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="107" arcType="transition" source="2" target="2">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="108" arcType="transition" source="2" target="2">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="109" arcType="transition" source="2" target="3">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="110" arcType="transition" source="2" target="0">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="111" arcType="transition" source="2" target="1">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="112" arcType="transition" source="2" target="2">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="113" arcType="transition" source="2" target="2">
		<!-- Nosync B1_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="114" arcType="transition" source="2" target="0">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="115" arcType="transition" source="2" target="1">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="116" arcType="transition" source="2" target="2">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="117" arcType="transition" source="2" target="2">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">d_inta</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">passa</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B2_reg_mux_output</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="118" arcType="transition" source="2" target="3">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B2_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B1_reg_mux_output</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="119" arcType="transition" source="2" target="2">
		<!-- Nosync B2_reg_mux_output_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B1_reg_mux_output</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="120" arcType="transition" source="3" target="3">
		<!-- Nosync passa_down -->
	</arc>
	<arc id="121" arcType="transition" source="3" target="3">
		<!-- Nosync passa_up -->
	</arc>
	<arc id="122" arcType="transition" source="3" target="3">
		<!-- Nosync d_inta_down -->
	</arc>
	<arc id="123" arcType="transition" source="3" target="3">
		<!-- Nosync d_inta_up -->
	</arc>
	<arc id="124" arcType="transition" source="3" target="0">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="125" arcType="transition" source="3" target="1">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="126" arcType="transition" source="3" target="2">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="127" arcType="transition" source="3" target="2">
		<!-- Nosync B1_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_inta</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="128" arcType="transition" source="3" target="0">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">passa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="129" arcType="transition" source="3" target="1">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">passa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mux_output</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="130" arcType="transition" source="3" target="2">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">passa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="131" arcType="transition" source="3" target="2">
		<!-- Nosync B2_reg_mux_output_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">passa</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net27</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_passa
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_passa</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_passa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_passa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_passa</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_passa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_passa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_passa</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="132" arcType="transition" source="0" target="0">
		<!-- Nosync pass_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="133" arcType="transition" source="0" target="1">
		<!-- Nosync pass_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_passa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="134" arcType="transition" source="0" target="2">
		<!-- Nosync passa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_passa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_passa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">passa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="135" arcType="transition" source="1" target="1">
		<!-- Nosync pass_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="136" arcType="transition" source="1" target="0">
		<!-- Nosync pass_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_passa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="137" arcType="transition" source="1" target="2">
		<!-- Nosync passa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_passa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_passa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">passa</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="138" arcType="transition" source="2" target="0">
		<!-- Nosync pass_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_passa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="139" arcType="transition" source="2" target="2">
		<!-- Nosync pass_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="140" arcType="transition" source="2" target="1">
		<!-- Nosync pass_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_passa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="141" arcType="transition" source="2" target="2">
		<!-- Nosync pass_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">pass</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton P_reg_mem_point
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_P_reg_mem_point</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_reg_mem_point</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_mem_point</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_P_reg_mem_point</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_reg_mem_point</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_mem_point</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_P_reg_mem_point</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">l_s_P_reg_mem_point</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="142" arcType="transition" source="0" target="0">
		<!-- Nosync d_int_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="143" arcType="transition" source="0" target="1">
		<!-- Nosync d_int_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="144" arcType="transition" source="0" target="2">
		<!-- Nosync pass_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_1_reg_mem_point</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mem_point</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">pass</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="145" arcType="transition" source="0" target="3">
		<!-- Nosync ema_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="146" arcType="transition" source="1" target="1">
		<!-- Nosync d_int_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="147" arcType="transition" source="1" target="0">
		<!-- Nosync d_int_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="148" arcType="transition" source="1" target="2">
		<!-- Nosync pass_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_1_reg_mem_point</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_mem_point</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">pass</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="149" arcType="transition" source="1" target="3">
		<!-- Nosync ema_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="150" arcType="transition" source="2" target="0">
		<!-- Nosync d_int_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="151" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="152" arcType="transition" source="2" target="1">
		<!-- Nosync d_int_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="153" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ema</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="154" arcType="transition" source="2" target="3">
		<!-- Nosync ema_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="155" arcType="transition" source="3" target="3">
		<!-- Nosync d_int_h_down -->
	</arc>
	<arc id="156" arcType="transition" source="3" target="3">
		<!-- Nosync d_int_h_up -->
	</arc>
	<arc id="157" arcType="transition" source="3" target="0">
		<!-- Nosync ema_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">pass</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="158" arcType="transition" source="3" target="1">
		<!-- Nosync ema_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">pass</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_mem_point</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="159" arcType="transition" source="3" target="2">
		<!-- Nosync ema_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">pass</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="160" arcType="transition" source="3" target="2">
		<!-- Nosync ema_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">pass</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_rowa
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_rowa</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_rowa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_rowa</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_rowa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_rowa</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="161" arcType="transition" source="0" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="162" arcType="transition" source="0" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="163" arcType="transition" source="0" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="164" arcType="transition" source="0" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="165" arcType="transition" source="0" target="2">
		<!-- Nosync rowa_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_rowa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="166" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="167" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="168" arcType="transition" source="1" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="169" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="170" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="171" arcType="transition" source="1" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="172" arcType="transition" source="1" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="173" arcType="transition" source="1" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="174" arcType="transition" source="1" target="1">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="175" arcType="transition" source="1" target="1">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="176" arcType="transition" source="1" target="2">
		<!-- Nosync rowa_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_rowa</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_rowa</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rowa</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="177" arcType="transition" source="2" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="178" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="179" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="180" arcType="transition" source="2" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="181" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="182" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="183" arcType="transition" source="2" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="184" arcType="transition" source="2" target="2">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="185" arcType="transition" source="2" target="2">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="186" arcType="transition" source="2" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_rowa</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="187" arcType="transition" source="2" target="2">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="188" arcType="transition" source="2" target="2">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_d_int_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_d_int_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_d_int_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_d_int_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_d_int_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_d_int_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="189" arcType="transition" source="0" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="190" arcType="transition" source="0" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="191" arcType="transition" source="0" target="2">
		<!-- Nosync d_int_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_d_int_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="192" arcType="transition" source="1" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="193" arcType="transition" source="1" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="194" arcType="transition" source="1" target="2">
		<!-- Nosync d_int_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_d_int_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="195" arcType="transition" source="2" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="196" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="197" arcType="transition" source="2" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="198" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_d_inta
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_d_inta</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_d_inta</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_inta</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_d_inta</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_d_inta</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_inta</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_d_inta</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="199" arcType="transition" source="0" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="200" arcType="transition" source="0" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="201" arcType="transition" source="0" target="2">
		<!-- Nosync d_inta_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_d_inta</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_inta</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="202" arcType="transition" source="1" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="203" arcType="transition" source="1" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="204" arcType="transition" source="1" target="2">
		<!-- Nosync d_inta_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_d_inta</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_inta</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="205" arcType="transition" source="2" target="0">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="206" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="207" arcType="transition" source="2" target="1">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_inta</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="208" arcType="transition" source="2" target="2">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_int</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_ema
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_ema</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_ema</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_ema</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_ema</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_ema</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="209" arcType="transition" source="0" target="0">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="210" arcType="transition" source="0" target="0">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="211" arcType="transition" source="0" target="0">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="212" arcType="transition" source="0" target="1">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="213" arcType="transition" source="0" target="0">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="214" arcType="transition" source="0" target="0">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="215" arcType="transition" source="0" target="0">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="216" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="217" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="218" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="219" arcType="transition" source="0" target="1">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="220" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="221" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="222" arcType="transition" source="0" target="0">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="223" arcType="transition" source="0" target="0">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="224" arcType="transition" source="0" target="0">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="225" arcType="transition" source="0" target="0">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="226" arcType="transition" source="0" target="1">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="227" arcType="transition" source="0" target="0">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="228" arcType="transition" source="0" target="0">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="229" arcType="transition" source="0" target="0">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="230" arcType="transition" source="0" target="2">
		<!-- Nosync ema_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_ema</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="231" arcType="transition" source="1" target="1">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="232" arcType="transition" source="1" target="0">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="233" arcType="transition" source="1" target="1">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="234" arcType="transition" source="1" target="0">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="235" arcType="transition" source="1" target="1">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="236" arcType="transition" source="1" target="0">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="237" arcType="transition" source="1" target="2">
		<!-- Nosync ema_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_ema</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_ema</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ema</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="238" arcType="transition" source="2" target="1">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="239" arcType="transition" source="2" target="2">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="240" arcType="transition" source="2" target="2">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="241" arcType="transition" source="2" target="2">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="242" arcType="transition" source="2" target="0">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="243" arcType="transition" source="2" target="2">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="244" arcType="transition" source="2" target="2">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="245" arcType="transition" source="2" target="2">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="246" arcType="transition" source="2" target="1">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="247" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="248" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="249" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="250" arcType="transition" source="2" target="0">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="251" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13e</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="252" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="253" arcType="transition" source="2" target="2">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="254" arcType="transition" source="2" target="1">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="255" arcType="transition" source="2" target="2">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="256" arcType="transition" source="2" target="2">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="257" arcType="transition" source="2" target="2">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="258" arcType="transition" source="2" target="0">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_ema</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="259" arcType="transition" source="2" target="2">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">welb</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="260" arcType="transition" source="2" target="2">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">net13e</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">welb</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">row</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="261" arcType="transition" source="2" target="2">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_row
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_row</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_row</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_row</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_row</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_row</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="262" arcType="transition" source="0" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="263" arcType="transition" source="0" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="264" arcType="transition" source="0" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="265" arcType="transition" source="0" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="266" arcType="transition" source="0" target="2">
		<!-- Nosync row_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_row</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="267" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="268" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="269" arcType="transition" source="1" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="270" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="271" arcType="transition" source="1" target="1">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="272" arcType="transition" source="1" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="273" arcType="transition" source="1" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="274" arcType="transition" source="1" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="275" arcType="transition" source="1" target="1">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="276" arcType="transition" source="1" target="1">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="277" arcType="transition" source="1" target="2">
		<!-- Nosync row_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_row</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_row</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">row</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="278" arcType="transition" source="2" target="0">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="279" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="280" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="281" arcType="transition" source="2" target="1">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="282" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="283" arcType="transition" source="2" target="2">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="284" arcType="transition" source="2" target="0">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="285" arcType="transition" source="2" target="2">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="286" arcType="transition" source="2" target="2">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="287" arcType="transition" source="2" target="1">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_row</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="288" arcType="transition" source="2" target="2">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="289" arcType="transition" source="2" target="2">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net39</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_welb
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_welb</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_welb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_welb</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_welb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_welb</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="290" arcType="transition" source="0" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="291" arcType="transition" source="0" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="292" arcType="transition" source="0" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="293" arcType="transition" source="0" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="294" arcType="transition" source="0" target="2">
		<!-- Nosync welb_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_welb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="295" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="296" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="297" arcType="transition" source="1" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="298" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="299" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="300" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="301" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="302" arcType="transition" source="1" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="303" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="304" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="305" arcType="transition" source="1" target="2">
		<!-- Nosync welb_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_welb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_welb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">welb</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="306" arcType="transition" source="2" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="307" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="308" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="309" arcType="transition" source="2" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="310" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="311" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="312" arcType="transition" source="2" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="313" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="314" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="315" arcType="transition" source="2" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_welb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="316" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="317" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_wela
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_wela</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="318" arcType="transition" source="0" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="319" arcType="transition" source="0" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="320" arcType="transition" source="0" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="321" arcType="transition" source="0" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="322" arcType="transition" source="0" target="2">
		<!-- Nosync wela_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="323" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="324" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="325" arcType="transition" source="1" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="326" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="327" arcType="transition" source="1" target="1">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="328" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="329" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="330" arcType="transition" source="1" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="331" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="332" arcType="transition" source="1" target="1">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="333" arcType="transition" source="1" target="2">
		<!-- Nosync wela_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wela</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="334" arcType="transition" source="2" target="0">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="335" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="336" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="337" arcType="transition" source="2" target="1">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="338" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="339" arcType="transition" source="2" target="2">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="340" arcType="transition" source="2" target="0">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="341" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="342" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="343" arcType="transition" source="2" target="1">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="344" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net13a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="345" arcType="transition" source="2" target="2">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_net45a
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_net45a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_net45a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_net45a</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="346" arcType="transition" source="0" target="0">
		<!-- Nosync net45_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="347" arcType="transition" source="0" target="1">
		<!-- Nosync net45_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="348" arcType="transition" source="0" target="2">
		<!-- Nosync net45a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net45a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="349" arcType="transition" source="1" target="1">
		<!-- Nosync net45_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="350" arcType="transition" source="1" target="0">
		<!-- Nosync net45_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="351" arcType="transition" source="1" target="2">
		<!-- Nosync net45a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_net45a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net45a</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="352" arcType="transition" source="2" target="0">
		<!-- Nosync net45_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="353" arcType="transition" source="2" target="2">
		<!-- Nosync net45_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="354" arcType="transition" source="2" target="1">
		<!-- Nosync net45_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="355" arcType="transition" source="2" target="2">
		<!-- Nosync net45_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net45</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_net13f
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_net13f</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13f</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_net13f</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13f</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_net13f</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="356" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="357" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13f</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="358" arcType="transition" source="0" target="2">
		<!-- Nosync net13f_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13f</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13f</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="359" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="360" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13f</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="361" arcType="transition" source="1" target="2">
		<!-- Nosync net13f_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_net13f</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13f</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13f</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="362" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13f</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="363" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="364" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13f</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="365" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_net13e
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_net13e</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13e</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_net13e</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13e</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_net13e</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="366" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="367" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13e</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="368" arcType="transition" source="0" target="2">
		<!-- Nosync net13e_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13e</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13e</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="369" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="370" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13e</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="371" arcType="transition" source="1" target="2">
		<!-- Nosync net13e_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_net13e</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13e</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13e</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="372" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13e</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="373" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="374" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13e</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="375" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_net13d
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_net13d</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13d</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_net13d</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13d</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_net13d</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="376" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="377" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13d</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="378" arcType="transition" source="0" target="2">
		<!-- Nosync net13d_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13d</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13d</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="379" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="380" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13d</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="381" arcType="transition" source="1" target="2">
		<!-- Nosync net13d_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_net13d</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13d</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13d</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="382" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13d</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="383" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="384" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13d</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="385" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_net13a
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_net13a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_net13a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_net13a</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="386" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="387" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="388" arcType="transition" source="0" target="2">
		<!-- Nosync net13a_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="389" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="390" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="391" arcType="transition" source="1" target="2">
		<!-- Nosync net13a_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net13a</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="392" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="393" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="394" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="395" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton P_reg_latch_a
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_P_reg_latch_a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_reg_latch_a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latch_a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_P_reg_latch_a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_reg_latch_a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latch_a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_P_reg_latch_a</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">l_s_P_reg_latch_a</attribute>
	</node>
 
	<arc id="396" arcType="transition" source="0" target="0">
		<!-- Nosync a_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="397" arcType="transition" source="0" target="1">
		<!-- Nosync a_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="398" arcType="transition" source="0" target="2">
		<!-- Nosync net39_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_1_reg_latch_a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latch_a</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net39</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="399" arcType="transition" source="0" target="3">
		<!-- Nosync en_latcha_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="400" arcType="transition" source="1" target="1">
		<!-- Nosync a_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="401" arcType="transition" source="1" target="0">
		<!-- Nosync a_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="402" arcType="transition" source="1" target="2">
		<!-- Nosync net39_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_1_reg_latch_a</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latch_a</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net39</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="403" arcType="transition" source="1" target="3">
		<!-- Nosync en_latcha_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="404" arcType="transition" source="2" target="0">
		<!-- Nosync a_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="405" arcType="transition" source="2" target="2">
		<!-- Nosync a_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="406" arcType="transition" source="2" target="1">
		<!-- Nosync a_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="407" arcType="transition" source="2" target="2">
		<!-- Nosync a_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latcha</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="408" arcType="transition" source="2" target="3">
		<!-- Nosync en_latcha_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="409" arcType="transition" source="3" target="3">
		<!-- Nosync a_h_down -->
	</arc>
	<arc id="410" arcType="transition" source="3" target="3">
		<!-- Nosync a_h_up -->
	</arc>
	<arc id="411" arcType="transition" source="3" target="0">
		<!-- Nosync en_latcha_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="412" arcType="transition" source="3" target="1">
		<!-- Nosync en_latcha_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latch_a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="413" arcType="transition" source="3" target="2">
		<!-- Nosync en_latcha_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="414" arcType="transition" source="3" target="2">
		<!-- Nosync en_latcha_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">a_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net39</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton P_reg_latchwen
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_P_reg_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_reg_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_P_reg_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_reg_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_P_reg_latchwen</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">l_s_P_reg_latchwen</attribute>
	</node>
 
	<arc id="415" arcType="transition" source="0" target="0">
		<!-- Nosync wen_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="416" arcType="transition" source="0" target="1">
		<!-- Nosync wen_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="417" arcType="transition" source="0" target="2">
		<!-- Nosync net45_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_1_reg_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latchwen</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="418" arcType="transition" source="0" target="3">
		<!-- Nosync en_latchwen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="419" arcType="transition" source="1" target="1">
		<!-- Nosync wen_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="420" arcType="transition" source="1" target="0">
		<!-- Nosync wen_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="421" arcType="transition" source="1" target="2">
		<!-- Nosync net45_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_1_reg_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latchwen</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">net45</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="422" arcType="transition" source="1" target="3">
		<!-- Nosync en_latchwen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="423" arcType="transition" source="2" target="0">
		<!-- Nosync wen_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="424" arcType="transition" source="2" target="2">
		<!-- Nosync wen_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="425" arcType="transition" source="2" target="1">
		<!-- Nosync wen_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="426" arcType="transition" source="2" target="2">
		<!-- Nosync wen_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchwen</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="427" arcType="transition" source="2" target="3">
		<!-- Nosync en_latchwen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="428" arcType="transition" source="3" target="3">
		<!-- Nosync wen_h_down -->
	</arc>
	<arc id="429" arcType="transition" source="3" target="3">
		<!-- Nosync wen_h_up -->
	</arc>
	<arc id="430" arcType="transition" source="3" target="0">
		<!-- Nosync en_latchwen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="431" arcType="transition" source="3" target="1">
		<!-- Nosync en_latchwen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="432" arcType="transition" source="3" target="2">
		<!-- Nosync en_latchwen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="433" arcType="transition" source="3" target="2">
		<!-- Nosync en_latchwen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">wen_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">net45</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton P_reg_latchd
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_P_reg_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_reg_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_P_reg_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_reg_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_reg_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_P_reg_latchd</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">l_s_P_reg_latchd</attribute>
	</node>
 
	<arc id="434" arcType="transition" source="0" target="0">
		<!-- Nosync d_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="435" arcType="transition" source="0" target="1">
		<!-- Nosync d_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="436" arcType="transition" source="0" target="2">
		<!-- Nosync d_int_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta0_1_reg_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latchd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="437" arcType="transition" source="0" target="3">
		<!-- Nosync en_latchd_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="438" arcType="transition" source="1" target="1">
		<!-- Nosync d_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="439" arcType="transition" source="1" target="0">
		<!-- Nosync d_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="440" arcType="transition" source="1" target="2">
		<!-- Nosync d_int_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">delta1_1_reg_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x_reg_latchd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_int</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="441" arcType="transition" source="1" target="3">
		<!-- Nosync en_latchd_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="442" arcType="transition" source="2" target="0">
		<!-- Nosync d_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="443" arcType="transition" source="2" target="2">
		<!-- Nosync d_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="444" arcType="transition" source="2" target="1">
		<!-- Nosync d_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="445" arcType="transition" source="2" target="2">
		<!-- Nosync d_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">en_latchd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="446" arcType="transition" source="2" target="3">
		<!-- Nosync en_latchd_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="447" arcType="transition" source="3" target="3">
		<!-- Nosync d_h_down -->
	</arc>
	<arc id="448" arcType="transition" source="3" target="3">
		<!-- Nosync d_h_up -->
	</arc>
	<arc id="449" arcType="transition" source="3" target="0">
		<!-- Nosync en_latchd_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="450" arcType="transition" source="3" target="1">
		<!-- Nosync en_latchd_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_reg_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="451" arcType="transition" source="3" target="2">
		<!-- Nosync en_latchd_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="452" arcType="transition" source="3" target="2">
		<!-- Nosync en_latchd_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_h</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">d_int</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_en_latchd
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_en_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_en_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_en_latchd</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="453" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="454" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="455" arcType="transition" source="0" target="2">
		<!-- Nosync en_latchd_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="456" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="457" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="458" arcType="transition" source="1" target="2">
		<!-- Nosync en_latchd_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="459" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="460" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="461" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="462" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_en_latcha
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_en_latcha</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latcha</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_en_latcha</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latcha</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_en_latcha</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="463" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="464" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="465" arcType="transition" source="0" target="2">
		<!-- Nosync en_latcha_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latcha</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="466" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="467" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="468" arcType="transition" source="1" target="2">
		<!-- Nosync en_latcha_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_en_latcha</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latcha</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="469" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="470" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="471" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latcha</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="472" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_en_latchwen
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_en_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_en_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_en_latchwen</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="473" arcType="transition" source="0" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="474" arcType="transition" source="0" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="475" arcType="transition" source="0" target="2">
		<!-- Nosync en_latchwen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="476" arcType="transition" source="1" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="477" arcType="transition" source="1" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="478" arcType="transition" source="1" target="2">
		<!-- Nosync en_latchwen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="479" arcType="transition" source="2" target="1">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="480" arcType="transition" source="2" target="2">
		<!-- Nosync ck_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="481" arcType="transition" source="2" target="0">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="482" arcType="transition" source="2" target="2">
		<!-- Nosync ck_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">ck</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_a_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_a_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_a_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_a_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_a_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_a_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_a_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_a_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="483" arcType="transition" source="0" target="0">
		<!-- Nosync a_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="484" arcType="transition" source="0" target="1">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_a_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="485" arcType="transition" source="0" target="2">
		<!-- Nosync a_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_a_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_a_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">a_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="486" arcType="transition" source="1" target="1">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="487" arcType="transition" source="1" target="0">
		<!-- Nosync a_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_a_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="488" arcType="transition" source="1" target="2">
		<!-- Nosync a_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_a_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_a_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">a_h</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="489" arcType="transition" source="2" target="0">
		<!-- Nosync a_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_a_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="490" arcType="transition" source="2" target="2">
		<!-- Nosync a_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="491" arcType="transition" source="2" target="1">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_a_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="492" arcType="transition" source="2" target="2">
		<!-- Nosync a_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">a_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_wen_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_wen_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_wen_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_wen_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="493" arcType="transition" source="0" target="0">
		<!-- Nosync wen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="494" arcType="transition" source="0" target="1">
		<!-- Nosync wen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="495" arcType="transition" source="0" target="2">
		<!-- Nosync wen_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="496" arcType="transition" source="1" target="1">
		<!-- Nosync wen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="497" arcType="transition" source="1" target="0">
		<!-- Nosync wen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="498" arcType="transition" source="1" target="2">
		<!-- Nosync wen_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="499" arcType="transition" source="2" target="0">
		<!-- Nosync wen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="500" arcType="transition" source="2" target="2">
		<!-- Nosync wen_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="501" arcType="transition" source="2" target="1">
		<!-- Nosync wen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="502" arcType="transition" source="2" target="2">
		<!-- Nosync wen_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">wen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_d_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_d_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_d_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_d_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="503" arcType="transition" source="0" target="0">
		<!-- Nosync d_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="504" arcType="transition" source="0" target="1">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="505" arcType="transition" source="0" target="2">
		<!-- Nosync d_h_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="506" arcType="transition" source="1" target="1">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="507" arcType="transition" source="1" target="0">
		<!-- Nosync d_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="508" arcType="transition" source="1" target="2">
		<!-- Nosync d_h_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">d_h</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="509" arcType="transition" source="2" target="0">
		<!-- Nosync d_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="510" arcType="transition" source="2" target="2">
		<!-- Nosync d_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="511" arcType="transition" source="2" target="1">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="512" arcType="transition" source="2" target="2">
		<!-- Nosync d_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Ass_q_0
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">t</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_passa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_mem_point</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_rowa</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_inta</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_ema</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_row</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_welb</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13f</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13e</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13d</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latch_a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_reg_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latcha</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_a_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_q_0</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">d_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_0</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ck</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net27</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B1_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B2_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">i_B_reg_mux_output</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">passa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">pass</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rowa</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_inta</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ema</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">row</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">welb</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wela</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13f</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13e</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13d</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net13a</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net39</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">net45</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_int</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchd</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latcha</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">en_latchwen</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">a_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">wen_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">d_h</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">q_0</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_2_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mux_output</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_passa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_mem_point</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_rowa</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_int_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_ema</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_row</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_welb</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13f</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latch_a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_reg_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latcha</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_a_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta0_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delta1_q_0</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">l_x0_q_0</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta0_q_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_q_0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">l_x1_q_0</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delta1_q_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_q_0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">l_f_q_0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="513" arcType="transition" source="0" target="0">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="514" arcType="transition" source="0" target="1">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_q_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="515" arcType="transition" source="0" target="2">
		<!-- Nosync q_0_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta0_q_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_q_0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">q_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="516" arcType="transition" source="1" target="1">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="517" arcType="transition" source="1" target="0">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_q_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="518" arcType="transition" source="1" target="2">
		<!-- Nosync q_0_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">delta1_q_0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_q_0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">q_0</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="519" arcType="transition" source="2" target="0">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_q_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="520" arcType="transition" source="2" target="2">
		<!-- Nosync net27_down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="521" arcType="transition" source="2" target="1">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_q_0</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="522" arcType="transition" source="2" target="2">
		<!-- Nosync net27_up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">net27</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>