<!-- ************************************************************
 * Program examples/Valmem/spsmall.imi
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
 automaton abs_net27
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_abs_net27</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_abs_net27</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_abs_net27</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_q_0</attribute>
							<attribute name="name">d_up_net27</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net27</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">C_abs_net27</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync up_d_inta -->
	</arc>
	<arc id="2" arcType="transition" source="0" target="3">
		<!-- Nosync down_wela -->
	</arc>
	<arc id="3" arcType="transition" source="0" target="0">
		<!-- Nosync up_wela -->
	</arc>
	<arc id="4" arcType="transition" source="0" target="0">
		<!-- Nosync down_d_inta -->
	</arc>

	<arc id="5" arcType="transition" source="1" target="2">
		<!-- Nosync down_wela -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net27</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="6" arcType="transition" source="1" target="1">
		<!-- Nosync up_wela -->
	</arc>
	<arc id="7" arcType="transition" source="1" target="1">
		<!-- Nosync up_d_inta -->
	</arc>
	<arc id="8" arcType="transition" source="1" target="3">
		<!-- Nosync down_d_inta -->
	</arc>

	<arc id="9" arcType="transition" source="2" target="2">
		<!-- Nosync down_wela -->
	</arc>
	<arc id="10" arcType="transition" source="2" target="3">
		<!-- Nosync up_wela -->
	</arc>
	<arc id="11" arcType="transition" source="2" target="3">
		<!-- Nosync up_d_inta -->
	</arc>
	<arc id="12" arcType="transition" source="2" target="2">
		<!-- Nosync down_d_inta -->
	</arc>
	<arc id="13" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_1</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_q_0</attribute>
							<attribute name="name">d_up_net27</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net27</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="14" arcType="transition" source="3" target="3">
		<!-- Nosync down_wela -->
	</arc>
	<arc id="15" arcType="transition" source="3" target="3">
		<!-- Nosync up_wela -->
	</arc>
	<arc id="16" arcType="transition" source="3" target="3">
		<!-- Nosync down_d_inta -->
	</arc>
	<arc id="17" arcType="transition" source="3" target="3">
		<!-- Nosync up_d_inta -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton f2_wela
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">e_00_0_wela</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">e_01_1_wela</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">e_10_1_wela</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">e_11_1_wela</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">e_00_X_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">e_01_X_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">e_10_X_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">e_11_X_wela</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="18" arcType="transition" source="0" target="5">
		<!-- Nosync up_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="19" arcType="transition" source="0" target="6">
		<!-- Nosync up_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="0" target="0">
		<!-- Nosync down_net45a -->
	</arc>
	<arc id="21" arcType="transition" source="0" target="0">
		<!-- Nosync down_net13a -->
	</arc>

	<arc id="22" arcType="transition" source="1" target="4">
		<!-- Nosync down_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="1" target="7">
		<!-- Nosync up_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="1" target="1">
		<!-- Nosync up_net45a -->
	</arc>
	<arc id="25" arcType="transition" source="1" target="1">
		<!-- Nosync down_net13a -->
	</arc>

	<arc id="26" arcType="transition" source="2" target="7">
		<!-- Nosync up_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="27" arcType="transition" source="2" target="4">
		<!-- Nosync down_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="2" target="2">
		<!-- Nosync down_net45a -->
	</arc>
	<arc id="29" arcType="transition" source="2" target="2">
		<!-- Nosync up_net13a -->
	</arc>

	<arc id="30" arcType="transition" source="3" target="6">
		<!-- Nosync down_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="3" target="5">
		<!-- Nosync down_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="32" arcType="transition" source="3" target="3">
		<!-- Nosync up_net45a -->
	</arc>
	<arc id="33" arcType="transition" source="3" target="3">
		<!-- Nosync up_net13a -->
	</arc>

	<arc id="34" arcType="transition" source="4" target="5">
		<!-- Nosync up_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="4" target="6">
		<!-- Nosync up_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="36" arcType="transition" source="4" target="4">
		<!-- Nosync down_net45a -->
	</arc>
	<arc id="37" arcType="transition" source="4" target="4">
		<!-- Nosync down_net13a -->
	</arc>
	<arc id="38" arcType="transition" source="4" target="0">
		<!-- Nosync down_wela -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="39" arcType="transition" source="5" target="4">
		<!-- Nosync down_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="40" arcType="transition" source="5" target="7">
		<!-- Nosync up_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="41" arcType="transition" source="5" target="5">
		<!-- Nosync up_net45a -->
	</arc>
	<arc id="42" arcType="transition" source="5" target="5">
		<!-- Nosync down_net13a -->
	</arc>
	<arc id="43" arcType="transition" source="5" target="1">
		<!-- Nosync up_wela -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="44" arcType="transition" source="6" target="7">
		<!-- Nosync up_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="6" target="4">
		<!-- Nosync down_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="46" arcType="transition" source="6" target="6">
		<!-- Nosync down_net45a -->
	</arc>
	<arc id="47" arcType="transition" source="6" target="6">
		<!-- Nosync up_net13a -->
	</arc>
	<arc id="48" arcType="transition" source="6" target="2">
		<!-- Nosync up_wela -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="49" arcType="transition" source="7" target="6">
		<!-- Nosync down_net45a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="50" arcType="transition" source="7" target="5">
		<!-- Nosync down_net13a -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wela</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="51" arcType="transition" source="7" target="7">
		<!-- Nosync up_net45a -->
	</arc>
	<arc id="52" arcType="transition" source="7" target="7">
		<!-- Nosync up_net13a -->
	</arc>
	<arc id="53" arcType="transition" source="7" target="3">
		<!-- Nosync up_wela -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_wela</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wela</attribute>
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
 automaton not_net13a
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_not_net13a</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_not_net13a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_not_net13a</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="54" arcType="transition" source="0" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="55" arcType="transition" source="0" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="56" arcType="transition" source="1" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="57" arcType="transition" source="1" target="0">
		<!-- Nosync down_net13a -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="58" arcType="transition" source="2" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net13a</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="59" arcType="transition" source="2" target="0">
		<!-- Nosync up_net13a -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_net13a</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net13a</attribute>
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
 automaton reg_net45
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">e0d0_U_reg_net45</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">e1d0_X_reg_net45</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_dn_net45a</attribute>
							<attribute name="name">d_dn_net45</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">e1d0_0_reg_net45</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">e0d1_U_reg_net45</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">e1d1_X_reg_net45</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_net45a</attribute>
							<attribute name="name">d_up_net45</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">e1d1_1_reg_net45</attribute>
	</node>
 
	<arc id="60" arcType="transition" source="0" target="0">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="61" arcType="transition" source="0" target="0">
		<!-- Nosync down_wen_h -->
	</arc>
	<arc id="62" arcType="transition" source="0" target="1">
		<!-- Nosync up_en_latchwen -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="63" arcType="transition" source="0" target="3">
		<!-- Nosync up_wen_h -->
	</arc>

	<arc id="64" arcType="transition" source="1" target="1">
		<!-- Nosync down_wen_h -->
	</arc>
	<arc id="65" arcType="transition" source="1" target="1">
		<!-- Nosync up_en_latchwen -->
	</arc>
	<arc id="66" arcType="transition" source="1" target="0">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="67" arcType="transition" source="1" target="4">
		<!-- Nosync up_wen_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="68" arcType="transition" source="1" target="2">
		<!-- Nosync down_net45a -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_dn_net45a</attribute>
							<attribute name="name">d_dn_net45</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="69" arcType="transition" source="2" target="2">
		<!-- Nosync down_wen_h -->
	</arc>
	<arc id="70" arcType="transition" source="2" target="0">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="71" arcType="transition" source="2" target="2">
		<!-- Nosync up_en_latchwen -->
	</arc>
	<arc id="72" arcType="transition" source="2" target="4">
		<!-- Nosync up_wen_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="73" arcType="transition" source="3" target="3">
		<!-- Nosync up_wen_h -->
	</arc>
	<arc id="74" arcType="transition" source="3" target="3">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="75" arcType="transition" source="3" target="4">
		<!-- Nosync up_en_latchwen -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="76" arcType="transition" source="3" target="0">
		<!-- Nosync down_wen_h -->
	</arc>

	<arc id="77" arcType="transition" source="4" target="3">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="78" arcType="transition" source="4" target="4">
		<!-- Nosync up_wen_h -->
	</arc>
	<arc id="79" arcType="transition" source="4" target="4">
		<!-- Nosync up_en_latchwen -->
	</arc>
	<arc id="80" arcType="transition" source="4" target="1">
		<!-- Nosync down_wen_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="81" arcType="transition" source="4" target="5">
		<!-- Nosync up_net45a -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_net45a</attribute>
							<attribute name="name">d_up_net45</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_net45</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="82" arcType="transition" source="5" target="3">
		<!-- Nosync down_en_latchwen -->
	</arc>
	<arc id="83" arcType="transition" source="5" target="5">
		<!-- Nosync up_en_latchwen -->
	</arc>
	<arc id="84" arcType="transition" source="5" target="1">
		<!-- Nosync down_wen_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_net45</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="85" arcType="transition" source="5" target="5">
		<!-- Nosync up_wen_h -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton reg_d_int
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">e0d0_U_reg_d_int</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">e1d0_X_reg_d_int</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_dn_d_inta</attribute>
							<attribute name="name">d_dn_d_int</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">e1d0_0_reg_d_int</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">e0d1_U_reg_d_int</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">e1d1_X_reg_d_int</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_d_inta</attribute>
							<attribute name="name">d_up_d_int</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">e1d1_1_reg_d_int</attribute>
	</node>
 
	<arc id="86" arcType="transition" source="0" target="0">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="87" arcType="transition" source="0" target="0">
		<!-- Nosync down_d_h -->
	</arc>
	<arc id="88" arcType="transition" source="0" target="1">
		<!-- Nosync up_en_latchd -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="89" arcType="transition" source="0" target="3">
		<!-- Nosync up_d_h -->
	</arc>

	<arc id="90" arcType="transition" source="1" target="1">
		<!-- Nosync down_d_h -->
	</arc>
	<arc id="91" arcType="transition" source="1" target="1">
		<!-- Nosync up_en_latchd -->
	</arc>
	<arc id="92" arcType="transition" source="1" target="0">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="93" arcType="transition" source="1" target="4">
		<!-- Nosync up_d_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="94" arcType="transition" source="1" target="2">
		<!-- Nosync down_d_inta -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_dn_d_inta</attribute>
							<attribute name="name">d_dn_d_int</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="95" arcType="transition" source="2" target="2">
		<!-- Nosync down_d_h -->
	</arc>
	<arc id="96" arcType="transition" source="2" target="0">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="97" arcType="transition" source="2" target="2">
		<!-- Nosync up_en_latchd -->
	</arc>
	<arc id="98" arcType="transition" source="2" target="4">
		<!-- Nosync up_d_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="99" arcType="transition" source="3" target="3">
		<!-- Nosync up_d_h -->
	</arc>
	<arc id="100" arcType="transition" source="3" target="3">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="101" arcType="transition" source="3" target="4">
		<!-- Nosync up_en_latchd -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="102" arcType="transition" source="3" target="0">
		<!-- Nosync down_d_h -->
	</arc>

	<arc id="103" arcType="transition" source="4" target="3">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="104" arcType="transition" source="4" target="4">
		<!-- Nosync up_d_h -->
	</arc>
	<arc id="105" arcType="transition" source="4" target="4">
		<!-- Nosync up_en_latchd -->
	</arc>
	<arc id="106" arcType="transition" source="4" target="1">
		<!-- Nosync down_d_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="107" arcType="transition" source="4" target="5">
		<!-- Nosync up_d_inta -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">d_up_d_inta</attribute>
							<attribute name="name">d_up_d_int</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_int</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="108" arcType="transition" source="5" target="3">
		<!-- Nosync down_en_latchd -->
	</arc>
	<arc id="109" arcType="transition" source="5" target="5">
		<!-- Nosync up_en_latchd -->
	</arc>
	<arc id="110" arcType="transition" source="5" target="1">
		<!-- Nosync down_d_h -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_int</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="111" arcType="transition" source="5" target="5">
		<!-- Nosync up_d_h -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton not_en_latchd
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_not_en_latchd</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_not_en_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_not_en_latchd</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="112" arcType="transition" source="0" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="113" arcType="transition" source="0" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="114" arcType="transition" source="1" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="115" arcType="transition" source="1" target="0">
		<!-- Nosync down_en_latchd -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="116" arcType="transition" source="2" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="117" arcType="transition" source="2" target="0">
		<!-- Nosync up_en_latchd -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_en_latchd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchd</attribute>
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
 automaton not_en_latchwen
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_not_en_latchwen</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_not_en_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_not_en_latchwen</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="118" arcType="transition" source="0" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="119" arcType="transition" source="0" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="120" arcType="transition" source="1" target="2">
		<!-- Nosync down_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="121" arcType="transition" source="1" target="0">
		<!-- Nosync down_en_latchwen -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="122" arcType="transition" source="2" target="1">
		<!-- Nosync up_ck -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_en_latchwen</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="123" arcType="transition" source="2" target="0">
		<!-- Nosync up_en_latchwen -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_en_latchwen</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_en_latchwen</attribute>
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
 automaton retard_wen_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_ret_wen_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_ret_wen_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_ret_wen_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="124" arcType="transition" source="0" target="2">
		<!-- Nosync down_wen -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="125" arcType="transition" source="1" target="2">
		<!-- Nosync down_wen -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_wen_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="126" arcType="transition" source="1" target="0">
		<!-- Nosync up_wen_h -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="127" arcType="transition" source="2" target="0">
		<!-- Nosync down_wen_h -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_wen_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_wen_h</attribute>
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
 automaton retard_d_h
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_ret_d_h</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">A_ret_d_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_up_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">B_ret_d_h</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d_dn_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="128" arcType="transition" source="0" target="1">
		<!-- Nosync up_d_0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="129" arcType="transition" source="1" target="0">
		<!-- Nosync up_d_h -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_up_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="130" arcType="transition" source="2" target="1">
		<!-- Nosync up_d_0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x_d_h</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="131" arcType="transition" source="2" target="0">
		<!-- Nosync down_d_h -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d_dn_d_h</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x_d_h</attribute>
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
 automaton env
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net27</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wela</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net13a</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_net45</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_int</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_en_latchwen</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_wen_h</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x_d_h</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d_up_q_0</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net27</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_inta</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wela</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net13a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_net45</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_int</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_en_latchwen</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_wen_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_up_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d_dn_d_h</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tsetupwen</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">init_env</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tsetupd</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">env1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">tHI</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">env2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tsetupwen</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">env3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">env4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tHI</attribute>
							</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">env5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tHI</attribute>
							</attribute>
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tLO</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">env6</attribute>
	</node>
 
	<arc id="132" arcType="transition" source="0" target="1">
		<!-- Nosync up_d_0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tsetupd</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="133" arcType="transition" source="1" target="2">
		<!-- Nosync down_ck -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tHI</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="134" arcType="transition" source="2" target="3">
		<!-- Nosync down_wen -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tsetupwen</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="135" arcType="transition" source="3" target="4">
		<!-- Nosync up_ck -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tHI</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="136" arcType="transition" source="4" target="5">
		<!-- Nosync down_ck -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tHI</attribute>
							</attribute>
							<attribute name="name">tLO</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="137" arcType="transition" source="5" target="6">
		<!-- Nosync up_ck -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tHI</attribute>
							</attribute>
							<attribute name="*">
								<attribute name="const">2</attribute>
								<attribute name="name">tLO</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

</model>