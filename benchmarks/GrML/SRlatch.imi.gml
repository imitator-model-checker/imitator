<!-- ************************************************************
 * Program examples/SRlatch/SRlatch.imi
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
 automaton norGate1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckNor1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNor2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dNor1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t_down</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Nor1_000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Nor1_001</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Nor1_010</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Nor1_011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">Nor1_100</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">Nor1_101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">Nor1_110</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Nor1_111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="2">
		<!-- Nosync overQ_Up -->
	</arc>
	<arc id="2" arcType="transition" source="0" target="1">
		<!-- Nosync Q_Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="1" target="3">
		<!-- Nosync overQ_Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="2" target="0">
		<!-- Nosync overQ_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="5" arcType="transition" source="3" target="1">
		<!-- Nosync overQ_Down -->
	</arc>
	<arc id="6" arcType="transition" source="3" target="2">
		<!-- Nosync Q_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="4" target="0">
		<!-- Nosync R_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="4" target="6">
		<!-- Nosync overQ_Up -->
	</arc>

	<arc id="9" arcType="transition" source="5" target="1">
		<!-- Nosync R_Down -->
	</arc>
	<arc id="10" arcType="transition" source="5" target="7">
		<!-- Nosync overQ_Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="11" arcType="transition" source="5" target="4">
		<!-- Nosync Q_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="6" target="2">
		<!-- Nosync R_Down -->
	</arc>
	<arc id="13" arcType="transition" source="6" target="4">
		<!-- Nosync overQ_Down -->
	</arc>

	<arc id="14" arcType="transition" source="7" target="3">
		<!-- Nosync R_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="15" arcType="transition" source="7" target="5">
		<!-- Nosync overQ_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="16" arcType="transition" source="7" target="6">
		<!-- Nosync Q_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor1_l</attribute>
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
 automaton norGate2
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckNor1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNor2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dNor1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t_down</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Nor2_000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Nor2_001</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Nor2_010</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Nor2_011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">Nor2_100</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">Nor2_101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">Nor2_110</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Nor2_111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNor2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="17" arcType="transition" source="0" target="4">
		<!-- Nosync Q_Up -->
	</arc>
	<arc id="18" arcType="transition" source="0" target="1">
		<!-- Nosync overQ_Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="19" arcType="transition" source="1" target="5">
		<!-- Nosync Q_Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="20" arcType="transition" source="2" target="6">
		<!-- Nosync Q_Up -->
	</arc>
	<arc id="21" arcType="transition" source="2" target="0">
		<!-- Nosync S_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="22" arcType="transition" source="3" target="7">
		<!-- Nosync Q_Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="3" target="1">
		<!-- Nosync S_Down -->
	</arc>
	<arc id="24" arcType="transition" source="3" target="2">
		<!-- Nosync overQ_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="25" arcType="transition" source="4" target="0">
		<!-- Nosync Q_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="26" arcType="transition" source="5" target="1">
		<!-- Nosync Q_Down -->
	</arc>
	<arc id="27" arcType="transition" source="5" target="4">
		<!-- Nosync overQ_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="28" arcType="transition" source="6" target="2">
		<!-- Nosync Q_Down -->
	</arc>
	<arc id="29" arcType="transition" source="6" target="4">
		<!-- Nosync S_Down -->
	</arc>

	<arc id="30" arcType="transition" source="7" target="3">
		<!-- Nosync Q_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="7" target="5">
		<!-- Nosync S_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNor2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="32" arcType="transition" source="7" target="6">
		<!-- Nosync overQ_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNor2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNor2_l</attribute>
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
				<attribute name="name">ckNor1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNor2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dNor1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNor2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t_down</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">env_11</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">env_10</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t_down</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">env_final</attribute>
	</node>
 
	<arc id="33" arcType="transition" source="0" target="1">
		<!-- Nosync S_Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">s</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="34" arcType="transition" source="1" target="2">
		<!-- Nosync R_Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t_down</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

</model>