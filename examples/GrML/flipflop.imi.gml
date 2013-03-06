<!-- ************************************************************
 * Program examples/Flipflop/flipflop.imi
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
 automaton input
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG3</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG4</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">qLevel</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Input0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">tLO</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tSetup</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Input1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">tLO</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Input2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tLO</attribute>
							<attribute name="name">tHold</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Input3</attribute>
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
		<attribute name="name">Input4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync dUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tLO</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tSetup</attribute>
							<attribute name="name">s</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync ckUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tLO</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="2" target="3">
		<!-- Nosync dDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">tLO</attribute>
							<attribute name="name">tHold</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">s</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="3" target="4">
		<!-- Nosync ckDown -->
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
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">s</attribute>
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
 automaton g1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG3</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG4</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">qLevel</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">G10000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">G10001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">G10010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">G10011</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">G10100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">G10101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">G10110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">G10111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">G11000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">G11001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">G11010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">G11011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">G11100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">G11101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">G11110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">G11111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG1_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="5" arcType="transition" source="0" target="8">
		<!-- Nosync dUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="6" arcType="transition" source="0" target="4">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="7" arcType="transition" source="0" target="2">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="0" target="1">
		<!-- Nosync qG1Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="9" arcType="transition" source="1" target="9">
		<!-- Nosync dUp -->
	</arc>
	<arc id="10" arcType="transition" source="1" target="5">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="11" arcType="transition" source="1" target="3">
		<!-- Nosync qG2Up -->
	</arc>

	<arc id="12" arcType="transition" source="2" target="10">
		<!-- Nosync dUp -->
	</arc>
	<arc id="13" arcType="transition" source="2" target="6">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="14" arcType="transition" source="2" target="0">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="15" arcType="transition" source="2" target="3">
		<!-- Nosync qG1Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="16" arcType="transition" source="3" target="11">
		<!-- Nosync dUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="17" arcType="transition" source="3" target="7">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="18" arcType="transition" source="3" target="1">
		<!-- Nosync qG2Down -->
	</arc>

	<arc id="19" arcType="transition" source="4" target="12">
		<!-- Nosync dUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="4" target="0">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="21" arcType="transition" source="4" target="6">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="22" arcType="transition" source="4" target="5">
		<!-- Nosync qG1Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="23" arcType="transition" source="5" target="13">
		<!-- Nosync dUp -->
	</arc>
	<arc id="24" arcType="transition" source="5" target="1">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="25" arcType="transition" source="5" target="7">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="26" arcType="transition" source="6" target="14">
		<!-- Nosync dUp -->
	</arc>
	<arc id="27" arcType="transition" source="6" target="2">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="6" target="4">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="29" arcType="transition" source="7" target="15">
		<!-- Nosync dUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="30" arcType="transition" source="7" target="3">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="31" arcType="transition" source="7" target="5">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="32" arcType="transition" source="7" target="6">
		<!-- Nosync qG1Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="33" arcType="transition" source="8" target="0">
		<!-- Nosync dDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="34" arcType="transition" source="8" target="12">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="8" target="10">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="36" arcType="transition" source="8" target="9">
		<!-- Nosync qG1Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="37" arcType="transition" source="9" target="1">
		<!-- Nosync dDown -->
	</arc>
	<arc id="38" arcType="transition" source="9" target="13">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="39" arcType="transition" source="9" target="11">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="40" arcType="transition" source="10" target="2">
		<!-- Nosync dDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="41" arcType="transition" source="10" target="14">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="42" arcType="transition" source="10" target="8">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="43" arcType="transition" source="11" target="3">
		<!-- Nosync dDown -->
	</arc>
	<arc id="44" arcType="transition" source="11" target="15">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="11" target="9">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="46" arcType="transition" source="11" target="10">
		<!-- Nosync qG1Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="47" arcType="transition" source="12" target="4">
		<!-- Nosync dDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="48" arcType="transition" source="12" target="8">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="49" arcType="transition" source="12" target="14">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="50" arcType="transition" source="12" target="13">
		<!-- Nosync qG1Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="51" arcType="transition" source="13" target="5">
		<!-- Nosync dDown -->
	</arc>
	<arc id="52" arcType="transition" source="13" target="9">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="53" arcType="transition" source="13" target="15">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="54" arcType="transition" source="14" target="6">
		<!-- Nosync dDown -->
	</arc>
	<arc id="55" arcType="transition" source="14" target="10">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="56" arcType="transition" source="14" target="12">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="57" arcType="transition" source="15" target="7">
		<!-- Nosync dDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="58" arcType="transition" source="15" target="11">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="59" arcType="transition" source="15" target="13">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="60" arcType="transition" source="15" target="14">
		<!-- Nosync qG1Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG1_l</attribute>
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
 automaton g2
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG3</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG4</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">qLevel</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">G2001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">G2000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">G2011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">G2010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">G2101</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">G2100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">G2111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG2_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">G2110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="61" arcType="transition" source="0" target="4">
		<!-- Nosync qG1Up -->
	</arc>
	<arc id="62" arcType="transition" source="0" target="2">
		<!-- Nosync ckUp -->
	</arc>

	<arc id="63" arcType="transition" source="1" target="5">
		<!-- Nosync qG1Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="64" arcType="transition" source="1" target="3">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="65" arcType="transition" source="1" target="0">
		<!-- Nosync qG2Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="66" arcType="transition" source="2" target="6">
		<!-- Nosync qG1Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="67" arcType="transition" source="2" target="0">
		<!-- Nosync ckDown -->
	</arc>

	<arc id="68" arcType="transition" source="3" target="7">
		<!-- Nosync qG1Up -->
	</arc>
	<arc id="69" arcType="transition" source="3" target="1">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="70" arcType="transition" source="3" target="2">
		<!-- Nosync qG2Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="71" arcType="transition" source="4" target="0">
		<!-- Nosync qG1Down -->
	</arc>
	<arc id="72" arcType="transition" source="4" target="6">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="73" arcType="transition" source="5" target="1">
		<!-- Nosync qG1Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="74" arcType="transition" source="5" target="7">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="75" arcType="transition" source="5" target="4">
		<!-- Nosync qG2Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="76" arcType="transition" source="6" target="2">
		<!-- Nosync qG1Down -->
	</arc>
	<arc id="77" arcType="transition" source="6" target="4">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="78" arcType="transition" source="6" target="7">
		<!-- Nosync qG2Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG2_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="79" arcType="transition" source="7" target="3">
		<!-- Nosync qG1Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="80" arcType="transition" source="7" target="5">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG2</attribute>
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
 automaton g3
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG3</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG4</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">qLevel</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">G30000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">G30001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">G30010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">G30011</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">G30100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">G30101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">G30110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">G30111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">G31000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">G31001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">G31010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">G31011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">G31100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">G31101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">G31110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">G31111</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG3_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="81" arcType="transition" source="0" target="8">
		<!-- Nosync qUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="82" arcType="transition" source="0" target="4">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="83" arcType="transition" source="0" target="2">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="84" arcType="transition" source="0" target="1">
		<!-- Nosync qG3Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="85" arcType="transition" source="1" target="9">
		<!-- Nosync qUp -->
	</arc>
	<arc id="86" arcType="transition" source="1" target="5">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="87" arcType="transition" source="1" target="3">
		<!-- Nosync qG2Up -->
	</arc>

	<arc id="88" arcType="transition" source="2" target="10">
		<!-- Nosync qUp -->
	</arc>
	<arc id="89" arcType="transition" source="2" target="6">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="90" arcType="transition" source="2" target="0">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="91" arcType="transition" source="2" target="3">
		<!-- Nosync qG3Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="92" arcType="transition" source="3" target="11">
		<!-- Nosync qUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="93" arcType="transition" source="3" target="7">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="94" arcType="transition" source="3" target="1">
		<!-- Nosync qG2Down -->
	</arc>

	<arc id="95" arcType="transition" source="4" target="12">
		<!-- Nosync qUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="96" arcType="transition" source="4" target="0">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="97" arcType="transition" source="4" target="6">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="98" arcType="transition" source="4" target="5">
		<!-- Nosync qG3Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="99" arcType="transition" source="5" target="13">
		<!-- Nosync qUp -->
	</arc>
	<arc id="100" arcType="transition" source="5" target="1">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="101" arcType="transition" source="5" target="7">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="102" arcType="transition" source="6" target="14">
		<!-- Nosync qUp -->
	</arc>
	<arc id="103" arcType="transition" source="6" target="2">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="104" arcType="transition" source="6" target="4">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="105" arcType="transition" source="7" target="15">
		<!-- Nosync qUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="106" arcType="transition" source="7" target="3">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="107" arcType="transition" source="7" target="5">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="108" arcType="transition" source="7" target="6">
		<!-- Nosync qG3Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="109" arcType="transition" source="8" target="0">
		<!-- Nosync qDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="110" arcType="transition" source="8" target="12">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="111" arcType="transition" source="8" target="10">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="112" arcType="transition" source="8" target="9">
		<!-- Nosync qG3Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="113" arcType="transition" source="9" target="1">
		<!-- Nosync qDown -->
	</arc>
	<arc id="114" arcType="transition" source="9" target="13">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="115" arcType="transition" source="9" target="11">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="116" arcType="transition" source="10" target="2">
		<!-- Nosync qDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="117" arcType="transition" source="10" target="14">
		<!-- Nosync ckUp -->
	</arc>
	<arc id="118" arcType="transition" source="10" target="8">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="119" arcType="transition" source="11" target="3">
		<!-- Nosync qDown -->
	</arc>
	<arc id="120" arcType="transition" source="11" target="15">
		<!-- Nosync ckUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="121" arcType="transition" source="11" target="9">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="122" arcType="transition" source="11" target="10">
		<!-- Nosync qG3Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="123" arcType="transition" source="12" target="4">
		<!-- Nosync qDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="124" arcType="transition" source="12" target="8">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="125" arcType="transition" source="12" target="14">
		<!-- Nosync qG2Up -->
	</arc>
	<arc id="126" arcType="transition" source="12" target="13">
		<!-- Nosync qG3Up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="127" arcType="transition" source="13" target="5">
		<!-- Nosync qDown -->
	</arc>
	<arc id="128" arcType="transition" source="13" target="9">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="129" arcType="transition" source="13" target="15">
		<!-- Nosync qG2Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="130" arcType="transition" source="14" target="6">
		<!-- Nosync qDown -->
	</arc>
	<arc id="131" arcType="transition" source="14" target="10">
		<!-- Nosync ckDown -->
	</arc>
	<arc id="132" arcType="transition" source="14" target="12">
		<!-- Nosync qG2Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="133" arcType="transition" source="15" target="7">
		<!-- Nosync qDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="134" arcType="transition" source="15" target="11">
		<!-- Nosync ckDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="135" arcType="transition" source="15" target="13">
		<!-- Nosync qG2Down -->
	</arc>
	<arc id="136" arcType="transition" source="15" target="14">
		<!-- Nosync qG3Down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG3</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG3_l</attribute>
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
 automaton g4
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG3</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckG4</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">qLevel</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">tHI</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tLO</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">tHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG1_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG2_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG3_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dG4_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">G401</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">G411</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG4_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">G410</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">G400</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dG4_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="137" arcType="transition" source="0" target="1">
		<!-- Nosync qG3Up -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG4</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="138" arcType="transition" source="1" target="0">
		<!-- Nosync qG3Down -->
	</arc>
	<arc id="139" arcType="transition" source="1" target="2">
		<!-- Nosync qDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG4_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">qLevel</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="140" arcType="transition" source="2" target="3">
		<!-- Nosync qG3Down -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckG4</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="141" arcType="transition" source="3" target="2">
		<!-- Nosync qG3Up -->
	</arc>
	<arc id="142" arcType="transition" source="3" target="0">
		<!-- Nosync qUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckG4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dG4_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">qLevel</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>