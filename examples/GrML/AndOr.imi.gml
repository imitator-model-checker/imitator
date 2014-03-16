<!-- ************************************************************
 * Program examples/AndOr/AndOr.imi
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
				<attribute name="name">ckOr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">cka</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckb</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dA_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">InputInit</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dB_High_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Input2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dA_High_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">cka</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Input3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dB_Low_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckb</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Input4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dA_Low_u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">cka</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync bDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dB_High_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync aDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">cka</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dA_High_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">cka</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="2" target="3">
		<!-- Nosync bUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckb</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dB_Low_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckb</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="3" target="0">
		<!-- Nosync aUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">cka</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dA_Low_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">cka</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckb</attribute>
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
 automaton andGate
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckOr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">cka</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckb</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dA_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">And000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">And001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dAnd_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">And010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">And011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dAnd_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">And100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">And101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dAnd_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">And110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dAnd_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckAnd</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">And111</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="5" arcType="transition" source="0" target="4">
		<!-- Nosync tUp -->
	</arc>
	<arc id="6" arcType="transition" source="0" target="2">
		<!-- Nosync bUp -->
	</arc>

	<arc id="7" arcType="transition" source="1" target="5">
		<!-- Nosync tUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="1" target="3">
		<!-- Nosync bUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="9" arcType="transition" source="1" target="0">
		<!-- Nosync xDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAnd_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="10" arcType="transition" source="2" target="6">
		<!-- Nosync tUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="11" arcType="transition" source="2" target="0">
		<!-- Nosync bDown -->
	</arc>

	<arc id="12" arcType="transition" source="3" target="7">
		<!-- Nosync tUp -->
	</arc>
	<arc id="13" arcType="transition" source="3" target="1">
		<!-- Nosync bDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="14" arcType="transition" source="3" target="2">
		<!-- Nosync xDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAnd_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="4" target="0">
		<!-- Nosync tDown -->
	</arc>
	<arc id="16" arcType="transition" source="4" target="6">
		<!-- Nosync bUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="17" arcType="transition" source="5" target="1">
		<!-- Nosync tDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="18" arcType="transition" source="5" target="7">
		<!-- Nosync bUp -->
	</arc>
	<arc id="19" arcType="transition" source="5" target="4">
		<!-- Nosync xDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAnd_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="20" arcType="transition" source="6" target="2">
		<!-- Nosync tDown -->
	</arc>
	<arc id="21" arcType="transition" source="6" target="4">
		<!-- Nosync bDown -->
	</arc>
	<arc id="22" arcType="transition" source="6" target="7">
		<!-- Nosync xUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAnd_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="23" arcType="transition" source="7" target="3">
		<!-- Nosync tDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="7" target="5">
		<!-- Nosync bDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
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
 automaton orGate
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckOr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">cka</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckb</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dA_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dA_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_High_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dB_Low_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dOr_u</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_l</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAnd_u</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Or000</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Or001</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dOr_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Or010</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dOr_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Or011</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">Or100</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dOr_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">Or101</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">Or110</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">dOr_u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ckOr</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Or111</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="25" arcType="transition" source="0" target="4">
		<!-- Nosync xUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="26" arcType="transition" source="0" target="2">
		<!-- Nosync aUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="27" arcType="transition" source="1" target="5">
		<!-- Nosync xUp -->
	</arc>
	<arc id="28" arcType="transition" source="1" target="3">
		<!-- Nosync aUp -->
	</arc>
	<arc id="29" arcType="transition" source="1" target="0">
		<!-- Nosync tDown -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dOr_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="30" arcType="transition" source="2" target="6">
		<!-- Nosync xUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="2" target="0">
		<!-- Nosync aDown -->
	</arc>
	<arc id="32" arcType="transition" source="2" target="3">
		<!-- Nosync tUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dOr_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="33" arcType="transition" source="3" target="7">
		<!-- Nosync xUp -->
	</arc>
	<arc id="34" arcType="transition" source="3" target="1">
		<!-- Nosync aDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="35" arcType="transition" source="4" target="0">
		<!-- Nosync xDown -->
	</arc>
	<arc id="36" arcType="transition" source="4" target="6">
		<!-- Nosync aUp -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="37" arcType="transition" source="4" target="5">
		<!-- Nosync tUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dOr_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="38" arcType="transition" source="5" target="1">
		<!-- Nosync xDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="39" arcType="transition" source="5" target="7">
		<!-- Nosync aUp -->
	</arc>

	<arc id="40" arcType="transition" source="6" target="2">
		<!-- Nosync xDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="41" arcType="transition" source="6" target="4">
		<!-- Nosync aDown -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckOr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="42" arcType="transition" source="6" target="7">
		<!-- Nosync tUp -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckOr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dOr_l</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="43" arcType="transition" source="7" target="3">
		<!-- Nosync xDown -->
	</arc>
	<arc id="44" arcType="transition" source="7" target="5">
		<!-- Nosync aDown -->
	</arc>
</model>