<!-- ************************************************************
 * Program examples/CSMACD/csmacdPrism.imi
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
 automaton medium
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">lambda</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">sigma</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">timeslot</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Init</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Transmit</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Collide</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync send1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="2" arcType="transition" source="0" target="1">
		<!-- Nosync send2 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="1" target="2">
		<!-- Nosync send1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="4" arcType="transition" source="1" target="2">
		<!-- Nosync send2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="5" arcType="transition" source="1" target="1">
		<!-- Nosync busy1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="6" arcType="transition" source="1" target="1">
		<!-- Nosync busy2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="7" arcType="transition" source="1" target="0">
		<!-- Nosync end1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="1" target="0">
		<!-- Nosync end2 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="9" arcType="transition" source="2" target="0">
		<!-- Nosync cd -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">sigma</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
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
 automaton sender1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">lambda</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">sigma</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">timeslot</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Init1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Transmit1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">lambda</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Collide1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Wait1_0</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">Wait1_1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">Wait1_2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">Wait1_3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Done1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="10" arcType="transition" source="0" target="1">
		<!-- Nosync send1 -->
	</arc>

	<arc id="11" arcType="transition" source="1" target="7">
		<!-- Nosync end1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">lambda</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="12" arcType="transition" source="1" target="2">
		<!-- Nosync cd -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="13" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_1</attribute>
	</arc>
	<arc id="14" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_2</attribute>
	</arc>
	<arc id="15" arcType="transition" source="2" target="5">
		<attribute name="label">nosync_3</attribute>
	</arc>
	<arc id="16" arcType="transition" source="2" target="6">
		<attribute name="label">nosync_4</attribute>
	</arc>

	<arc id="17" arcType="transition" source="3" target="2">
		<!-- Nosync busy1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="18" arcType="transition" source="3" target="1">
		<!-- Nosync send1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="19" arcType="transition" source="4" target="2">
		<!-- Nosync busy1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="4" target="1">
		<!-- Nosync send1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="21" arcType="transition" source="5" target="2">
		<!-- Nosync busy1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="22" arcType="transition" source="5" target="1">
		<!-- Nosync send1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="23" arcType="transition" source="6" target="2">
		<!-- Nosync busy1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="6" target="1">
		<!-- Nosync send1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x1</attribute>
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
 automaton sender2
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">lambda</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">sigma</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">timeslot</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Init2</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Transmit2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">lambda</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Collide2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">Wait2_0</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">Wait2_1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">Wait2_2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">Wait2_3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Done2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="25" arcType="transition" source="0" target="1">
		<!-- Nosync send2 -->
	</arc>

	<arc id="26" arcType="transition" source="1" target="7">
		<!-- Nosync end2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">lambda</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="27" arcType="transition" source="1" target="2">
		<!-- Nosync cd -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="28" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_5</attribute>
	</arc>
	<arc id="29" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_6</attribute>
	</arc>
	<arc id="30" arcType="transition" source="2" target="5">
		<attribute name="label">nosync_7</attribute>
	</arc>
	<arc id="31" arcType="transition" source="2" target="6">
		<attribute name="label">nosync_8</attribute>
	</arc>

	<arc id="32" arcType="transition" source="3" target="2">
		<!-- Nosync busy2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="33" arcType="transition" source="3" target="1">
		<!-- Nosync send2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="34" arcType="transition" source="4" target="2">
		<!-- Nosync busy2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="4" target="1">
		<!-- Nosync send2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">timeslot</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="36" arcType="transition" source="5" target="2">
		<!-- Nosync busy2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="37" arcType="transition" source="5" target="1">
		<!-- Nosync send2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="38" arcType="transition" source="6" target="2">
		<!-- Nosync busy2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="39" arcType="transition" source="6" target="1">
		<!-- Nosync send2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">timeslot</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

</model>