<!-- ************************************************************
 * Program examples/Examples/exSITH.imi
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
 automaton A
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">x2</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">p1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">p2</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">q0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">2</attribute>
							<attribute name="name">p1</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">q1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">q2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">p2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">q3</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">qBad</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_1</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="2" arcType="transition" source="0" target="4">
		<attribute name="label">nosync_2</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">p2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="1" target="1">
		<attribute name="label">nosync_3</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="4" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_4</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">p1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="5" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_5</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">p2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="6" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_6</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">4</attribute>
							<attribute name="name">p1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="3" target="2">
		<attribute name="label">nosync_7</attribute>
	</arc>

</model>