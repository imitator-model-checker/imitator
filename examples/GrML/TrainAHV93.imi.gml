<!-- ************************************************************
 * Program examples/Train/TrainAHV93.imi
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
 automaton train
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">b</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">c</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">f</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">train0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">train1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">train2</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">train3</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync approach -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync inn -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">a</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="2" target="3">
		<!-- Nosync out -->
	</arc>

	<arc id="4" arcType="transition" source="3" target="0">
		<!-- Nosync exit -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">b</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x</attribute>
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
 automaton gate
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">b</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">c</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">f</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">gate0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">gate1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">gate2</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">gate3</attribute>
	</node>
 
	<arc id="5" arcType="transition" source="0" target="1">
		<!-- Nosync lower -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<!-- Nosync down -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">y</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">c</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">y</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="2" target="3">
		<!-- Nosync raise -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="8" arcType="transition" source="3" target="0">
		<!-- Nosync up -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">y</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">c</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">d</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">y</attribute>
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
 automaton controller
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">a</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">b</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">c</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">e</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">f</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">controller0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">controller1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">controller2</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">controller3</attribute>
	</node>
 
	<arc id="9" arcType="transition" source="0" target="1">
		<!-- Nosync approach -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">z</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="10" arcType="transition" source="1" target="2">
		<!-- Nosync lower -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">z</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">e</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">z</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="2" target="3">
		<!-- Nosync exit -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">z</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="3" target="0">
		<!-- Nosync raise -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">z</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">e</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">f</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">z</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>