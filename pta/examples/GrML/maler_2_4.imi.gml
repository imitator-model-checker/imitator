<!-- ************************************************************
 * Program examples/Jobshop/maler_2_4.imi
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
 automaton job1
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
				<attribute name="name">s</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">m1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m3</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m4</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d11</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d12</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d13</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d14</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d21</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d22</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d23</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d24</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">I1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">J1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d13</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">K1</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">L1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d11</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">M1</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">N1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d12</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">P1</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Q1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d14</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">End1</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m3</attribute>
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
			<attribute name="update">
				<attribute name="name">m3</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d13</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="2" target="3">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m1</attribute>
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
			<attribute name="update">
				<attribute name="name">m1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="3" target="4">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d11</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="5" arcType="transition" source="4" target="5">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m2</attribute>
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
			<attribute name="update">
				<attribute name="name">m2</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="6" arcType="transition" source="5" target="6">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d12</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="6" target="7">
		<!-- Nosync b1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m4</attribute>
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
			<attribute name="update">
				<attribute name="name">m4</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="8" arcType="transition" source="7" target="8">
		<!-- Nosync FIN1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d14</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m4</attribute>
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
 automaton job2
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
				<attribute name="name">s</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">m1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m3</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">m4</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">d11</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d12</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d13</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d14</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d21</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d22</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d23</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">d24</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">I2</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">J2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d22</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">K2</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">L2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d23</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">M2</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">N2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d21</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">P2</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">Q2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">d24</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">End2</attribute>
	</node>
 
	<arc id="9" arcType="transition" source="0" target="1">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m2</attribute>
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
			<attribute name="update">
				<attribute name="name">m2</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="10" arcType="transition" source="1" target="2">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d22</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="2" target="3">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m3</attribute>
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
			<attribute name="update">
				<attribute name="name">m3</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="3" target="4">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d23</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m3</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="13" arcType="transition" source="4" target="5">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m1</attribute>
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
			<attribute name="update">
				<attribute name="name">m1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="14" arcType="transition" source="5" target="6">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d21</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="6" target="7">
		<!-- Nosync b2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">m4</attribute>
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
			<attribute name="update">
				<attribute name="name">m4</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="16" arcType="transition" source="7" target="8">
		<!-- Nosync FIN2 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">d24</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">m4</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

</model>