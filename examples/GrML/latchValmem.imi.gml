<!-- ************************************************************
 * Program examples/Latch/latchValmem.imi
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
 automaton clock1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">ClockLow1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dClockLow</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckClock</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">ClockHigh1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dClockHigh</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckClock</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync clockUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckClock</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dClockLow</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckClock</attribute>
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
 automaton d1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">DLow1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dClockLow</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">dSetup</attribute>
							<attribute name="name">ckD</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">DHigh1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dSetup</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckD</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">DHigh2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dHold</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckD</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">DLow2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckD</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="2" arcType="transition" source="0" target="1">
		<!-- Nosync dUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">dClockLow</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="+">
							<attribute name="name">dSetup</attribute>
							<attribute name="name">ckD</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckD</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="3" arcType="transition" source="1" target="2">
		<!-- Nosync clockUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">dSetup</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckD</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckD</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="2" target="3">
		<!-- Nosync dDown1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">dHold</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckD</attribute>
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
 automaton not1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Not1High1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Not1High1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNot1Down</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNot1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Not1Low1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="5" arcType="transition" source="0" target="1">
		<!-- Nosync clockUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNot1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<!-- Nosync not1Down1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNot1Down</attribute>
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
 automaton not2
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">Not2Low1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Not2Low1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dNot2Up</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckNot2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">Not2High1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="7" arcType="transition" source="0" target="1">
		<!-- Nosync not1Down1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckNot2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="8" arcType="transition" source="1" target="2">
		<!-- Nosync not2Up1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckNot2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dNot2Up</attribute>
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
 automaton xor1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">XorLow1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">XorLow1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dXorUp2Up</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">XorHigh1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">XorHigh1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dXorDown1Up</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">XorLow2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="9" arcType="transition" source="0" target="1">
		<!-- Nosync clockUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckXor</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="10" arcType="transition" source="1" target="2">
		<!-- Nosync xorUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dXorUp2Up</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="2" target="3">
		<!-- Nosync not2Up1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckXor</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="3" target="4">
		<!-- Nosync xorDown1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckXor</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dXorDown1Up</attribute>
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
 automaton and1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">AndLow1</attribute>
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
 
	<node id="1" nodeType="state">
		<attribute name="name">AndLow1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dAndUp2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">AndHigh1</attribute>
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
		<attribute name="name">AndHigh1bis</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dAndDown1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">AndLow2</attribute>
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
 
	<arc id="13" arcType="transition" source="0" target="1">
		<!-- Nosync clockUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="14" arcType="transition" source="1" target="2">
		<!-- Nosync andUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAndUp2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="2" target="3">
		<!-- Nosync xorDown1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckAnd</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="16" arcType="transition" source="3" target="4">
		<!-- Nosync andDown1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckAnd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dAndDown1</attribute>
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
 automaton latch1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">s</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckClock</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckD</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckNot2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckXor</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckAnd</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckLatch</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">q</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">dClockHigh</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dClockLow</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dSetup</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dHold</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Down</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dNot2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorDown1Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dXorUp2Up</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndUp2</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dAndDown1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">dLatchUp</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">LatchD0E0</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">LatchD0E1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">LatchD1E0</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">LatchD1E1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">dLatchUp</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">LatchD1E1B</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="17" arcType="transition" source="0" target="2">
		<!-- Nosync dUp1 -->
	</arc>
	<arc id="18" arcType="transition" source="0" target="1">
		<!-- Nosync andUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckLatch</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="19" arcType="transition" source="1" target="3">
		<!-- Nosync dUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckLatch</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="1" target="0">
		<!-- Nosync andDown1 -->
	</arc>

	<arc id="21" arcType="transition" source="2" target="3">
		<!-- Nosync andUp1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckLatch</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="22" arcType="transition" source="2" target="0">
		<!-- Nosync dDown1 -->
	</arc>

	<arc id="23" arcType="transition" source="3" target="4">
		<!-- Nosync latchUp1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckLatch</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">dLatchUp</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">q</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="3" target="1">
		<!-- Nosync dDown1 -->
	</arc>
	<arc id="25" arcType="transition" source="3" target="2">
		<!-- Nosync andDown1 -->
	</arc>

	<arc id="26" arcType="transition" source="4" target="2">
		<!-- Nosync andDown1 -->
	</arc>
	<arc id="27" arcType="transition" source="4" target="1">
		<!-- Nosync dDown1 -->
	</arc>
</model>