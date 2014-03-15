<!-- ************************************************************
 * Program examples/RCP/RCP.imi
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
 automaton node1
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckN1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckN2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">rc_fast_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_fast_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delay</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">N1root_contention</attribute>
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
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">N1rec_req_fast</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_fast_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">N1rec_req_slow</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_slow_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">N1almost_root</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">N1root</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">N1rec_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">N1rec_idle_fast</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_fast_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">N1rec_idle_slow</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_slow_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">N1snt_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">N1almost_child</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">N1child</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="2">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="2" arcType="transition" source="0" target="1">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="3" arcType="transition" source="0" target="5">
		<!-- Nosync rec_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="1" target="3">
		<!-- Nosync snd_ack12 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_fast_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="5" arcType="transition" source="1" target="6">
		<!-- Nosync rec_idle21 -->
	</arc>

	<arc id="6" arcType="transition" source="2" target="3">
		<!-- Nosync snd_ack12 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_slow_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="7" arcType="transition" source="2" target="7">
		<!-- Nosync rec_idle21 -->
	</arc>

	<arc id="8" arcType="transition" source="3" target="4">
		<!-- Nosync root1 -->
	</arc>


	<arc id="9" arcType="transition" source="5" target="7">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="10" arcType="transition" source="5" target="6">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="11" arcType="transition" source="5" target="0">
		<!-- Nosync rec_req21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="6" target="8">
		<!-- Nosync snd_req12 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_fast_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="13" arcType="transition" source="6" target="1">
		<!-- Nosync rec_req21 -->
	</arc>

	<arc id="14" arcType="transition" source="7" target="8">
		<!-- Nosync snd_req12 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_slow_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="15" arcType="transition" source="7" target="2">
		<!-- Nosync rec_req21 -->
	</arc>

	<arc id="16" arcType="transition" source="8" target="9">
		<!-- Nosync rec_ack21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="17" arcType="transition" source="8" target="0">
		<!-- Nosync rec_req21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="18" arcType="transition" source="9" target="10">
		<!-- Nosync child1 -->
	</arc>

</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton node2
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckN1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckN2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">rc_fast_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_fast_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delay</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">N2root_contention</attribute>
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
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">N2rec_req_fast</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_fast_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">N2rec_req_slow</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_slow_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">N2almost_root</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">N2root</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">N2rec_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">N2rec_idle_fast</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_fast_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">N2rec_idle_slow</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">rc_slow_max</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">N2snt_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">N2almost_child</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">N2child</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="19" arcType="transition" source="0" target="2">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="0" target="1">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="21" arcType="transition" source="0" target="5">
		<!-- Nosync rec_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="22" arcType="transition" source="1" target="3">
		<!-- Nosync snd_ack21 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_fast_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="1" target="6">
		<!-- Nosync rec_idle12 -->
	</arc>

	<arc id="24" arcType="transition" source="2" target="3">
		<!-- Nosync snd_ack21 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_slow_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="25" arcType="transition" source="2" target="7">
		<!-- Nosync rec_idle12 -->
	</arc>

	<arc id="26" arcType="transition" source="3" target="4">
		<!-- Nosync root2 -->
	</arc>


	<arc id="27" arcType="transition" source="5" target="7">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="5" target="6">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="29" arcType="transition" source="5" target="0">
		<!-- Nosync rec_req12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="30" arcType="transition" source="6" target="8">
		<!-- Nosync snd_req21 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_fast_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="6" target="1">
		<!-- Nosync rec_req12 -->
	</arc>

	<arc id="32" arcType="transition" source="7" target="8">
		<!-- Nosync snd_req21 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ckN2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">rc_slow_min</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="33" arcType="transition" source="7" target="2">
		<!-- Nosync rec_req12 -->
	</arc>

	<arc id="34" arcType="transition" source="8" target="9">
		<!-- Nosync rec_ack12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="8" target="0">
		<!-- Nosync rec_req12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckN2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="36" arcType="transition" source="9" target="10">
		<!-- Nosync child2 -->
	</arc>

</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton wire12
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckN1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckN2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">rc_fast_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_fast_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delay</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">W12empty</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">W12rec_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">W12rec_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">W12rec_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">W12rec_ack_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">W12rec_idle_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">W12rec_req_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">W12rec_ack_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">W12rec_idle_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">W12rec_req_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW12x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="37" arcType="transition" source="0" target="1">
		<!-- Nosync snd_ack12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="38" arcType="transition" source="0" target="2">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="39" arcType="transition" source="0" target="3">
		<!-- Nosync snd_req12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="40" arcType="transition" source="1" target="1">
		<!-- Nosync snd_ack12 -->
	</arc>
	<arc id="41" arcType="transition" source="1" target="4">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="42" arcType="transition" source="1" target="7">
		<!-- Nosync snd_req12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="43" arcType="transition" source="1" target="0">
		<!-- Nosync rec_ack12 -->
	</arc>

	<arc id="44" arcType="transition" source="2" target="8">
		<!-- Nosync snd_ack12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="2" target="2">
		<!-- Nosync snd_idle12 -->
	</arc>
	<arc id="46" arcType="transition" source="2" target="5">
		<!-- Nosync snd_req12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="47" arcType="transition" source="2" target="0">
		<!-- Nosync rec_idle12 -->
	</arc>

	<arc id="48" arcType="transition" source="3" target="6">
		<!-- Nosync snd_ack12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="49" arcType="transition" source="3" target="9">
		<!-- Nosync snd_idle12 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW12y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="50" arcType="transition" source="3" target="3">
		<!-- Nosync snd_req12 -->
	</arc>
	<arc id="51" arcType="transition" source="3" target="0">
		<!-- Nosync rec_req12 -->
	</arc>

	<arc id="52" arcType="transition" source="4" target="4">
		<!-- Nosync snd_idle12 -->
	</arc>
	<arc id="53" arcType="transition" source="4" target="2">
		<!-- Nosync rec_ack12 -->
	</arc>

	<arc id="54" arcType="transition" source="5" target="5">
		<!-- Nosync snd_req12 -->
	</arc>
	<arc id="55" arcType="transition" source="5" target="3">
		<!-- Nosync rec_idle12 -->
	</arc>

	<arc id="56" arcType="transition" source="6" target="6">
		<!-- Nosync snd_ack12 -->
	</arc>
	<arc id="57" arcType="transition" source="6" target="1">
		<!-- Nosync rec_req12 -->
	</arc>

	<arc id="58" arcType="transition" source="7" target="7">
		<!-- Nosync snd_req12 -->
	</arc>
	<arc id="59" arcType="transition" source="7" target="3">
		<!-- Nosync rec_ack12 -->
	</arc>

	<arc id="60" arcType="transition" source="8" target="8">
		<!-- Nosync snd_ack12 -->
	</arc>
	<arc id="61" arcType="transition" source="8" target="1">
		<!-- Nosync rec_idle12 -->
	</arc>

	<arc id="62" arcType="transition" source="9" target="9">
		<!-- Nosync snd_idle12 -->
	</arc>
	<arc id="63" arcType="transition" source="9" target="2">
		<!-- Nosync rec_req12 -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton wire21
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckN1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckN2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">rc_fast_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_fast_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delay</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">W21empty</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">W21rec_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">W21rec_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">W21rec_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21y</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">W21rec_ack_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">W21rec_idle_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">W21rec_req_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">W21rec_ack_req</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">W21rec_idle_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">W21rec_req_idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">delay</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ckW21x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="64" arcType="transition" source="0" target="1">
		<!-- Nosync snd_ack21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="65" arcType="transition" source="0" target="2">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="66" arcType="transition" source="0" target="3">
		<!-- Nosync snd_req21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="67" arcType="transition" source="1" target="1">
		<!-- Nosync snd_ack21 -->
	</arc>
	<arc id="68" arcType="transition" source="1" target="4">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="69" arcType="transition" source="1" target="7">
		<!-- Nosync snd_req21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="70" arcType="transition" source="1" target="0">
		<!-- Nosync rec_ack21 -->
	</arc>

	<arc id="71" arcType="transition" source="2" target="8">
		<!-- Nosync snd_ack21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="72" arcType="transition" source="2" target="2">
		<!-- Nosync snd_idle21 -->
	</arc>
	<arc id="73" arcType="transition" source="2" target="5">
		<!-- Nosync snd_req21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="74" arcType="transition" source="2" target="0">
		<!-- Nosync rec_idle21 -->
	</arc>

	<arc id="75" arcType="transition" source="3" target="6">
		<!-- Nosync snd_ack21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="76" arcType="transition" source="3" target="9">
		<!-- Nosync snd_idle21 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ckW21y</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="77" arcType="transition" source="3" target="3">
		<!-- Nosync snd_req21 -->
	</arc>
	<arc id="78" arcType="transition" source="3" target="0">
		<!-- Nosync rec_req21 -->
	</arc>

	<arc id="79" arcType="transition" source="4" target="4">
		<!-- Nosync snd_idle21 -->
	</arc>
	<arc id="80" arcType="transition" source="4" target="2">
		<!-- Nosync rec_ack21 -->
	</arc>

	<arc id="81" arcType="transition" source="5" target="5">
		<!-- Nosync snd_req21 -->
	</arc>
	<arc id="82" arcType="transition" source="5" target="3">
		<!-- Nosync rec_idle21 -->
	</arc>

	<arc id="83" arcType="transition" source="6" target="6">
		<!-- Nosync snd_ack21 -->
	</arc>
	<arc id="84" arcType="transition" source="6" target="1">
		<!-- Nosync rec_req21 -->
	</arc>

	<arc id="85" arcType="transition" source="7" target="7">
		<!-- Nosync snd_req21 -->
	</arc>
	<arc id="86" arcType="transition" source="7" target="3">
		<!-- Nosync rec_ack21 -->
	</arc>

	<arc id="87" arcType="transition" source="8" target="8">
		<!-- Nosync snd_ack21 -->
	</arc>
	<arc id="88" arcType="transition" source="8" target="1">
		<!-- Nosync rec_idle21 -->
	</arc>

	<arc id="89" arcType="transition" source="9" target="9">
		<!-- Nosync snd_idle21 -->
	</arc>
	<arc id="90" arcType="transition" source="9" target="2">
		<!-- Nosync rec_req21 -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton s1o
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">ckN1</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckN2</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW12y</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ckW21y</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">rc_fast_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_fast_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_min</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">rc_slow_max</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">delay</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">S1oStart</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">S1o1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">S1o2</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">S1o3</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">S1o4</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">S1oEnd</attribute>
	</node>
 
	<arc id="91" arcType="transition" source="0" target="1">
		<!-- Nosync root1 -->
	</arc>
	<arc id="92" arcType="transition" source="0" target="2">
		<!-- Nosync child1 -->
	</arc>
	<arc id="93" arcType="transition" source="0" target="3">
		<!-- Nosync child2 -->
	</arc>
	<arc id="94" arcType="transition" source="0" target="4">
		<!-- Nosync root2 -->
	</arc>

	<arc id="95" arcType="transition" source="1" target="5">
		<!-- Nosync child2 -->
	</arc>

	<arc id="96" arcType="transition" source="2" target="5">
		<!-- Nosync root2 -->
	</arc>

	<arc id="97" arcType="transition" source="3" target="5">
		<!-- Nosync root1 -->
	</arc>

	<arc id="98" arcType="transition" source="4" target="5">
		<!-- Nosync child1 -->
	</arc>

</model>