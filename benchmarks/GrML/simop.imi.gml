<!-- ************************************************************
 * Program examples/SIMOP/simop.imi
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
 automaton PLC
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">PLCclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">COMclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">NETclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">RIOclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ENVclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">tt</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yy</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">zz</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">PLCmtt</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">PLCct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">NETd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">RIOd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SIGmrt</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">PLCinit</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">PLC1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">PLC3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">PLC4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">PLC5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">yy</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">PLC6</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">PLC7</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">PLC9</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync PLCbeg -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">PLCclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="1">
		<!-- Nosync COMin0 -->
	</arc>
	<arc id="3" arcType="transition" source="1" target="2">
		<!-- Nosync PLCout0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCmtt</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="2" target="1">
		<attribute name="label">nosync_1</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">PLCclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="5" arcType="transition" source="2" target="5">
		<!-- Nosync COMin1 -->
	</arc>
	<arc id="6" arcType="transition" source="2" target="2">
		<!-- Nosync COMin0 -->
	</arc>

	<arc id="7" arcType="transition" source="3" target="3">
		<!-- Nosync COMin1 -->
	</arc>
	<arc id="8" arcType="transition" source="3" target="4">
		<attribute name="label">nosync_2</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCmtt</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">yy</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="9" arcType="transition" source="4" target="5">
		<!-- Nosync PLCout0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">yy</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="10" arcType="transition" source="4" target="4">
		<!-- Nosync COMin1 -->
	</arc>

	<arc id="11" arcType="transition" source="5" target="6">
		<attribute name="label">nosync_3</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">PLCclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="12" arcType="transition" source="5" target="5">
		<!-- Nosync COMin1 -->
	</arc>

	<arc id="13" arcType="transition" source="6" target="6">
		<!-- Nosync COMin1 -->
	</arc>
	<arc id="14" arcType="transition" source="6" target="7">
		<!-- Nosync PLCout1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCmtt</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="7" target="6">
		<attribute name="label">nosync_4</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">PLCct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">PLCclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">PLCclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="16" arcType="transition" source="7" target="7">
		<!-- Nosync COMin1 -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton COM
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">PLCclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">COMclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">NETclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">RIOclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ENVclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">tt</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yy</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">zz</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">PLCmtt</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">PLCct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">NETd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">RIOd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SIGmrt</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">COMinit</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">COM1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">COM2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">COM3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">COM4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">COM5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">COM6</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">COM7</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">COM8</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">COM9</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">COM10</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">COM11</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">COM12</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">COM13</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">COM14</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">COM15</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="17" arcType="transition" source="0" target="1">
		<!-- Nosync PLCbeg -->
	</arc>

	<arc id="18" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_5</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">COMclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="19" arcType="transition" source="1" target="1">
		<!-- Nosync PLCout0 -->
	</arc>

	<arc id="20" arcType="transition" source="2" target="3">
		<!-- Nosync CNreq0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="21" arcType="transition" source="2" target="2">
		<!-- Nosync PLCout0 -->
	</arc>

	<arc id="22" arcType="transition" source="3" target="4">
		<!-- Nosync NCrep0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">tt</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="3" target="3">
		<!-- Nosync PLCout0 -->
	</arc>
	<arc id="24" arcType="transition" source="3" target="5">
		<!-- Nosync NCrep1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">tt</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="25" arcType="transition" source="4" target="6">
		<!-- Nosync COMin0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="26" arcType="transition" source="5" target="6">
		<!-- Nosync COMin1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="27" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_6</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">COMclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="6" target="6">
		<!-- Nosync PLCout0 -->
	</arc>
	<arc id="29" arcType="transition" source="6" target="11">
		<!-- Nosync PLCout1 -->
	</arc>

	<arc id="30" arcType="transition" source="7" target="8">
		<!-- Nosync CNreq0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="7" target="7">
		<!-- Nosync PLCout1 -->
	</arc>

	<arc id="32" arcType="transition" source="8" target="9">
		<!-- Nosync NCrep0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">tt</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="33" arcType="transition" source="8" target="8">
		<!-- Nosync PLCout1 -->
	</arc>
	<arc id="34" arcType="transition" source="8" target="10">
		<!-- Nosync NCrep1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">tt</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="35" arcType="transition" source="9" target="11">
		<!-- Nosync COMin0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="36" arcType="transition" source="10" target="11">
		<!-- Nosync COMin1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="37" arcType="transition" source="11" target="12">
		<attribute name="label">nosync_7</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">COMclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="38" arcType="transition" source="11" target="11">
		<!-- Nosync PLCout1 -->
	</arc>

	<arc id="39" arcType="transition" source="12" target="13">
		<!-- Nosync CNreq1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="40" arcType="transition" source="12" target="12">
		<!-- Nosync PLCout1 -->
	</arc>

	<arc id="41" arcType="transition" source="13" target="14">
		<!-- Nosync NCrep1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">tt</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="42" arcType="transition" source="13" target="13">
		<!-- Nosync PLCout1 -->
	</arc>

	<arc id="43" arcType="transition" source="14" target="15">
		<!-- Nosync COMin1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">tt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="44" arcType="transition" source="15" target="12">
		<attribute name="label">nosync_8</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">COMct</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">COMclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">COMclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="15" target="15">
		<!-- Nosync PLCout1 -->
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton NET
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">PLCclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">COMclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">NETclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">RIOclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ENVclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">tt</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yy</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">zz</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">PLCmtt</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">PLCct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">NETd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">RIOd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SIGmrt</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">NET1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">NET2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">NET3</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">NET4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">NET5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">NET6</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">NET7</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="46" arcType="transition" source="0" target="1">
		<!-- Nosync CNreq0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">NETclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="47" arcType="transition" source="0" target="4">
		<!-- Nosync CNreq1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">NETclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="48" arcType="transition" source="1" target="2">
		<!-- Nosync NRreq0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="49" arcType="transition" source="2" target="3">
		<!-- Nosync RNrep0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">NETclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="50" arcType="transition" source="2" target="6">
		<!-- Nosync RNrep1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">NETclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="51" arcType="transition" source="3" target="0">
		<!-- Nosync NCrep0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="52" arcType="transition" source="4" target="5">
		<!-- Nosync NRreq1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="53" arcType="transition" source="5" target="6">
		<!-- Nosync RNrep1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">NETclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="54" arcType="transition" source="6" target="0">
		<!-- Nosync NCrep1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">NETd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">NETclk</attribute>
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
 automaton RIO
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">PLCclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">COMclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">NETclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">RIOclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ENVclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">tt</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yy</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">zz</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">PLCmtt</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">PLCct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">NETd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">RIOd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SIGmrt</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">RIO1</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">RIO2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">RIO3</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">RIO4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">RIO5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">RIO6</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">RIO7</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">RIO8</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">RIO9</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">RIO10</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="55" arcType="transition" source="0" target="1">
		<!-- Nosync NRreq0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">RIOclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="56" arcType="transition" source="1" target="2">
		<!-- Nosync SIGout0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zz</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="57" arcType="transition" source="1" target="3">
		<!-- Nosync SIGin1 -->
	</arc>

	<arc id="58" arcType="transition" source="2" target="0">
		<!-- Nosync RNrep0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="59" arcType="transition" source="3" target="4">
		<!-- Nosync SIGout0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zz</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="60" arcType="transition" source="4" target="5">
		<!-- Nosync RNrep0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="61" arcType="transition" source="5" target="6">
		<!-- Nosync NRreq0 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">RIOclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="62" arcType="transition" source="5" target="8">
		<!-- Nosync NRreq1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">RIOclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="63" arcType="transition" source="6" target="7">
		<!-- Nosync SIGout0 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zz</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="64" arcType="transition" source="7" target="5">
		<!-- Nosync RNrep1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="65" arcType="transition" source="8" target="9">
		<!-- Nosync SIGout1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">RIOd</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">RIOclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zz</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="66" arcType="transition" source="9" target="5">
		<!-- Nosync RNrep1 -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">zz</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
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
 automaton ENV
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">PLCclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">COMclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">NETclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">RIOclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ENVclk</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">tt</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yy</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">zz</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">PLCmtt</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">PLCct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">COMct</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">NETd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">RIOd</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SIGmrt</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">ENVinit</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">ENV1</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">ENV2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">SIGmrt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ENVclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">ENV4</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">yy</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">ENV5</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">yy</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="67" arcType="transition" source="0" target="1">
		<!-- Nosync SIGout0 -->
	</arc>

	<arc id="68" arcType="transition" source="1" target="2">
		<!-- Nosync SIGin1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ENVclk</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="69" arcType="transition" source="1" target="1">
		<!-- Nosync SIGout0 -->
	</arc>

	<arc id="70" arcType="transition" source="2" target="2">
		<!-- Nosync SIGout0 -->
	</arc>
	<arc id="71" arcType="transition" source="2" target="3">
		<!-- Nosync SIGout1 -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">yy</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="72" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_9</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">SIGmrt</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ENVclk</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">yy</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>


</model>