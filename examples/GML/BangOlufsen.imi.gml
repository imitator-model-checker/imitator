<!-- ************************************************************
 * Program examples/BangOlufsen/BangOlufsen.imi
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
 automaton bus
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">c_analyze</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">active</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_1</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="1">
		<attribute name="label">nosync_2</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zero</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">one</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="3" arcType="transition" source="1" target="1">
		<attribute name="label">nosync_3</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">zero</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">one</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="4" arcType="transition" source="1" target="1">
		<attribute name="label">nosync_4</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_Pn</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_Pn</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">one</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">zero</attribute>
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
 automaton Frame_Generator_A
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">start</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">first</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">msg</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">last</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">c_set_stop</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_continue</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">c_set_msg</attribute>
	</node>
 
	<arc id="5" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_5</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_frame</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_no</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="const">10</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_eof</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_stop</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_T4</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_6</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_new_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="7" arcType="transition" source="2" target="6">
		<attribute name="label">nosync_7</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">A_reset</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">A_new_Pn</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_T4</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_no</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">A_no</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_start</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_8</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_reset</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_new_Pn</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">A_msg</attribute>
						<attribute name="const">-1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="9" arcType="transition" source="2" target="0">
		<attribute name="label">nosync_9</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_reset</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_reset</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="10" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_10</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">A_reset</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">A_new_Pn</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_T4</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="3" target="0">
		<attribute name="label">nosync_11</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_new_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_eof</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="12" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_12</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_T4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="13" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_13</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_T4</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">A_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">7</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="14" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_14</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_T4</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="const">7</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_msg</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_stop</attribute>
				<attribute name="expr">
					<attribute name="const">4</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="15" arcType="transition" source="5" target="2">
		<attribute name="label">nosync_15</attribute>
	</arc>

	<arc id="16" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_16</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="const">2</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="17" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_17</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="const">4</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="18" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_18</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="const">6</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="19" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_19</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_no</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_no</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_msg</attribute>
				<attribute name="expr">
					<attribute name="const">8</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_T4</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Detector_A
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">wait_call</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">c_ex_Pf</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">c_ex_S1</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">c_ex_Pn</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">c_ex_S2</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_ex1_S1</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">c_ex1_S2</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">c_calc_res</attribute>
	</node>
 
	<arc id="20" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_20</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_check</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_check</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_res</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="21" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_21</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pf</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="22" arcType="transition" source="1" target="5">
		<attribute name="label">nosync_22</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pf</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="23" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_23</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="24" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_24</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_S1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="25" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_25</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_S1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_err</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">A_err</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="26" arcType="transition" source="3" target="4">
		<attribute name="label">nosync_26</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="27" arcType="transition" source="3" target="5">
		<attribute name="label">nosync_27</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="28" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_28</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_S2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="29" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_29</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_S2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_err</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">A_err</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="30" arcType="transition" source="4" target="2">
		<attribute name="label">nosync_30</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="31" arcType="transition" source="5" target="6">
		<attribute name="label">nosync_31</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="32" arcType="transition" source="5" target="7">
		<attribute name="label">nosync_32</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="33" arcType="transition" source="6" target="0">
		<attribute name="label">nosync_33</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="34" arcType="transition" source="6" target="7">
		<attribute name="label">nosync_34</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="35" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_35</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="36" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_36</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_res</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="37" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_37</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">A_err</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_res</attribute>
				<attribute name="expr">
					<attribute name="const">2</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Sender_A
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">start</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">c_ex_start</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">other_started</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">4</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">ex_silence1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_goto_idle</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">ex_silence2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">transmit</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">c_check_eof</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">loc_stop</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t50000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">newPn</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t40</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">sample</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">c_call_observe</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">c_call_check</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">ex_jam</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">c_nPf</attribute>
	</node>
 
	<node id="16" nodeType="state">
		<attribute name="name">jam</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t25000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="17" nodeType="state">
		<attribute name="name">until_silence</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="18" nodeType="state">
		<attribute name="name">hold</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">36</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="38" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_38</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="39" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_39</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="40" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_40</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_start</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="41" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_41</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_start</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="42" arcType="transition" source="3" target="3">
		<attribute name="label">nosync_42</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_start</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">4</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="43" arcType="transition" source="3" target="4">
		<attribute name="label">nosync_43</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_start</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">4</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="44" arcType="transition" source="4" target="6">
		<attribute name="label">nosync_44</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">3</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="45" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_45</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">3</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="46" arcType="transition" source="5" target="1">
		<attribute name="label">nosync_46</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="47" arcType="transition" source="6" target="5">
		<attribute name="label">nosync_47</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="48" arcType="transition" source="6" target="7">
		<attribute name="label">nosync_48</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="49" arcType="transition" source="7" target="8">
		<attribute name="label">nosync_49</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_err</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_diff</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_frame</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="50" arcType="transition" source="8" target="9">
		<attribute name="label">nosync_50</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_eof</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="51" arcType="transition" source="8" target="10">
		<attribute name="label">nosync_51</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_S1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="52" arcType="transition" source="8" target="10">
		<attribute name="label">nosync_52</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_S1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="53" arcType="transition" source="9" target="1">
		<attribute name="label">nosync_53</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t50000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="54" arcType="transition" source="10" target="11">
		<attribute name="label">nosync_54</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t40</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_new_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="55" arcType="transition" source="10" target="11">
		<attribute name="label">nosync_55</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t40</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">A_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="56" arcType="transition" source="11" target="12">
		<attribute name="label">nosync_56</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_S2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="57" arcType="transition" source="11" target="12">
		<attribute name="label">nosync_57</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_S2</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="58" arcType="transition" source="12" target="13">
		<attribute name="label">nosync_58</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_observe</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="59" arcType="transition" source="13" target="14">
		<attribute name="label">nosync_59</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_check</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_stop</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="60" arcType="transition" source="13" target="14">
		<attribute name="label">nosync_60</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_stop</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="61" arcType="transition" source="14" target="15">
		<attribute name="label">nosync_61</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="62" arcType="transition" source="14" target="16">
		<attribute name="label">nosync_62</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_reset</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="63" arcType="transition" source="14" target="17">
		<attribute name="label">nosync_63</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">2</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_reset</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="64" arcType="transition" source="15" target="8">
		<attribute name="label">nosync_64</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="65" arcType="transition" source="15" target="8">
		<attribute name="label">nosync_65</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="66" arcType="transition" source="16" target="7">
		<attribute name="label">nosync_66</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t25000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_res</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="67" arcType="transition" source="17" target="17">
		<attribute name="label">nosync_67</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="68" arcType="transition" source="17" target="18">
		<attribute name="label">nosync_68</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">A_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="69" arcType="transition" source="18" target="5">
		<attribute name="label">nosync_69</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">36</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">A_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_res</attribute>
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
 automaton Frame_Generator_B
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">start</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">first</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">msg</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">last</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">c_set_stop</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_continue</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">c_set_msg</attribute>
	</node>
 
	<arc id="70" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_70</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_frame</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_no</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="const">10</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_eof</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_stop</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_T4</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="71" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_71</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_new_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="72" arcType="transition" source="2" target="6">
		<attribute name="label">nosync_72</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">B_reset</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">B_new_Pn</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_T4</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_no</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">B_no</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_start</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="73" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_73</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_reset</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_new_Pn</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">B_msg</attribute>
						<attribute name="const">-1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="74" arcType="transition" source="2" target="0">
		<attribute name="label">nosync_74</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_reset</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_reset</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="75" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_75</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">B_reset</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">B_new_Pn</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_T4</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="76" arcType="transition" source="3" target="0">
		<attribute name="label">nosync_76</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_new_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_eof</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="77" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_77</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_T4</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="78" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_78</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_T4</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">B_msg</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">7</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="79" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_79</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_T4</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="const">7</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_msg</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_stop</attribute>
				<attribute name="expr">
					<attribute name="const">4</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="80" arcType="transition" source="5" target="2">
		<attribute name="label">nosync_80</attribute>
	</arc>

	<arc id="81" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_81</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="const">2</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="82" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_82</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="const">4</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="83" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_83</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="const">2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_no</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="const">6</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="84" arcType="transition" source="6" target="2">
		<attribute name="label">nosync_84</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_no</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_no</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_msg</attribute>
				<attribute name="expr">
					<attribute name="const">8</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_T4</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Detector_B
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">wait_call</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">c_ex_Pf</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">c_ex_S1</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">c_ex_Pn</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">c_ex_S2</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_ex1_S1</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">c_ex1_S2</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">c_calc_res</attribute>
	</node>
 
	<arc id="85" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_85</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_check</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_check</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_res</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="86" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_86</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pf</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="87" arcType="transition" source="1" target="5">
		<attribute name="label">nosync_87</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pf</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="88" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_88</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="89" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_89</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_S1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="90" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_90</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_S1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_err</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">B_err</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="91" arcType="transition" source="3" target="4">
		<attribute name="label">nosync_91</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="92" arcType="transition" source="3" target="5">
		<attribute name="label">nosync_92</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="93" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_93</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_S2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="94" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_94</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_S2</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_err</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">B_err</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="95" arcType="transition" source="4" target="2">
		<attribute name="label">nosync_95</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="96" arcType="transition" source="5" target="6">
		<attribute name="label">nosync_96</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="97" arcType="transition" source="5" target="7">
		<attribute name="label">nosync_97</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="98" arcType="transition" source="6" target="0">
		<attribute name="label">nosync_98</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="99" arcType="transition" source="6" target="7">
		<attribute name="label">nosync_99</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_S2</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="100" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_100</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="101" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_101</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="const">3</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_res</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="102" arcType="transition" source="7" target="0">
		<attribute name="label">nosync_102</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">B_err</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">3</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_res</attribute>
				<attribute name="expr">
					<attribute name="const">2</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Sender_B
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">start</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">idle</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">c_ex_start</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">other_started</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">4</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">ex_silence1</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">3</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">c_goto_idle</attribute>
	</node>
 
	<node id="6" nodeType="state">
		<attribute name="name">ex_silence2</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="7" nodeType="state">
		<attribute name="name">transmit</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="8" nodeType="state">
		<attribute name="name">c_check_eof</attribute>
	</node>
 
	<node id="9" nodeType="state">
		<attribute name="name">loc_stop</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t50000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="10" nodeType="state">
		<attribute name="name">newPn</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t40</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="11" nodeType="state">
		<attribute name="name">sample</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="12" nodeType="state">
		<attribute name="name">c_call_observe</attribute>
	</node>
 
	<node id="13" nodeType="state">
		<attribute name="name">c_call_check</attribute>
	</node>
 
	<node id="14" nodeType="state">
		<attribute name="name">ex_jam</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="15" nodeType="state">
		<attribute name="name">c_nPf</attribute>
	</node>
 
	<node id="16" nodeType="state">
		<attribute name="name">jam</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t25000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="17" nodeType="state">
		<attribute name="name">until_silence</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="18" nodeType="state">
		<attribute name="name">hold</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">36</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="103" arcType="transition" source="0" target="1">
		<attribute name="label">nosync_103</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="104" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_104</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="105" arcType="transition" source="2" target="3">
		<attribute name="label">nosync_105</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_start</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="106" arcType="transition" source="2" target="4">
		<attribute name="label">nosync_106</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">A_start</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="107" arcType="transition" source="3" target="3">
		<attribute name="label">nosync_107</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_start</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">4</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="108" arcType="transition" source="3" target="4">
		<attribute name="label">nosync_108</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_start</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">4</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="109" arcType="transition" source="4" target="6">
		<attribute name="label">nosync_109</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">3</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="110" arcType="transition" source="4" target="5">
		<attribute name="label">nosync_110</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="*">
								<attribute name="const">3</attribute>
								<attribute name="name">t781</attribute>
							</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="111" arcType="transition" source="5" target="1">
		<attribute name="label">nosync_111</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="112" arcType="transition" source="6" target="5">
		<attribute name="label">nosync_112</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="113" arcType="transition" source="6" target="7">
		<attribute name="label">nosync_113</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="114" arcType="transition" source="7" target="8">
		<attribute name="label">nosync_114</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t781</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_err</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_diff</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_frame</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="115" arcType="transition" source="8" target="9">
		<attribute name="label">nosync_115</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_eof</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="116" arcType="transition" source="8" target="10">
		<attribute name="label">nosync_116</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_S1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="117" arcType="transition" source="8" target="10">
		<attribute name="label">nosync_117</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_S1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="118" arcType="transition" source="9" target="1">
		<attribute name="label">nosync_118</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t50000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="119" arcType="transition" source="10" target="11">
		<attribute name="label">nosync_119</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t40</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_new_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="120" arcType="transition" source="10" target="11">
		<attribute name="label">nosync_120</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t40</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">B_err</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="121" arcType="transition" source="11" target="12">
		<attribute name="label">nosync_121</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_S2</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="122" arcType="transition" source="11" target="12">
		<attribute name="label">nosync_122</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_S2</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="123" arcType="transition" source="12" target="13">
		<attribute name="label">nosync_123</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_observe</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="124" arcType="transition" source="13" target="14">
		<attribute name="label">nosync_124</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_check</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_stop</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="125" arcType="transition" source="13" target="14">
		<attribute name="label">nosync_125</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_stop</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="126" arcType="transition" source="14" target="15">
		<attribute name="label">nosync_126</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="127" arcType="transition" source="14" target="16">
		<attribute name="label">nosync_127</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_reset</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="128" arcType="transition" source="14" target="17">
		<attribute name="label">nosync_128</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">2</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_reset</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="129" arcType="transition" source="15" target="8">
		<attribute name="label">nosync_129</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="130" arcType="transition" source="15" target="8">
		<attribute name="label">nosync_130</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">B_Pn</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_Pf</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="131" arcType="transition" source="16" target="7">
		<attribute name="label">nosync_131</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">t25000</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_Pn</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_start</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_res</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="132" arcType="transition" source="17" target="17">
		<attribute name="label">nosync_132</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">zero</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="133" arcType="transition" source="17" target="18">
		<attribute name="label">nosync_133</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">one</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">t781</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">B_c</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_c</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="134" arcType="transition" source="18" target="5">
		<attribute name="label">nosync_134</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="*">
							<attribute name="const">36</attribute>
							<attribute name="name">t781</attribute>
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">B_c</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_res</attribute>
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
 automaton Observer_A
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">compare</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="135" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_135</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_S1</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">A_Pf</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_Pn</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">A_S2</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_observe</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="136" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_136</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_S2</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_observe</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_Pn</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_diff</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="137" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_137</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_Pf</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">A_S1</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_observe</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">A_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">A_diff</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Observer_B
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">compare</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<arc id="138" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_138</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_S1</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">B_Pf</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_Pn</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">B_S2</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_observe</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="139" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_139</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_S2</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_observe</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_Pn</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_diff</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="140" arcType="transition" source="0" target="0">
		<attribute name="label">nosync_140</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_Pf</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">B_S1</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_observe</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">B_observe</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">B_diff</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton SUPER_OBSERVER
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">A_c</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">B_c</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">A_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_no</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_msg</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_eof</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_start</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_stop</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_T4</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_frame</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_new_Pn</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_reset</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_check</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_err</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_res</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_diff</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_observe</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_S2</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">A_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">B_Pf</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">one</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">zero</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">t781</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t40</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t50000</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">t25000</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">No_Error</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">Error</attribute>
	</node>
 
	<arc id="141" arcType="transition" source="0" target="1">
		<!-- Nosync ERREUR_DECOUVERTE -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="142" arcType="transition" source="0" target="1">
		<!-- Nosync ERREUR_DECOUVERTE -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_diff</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="143" arcType="transition" source="0" target="1">
		<!-- Nosync ERREUR_DECOUVERTE -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">B_res</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="144" arcType="transition" source="0" target="1">
		<!-- Nosync ERREUR_DECOUVERTE -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_diff</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">A_eof</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

</model>