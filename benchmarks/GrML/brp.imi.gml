<!-- ************************************************************
 * Program examples/BRP/brp.imi
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
 automaton sender
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">idleS</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">next_frame</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">wait_ack</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">T1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">success</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">error</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">EndS</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="1" arcType="transition" source="0" target="1">
		<!-- Nosync Sin -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">b1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="2" arcType="transition" source="1" target="2">
		<!-- Nosync F -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">N</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">i</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">bN</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rc</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="3" arcType="transition" source="1" target="2">
		<!-- Nosync F -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">N</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">i</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">bN</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rc</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="4" arcType="transition" source="2" target="4">
		<!-- Nosync Sout_DK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">MAX</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">rc</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">N</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">i</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">T1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="5" arcType="transition" source="2" target="4">
		<!-- Nosync Sout_NOK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">MAX</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">rc</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">T1</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="name">x</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">N</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">i</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="6" arcType="transition" source="2" target="3">
		<!-- Nosync B -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">T1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="7" arcType="transition" source="2" target="3">
		<!-- Nosync B -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">ab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">T1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">ab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="8" arcType="transition" source="2" target="2">
		<!-- Nosync F -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">T1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">MAX</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">rc</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rc</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">rc</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="9" arcType="transition" source="3" target="1">
		<attribute name="label">nosync_1</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">N</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">i</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">i</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">i</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">b1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="10" arcType="transition" source="3" target="5">
		<!-- Nosync Sout_OK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">N</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">i</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">x</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="11" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_2</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">x</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ab</attribute>
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
 automaton receiver
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">new_file</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">z</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">fst_safe</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">w</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">frame_received</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">w</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="3" nodeType="state">
		<attribute name="name">frame_reported</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">w</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="4" nodeType="state">
		<attribute name="name">idleR</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">TR</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">z</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="5" nodeType="state">
		<attribute name="name">FailureR</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">z</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="12" arcType="transition" source="0" target="1">
		<!-- Nosync G -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">z</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">w</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="13" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_3</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
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
				<attribute name="name">exp_ab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="14" arcType="transition" source="1" target="2">
		<attribute name="label">nosync_4</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
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
				<attribute name="name">exp_ab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="15" arcType="transition" source="1" target="5">
		<attribute name="label">nosync_5</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">rb1</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="16" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_FST -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">exp_ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">rab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rbN</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="17" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_FST -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">exp_ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">rab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rbN</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="18" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_INC -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">exp_ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">rab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rbN</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="19" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_INC -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">exp_ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">rab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rbN</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rb1</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="20" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_OK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">exp_ab</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rab</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">1</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rbN</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="21" arcType="transition" source="2" target="3">
		<!-- Nosync Rout_OK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">exp_ab</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
						<attribute name="equal">
							<attribute name="expr">
								<attribute name="name">rab</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rbN</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="22" arcType="transition" source="2" target="4">
		<!-- Nosync A -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">exp_ab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="23" arcType="transition" source="2" target="4">
		<!-- Nosync A -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">exp_ab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rab</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="24" arcType="transition" source="3" target="4">
		<!-- Nosync A -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">exp_ab</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">z</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">exp_ab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="25" arcType="transition" source="3" target="4">
		<!-- Nosync A -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="equal">
					<attribute name="expr">
						<attribute name="name">exp_ab</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">z</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">exp_ab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="26" arcType="transition" source="4" target="2">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="greater">
					<attribute name="expr">
						<attribute name="name">TR</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">z</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">w</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="27" arcType="transition" source="4" target="0">
		<!-- Nosync Rout_NOK -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rbN</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">TR</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">z</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="28" arcType="transition" source="4" target="0">
		<attribute name="label">nosync_6</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">rbN</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">1</attribute>
						</attribute>
					</attribute>
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">TR</attribute>
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

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton channelK
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">startK</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">in_transitK</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">TD</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">u</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">BadK</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">u</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="29" arcType="transition" source="0" target="1">
		<!-- Nosync F -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">u</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="30" arcType="transition" source="1" target="0">
		<attribute name="label">nosync_7</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="31" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="32" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="33" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="34" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="35" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="36" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">1</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="37" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">1</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">1</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="38" arcType="transition" source="1" target="0">
		<!-- Nosync G -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="and">
						<attribute name="and">
							<attribute name="and">
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">bN</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
								<attribute name="equal">
									<attribute name="expr">
										<attribute name="name">b1</attribute>
									</attribute>
									<attribute name="expr">
										<attribute name="const">0</attribute>
									</attribute>
								</attribute>
							</attribute>
							<attribute name="equal">
								<attribute name="expr">
									<attribute name="name">ab</attribute>
								</attribute>
								<attribute name="expr">
									<attribute name="const">0</attribute>
								</attribute>
							</attribute>
						</attribute>
						<attribute name="greater">
							<attribute name="expr">
								<attribute name="name">u</attribute>
							</attribute>
							<attribute name="expr">
								<attribute name="const">0</attribute>
							</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">u</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">rb1</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rbN</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">rab</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="39" arcType="transition" source="1" target="2">
		<!-- Nosync F -->
	</arc>

</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton channelL
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">startL</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">v</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">in_transitL</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">TD</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">v</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="2" nodeType="state">
		<attribute name="name">BadL</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">v</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="40" arcType="transition" source="0" target="1">
		<!-- Nosync A -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">v</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="41" arcType="transition" source="1" target="0">
		<!-- Nosync B -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">v</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">v</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="42" arcType="transition" source="1" target="0">
		<attribute name="label">nosync_8</attribute>
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="greater">
						<attribute name="expr">
							<attribute name="name">v</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="const">0</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">TD</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">v</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="43" arcType="transition" source="1" target="2">
		<!-- Nosync A -->
	</arc>

</model>

<?xml version="1.0" encoding="UTF-8"?>
<model formalismUrl="http://alligator.lip6.fr/parametric-timed-automaton.fml"
    xmlns="http://gml.lip6.fr/model">
<!-- ************************************************************
 automaton Sclient
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">startSC</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">SYNC</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">ys</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">transSC</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">ys</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="44" arcType="transition" source="0" target="1">
		<!-- Nosync Sin -->
		<attribute name="guard">
			<attribute name="boolExpr">
				<attribute name="and">
					<attribute name="equal">
						<attribute name="expr">
							<attribute name="name">SYNC</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">ys</attribute>
						</attribute>
					</attribute>
					<attribute name="greaterEqual">
						<attribute name="expr">
							<attribute name="name">Maxr</attribute>
						</attribute>
						<attribute name="expr">
							<attribute name="name">retry</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ys</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
			<attribute name="update">
				<attribute name="name">retry</attribute>
				<attribute name="expr">
					<attribute name="+">
						<attribute name="name">retry</attribute>
						<attribute name="const">1</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>

	<arc id="45" arcType="transition" source="1" target="0">
		<!-- Nosync Sout_OK -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ys</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="46" arcType="transition" source="1" target="0">
		<!-- Nosync Sout_DK -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ys</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="47" arcType="transition" source="1" target="0">
		<!-- Nosync Sout_NOK -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">ys</attribute>
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
 automaton Rclient
 ************************************************************ -->

	 <attribute name="variables">
		 <attribute name="clocks">
			<attribute name="clock">
				<attribute name="name">x</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">ys</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">yr</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">z</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">w</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">u</attribute>
			</attribute>
			<attribute name="clock">
				<attribute name="name">v</attribute>
			</attribute>
		</attribute>
		 <attribute name="discretes">
			<attribute name="discrete">
				<attribute name="name">i</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">ab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rc</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">b1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">bN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rb1</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rbN</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">retry</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">Maxr</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">rab</attribute>
			</attribute>
			<attribute name="discrete">
				<attribute name="name">exp_ab</attribute>
			</attribute>
		</attribute>
	</attribute>

	 <attribute name="constants">
		 <attribute name="parameters">
			<attribute name="parameter">
				<attribute name="name">MAX</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">N</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">SYNC</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">T1</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TR</attribute>
			</attribute>
			<attribute name="parameter">
				<attribute name="name">TD</attribute>
			</attribute>
		</attribute>
	</attribute>
 
	<node id="0" nodeType="state">
		<attribute name="name">startRC</attribute>
		<attribute name="type">
			<attribute name="initialState"/>
		</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">yr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<node id="1" nodeType="state">
		<attribute name="name">transRC</attribute>
		<attribute name="invariant">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">yr</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="const">0</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
	</node>
 
	<arc id="48" arcType="transition" source="0" target="1">
		<!-- Nosync Rout_FST -->
		<attribute name="updates">
			<attribute name="update">
				<attribute name="name">yr</attribute>
				<attribute name="expr">
					<attribute name="const">0</attribute>
				</attribute>
			</attribute>
		</attribute>
	</arc>
	<arc id="49" arcType="transition" source="0" target="0">
		<!-- Nosync Rout_OK -->
	</arc>

	<arc id="50" arcType="transition" source="1" target="0">
		<!-- Nosync Rout_OK -->
	</arc>
	<arc id="51" arcType="transition" source="1" target="1">
		<!-- Nosync Rout_INC -->
	</arc>
	<arc id="52" arcType="transition" source="1" target="0">
		<!-- Nosync Rout_NOK -->
	</arc>
</model>