<!--
	TEST OF GML INPUT SYNTAX FOR IMITATOR II
	Author: Etienne ANDRE
	Created: 2011/11/23
-->

	<?xml
		version = "1.0" 
		encoding = "UTF-8"
	?>

<model formalismUrl = "http://alligator.lip6.fr/timed-automata.fml"
    xmlns = "http://gml.lip6.fr/model" >
	
	<attribute name="variables">
		 <attribute name="clocks">
            <attribute name="name">x</attribute>
            <attribute name="name">y</attribute>
        </attribute>

	</attribute>

	<attribute name="constants">
		 <attribute name="parameters">
            <attribute name="name">p</attribute>
            <attribute name="name">p2</attribute>
        </attribute>

        <attribute name="globalconstants">
        </attribute>

	</attribute>

	
    <node id="1" nodeType="state">
        <attribute name="name">q1</attribute>
    </node>

	<node id="6" nodeType="state">
        <attribute name="name">q6</attribute>
		<attribute name="invariant" />
    </node>

	<node id="3" nodeType="state">
        <attribute name="name">q3</attribute>
		<attribute name="type" >
        </attribute>
		<attribute name="invariant" ></attribute>
    </node>

	<node id="2" nodeType="state">
        <attribute name="name">q2</attribute>
		<attribute name="type" />
		<attribute name="invariant" />
    </node>

	<node id="4" nodeType="state">
        <attribute name="name">q4</attribute>
		<attribute name="type" >
            <attribute name="initialState"/>
            <attribute name="finalState"/>
        </attribute>
		<attribute name="invariant" >
			<attribute name="boolExpr" >
				<attribute name="equal" >
					<attribute name="expr" >
						<attribute name="const">98798379437/9809830840398408390483984093</attribute>
					</attribute>
					<attribute name="expr" >
						<attribute name="name">x</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
    </node>

   
	<node id="5" nodeType="state">
        <attribute name="name">q5</attribute>
		<attribute name="type" >
            <attribute name="finalState"/>
        </attribute>
		<attribute name="invariant" >
			<attribute name="boolExpr" >
				<attribute name="and" >
					<attribute name="equal" >
						<attribute name="expr" >
							<attribute name="const">1.897897987897897896876565475654677</attribute>
						</attribute>
						<attribute name="expr" >
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
					<attribute name="greater" >
						<attribute name="expr" >
							<attribute name="*" >
								<attribute name="const">.390483984095486553697348778638743</attribute>
								<attribute name="name">p</attribute>
							</attribute>
						</attribute>
						<attribute name="expr" >
							<attribute name="name">x</attribute>
						</attribute>
					</attribute>
				</attribute>
			</attribute>
		</attribute>
    </node>

	<arc id="5" arcType="transition" source="3" target="5">
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<attribute name="label">MyLabel_Name1</attribute>
		<attribute name="updates" />
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<attribute name="updates" />
	</arc>

	<arc id="6" arcType="transition" source="1" target="2">
		<attribute name="label">MyLabel_Name1</attribute>
		<attribute name="guard" />
		<attribute name="updates" />
	</arc>

    <arc id="5" arcType="transition" source="1" target="2">
        <attribute name="label">a</attribute>
        <attribute name="guard">
            <attribute name="boolExpr">
                <attribute name="less">
                    <attribute name="expr">
                        <attribute name="name">y</attribute>
                    </attribute>
                    <attribute name="expr">
                        <attribute name="const">4</attribute>
                    </attribute>
                </attribute>
            </attribute>
        </attribute>
        <attribute name="updates">
        </attribute>
    </arc>
	
    <arc id="8" arcType="transition" source="2" target="2">
        <attribute name="label">c</attribute>
        <attribute name="guard"/>
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
