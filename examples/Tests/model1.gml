<?xml version="1.0" encoding="UTF-8"?>

<model formalismUrl="http://formalisms.cosyverif.org/parametric-timed-automaton.fml" xmlns="http://cosyverif.org/ns/model">
  <attribute name="declaration">
    <attribute name="variables">
      <attribute name="discretes">
        <attribute name="discrete">
          i
        </attribute>
        <attribute name="discrete">
          j
        </attribute>
        <attribute name="discrete">
          k
        </attribute>
      </attribute>
      <attribute name="clocks">
        <attribute name="clock">
          x1
        </attribute>
        <attribute name="clock">
          x2
        </attribute>
      </attribute>
    </attribute>
    <attribute name="constants">
      <attribute name="parameters">
        <attribute name="parameter">
          p1
        </attribute>
        <attribute name="parameter">
          p2
        </attribute>
        <attribute name="parameter">
          p3
        </attribute>
      </attribute>
    </attribute>
  </attribute>
  
  <attribute name="initialConstraint">
    <attribute name="boolExpr">
		<attribute name="lessEqual">
			<attribute name="expr">
				<attribute name="name">
					x1
				</attribute>
			</attribute>
			<attribute name="expr">
				<attribute name="name">
					p2
				</attribute>
			</attribute>
		</attribute>
	</attribute>
  </attribute>
  
  <node id="3" nodeType="state">
    <attribute name="invariant">
      <attribute name="boolExpr">
		<attribute name="boolValue">
			true
		</attribute>
	</attribute>
   </attribute>
   <attribute name="name">
    </attribute>
    <attribute name="type">initial
    </attribute>
  </node>
  <node id="4" nodeType="state">
    <attribute name="invariant">
      <attribute name="boolExpr"><attribute name="boolValue">
        true
      </attribute></attribute>
    </attribute>
    <attribute name="name">
    </attribute>
    <attribute name="type">
    </attribute>
  </node>
  <node id="5" nodeType="state">
    <attribute name="invariant">
      <attribute name="boolExpr"><attribute name="boolValue">
        true
      </attribute></attribute>
    </attribute>
    <attribute name="name">
    </attribute>
    <attribute name="type">
    </attribute>
  </node>
  <arc id="6" arcType="Transition" source="3" target="4">
    <attribute name="updates">
    </attribute>
    <attribute name="guard">
      <attribute name="boolExpr">
		<attribute name="and">
			<attribute name="boolExpr">
				<attribute name="greaterEqual">
					<attribute name="expr">
						<attribute name="name">
							p1
						</attribute>
					</attribute>
					<attribute name="expr">
						<attribute name="name">
							x1
						</attribute>
					</attribute>
				</attribute>
			</attribute>
        <attribute name="boolExpr"><attribute name="equal">
          <attribute name="expr"><attribute name="name">
            x2
          </attribute></attribute>
          <attribute name="expr"><attribute name="name">
            x1
          </attribute></attribute>
        </attribute></attribute>
      </attribute></attribute>
    </attribute>
    <attribute name="label">
      monLabel
    </attribute>
  </arc>
  <arc id="7" arcType="Transition" source="5" target="3">
    <attribute name="updates">
    </attribute>
    <attribute name="guard">
      <attribute name="boolExpr"><attribute name="boolValue">
        true
      </attribute></attribute>
    </attribute>
    <attribute name="label">
    </attribute>
  </arc>
  <arc id="8" arcType="Transition" source="5" target="4">
    <attribute name="updates">
    </attribute>
    <attribute name="guard">
      <attribute name="boolExpr"><attribute name="boolValue">
        true
      </attribute></attribute>
    </attribute>
    <attribute name="label">
    </attribute>
  </arc>
</model>