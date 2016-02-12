#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
#
#             Data for non-regression tests
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2015/10/23
# Last modified: 2016/02/10
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests = [
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the state space',
		'input_files': ['flipflop.imi'],
		'options'    : '-mode statespace -output-states',
		'expectations' : [
			{'file': 'flipflop-statespace.states' , 'content' : """second
************************************************************/

  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  input: Input0, g1: G10011, g2: G2101, g3: G30011, g4: G410, observer: locobs_0, qLevel = 0 ==> 
& dG3_u >= 8
& dG4_u >= 3
& s >= 0
& ckG1 >= s
& ckG2 >= s
& ckG3 >= s
& ckG4 >= s
& 5 >= s

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 1:
  input: Input1, g1: G11011, g2: G2101, g3: G30011, g4: G410, observer: locobs_0, qLevel = 0 ==> 
& ckG4 >= s
& dG3_u >= 8
& dG4_u >= 3
& s >= 5
& ckG2 >= s
& ckG3 >= s
& 12 >= s
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 2:
  input: Input1, g1: G11010, g2: G2001, g3: G30011, g4: G410, observer: locobs_0, qLevel = 0 ==> 
& ckG4 >= s
& dG3_u >= 8
& dG4_u >= 3
& s >= 12
& ckG2 >= s
& ckG3 >= s
& 15 >= s
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 3:
  input: Input2, g1: G11110, g2: G2011, g3: G30111, g4: G410, observer: locobs_0, qLevel = 0 ==> 
& 32 >= s
& ckG4 >= s
& dG3_u >= 8
& dG4_u >= 3
& ckG2 >= s
& s >= 15
& dG3_u + 15 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 4:
  input: Input3, g1: G10110, g2: G2011, g3: G30111, g4: G410, observer: locobs_0, qLevel = 0 ==> 
& ckG4 >= s
& dG3_u + 15 >= s
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 5:
  input: Input2, g1: G11110, g2: G2011, g3: G30110, g4: G400, observer: locobs_0, qLevel = 0 ==> 
& dG4_u >= ckG4
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 23 + ckG4
& ckG4 >= 0
& ckG2 >= s
& 32 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 6:
  input: Input4, g1: G10010, g2: G2001, g3: G30011, g4: G410, observer: locobs_2, qLevel = 0 ==> 
& dG3_u >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 39
& ckG1 = 0
& ckG3 = 24
& s = 0

  Projection onto the parameters:
   dG3_u >= 24
& dG4_u >= 3

  /************************************************************/
  STATE 7:
  input: Input3, g1: G10110, g2: G2011, g3: G30110, g4: G400, observer: locobs_0, qLevel = 0 ==> 
& dG4_u >= ckG4
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 32 + ckG4
& ckG4 >= 0
& ckG2 >= s
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 17

  /************************************************************/
  STATE 8:
  input: Input3, g1: G10110, g2: G2011, g3: G30110, g4: G400, observer: locobs_0, qLevel = 0 ==> 
& ckG4 + 32 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= ckG4
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& s >= 23 + ckG4
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG3_u + dG4_u >= 17
& dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 9:
  input: Input2, g1: G11110, g2: G2011, g3: G31110, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 23 + ckG4
& ckG4 >= 3
& ckG2 >= s
& 32 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 10:
  input: Input4, g1: G10010, g2: G2001, g3: G30010, g4: G400, observer: locobs_2, qLevel = 0 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 0
& 7 >= ckG4
& ckG1 = 0
& ckG3 = 0
& s = 0

  Projection onto the parameters:
   dG3_u + dG4_u >= 24
& dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 11:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 32 + ckG4
& ckG4 >= 3
& ckG2 >= s
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 17

  /************************************************************/
  STATE 12:
  input: Input4, g1: G10010, g2: G2001, g3: G30010, g4: G400, observer: locobs_2, qLevel = 0 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& ckG1 = 0
& ckG3 = 0
& s = 0

  Projection onto the parameters:
   dG4_u >= 7
& dG3_u + dG4_u >= 24
& dG3_u >= 8

  /************************************************************/
  STATE 13:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& ckG4 + 32 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u + s >= 32 + ckG4
& dG4_u >= 3
& s >= 32
& s >= 23 + ckG4
& ckG4 >= 3
& ckG2 >= s
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u + dG4_u >= 17
& dG3_u >= 8

  /************************************************************/
  STATE 14:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& ckG4 + 29 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& s >= 23 + ckG4
& 39 >= s
& s = 15 + ckG3
& s = 5 + ckG1

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 15:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& dG3_u + ckG4 >= 24
& dG4_u + 7 >= ckG4
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& ckG1 = 0
& ckG3 = 24
& s = 0

  Projection onto the parameters:
   dG3_u + dG4_u >= 17
& dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 16:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401, observer: locobs_2, qLevel = 1 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& ckG1 = 0
& ckG3 = 0
& s = 0

  Projection onto the parameters:
   dG4_u >= 7
& dG3_u + dG4_u >= 24
& dG3_u >= 8

  /************************************************************/
  STATE 17:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 3
& 7 >= ckG4
& ckG1 = 0
& ckG3 = 24
& s = 0

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 18:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401, observer: locobs_2, qLevel = 1 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 3
& 7 >= ckG4
& ckG1 = 0
& ckG3 = 0
& s = 0

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u + dG4_u >= 24
& dG3_u >= 17

  /************************************************************/
  STATE 19:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401, observer: locobs_1, qLevel = 1 ==> 
& dG3_u + ckG4 >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 10
& 16 >= ckG4
& ckG1 = 0
& ckG3 = 24
& s = 0

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  DESCRIPTION OF THE TRANSITIONS
  s_7 -> s_10 via "ckDown"
  s_8 -> s_12 via "ckDown"
  s_14 -> s_19 via "ckDown"
  s_8 -> s_13 via "qUp"
  s_3 -> s_4 via "dDown"
  s_0 -> s_1 via "dUp"
  s_10 -> s_18 via "qUp"
  s_13 -> s_15 via "ckDown"
  s_9 -> s_14 via "dDown"
  s_5 -> s_8 via "dDown"
  s_4 -> s_7 via "qG3Down"
  s_11 -> s_17 via "ckDown"
  s_2 -> s_3 via "ckUp"
  s_5 -> s_9 via "qUp"
  s_3 -> s_5 via "qG3Down"
  s_7 -> s_11 via "qUp"
  s_4 -> s_6 via "ckDown"
  s_1 -> s_2 via "qG1Down"
  s_12 -> s_16 via "qUp"
			"""
			} #end statespace file
		] # end expectations
	} # end test case
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test urgency',
		'input_files': ['testUrgency.imi'],
		'options'    : '-mode statespace -output-states',
		'expectations' : [
			{'file': 'testUrgency-statespace.states' , 'content' : """
		  DESCRIPTION OF THE TRANSITIONS
  s_4 -> s_4 via "a"
  s_2 -> s_4 via "c"
  s_0 -> s_2 via "b"
  s_0 -> s_1 via "b"
  s_2 -> s_4 via "b"
  s_3 -> s_3 via "a"
  s_3 -> s_1 via "b"
  s_0 -> s_0 via "a"
  s_1 -> s_3 via "c"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with basic unreachability',
		'input_files': ['F3.imi'],
		'options'    : '-mode EF -merge -incl -output-result',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
 Delta > delta
& delta >= 0
 OR 
 Delta > 2*delta
& delta >= 0
END CONSTRAINT
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with complex unreachability property',
		'input_files': ['coffeeDrinker-TACAS.imi'],
		'options'    : '-mode EF -merge -output-result',
		'expectations' : [
			{'file': 'coffeeDrinker-TACAS.res' , 'content' : """
BEGIN CONSTRAINT
 p_add_sugar + p_coffee >= 15
& p_button > 0
& 15 >= p_add_sugar
& p_coffee > 0
& p_add_sugar >= 3*p_button
 OR 
 p_add_sugar + p_coffee >= 15
& p_button >= 5
& 15 >= p_add_sugar
& p_coffee > 0
& p_add_sugar >= 2*p_button
 OR 
 3*p_button >= p_add_sugar + p_coffee
& p_coffee > 0
& 15 >= p_add_sugar + p_coffee
& p_add_sugar >= 2*p_button
 OR 
 p_add_sugar >= 15
& 5 >= p_button
& p_button > 0
& p_coffee > 0
 OR 
 p_add_sugar >= 15
& 15 >= 2*p_button
& p_button >= 5
& p_coffee > 0
 OR 
 p_add_sugar + p_coffee >= 15
& 2*p_button >= 15
& 15 >= p_add_sugar
& p_coffee > 0
& p_add_sugar >= p_button
 OR 
 2*p_button >= p_add_sugar + p_coffee
& p_coffee > 0
& 15 >= p_add_sugar + p_coffee
& p_add_sugar >= p_button
 OR 
 p_add_sugar + p_coffee >= 15
& p_button >= 15
& 15 >= p_add_sugar
& p_coffee > 0
& p_add_sugar > 0
 OR 
 p_button >= p_add_sugar + p_coffee
& p_coffee > 0
& 15 >= p_add_sugar + p_coffee
& p_add_sugar > 0
 OR 
 p_add_sugar >= 15
& 15 >= p_button
& 2*p_button >= 15
& p_coffee > 0
 OR 
 p_add_sugar >= 15
& p_button >= 15
& p_coffee > 0
END CONSTRAINT
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example without clocks',
		'input_files': ['PDFC1.imi'],
		'options'    : '-mode PDFC -output-result  -output-states',
		'expectations' : [
			{'file': 'PDFC1.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : good
------------------------------------------------------------
Number of states              : 2
Number of transitions         : 2
Number of computed states     : 3
"""
			} #end result file
			,
			{'file': 'PDFC1-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 >= 0
& p2 >= 0

  Projection onto the parameters:
   p1 >= 0
& p2 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p2 >= p1

  Projection onto the parameters:
   p1 >= 0
& p2 >= p1

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a"
  s_1 -> s_1 via "a"
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example with clocks (false result)',
		'input_files': ['PDFC3.imi'],
		'options'    : '-mode PDFC -output-result  -output-states',
		'expectations' : [
			{'file': 'PDFC3.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : good
------------------------------------------------------------
Number of states              : 2
Number of transitions         : 1
Number of computed states     : 2
"""
			} #end result file
			,
			{'file': 'PDFC3-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 + 5 >= x
& p1 >= 0
& p2 >= 0
& x >= 0

  Projection onto the parameters:
   p1 >= 0
& p2 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p1 + 5 >= p2
& p2 >= 0
& x >= p2
& 10 >= x

  Projection onto the parameters:
   10 >= p2
& p1 + 5 >= p2
& p2 >= 0
& p1 >= 0

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a"
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example with clocks (normal result)',
		'input_files': ['PDFC4.imi'],
		'options'    : '-mode PDFC -output-result  -output-states',
		'expectations' : [
			{'file': 'PDFC4.res' , 'content' : """
BEGIN CONSTRAINT
 p1 + 5 >= p2
& p1 >= 0
& p2 >= 0
& 10 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : good
------------------------------------------------------------
Number of states              : 2
Number of transitions         : 2
Number of computed states     : 3
"""
			} #end result file
			,
			{'file': 'PDFC4-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 + 5 >= x
& p1 >= 0
& p2 >= 0
& x >= 0

  Projection onto the parameters:
   p1 >= 0
& p2 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p1 + 5 >= p2
& p2 >= 0
& x >= p2
& 10 >= x

  Projection onto the parameters:
   10 >= p2
& p1 + 5 >= p2
& p2 >= 0
& p1 >= 0

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a"
  s_1 -> s_1 via "a"
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the inverse method (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop.pi0'],
		'options'    : '-output-result',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
dG3_u + dG4_u >= 17 & dG3_u >= 8 & dG4_u >= 3 & 17 > dG3_u & 24 > dG3_u + dG4_u
END CONSTRAINT
			"""
			} #end result file
		] # end expectations
	} # end test case
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the inverse method (SIMOP)',
		'input_files': ['simop.imi', 'simop.pi0'],
		'options'    : '-merge -no-random -output-result',
		'expectations' : [
			{'file': 'simop.res' , 'content' : """
BEGIN CONSTRAINT
500 >= COMct
    & COMct > 495
    & SIGmrt > 70 + 4*COMct
END CONSTRAINT
			"""
			} #end result file
		] # end expectations
	} # end test case
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM: here IM',
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion.pi0'],
		'options'    : '-output-result',
		'expectations' : [
			{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		  p = 2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM: here IMK',
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion.pi0'],
		'options'    : '-IMK -output-result',
		'expectations' : [
			{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		 p > 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM: here IMunion',
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion.pi0'],
		'options'    : '-IMunion -output-result',
		'expectations' : [
			{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		p > 1 & 4 >= p
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM (bis): here IM',
		'input_files': ['exVariantes.imi', 'exVariantes.pi0'],
		'options'    : '-output-result',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
	p2 >= 3 & 5*p1 > p2 & p2 >= 4*p1
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM (bis): here IMK',
		'input_files': ['exVariantes.imi', 'exVariantes.pi0'],
		'options'    : '-IMK -output-result -no-random',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
		   5*p1 > p2
    & p2 > 2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM (bis): here IMunion',
		'input_files': ['exVariantes.imi', 'exVariantes.pi0'],
		'options'    : '-IMunion -output-result -no-random',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
		 5*p1 > p2 & p2 >= 3 OR 5*p1 > p2 & p2 > 2 & p2 >= 4*p1
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (good reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP.pigood'],
		'options'    : '-PRP -output-result -output-states',
		'expectations' : [
			{'file': 'testPRP.res' , 'content' : """
BEGIN CONSTRAINT
	4 > p2
    & 3 > p1
    & p1 >= 0
    & p2 >= 0
END CONSTRAINT
		  """
			} # end result file
			,
			{'file': 'testPRP-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 >= 0
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p2 >= 0
& x >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 2:
  pta: l3 ==> 
& p1 >= x
& p2 >= 0
& x >= 0
& 1 >= p2
& x = y

  Projection onto the parameters:
   p1 >= 0
& 1 >= p2
& p2 >= 0

  /************************************************************/
  STATE 3:
  pta: l4 ==> 
& p1 >= p2
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= p2

  DESCRIPTION OF THE TRANSITIONS
  s_1 -> s_2
  s_0 -> s_1
  s_1 -> s_3
  s_2 -> s_2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (bad reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP.pibad'],
		'options'    : '-PRP -output-result -output-states',
		'expectations' : [
			{'file': 'testPRP.res' , 'content' : """
		 & p1 >= 0
OR
  p2 >= 0
& p1 >= 3
		  """
			} # end result file
			,
			{'file': 'testPRP-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 >= 0
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p2 >= 0
& x >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 2:
  pta: locBad1 ==> 
& p1 >= 0
& p2 >= 4
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 4
& p1 >= 0

  /************************************************************/
  STATE 3:
  pta: l4 ==> 
& p1 >= p2
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= p2

  /************************************************************/
  STATE 4:
  pta: locBad2 ==> 
& p1 >= 3
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 3

  DESCRIPTION OF THE TRANSITIONS
  s_1 -> s_4
  s_0 -> s_1
  s_1 -> s_3
  s_0 -> s_2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (looping reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP.piloop'],
		'options'    : '-PRP -output-result -output-states -depth-limit 10',
		'expectations' : [
			{'file': 'testPRP.res' , 'content' : """
 p2 >= 4
    & p1 >= 0
    OR
      p2 >= 0
    & p1 >= 3
		  """
			} # end result file
			,
			{'file': 'testPRP-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==> 
& p1 >= 0
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==> 
& p1 >= 0
& p2 >= 0
& x >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 2:
  pta: locBad1 ==> 
& p1 >= 0
& p2 >= 4
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 4
& p1 >= 0

  /************************************************************/
  STATE 3:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 4:
  pta: l4 ==> 
& p1 >= p2
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= p2

  /************************************************************/
  STATE 5:
  pta: locBad2 ==> 
& p1 >= 3
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p2 >= 0
& p1 >= 3

  /************************************************************/
  STATE 6:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 1 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 7:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 2 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 8:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 3 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 9:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 4 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 10:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 5 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 11:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 6 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 12:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 7 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  /************************************************************/
  STATE 13:
  pta: infiniteLoop ==> 
& 1 >= x
& p1 >= 0
& p2 >= 5
& x >= 0
& x + 8 = y

  Projection onto the parameters:
   p2 >= 5
& p1 >= 0

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_3
  s_3 -> s_6
  s_8 -> s_9
  s_1 -> s_5
  s_0 -> s_1
  s_1 -> s_4
  s_10 -> s_11
  s_7 -> s_8
  s_0 -> s_2
  s_6 -> s_7
  s_12 -> s_13
  s_11 -> s_12
  s_9 -> s_10


		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode cover + graphical output',
		'input_files': ['flipflop.imi', 'flipflop.v0'],
		'options'    : '-mode cover -output-result -output-cart -output-graphics-source',
		'expectations' : [
			# WARNING: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'flipflop.res' , 'content' : """
 Pi1:
  dG3_u = 8
& dG4_u = 3

 K1:
 dG3_u >= 8
& dG4_u >= 3
& 17 > dG3_u + dG4_u

"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Tile #2

 Pi2:
  dG3_u = 14
& dG4_u = 3

 K2:
 dG3_u + dG4_u >= 17
& dG3_u >= 8
& dG4_u >= 3
& 17 > dG3_u
& 24 > dG3_u + dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 Tile #3

 Pi3:
  dG3_u = 17
& dG4_u = 3

 K3:
 dG3_u >= 17
& dG4_u >= 3
& 24 > dG3_u + dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Tile #4

 Pi4:
  dG3_u = 21
& dG4_u = 3

 K4:
 dG3_u + dG4_u >= 24
& dG4_u >= 3
& 7 > dG4_u
& 24 > dG3_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Tile #5

 Pi5:
  dG3_u = 24
& dG4_u = 3

 K5:
 dG3_u >= 24
& dG4_u >= 3
& 7 > dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Tile #6

 Pi6:
  dG3_u = 17
& dG4_u = 7

 K6:
 dG3_u >= 17
& dG4_u >= 7
& 24 > dG3_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Tile #7

 Pi7:
  dG3_u = 24
& dG4_u = 7

 K7:
 dG3_u >= 24
& dG4_u >= 7

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : bad
Number of random selections   : 0
------------------------------------------------------------
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 Tile #8

 Pi8:
  dG3_u = 16
& dG4_u = 8

 K8:
 dG3_u >= 8
& dG3_u + dG4_u >= 24
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : bad
Number of random selections   : 0
------------------------------------------------------------
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Number of integers in v0      : 644
Number of tiles computed      : 8
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 636
Average number of states      : 15.1
Average number of transitions : 14.1
"""
			} # end BC file
			, 
			{'file': 'flipflop_cart_bc_points_2.txt' , 'content' : """14. 3.
8. 9.
8. 16.
17. 7.
17. 3.
14. 3.
# """
			} # end tile file
			,
			{'file': 'flipflop_cart_bc_points_8.txt' , 'content' : """17. 31.
17. 7.
8. 16.
8. 31.
17. 31.
# """
			} # end tile file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode random',
		'input_files': ['flipflop.imi', 'flipflop.v0'],
		'options'    : '-mode random200 -output-result',
		'expectations' : [
			# WARNING: this algorithm is… random! hence no absolute guarantee to find the result (this said, a max_tries of 200 generally allows one to find all tiles with a good probability)
			# WARNING: no other way for now that checking separately the constraints (because the computation times may of course differ)… and to check them separately as the order is of course unknown
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 8
& dG4_u >= 3
& 17 > dG3_u + dG4_u

"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 17
& dG3_u >= 8
& dG4_u >= 3
& 17 > dG3_u
& 24 > dG3_u + dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 17
& dG4_u >= 3
& 24 > dG3_u + dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG4_u >= 3
& 7 > dG4_u
& 24 > dG3_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 24
& dG4_u >= 3
& 7 > dG4_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 17
& dG4_u >= 7
& 24 > dG3_u
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 24
& dG4_u >= 7

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : bad
Number of random selections   : 0
------------------------------------------------------------
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 8
& dG3_u + dG4_u >= 24
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
State space nature            : bad
Number of random selections   : 0
------------------------------------------------------------
"""
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
Average number of states      : 15.1
Average number of transitions : 14.1
"""
			} # end BC file
			, 
			# NOTE: impossible to check graphics source, as the order of the tiles is not known
			#{'file': 'flipflop_cart_bc_points_2.txt' , 'content' : """14. 3.
#8. 9.
#8. 16.
#17. 7.
#17. 3.
#14. 3.
## """
			#} # end tile file
			#,
			#{'file': 'flipflop_cart_bc_points_8.txt' , 'content' : """17. 31.
#17. 7.
#8. 16.
#8. 31.
#17. 31.
## """
			#} # end tile file
			#,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with observer + depth-limit + project-result (quite basic)',
		'input_files': ['coffeeDrinker-TACAS-within.imi'],
		'options'    : '-mode EF -merge -output-result -depth-limit 10',
		'expectations' : [
			{'file': 'coffeeDrinker-TACAS-within.res' , 'content' : """
BEGIN CONSTRAINT
 p_coffee > 0
END CONSTRAINT
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Simple example without merging',
		'input_files': ['exActionsNonPreserved.imi'],
		'options'    : '-mode statespace -output-states',
		'expectations' : [
			{'file': 'exActionsNonPreserved-statespace.states' , 'content' : """
		*/

  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: L1 ==> 
& 2 >= x
& p >= 0
& x >= 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 1:
  pta: L2 ==> 
& p >= 0
& x >= 2
& 2 >= p

  Projection onto the parameters:
   p >= 0
& 2 >= p

  /************************************************************/
  STATE 2:
  pta: L2 ==> 
& p >= 2
& x >= 2

  Projection onto the parameters:
   p >= 2

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_2 via "b"
  s_0 -> s_1 via "a"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Simple example with merging',
		'input_files': ['exActionsNonPreserved.imi'],
		'options'    : '-mode statespace -output-states -merge',
		'expectations' : [
			{'file': 'exActionsNonPreserved-statespace.states' , 'content' : """
		*/

  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: L1 ==> 
& 2 >= x
& p >= 0
& x >= 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 1:
  pta: L2 ==> 
& p >= 0
& x >= 2

  Projection onto the parameters:
   p >= 0

  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "b"
  s_0 -> s_1 via "a"
  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the model printer',
		'input_files': ['Sched5.imi'],
		'options'    : '-PTA2IMI',
		'expectations' : [
			{'file': 'Sched5-regenerated.imi' , 'content' : """
 ************************************************************)
var 
	t1_c, t1_d, t1_urgent, t1_arr_x, t2_c, t2_d, t2_urgent, t2_arr_x, t3_c, t3_d, t3_urgent, t3_arr_x, t4_c, t4_d, t4_urgent, t4_arr_x, t5_c, t5_d, t5_urgent, t5_arr_x, CPU1_urgent
		: clock;

	t4_C, t5_C
		: parameter;


(************************************************************)
 automaton Task_t1
(************************************************************)
 synclabs: t1_arr_event, t1_arr, t1_dis, t1_miss, t1_end;
 
 
loc t1_loc_idle: while True wait
	when True do {t1_urgent' = 0}  sync t1_arr_event goto t1_loc_act_event;
 
loc t1_loc_act_event: while  0 >= t1_urgent wait
	when  t1_urgent = 0 do {t1_c' = 0, t1_d' = 0}  sync t1_arr goto t1_loc_act;
 
loc t1_loc_act: while  8 >= t1_d stop{t1_c}
	when True do {}  sync t1_dis goto t1_loc_exe;
	when  t1_d >= 8 do {}  sync t1_miss goto t1_loc_miss;
 
loc t1_loc_exe: while  8 >= t1_d
& 2 >= t1_c wait
	when  t1_d >= 8
& 2 > t1_c do {}  sync t1_miss goto t1_loc_miss;
	when  t1_c = 2 do {}  sync t1_end goto t1_loc_idle;
 
loc t1_loc_miss: while True wait
 end (* Task_t1 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t1_arr
(************************************************************)
 synclabs: t1_arr_event;
 
 
loc t1_arr_loc_arr: while  8 >= t1_arr_x wait
	when  t1_arr_x = 8 do {t1_arr_x' = 0}  sync t1_arr_event goto t1_arr_loc_arr;
 end (* Periodic_t1_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t2
(************************************************************)
 synclabs: t2_arr_event, t2_arr, t2_dis, t2_miss, t2_pre, t2_end;
 
 
loc t2_loc_idle: while True wait
	when True do {t2_urgent' = 0}  sync t2_arr_event goto t2_loc_act_event;
 
loc t2_loc_act_event: while  0 >= t2_urgent wait
	when  t2_urgent = 0 do {t2_c' = 0, t2_d' = 0}  sync t2_arr goto t2_loc_act;
 
loc t2_loc_act: while  20 >= t2_d stop{t2_c}
	when True do {}  sync t2_dis goto t2_loc_exe;
	when  t2_d >= 20 do {}  sync t2_miss goto t2_loc_miss;
 
loc t2_loc_exe: while  20 >= t2_d
& 5 >= t2_c wait
	when  5 > t2_c do {}  sync t2_pre goto t2_loc_act;
	when  t2_d >= 20
& 5 > t2_c do {}  sync t2_miss goto t2_loc_miss;
	when  t2_c = 5 do {}  sync t2_end goto t2_loc_idle;
 
loc t2_loc_miss: while True wait
 end (* Task_t2 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t2_arr
(************************************************************)
 synclabs: t2_arr_event;
 
 
loc t2_arr_loc_arr: while  20 >= t2_arr_x wait
	when  t2_arr_x = 20 do {t2_arr_x' = 0}  sync t2_arr_event goto t2_arr_loc_arr;
 end (* Periodic_t2_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t3
(************************************************************)
 synclabs: t3_arr_event, t3_arr, t3_dis, t3_miss, t3_pre, t3_end;
 
 
loc t3_loc_idle: while True wait
	when True do {t3_urgent' = 0}  sync t3_arr_event goto t3_loc_act_event;
 
loc t3_loc_act_event: while  0 >= t3_urgent wait
	when  t3_urgent = 0 do {t3_c' = 0, t3_d' = 0}  sync t3_arr goto t3_loc_act;
 
loc t3_loc_act: while  50 >= t3_d stop{t3_c}
	when True do {}  sync t3_dis goto t3_loc_exe;
	when  t3_d >= 50 do {}  sync t3_miss goto t3_loc_miss;
 
loc t3_loc_exe: while  50 >= t3_d
& 8 >= t3_c wait
	when  8 > t3_c do {}  sync t3_pre goto t3_loc_act;
	when  t3_d >= 50
& 8 > t3_c do {}  sync t3_miss goto t3_loc_miss;
	when  t3_c = 8 do {}  sync t3_end goto t3_loc_idle;
 
loc t3_loc_miss: while True wait
 end (* Task_t3 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t3_arr
(************************************************************)
 synclabs: t3_arr_event;
 
 
loc t3_arr_loc_arr: while  50 >= t3_arr_x wait
	when  t3_arr_x = 50 do {t3_arr_x' = 0}  sync t3_arr_event goto t3_arr_loc_arr;
 end (* Periodic_t3_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t4
(************************************************************)
 synclabs: t4_arr_event, t4_arr, t4_dis, t4_miss, t4_pre, t4_end;
 
 
loc t4_loc_idle: while True wait
	when True do {t4_urgent' = 0}  sync t4_arr_event goto t4_loc_act_event;
 
loc t4_loc_act_event: while  0 >= t4_urgent wait
	when  t4_urgent = 0 do {t4_c' = 0, t4_d' = 0}  sync t4_arr goto t4_loc_act;
 
loc t4_loc_act: while  100 >= t4_d stop{t4_c}
	when True do {}  sync t4_dis goto t4_loc_exe;
	when  t4_d >= 100 do {}  sync t4_miss goto t4_loc_miss;
 
loc t4_loc_exe: while  t4_C >= t4_c
& 100 >= t4_d wait
	when  t4_C > t4_c do {}  sync t4_pre goto t4_loc_act;
	when  t4_C > t4_c
& t4_d >= 100 do {}  sync t4_miss goto t4_loc_miss;
	when  t4_C = t4_c do {}  sync t4_end goto t4_loc_idle;
 
loc t4_loc_miss: while True wait
 end (* Task_t4 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t4_arr
(************************************************************)
 synclabs: t4_arr_event;
 
 
loc t4_arr_loc_arr: while  100 >= t4_arr_x wait
	when  t4_arr_x = 100 do {t4_arr_x' = 0}  sync t4_arr_event goto t4_arr_loc_arr;
 end (* Periodic_t4_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t5
(************************************************************)
 synclabs: t5_arr_event, t5_arr, t5_dis, t5_miss, t5_pre, t5_end;
 
 
loc t5_loc_idle: while True wait
	when True do {t5_urgent' = 0}  sync t5_arr_event goto t5_loc_act_event;
 
loc t5_loc_act_event: while  0 >= t5_urgent wait
	when  t5_urgent = 0 do {t5_c' = 0, t5_d' = 0}  sync t5_arr goto t5_loc_act;
 
loc t5_loc_act: while  200 >= t5_d stop{t5_c}
	when True do {}  sync t5_dis goto t5_loc_exe;
	when  t5_d >= 200 do {}  sync t5_miss goto t5_loc_miss;
 
loc t5_loc_exe: while  t5_C >= t5_c
& 200 >= t5_d wait
	when  t5_C > t5_c do {}  sync t5_pre goto t5_loc_act;
	when  t5_C > t5_c
& t5_d >= 200 do {}  sync t5_miss goto t5_loc_miss;
	when  t5_C = t5_c do {}  sync t5_end goto t5_loc_idle;
 
loc t5_loc_miss: while True wait
 end (* Task_t5 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t5_arr
(************************************************************)
 synclabs: t5_arr_event;
 
 
loc t5_arr_loc_arr: while  200 >= t5_arr_x wait
	when  t5_arr_x = 200 do {t5_arr_x' = 0}  sync t5_arr_event goto t5_arr_loc_arr;
 end (* Periodic_t5_arr *)
(************************************************************)


(************************************************************)
 automaton sched_CPU1
(************************************************************)
 synclabs: t1_arr, t2_arr, t3_arr, t4_arr, t5_arr, t1_dis, t2_dis, t3_dis, t4_dis, t5_dis, t5_end, t5_pre, t4_end, t4_pre, t3_end, t3_pre, t2_end, t2_pre, t1_end;
 
 
loc CPU1_loc_: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_At3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_At4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_At5;
 
loc CPU1_loc_At1: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1;
 
loc CPU1_loc_At2: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2;
 
loc CPU1_loc_At3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;
 
loc CPU1_loc_At4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;
 
loc CPU1_loc_At5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;
 
loc CPU1_loc_Rt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt5;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_At3Rt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_At4Rt5;
	when True do {CPU1_urgent' = 0}  sync t5_end goto CPU1_loc_Et5;
 
loc CPU1_loc_Et5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  (* sync nosync_1*)  goto CPU1_loc_stop;
 
loc CPU1_loc_At1Rt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At1Wt5;
 
loc CPU1_loc_At1Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt5;
 
loc CPU1_loc_At2Rt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At2Wt5;
 
loc CPU1_loc_At2Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt5;
 
loc CPU1_loc_At3Rt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At3Wt5;
 
loc CPU1_loc_At3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;
 
loc CPU1_loc_At4Rt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At4Wt5;
 
loc CPU1_loc_At4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;
 
loc CPU1_loc_Rt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt4;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt4;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_At3Rt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_end goto CPU1_loc_Et4;
 
loc CPU1_loc_Et4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  (* sync nosync_2*)  goto CPU1_loc_stop;
 
loc CPU1_loc_At1Rt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At1Wt4;
 
loc CPU1_loc_At1Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt4;
 
loc CPU1_loc_At2Rt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At2Wt4;
 
loc CPU1_loc_At2Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4;
 
loc CPU1_loc_At3Rt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At3Wt4;
 
loc CPU1_loc_At3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;
 
loc CPU1_loc_Rt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_At3Rt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_end goto CPU1_loc_Et4Wt5;
 
loc CPU1_loc_Et4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;
 
loc CPU1_loc_At1Rt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At1Wt4Wt5;
 
loc CPU1_loc_At1Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt4Wt5;
 
loc CPU1_loc_At2Rt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At2Wt4Wt5;
 
loc CPU1_loc_At2Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4Wt5;
 
loc CPU1_loc_At3Rt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At3Wt4Wt5;
 
loc CPU1_loc_At3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;
 
loc CPU1_loc_Rt3: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt3;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_end goto CPU1_loc_Et3;
 
loc CPU1_loc_Et3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  (* sync nosync_3*)  goto CPU1_loc_stop;
 
loc CPU1_loc_At1Rt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3;
 
loc CPU1_loc_At1Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3;
 
loc CPU1_loc_At2Rt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3;
 
loc CPU1_loc_At2Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3;
 
loc CPU1_loc_Rt3Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_end goto CPU1_loc_Et3Wt5;
 
loc CPU1_loc_Et3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;
 
loc CPU1_loc_At1Rt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt5;
 
loc CPU1_loc_At1Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt5;
 
loc CPU1_loc_At2Rt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt5;
 
loc CPU1_loc_At2Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt5;
 
loc CPU1_loc_Rt3Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_end goto CPU1_loc_Et3Wt4;
 
loc CPU1_loc_Et3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;
 
loc CPU1_loc_At1Rt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt4;
 
loc CPU1_loc_At1Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt4;
 
loc CPU1_loc_At2Rt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt4;
 
loc CPU1_loc_At2Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4;
 
loc CPU1_loc_Rt3Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_At2Rt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_end goto CPU1_loc_Et3Wt4Wt5;
 
loc CPU1_loc_Et3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;
 
loc CPU1_loc_At1Rt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt4Wt5;
 
loc CPU1_loc_At1Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt4Wt5;
 
loc CPU1_loc_At2Rt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt4Wt5;
 
loc CPU1_loc_At2Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4Wt5;
 
loc CPU1_loc_Rt2: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt2Wt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt2Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt2Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2;
 
loc CPU1_loc_Et2: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  (* sync nosync_4*)  goto CPU1_loc_stop;
 
loc CPU1_loc_At1Rt2: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2;
 
loc CPU1_loc_At1Wt2: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2;
 
loc CPU1_loc_Rt2Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt5;
 
loc CPU1_loc_Et2Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;
 
loc CPU1_loc_At1Rt2Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt5;
 
loc CPU1_loc_At1Wt2Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt5;
 
loc CPU1_loc_Rt2Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt4;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt4;
 
loc CPU1_loc_Et2Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;
 
loc CPU1_loc_At1Rt2Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt4;
 
loc CPU1_loc_At1Wt2Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt4;
 
loc CPU1_loc_Rt2Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt4Wt5;
 
loc CPU1_loc_Et2Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;
 
loc CPU1_loc_At1Rt2Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt4Wt5;
 
loc CPU1_loc_At1Wt2Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt4Wt5;
 
loc CPU1_loc_Rt2Wt3: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt3;
 
loc CPU1_loc_Et2Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;
 
loc CPU1_loc_At1Rt2Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3;
 
loc CPU1_loc_At1Wt2Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3;
 
loc CPU1_loc_Rt2Wt3Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt3Wt5;
 
loc CPU1_loc_Et2Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;
 
loc CPU1_loc_At1Rt2Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt5;
 
loc CPU1_loc_At1Wt2Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt5;
 
loc CPU1_loc_Rt2Wt3Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt3Wt4;
 
loc CPU1_loc_Et2Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;
 
loc CPU1_loc_At1Rt2Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt4;
 
loc CPU1_loc_At1Wt2Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt4;
 
loc CPU1_loc_Rt2Wt3Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t2_end goto CPU1_loc_Et2Wt3Wt4Wt5;
 
loc CPU1_loc_Et2Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;
 
loc CPU1_loc_At1Rt2Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt4Wt5;
 
loc CPU1_loc_At1Wt2Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
 
loc CPU1_loc_Rt1: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1;
 
loc CPU1_loc_Et1: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  (* sync nosync_5*)  goto CPU1_loc_stop;
 
loc CPU1_loc_Rt1Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt5;
 
loc CPU1_loc_Et1Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;
 
loc CPU1_loc_Rt1Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt4;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt4;
 
loc CPU1_loc_Et1Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;
 
loc CPU1_loc_Rt1Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt4Wt5;
 
loc CPU1_loc_Et1Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;
 
loc CPU1_loc_Rt1Wt3: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt3;
 
loc CPU1_loc_Et1Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;
 
loc CPU1_loc_Rt1Wt3Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt3Wt5;
 
loc CPU1_loc_Et1Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;
 
loc CPU1_loc_Rt1Wt3Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt3Wt4;
 
loc CPU1_loc_Et1Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;
 
loc CPU1_loc_Rt1Wt3Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt3Wt4Wt5;
 
loc CPU1_loc_Et1Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;
 
loc CPU1_loc_Rt1Wt2: while True wait
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2;
 
loc CPU1_loc_Et1Wt2: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2;
 
loc CPU1_loc_Rt1Wt2Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt5;
 
loc CPU1_loc_Et1Wt2Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt5;
 
loc CPU1_loc_Rt1Wt2Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt4;
 
loc CPU1_loc_Et1Wt2Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4;
 
loc CPU1_loc_Rt1Wt2Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt4Wt5;
 
loc CPU1_loc_Et1Wt2Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4Wt5;
 
loc CPU1_loc_Rt1Wt2Wt3: while True wait
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt3;
 
loc CPU1_loc_Et1Wt2Wt3: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3;
 
loc CPU1_loc_Rt1Wt2Wt3Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt5;
 
loc CPU1_loc_Et1Wt2Wt3Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt5;
 
loc CPU1_loc_Rt1Wt2Wt3Wt4: while True wait
	when True do {CPU1_urgent' = 0}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt4;
 
loc CPU1_loc_Et1Wt2Wt3Wt4: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4;
 
loc CPU1_loc_Rt1Wt2Wt3Wt4Wt5: while True wait
	when True do {CPU1_urgent' = 0}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt4Wt5;
 
loc CPU1_loc_Et1Wt2Wt3Wt4Wt5: while  0 >= CPU1_urgent wait
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4Wt5;
 
loc CPU1_loc_stop: while True wait
 end (* sched_CPU1 *)
(************************************************************)


(************************************************************)
 automaton OBS_dline
(************************************************************)
 synclabs: t1_miss, t2_miss, t3_miss, t4_miss, t5_miss;
 
 
loc dline_loc_nomiss: while True wait
	when True do {t1_d' = 0}  sync t1_miss goto dline_loc_miss;
	when True do {t1_d' = 0}  sync t2_miss goto dline_loc_miss;
	when True do {t1_d' = 0}  sync t3_miss goto dline_loc_miss;
	when True do {t1_d' = 0}  sync t4_miss goto dline_loc_miss;
	when True do {t1_d' = 0}  sync t5_miss goto dline_loc_miss;
 
loc dline_loc_miss: while  t1_d = 0 stop{t1_d}
 end (* OBS_dline *)
(************************************************************)

(************************************************************)
(* Initial state *)
(************************************************************)

init := True
	(*------------------------------------------------------------*)
	(* Initial location *)
	(*------------------------------------------------------------*)
	& loc[Task_t1] = t1_loc_idle
	& loc[Periodic_t1_arr] = t1_arr_loc_arr
	& loc[Task_t2] = t2_loc_idle
	& loc[Periodic_t2_arr] = t2_arr_loc_arr
	& loc[Task_t3] = t3_loc_idle
	& loc[Periodic_t3_arr] = t3_arr_loc_arr
	& loc[Task_t4] = t4_loc_idle
	& loc[Periodic_t4_arr] = t4_arr_loc_arr
	& loc[Task_t5] = t5_loc_idle
	& loc[Periodic_t5_arr] = t5_arr_loc_arr
	& loc[sched_CPU1] = CPU1_loc_
	& loc[OBS_dline] = dline_loc_nomiss

	(*------------------------------------------------------------*)
	(* Initial discrete assignments *)
	(*------------------------------------------------------------*)

	(*------------------------------------------------------------*)
	(* Initial constraint *)
	(*------------------------------------------------------------*)
	 &  t5_C >= 10
& t4_C >= 10
& 50 >= t5_C
& 50 >= t4_C
& t1_c = 0
& t1_d = 0
& t1_urgent = 0
& t1_arr_x = 8
& t2_c = 0
& t2_d = 0
& t2_urgent = 0
& t2_arr_x = 20
& t3_c = 0
& t3_d = 0
& t3_urgent = 0
& t3_arr_x = 50
& t4_c = 0
& t4_d = 0
& t4_urgent = 0
& t4_arr_x = 100
& t5_c = 0
& t5_d = 0
& t5_urgent = 0
& t5_arr_x = 200
& CPU1_urgent = 0

;

(************************************************************)
(* Property specification *)
(************************************************************)

property := unreachable loc[OBS_dline] = dline_loc_miss


(************************************************************)
(* The end *)
(************************************************************)
end
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test conversion to HyTech',
		'input_files': ['flipflop.imi'],
		'options'    : '-PTA2HyTech',
		'expectations' : [
			{'file': 'flipflop.hy' , 'content' : """
 -- Created to be compatible with 'hytech-v1.04f-Linux_static'
 --************************************************************
var 
	s, ckG1, ckG2, ckG3, ckG4
		: clock;

	qLevel
		: discrete;

	dG3_u, dG4_u
		: parameter;


--************************************************************
 automaton input
--************************************************************
 synclabs: dUp, ckUp, dDown, ckDown;
 initially Input0;
 
loc Input0: while  5 >= s wait{}
	when  s = 5 do {}  sync dUp goto Input1;
 
loc Input1: while  15 >= s wait{}
	when  s = 15 do {}  sync ckUp goto Input2;
 
loc Input2: while  32 >= s wait{}
	when  s = 32 do {}  sync dDown goto Input3;
 
loc Input3: while  39 >= s wait{}
	when  s = 39 do {s' = 0}  sync ckDown goto Input4;
 
loc Input4: while  0 >= s wait{}
 end -- input
--************************************************************


--************************************************************
 automaton g1
--************************************************************
 synclabs: dUp, ckUp, qG2Up, qG1Up, qG2Down, ckDown, qG1Down, dDown;
 initially G10011;
 
loc G10000: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dUp goto G11000;
	when True do {ckG1' = 0}  sync ckUp goto G10100;
	when True do {ckG1' = 0}  sync qG2Up goto G10010;
	when  ckG1 >= 7 do {}  sync qG1Up goto G10001;
 
loc G10001: while  ckG1 >= 0 wait{}
	when True do {}  sync dUp goto G11001;
	when True do {}  sync ckUp goto G10101;
	when True do {}  sync qG2Up goto G10011;
 
loc G10010: while  7 >= ckG1 wait{}
	when True do {}  sync dUp goto G11010;
	when True do {}  sync ckUp goto G10110;
	when True do {ckG1' = 0}  sync qG2Down goto G10000;
	when  ckG1 >= 7 do {}  sync qG1Up goto G10011;
 
loc G10011: while  ckG1 >= 0 wait{}
	when True do {ckG1' = 0}  sync dUp goto G11011;
	when True do {ckG1' = 0}  sync ckUp goto G10111;
	when True do {}  sync qG2Down goto G10001;
 
loc G10100: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dUp goto G11100;
	when True do {ckG1' = 0}  sync ckDown goto G10000;
	when True do {}  sync qG2Up goto G10110;
	when  ckG1 >= 7 do {}  sync qG1Up goto G10101;
 
loc G10101: while  ckG1 >= 0 wait{}
	when True do {}  sync dUp goto G11101;
	when True do {}  sync ckDown goto G10001;
	when True do {ckG1' = 0}  sync qG2Up goto G10111;
 
loc G10110: while  ckG1 >= 0 wait{}
	when True do {}  sync dUp goto G11110;
	when True do {ckG1' = 0}  sync ckDown goto G10010;
	when True do {ckG1' = 0}  sync qG2Down goto G10100;
 
loc G10111: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dUp goto G11111;
	when True do {}  sync ckDown goto G10011;
	when True do {}  sync qG2Down goto G10101;
	when  ckG1 >= 7 do {}  sync qG1Down goto G10110;
 
loc G11000: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dDown goto G10000;
	when True do {ckG1' = 0}  sync ckUp goto G11100;
	when True do {}  sync qG2Up goto G11010;
	when  ckG1 >= 7 do {}  sync qG1Up goto G11001;
 
loc G11001: while  ckG1 >= 0 wait{}
	when True do {}  sync dDown goto G10001;
	when True do {}  sync ckUp goto G11101;
	when True do {ckG1' = 0}  sync qG2Up goto G11011;
 
loc G11010: while  ckG1 >= 0 wait{}
	when True do {ckG1' = 0}  sync dDown goto G10010;
	when True do {}  sync ckUp goto G11110;
	when True do {ckG1' = 0}  sync qG2Down goto G11000;
 
loc G11011: while  7 >= ckG1 wait{}
	when True do {}  sync dDown goto G10011;
	when True do {ckG1' = 0}  sync ckUp goto G11111;
	when True do {}  sync qG2Down goto G11001;
	when  ckG1 >= 7 do {}  sync qG1Down goto G11010;
 
loc G11100: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dDown goto G10100;
	when True do {ckG1' = 0}  sync ckDown goto G11000;
	when True do {}  sync qG2Up goto G11110;
	when  ckG1 >= 7 do {}  sync qG1Up goto G11101;
 
loc G11101: while  ckG1 >= 0 wait{}
	when True do {}  sync dDown goto G10101;
	when True do {}  sync ckDown goto G11001;
	when True do {ckG1' = 0}  sync qG2Up goto G11111;
 
loc G11110: while  ckG1 >= 0 wait{}
	when True do {}  sync dDown goto G10110;
	when True do {}  sync ckDown goto G11010;
	when True do {ckG1' = 0}  sync qG2Down goto G11100;
 
loc G11111: while  7 >= ckG1 wait{}
	when True do {ckG1' = 0}  sync dDown goto G10111;
	when True do {ckG1' = 0}  sync ckDown goto G11011;
	when True do {}  sync qG2Down goto G11101;
	when  ckG1 >= 7 do {}  sync qG1Down goto G11110;
 end -- g1
--************************************************************


--************************************************************
 automaton g2
--************************************************************
 synclabs: qG1Up, ckUp, qG2Up, ckDown, qG1Down, qG2Down;
 initially G2101;
 
loc G2001: while  ckG2 >= 0 wait{}
	when True do {}  sync qG1Up goto G2101;
	when True do {}  sync ckUp goto G2011;
 
loc G2000: while  6 >= ckG2 wait{}
	when True do {ckG2' = 0}  sync qG1Up goto G2100;
	when True do {ckG2' = 0}  sync ckUp goto G2010;
	when  ckG2 >= 5 do {}  sync qG2Up goto G2001;
 
loc G2011: while  ckG2 >= 0 wait{}
	when True do {ckG2' = 0}  sync qG1Up goto G2111;
	when True do {}  sync ckDown goto G2001;
 
loc G2010: while  6 >= ckG2 wait{}
	when True do {}  sync qG1Up goto G2110;
	when True do {ckG2' = 0}  sync ckDown goto G2000;
	when  ckG2 >= 5 do {}  sync qG2Up goto G2011;
 
loc G2101: while  ckG2 >= 0 wait{}
	when True do {}  sync qG1Down goto G2001;
	when True do {ckG2' = 0}  sync ckUp goto G2111;
 
loc G2100: while  6 >= ckG2 wait{}
	when True do {ckG2' = 0}  sync qG1Down goto G2000;
	when True do {}  sync ckUp goto G2110;
	when  ckG2 >= 5 do {}  sync qG2Up goto G2101;
 
loc G2111: while  6 >= ckG2 wait{}
	when True do {}  sync qG1Down goto G2011;
	when True do {}  sync ckDown goto G2101;
	when  ckG2 >= 5 do {}  sync qG2Down goto G2110;
 
loc G2110: while  ckG2 >= 0 wait{}
	when True do {ckG2' = 0}  sync qG1Down goto G2010;
	when True do {ckG2' = 0}  sync ckDown goto G2100;
 end -- g2
--************************************************************


--************************************************************
 automaton g3
--************************************************************
 synclabs: qUp, ckUp, qG2Up, qG3Up, qG2Down, ckDown, qG3Down, qDown;
 initially G30011;
 
loc G30000: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qUp goto G31000;
	when True do {ckG3' = 0}  sync ckUp goto G30100;
	when True do {ckG3' = 0}  sync qG2Up goto G30010;
	when  ckG3 >= 8 do {}  sync qG3Up goto G30001;
 
loc G30001: while  ckG3 >= 0 wait{}
	when True do {}  sync qUp goto G31001;
	when True do {}  sync ckUp goto G30101;
	when True do {}  sync qG2Up goto G30011;
 
loc G30010: while  dG3_u >= ckG3 wait{}
	when True do {}  sync qUp goto G31010;
	when True do {}  sync ckUp goto G30110;
	when True do {ckG3' = 0}  sync qG2Down goto G30000;
	when  ckG3 >= 8 do {}  sync qG3Up goto G30011;
 
loc G30011: while  ckG3 >= 0 wait{}
	when True do {ckG3' = 0}  sync qUp goto G31011;
	when True do {ckG3' = 0}  sync ckUp goto G30111;
	when True do {}  sync qG2Down goto G30001;
 
loc G30100: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qUp goto G31100;
	when True do {ckG3' = 0}  sync ckDown goto G30000;
	when True do {}  sync qG2Up goto G30110;
	when  ckG3 >= 8 do {}  sync qG3Up goto G30101;
 
loc G30101: while  ckG3 >= 0 wait{}
	when True do {}  sync qUp goto G31101;
	when True do {}  sync ckDown goto G30001;
	when True do {ckG3' = 0}  sync qG2Up goto G30111;
 
loc G30110: while  ckG3 >= 0 wait{}
	when True do {}  sync qUp goto G31110;
	when True do {ckG3' = 0}  sync ckDown goto G30010;
	when True do {ckG3' = 0}  sync qG2Down goto G30100;
 
loc G30111: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qUp goto G31111;
	when True do {}  sync ckDown goto G30011;
	when True do {}  sync qG2Down goto G30101;
	when  ckG3 >= 8 do {}  sync qG3Down goto G30110;
 
loc G31000: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qDown goto G30000;
	when True do {ckG3' = 0}  sync ckUp goto G31100;
	when True do {}  sync qG2Up goto G31010;
	when  ckG3 >= 8 do {}  sync qG3Up goto G31001;
 
loc G31001: while  ckG3 >= 0 wait{}
	when True do {}  sync qDown goto G30001;
	when True do {}  sync ckUp goto G31101;
	when True do {ckG3' = 0}  sync qG2Up goto G31011;
 
loc G31010: while  ckG3 >= 0 wait{}
	when True do {ckG3' = 0}  sync qDown goto G30010;
	when True do {}  sync ckUp goto G31110;
	when True do {ckG3' = 0}  sync qG2Down goto G31000;
 
loc G31011: while  dG3_u >= ckG3 wait{}
	when True do {}  sync qDown goto G30011;
	when True do {ckG3' = 0}  sync ckUp goto G31111;
	when True do {}  sync qG2Down goto G31001;
	when  ckG3 >= 8 do {}  sync qG3Down goto G31010;
 
loc G31100: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qDown goto G30100;
	when True do {ckG3' = 0}  sync ckDown goto G31000;
	when True do {}  sync qG2Up goto G31110;
	when  ckG3 >= 8 do {}  sync qG3Up goto G31101;
 
loc G31101: while  ckG3 >= 0 wait{}
	when True do {}  sync qDown goto G30101;
	when True do {}  sync ckDown goto G31001;
	when True do {ckG3' = 0}  sync qG2Up goto G31111;
 
loc G31110: while  ckG3 >= 0 wait{}
	when True do {}  sync qDown goto G30110;
	when True do {}  sync ckDown goto G31010;
	when True do {ckG3' = 0}  sync qG2Down goto G31100;
 
loc G31111: while  dG3_u >= ckG3 wait{}
	when True do {ckG3' = 0}  sync qDown goto G30111;
	when True do {ckG3' = 0}  sync ckDown goto G31011;
	when True do {}  sync qG2Down goto G31101;
	when  ckG3 >= 8 do {}  sync qG3Down goto G31110;
 end -- g3
--************************************************************


--************************************************************
 automaton g4
--************************************************************
 synclabs: qG3Up, qG3Down, qDown, qUp;
 initially G410;
 
loc G401: while  ckG4 >= 0 wait{}
	when True do {ckG4' = 0}  sync qG3Up goto G411;
 
loc G411: while  dG4_u >= ckG4 wait{}
	when True do {}  sync qG3Down goto G401;
	when  ckG4 >= 3 do {qLevel' = 0}  sync qDown goto G410;
 
loc G410: while  ckG4 >= 0 wait{}
	when True do {ckG4' = 0}  sync qG3Down goto G400;
 
loc G400: while  dG4_u >= ckG4 wait{}
	when True do {}  sync qG3Up goto G410;
	when  ckG4 >= 3 do {qLevel' = 1}  sync qUp goto G401;
 end -- g4
--************************************************************


--************************************************************
-- Initial state
--************************************************************
var init : region;

init := True
	------------------------------------------------------------
	-- Initial location
	------------------------------------------------------------
	& loc[input] = Input0
	& loc[g1] = G10011
	& loc[g2] = G2101
	& loc[g3] = G30011
	& loc[g4] = G410

	------------------------------------------------------------
	-- Initial discrete assignments 
	------------------------------------------------------------
	& qLevel = 0

	------------------------------------------------------------
	-- Initial constraint
	------------------------------------------------------------
	 &  ckG1 >= 0
& ckG2 >= 0
& ckG3 >= 0
& ckG4 >= 0
& dG3_u >= 8
& dG4_u >= 3
& s = 0

;

--************************************************************
--* Property specification 
--************************************************************

--property := if ckDown then qUp has happened before; (NOT CONSIDERED BY HYTECH)


--************************************************************
--* The end
--************************************************************

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	#,
	##------------------------------------------------------------
	#{
		#'purpose'    : 'XXXX',
		#'input_files': ['XXXX.imi', 'XXXX.pi0'],
		#'options'    : '-XXXX',
		#'expectations' : [
			#{'file': 'XXXX' , 'content' : """XXXX"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------]


### THE END
]