#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#             Script for regression tests
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2015/10/23
# Last modified: 2015/10/23
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests = [
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the state space',
		'input_files': ['flipflop.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'flipflop.states' , 'content' : """second
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
			}
		]
	}
	,
	#------------------------------------------------------------
]
