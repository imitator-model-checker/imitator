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
# Last modified: 2016/01/06
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
			{'file': 'testUrgency.states' , 'content' : """
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
			{'file': 'F3.res' , 'content' : """*)
 Delta > delta
& delta >= 0
 OR 
 Delta > 2*delta
& delta >= 0"""
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
			{'file': 'coffeeDrinker-TACAS.res' , 'content' : """*)

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
& p_coffee > 0"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the inverse method',
		'input_files': ['flipflop.imi', 'flipflop.pi0'],
		'options'    : '-output-result',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """*)

 dG3_u + dG4_u >= 17
& 17 > dG3_u
& 24 > dG3_u + dG4_u
& dG3_u >= 8
& dG4_u >= 3
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
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test variants of IM: here IMK',
		#'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion.pi0'],
		#'options'    : '-IMK -output-result',
		#'expectations' : [
			#{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		 #1 < p (TODO)
		#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM: here IMunion',
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion.pi0'],
		'options'    : '-IMunion -output-result',
		'expectations' : [
			{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		  p >= 2
& 4 >= p
 OR 
 p > 1
& 2 >= p
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
		  p2 >= 4*p1
    & p2 >= 3
    & 5*p1 > p2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test variants of IM (bis): here IMK',
		#'input_files': ['exVariantes.imi', 'exVariantes.pi0'],
		#'options'    : '-IMK -output-result',
		#'expectations' : [
			#{'file': 'exVariantes.res' , 'content' : """
		  #(TODO)
		  #"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test variants of IM (bis): here IMunion',
		'input_files': ['exVariantes.imi', 'exVariantes.pi0'],
		'options'    : '-IMunion -output-result -no-random',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
		  p2 >= 4*p1
    & p2 > 2
    & 5*p1 > p2
     OR 
     p2 >= 3
    & 5*p1 > p2
     OR 
     p2 >= 3
    & 5*p1 > p2
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
			{'file': 'flipflop_cart.res' , 'content' : """*)


 (***** Constraint 1*****)
 17 > dG3_u + dG4_u
& dG3_u >= 8
& dG4_u >= 3


 (***** Constraint 2*****)
 dG3_u + dG4_u >= 17
& 17 > dG3_u
& 24 > dG3_u + dG4_u
& dG3_u >= 8
& dG4_u >= 3


 (***** Constraint 3*****)
 24 > dG3_u + dG4_u
& dG3_u >= 17
& dG4_u >= 3


 (***** Constraint 4*****)
 7 > dG4_u
& 24 > dG3_u
& dG3_u + dG4_u >= 24
& dG4_u >= 3


 (***** Constraint 5*****)
 7 > dG4_u
& dG3_u >= 24
& dG4_u >= 3


 (***** Constraint 6*****)
 24 > dG3_u
& dG4_u >= 7
& dG3_u >= 17


 (***** Constraint 7*****)
 dG4_u >= 7
& dG3_u >= 24


 (***** Constraint 8*****)
 17 > dG3_u
& dG3_u >= 8
& dG3_u + dG4_u >= 24


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
		'purpose'    : 'Test EF with observer + depth-limit + project-result (quite basic)',
		'input_files': ['coffeeDrinker-TACAS-within.imi'],
		'options'    : '-mode EF -merge -output-result -depth-limit 10',
		'expectations' : [
			{'file': 'coffeeDrinker-TACAS-within.res' , 'content' : """*)
 p_coffee > 0
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
			{'file': 'exActionsNonPreserved.states' , 'content' : """
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
			{'file': 'exActionsNonPreserved.states' , 'content' : """
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
	##------------------------------------------------------------
]

