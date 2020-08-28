#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
#
#             Data for non-regression tests
#
# Université Paris 13, LIPN, CNRS, France
# Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
#
# File description  : non-regression tests data
#
# File contributors : Étienne André, Jaime Arias
#
# Created           : 2016/03/14
# Last modified     : 2020/08/28
#************************************************************


#************************************************************
# TESTS TO CARRY
#************************************************************
tests_distr = [
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC sequential (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-distributed sequential',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 3
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 9
Local number of transitions   : 8
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 9
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 7.0
Average number of transitions : 6.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC sequential (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '-distributed sequential',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 0
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 3
Local number of transitions   : 2
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 0
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 0
& 2 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 2
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 2
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 2
& 4 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 4
& 6 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 4
& 6 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 4
& 6 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			# NOTE: actual result
			{'file': 'testBC-grid2-plain.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC sequential (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed sequential',
		'nb_nodes'   : 4,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination		
		"""
			} #end statespace file
		] # end expectations
	} # end test case
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC random5 (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-distributed random5',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 3
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 9
Local number of transitions   : 8
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC random5 (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '-distributed random5',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 0
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 3
Local number of transitions   : 2
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 0
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 0
& 2 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 2
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 2
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 2
& 4 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 4
& 6 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 4
& 6 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 4
& 6 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			# NOTE: actual result
			{'file': 'testBC-grid2-plain.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC random+seq (flip-flop with 2 nodes)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed random5',
		'nb_nodes'   : 2,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
		"""
			} #end statespace file
		] # end expectations
	} # end test case
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC random+seq (flip-flop with 4 nodes)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed random5',
		'nb_nodes'   : 4,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
		"""
			} #end statespace file
		] # end expectations
	} # end test case
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC random+seq (flip-flop with 40 nodes)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed random5',
		'nb_nodes'   : 40,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
		"""
			} #end statespace file
		] # end expectations
	} # end test case
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC shuffle (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-distributed shuffle',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 3
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 9
Local number of transitions   : 8
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 9
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 7.0
Average number of transitions : 6.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC shuffle (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '-distributed shuffle',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 0
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 3
Local number of transitions   : 2
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 0
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 0
& 2 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 2
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 2
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 2
& 4 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 4
& 6 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 4
& 6 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 4
& 6 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			# NOTE: actual result
			{'file': 'testBC-grid2-plain.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC shuffle (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed shuffle',
		'nb_nodes'   : 4,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination		
		"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC static (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-distributed static',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 3
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 9
Local number of transitions   : 8
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 9
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 7.0
Average number of transitions : 6.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC static (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '-distributed static',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 0
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 3
Local number of transitions   : 2
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 0
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 0
& 2 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 2
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 2
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 2
& 4 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 4
& 6 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 4
& 6 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 4
& 6 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			# NOTE: actual result
			{'file': 'testBC-grid2-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 36
Number of tiles computed      : 16
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 20
Average number of states      : 5.0
Average number of transitions : 4.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
		
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC dynamic (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-distributed dynamic',
		'nb_nodes'   : 4,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
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
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'flipflop.res' , 'content' : """
 dG3_u + dG4_u >= 24
& dG3_u >= 8
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination		
		"""
			} #end statespace file
		] # end expectations
	} # end test case
			
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC dynamic (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-distributed dynamic',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 8
Local number of transitions   : 7
"""
			} # end BC file
			, 
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 3
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 9
Local number of transitions   : 8
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 9
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 7.0
Average number of transitions : 6.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC dynamic (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '-distributed dynamic',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 0
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 3
Local number of transitions   : 2
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 0
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 0
& 2 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 2
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 4
Local number of transitions   : 3
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 2
& 4 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 2
& 4 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 0
& p2 >= 4
& 6 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 5
Local number of transitions   : 4
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 2
& p2 >= 4
& 6 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 6
Local number of transitions   : 5
"""
			} # end BC file
			, 
			{'file': 'testBC-grid2-plain.res' , 'content' : """
:
 p1 >= 4
& p2 >= 4
& 6 > p2
& 6 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 7
Local number of transitions   : 6
"""
			} # end BC file
			, 
			# NOTE: actual result
			{'file': 'testBC-grid2-plain.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
"""
#Average number of states      : 5.1
#Average number of transitions : 4.1
#(************************************************************)
#GENERAL STATISTICS
#(************************************************************)
#------------------------------------------------------------
#Number of integers in v0      : 36
#Number of tiles computed      : 16
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC mode cover with sequential distribution (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-cover.imiprop'],
		'options'    : '-no-random -distributed sequential',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 0
& 1 > p2
& 1 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 1
& 1 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 2
& 1 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 3
& 1 > p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 0
& 2 > p2
& 1 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 11
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
 p1 >= 0
& p2 >= 2
& 4 >= p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p1 >= 2
& p2 >= 2
& 4 >= p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 11
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
Coverage                                : integer-complete
Termination                             : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: BC mode cover with shuffle distribution (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-cover.imiprop'],
		'options'    : '-no-random -distributed shuffle',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 0
& 1 > p2
& 1 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 1
& 1 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 2
& 1 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 0
& p1 >= 3
& 1 > p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 0
& 2 > p2
& 1 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 11
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 1
& 2 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 12
"""
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
 p1 >= 0
& p2 >= 2
& 4 >= p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 10
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			, 
			{'file': 'diffBCPRPC.res' , 'content' : """
 p1 >= 2
& p2 >= 2
& 4 >= p2
& 4 >= p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 11
"""
	# NOTE: used to be `bad` until version < 3
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
Coverage                                : integer-complete
Termination                             : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: PRPC with sequential distribution (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random -distributed sequential',
		'nb_nodes'   : 4,
		'expectations' : [
			{'file': 'diffBCPRPC.res' , 'content' : """
 BEGIN CONSTRAINT
 2 > p1
& p2 >= 0
& 2 > p2
& p1 >= 0
<good|bad>
 p2 >= 0
& p1 >= 2
& 4 >= p1
& 4 >= p2
OR
  p2 >= 2
& p1 >= 0
& 4 >= p1
& 4 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad
"""
			} # end BC file
			, 
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: PRPC with shuffle distribution (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random -distributed shuffle',
		'nb_nodes'   : 4,
		'expectations' : [
			{'file': 'diffBCPRPC.res' , 'content' : """
 BEGIN CONSTRAINT
 2 > p1
& p2 >= 0
& 2 > p2
& p1 >= 0
<good|bad>
 p2 >= 0
& p1 >= 2
& 4 >= p1
& 4 >= p2
OR
  p2 >= 2
& p1 >= 0
& 4 >= p1
& 4 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad
"""
			} # end BC file
			, 
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: PRPC with static distribution (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random -distributed static',
		'nb_nodes'   : 4,
		'expectations' : [
			{'file': 'diffBCPRPC.res' , 'content' : """
 BEGIN CONSTRAINT
 2 > p1
& p2 >= 0
& 2 > p2
& p1 >= 0
<good|bad>
 p2 >= 0
& p1 >= 2
& 4 >= p1
& 4 >= p2
OR
  p2 >= 2
& p1 >= 0
& 4 >= p1
& 4 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad
"""
			} # end BC file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: PRPC with dynamic distribution (on the case study BC vs. PRPC), 4 nodes',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random -distributed dynamic',
		'nb_nodes'   : 4,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'diffBCPRPC.res' , 'content' : """
 BEGIN CONSTRAINT
 2 > p1
& p2 >= 0
& 2 > p2
& p1 >= 0
<good|bad>
 p2 >= 0
& p1 >= 2
& 4 >= p1
& 4 >= p2
OR
  p2 >= 2
& p1 >= 0
& 4 >= p1
& 4 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad
"""
			} # end BC file
			, 
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Distributed cartography: PRPC with dynamic distribution (on the case study BC vs. PRPC), 8 nodes',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random -distributed dynamic',
		'nb_nodes'   : 8,
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'diffBCPRPC.res' , 'content' : """
 BEGIN CONSTRAINT
 2 > p1
& p2 >= 0
& 2 > p2
& p1 >= 0
<good|bad>
 p2 >= 0
& p1 >= 2
& 4 >= p1
& 4 >= p2
OR
  p2 >= 2
& p1 >= 0
& 4 >= p1
& 4 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad
"""
			} # end BC file
			, 
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
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