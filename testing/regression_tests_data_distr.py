#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
#
#             Data for non-regression tests
#               (distributed version only)
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2016/03/14
# Last modified: 2016/03/14
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests_distr = [
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the distributed cartography',
		'input_files': ['flipflop.imi', 'flipflop.v0'],
		'options'    : '-mode cover -distributed sequential -output-result',
		'nb_nodes'   : 4,
		'expectations' : [
		# NOTE: not much to test here, as the number of tiles/states/unsuccessful points/etc. all depend on the relative speed of the workers, and is hence entirely non-deterministic
			{'file': 'flipflop.res' , 'content' : """
Coverage                      : integer-complete
Termination                   : regular termination		
		"""
			} #end statespace file
		] # end expectations
	} # end test case
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