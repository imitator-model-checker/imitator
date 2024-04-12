#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
#
#             Data for non-regression tests
#
# Université Sorbonne Paris Nord, LIPN, CNRS, France
# Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
#
# File description  : non-regression tests data
#
# File contributors : Étienne André, Jaime Arias, Mikael Bisgaard Dahlsen-Jensen, Benjamin Loillier
#
# Created           : 2015/10/23
#************************************************************



#************************************************************
# TESTS TO CARRY
#************************************************************
tests = [

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TEMPLATE
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	##------------------------------------------------------------
	#{
		## Test version             : 1
		## Test author              : XXXXXXXXXX
		## Test since               : 2024/03/33
		## Last modified            : 2024/03/33
		## Test for IMITATOR version: 3.4
		#'purpose'    : 'Test something',
		#'tags'       : 'syntax,semantic,translation',
		#'input_files': ['somemodel.imi'],
		#'options'    : '-mode checksyntax',
		#'expectations' : [
			#{'file': 'somemodel.res' , 'content' : """
#here the content to check
		#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# FILE NOT FOUND TESTS
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/01/22
		# Last modified            : 2021/01/22
		# Test for IMITATOR version: 3
		'purpose'    : 'Test the absence of model',
		'input_files': ['thisfiledoesnotexist.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'thisfiledoesnotexist.res' , 'content' : """
Error                                   : model file not found
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/01/22
		# Last modified            : 2021/01/22
		# Test for IMITATOR version: 3
		'purpose'    : 'Test the absence of property',
		'input_files': ['testL.imi', 'thisfiledoesnotexist.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testL.res' , 'content' : """
Error                                   : property file not found
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	# TODO: same + `-mode checksyntax`

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TESTS TO CHECK THE SYNTAX
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/02
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test syntax rejection: non-linear clock',
		'input_files': ['testSyntaxNonLinearClock.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'testSyntaxNonLinearClock.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/13
		# Last modified            : 2021/07/13
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test new syntactic features of v3.1',
		'input_files': ['testSyntax31.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'testSyntax31.res' , 'content' : """
Number of IPTAs                         : 1
Number of clocks                        : 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# SYNTACTIC TESTS (statistics on models)
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Last modified            : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (L-PTA)',
		'input_files': ['testL.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testL.res' , 'content' : """
L/U subclass                            : L-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/06/25
		# Last modified            : 2021/06/25
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test model syntax check even when a (non-existing) property is passed',
		'input_files': ['testL.imi', 'nonexistingfile.imiprop'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testL.res' , 'content' : """
L/U subclass                            : L-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (U-PTA)',
		'input_files': ['testU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testU.res' , 'content' : """
L/U subclass                            : U-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (L/U-PTA)',
		'input_files': ['testLU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testLU.res' , 'content' : """
L/U subclass                            : L/U-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (none)',
		'input_files': ['testNotLU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testNotLU.res' , 'content' : """
L/U subclass                            : not L/U
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (flip-flop: U)',
		'input_files': ['flipflop.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
L/U subclass                            : U-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (JR15: L/U)',
		'input_files': ['JLR-TACAS13.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'JLR-TACAS13.res' , 'content' : """
L/U subclass                            : L/U-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Last modified            : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the L/U-nature (L-PTA with negative coefficients)',
		'input_files': ['testLneg.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testLneg.res' , 'content' : """
L/U subclass                            : L-PTA
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		## Test version             : 1
		## Test since               : 2019/07/09
		## Last modified            : 2019/07/09
		## Test disabled            : 2021/04/27
		## Test for IMITATOR version: 2.12
		#'purpose'    : 'Test the L/U-nature (L-PTA with p > d)',
		#'input_files': ['testLdiscrete.imi'],
		#'options'    : '-mode checksyntax ',
		#'expectations' : [
			#{'file': 'testLdiscrete.res' , 'content' : """
#L/U subclass                            : L-PTA
		#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/10/19
		# Last modified            : 2021/10/19
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test discrete dependent updates',
		'input_files': ['test-discrete-dependent-updates.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'test-discrete-dependent-updates-statespace.states' , 'content' : """
  pta: l1, i = 1, j = 10 ==> 
		"""
			} # end result file
			,
			{'file': 'test-discrete-dependent-updates-statespace.states' , 'content' : """
  pta: l2, i = 0, j = 1 ==>
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of silent actions: no',
		'input_files': ['testLU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testLU.res' , 'content' : """
Has silent actions?                            : false
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of silent actions: yes',
		'input_files': ['testNotLU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testNotLU.res' , 'content' : """
Has silent actions?                            : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of silent actions: JR15',
		'input_files': ['JLR-TACAS13.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'JLR-TACAS13.res' , 'content' : """
Has silent actions?                            : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of strong determinism: yes',
		'input_files': ['testLU.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testLU.res' , 'content' : """
Is strongly deterministic?              : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of strong determinism: yes',
		'input_files': ['testStrongDet.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testStrongDet.res' , 'content' : """
Is strongly deterministic?              : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of strong determinism: yes',
		'input_files': ['testStrongDet2.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testStrongDet2.res' , 'content' : """
Is strongly deterministic?              : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/07/09
		# Test for IMITATOR version: 2.12
		'purpose'    : 'Test the detection of strong determinism: no',
		'input_files': ['testNotStrongDet.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'testNotStrongDet.res' , 'content' : """
Is strongly deterministic?              : false
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TESTS FOR VALIDITY SYNTHESIS
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: unsatisfiable initial conditions (clock constraint)',
		'input_files': ['unsatisfiableInitStateClocks.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unsatisfiableInitStateClocks.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: unsatisfiable initial conditions (clock constraint + invariant)',
		'input_files': ['unsatisfiableInitStateClocksInv.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unsatisfiableInitStateClocksInv.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: unsatisfiable initial conditions (discrete+clock constraint)',
		'input_files': ['unsatisfiableInitStateDiscrete.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unsatisfiableInitStateDiscrete.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: unsatisfiable initial conditions (unsatisfiable discrete invariant)',
		'input_files': ['unsatisfiableInitStateDiscreteInv.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unsatisfiableInitStateDiscreteInv.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: basic model with parametric invariant',
		'input_files': ['safety/safety-initial-state.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state.res' , 'content' : """
BEGIN CONSTRAINT
p >= 3
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: basic model with parametric invariant (variant)',
		'input_files': ['safety/safety-initial-state2.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
p >= 3
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2022/10/07
		# Last modified            : 2022/10/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test validity synthesis: basic model with parametric invariant and negative variable',
		'input_files': ['safety/safety-initial-state-neg.imi', 'basic-properties/synth-valid.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state-neg.res' , 'content' : """
BEGIN CONSTRAINT
 p2 + 3 >= 0
& 0 >= 3 + p1
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test auto-removed parameter does not crash the initial constraint checking',
		'tags'       : 'syntax',
		'input_files': ['syntax/parameter-autoremove.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'parameter-autoremove.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: strange way to check valid syntax
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/03/09
		# Last modified            : 2023/03/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test auto-removed variable doesn’t crash a function return',
		'tags'       : 'syntax,functions',
		'input_files': ['functions/removed-function.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'removed-function.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: strange way to check valid syntax
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------


	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/22
		# Last modified            : 2023/08/22
		# Test for IMITATOR version: 3.4-beta
		'purpose'    : 'Test that unused functions syntax is still checked',
		'tags'       : 'syntax,functions',
		'input_files': ['functions/unused-function-invalid-syntax.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'unused-function-invalid-syntax.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/22
		# Last modified            : 2023/08/22
		# Test for IMITATOR version: 3.4-beta
		'purpose'    : 'Test that variables names are checked in function calls',
		'tags'       : 'syntax,functions',
		'input_files': ['functions/undeclared-var-in-function-call.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'undeclared-var-in-function-call.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TESTS ON UNSATISFIABLE INITIAL STATE
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/05
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test unsatisfiable initial conditions (clock constraint)',
		'input_files': ['unsatisfiableInitStateClocks.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'unsatisfiableInitStateClocks.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/05
		# Last modified            : 2021/07/05
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test state space (unsatisfiable initial conditions) even when a (useless, and non-existing) property is passed',
		'input_files': ['unsatisfiableInitStateClocks.imi', 'nonexistingfile.imiprop'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'unsatisfiableInitStateClocks.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/05
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test unsatisfiable initial conditions (clock constraint + invariant)',
		'input_files': ['unsatisfiableInitStateClocksInv.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'unsatisfiableInitStateClocksInv.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/05
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test unsatisfiable initial conditions (discrete+clock constraint)',
		'input_files': ['unsatisfiableInitStateDiscrete.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'unsatisfiableInitStateDiscrete.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/05
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test unsatisfiable initial conditions (unsatisfiable discrete invariant)',
		'input_files': ['unsatisfiableInitStateDiscreteInv.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'unsatisfiableInitStateDiscreteInv.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TESTS ON DISCRETE VARIABLES
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/02/05
		# Last modified            : 2021/02/05
		# Test for IMITATOR version: 3
		'purpose'    : 'Test division by 0',
		'input_files': ['divisionby0.imi'],
		'options'    : '-mode statespace',
		'expectations' : [
			{'file': 'divisionby0.res' , 'content' : """
Error                                   : division by 0
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test static division by zero detected (discrete int in init definition)',
		'tags'       : 'syntax',
		'input_files': ['syntax/division-by-zero-model-int-init.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'division-by-zero-model-int-init.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: only way to check that syntax is correct?!
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test static division by zero detected (discrete rational in init definition)',
		'tags'       : 'syntax',
		'input_files': ['syntax/division-by-zero-model-rational-init.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'division-by-zero-model-rational-init.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: only way to check that syntax is correct?!
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test static division by zero detected (constant int in definition)',
		'tags'       : 'syntax',
		'input_files': ['syntax/division-by-zero-model-int-def.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'division-by-zero-model-int-def.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: only way to check that syntax is correct?!
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test static division by zero detected (constant rational in definition)',
		'tags'       : 'syntax',
		'input_files': ['syntax/division-by-zero-model-rational-def.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'division-by-zero-model-rational-def.res' , 'content' : """
Number of IPTAs                         : 1
		""" # HACK: only way to check that syntax is correct?!
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test static division by zero detected in property',
		'tags'       : 'syntax',
		'input_files': ['syntax/division-by-zero-property.imi', 'syntax/division-by-zero-property.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'division-by-zero-property.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/15
		# Last modified            : 2024/03/15
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test that undeclared variables are detected in updates',
		'tags'       : 'syntax',
		'input_files': ['syntax/undeclared-variable-update.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'undeclared-variable-update.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test variable elimination in Boolean expressions',
		'input_files': ['testConditions.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'testConditions-statespace.states' , 'content' : """
  STATE 0:
  pta: l1, i = 0, j = 0, l = 0 ==>
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test evaluation of <if> branch in conditional update',
		'input_files': ['testConditions.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'testConditions-statespace.states' , 'content' : """
	STATE 1:
	pta: l2, i = 0, j = 0, l = 15 ==>
	& x >= 0
	& p = 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test evaluation of <else> branch in conditional update',
		'input_files': ['testConditions.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'testConditions-statespace.states' , 'content' : """
	STATE 2:
  pta: l1, i = 2, j = 0, l = 15 ==>
	& x >= 2
	& p = 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test controllable keyword',
		'tags'       : 'syntax,controllable,ptg',
		'input_files': ['parametric_timed_games/controllable_actions.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'controllable_actions-regenerated.imi' , 'content' : """
controllable actions: b;


var

 	x
		: clock;

	p
		: parameter;



(************************************************************)
  automaton pta
(************************************************************)
actions: a, b, c;
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test uncontrollable keyword',
		'tags'       : 'syntax,controllable,ptg',
		'input_files': ['parametric_timed_games/uncontrollable_actions.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'uncontrollable_actions-regenerated.imi' , 'content' : """
controllable actions: a, c;


var

 	x
		: clock;

	p
		: parameter;



(************************************************************)
  automaton pta
(************************************************************)
actions: a, b, c;
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test controllable keyword + include',
		'tags'       : 'syntax,controllable,ptg,include',
		'input_files': ['parametric_timed_games/include_controllable_actions.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'include_controllable_actions-regenerated.imi' , 'content' : """
controllable actions: a, b;

var
	x
		: clock;

	p
		: parameter;



(************************************************************)
 automaton pta1
(************************************************************)
 actions: a, b, c;
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test controllable keyword + include with inconsistent inclusion (WARNING, this test should perhaps rather fail?)',
		'tags'       : 'syntax,controllable,ptg,include',
		'input_files': ['parametric_timed_games/include_controllable_actions_inconsistent.imi'],
		'options'    : '-imi2IMI',
		# WARNING: this test so far only triggers a warning, but the model could be rejected
		'expectations' : [
			{'file': 'include_controllable_actions_inconsistent-regenerated.imi' , 'content' : """
controllable actions: b;

var
	x
		: clock;

	p
		: parameter;



(************************************************************)
 automaton pta1
(************************************************************)
 actions: a, b, c;
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test include files [1]',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-1a.imi'],
		'options'    : '-imi2IMI -no-var-autoremove',
		'expectations' : [
			{'file': 'example-include-1a-regenerated.imi' , 'content' : """
var
	x
		: clock;

	i
		: rational;

	p
		: parameter;


(************************************************************)
 automaton pta1
(************************************************************)
 actions: a;

loc l1: invariant True
	when  x = 1 do {x := 0;}  sync a goto l1;
 end (* pta1 *)
(************************************************************)


(************************************************************)
 automaton pta2
(************************************************************)
 actions: a;

loc l1: invariant True
	when  x = 1 do {x := 0;}  sync a goto l1;
 end (* pta2 *)
(************************************************************)


(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete = 
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[pta1] := l1, 
		loc[pta2] := l1,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
		i := 0
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous = 
		&  p >= 0
		& x = 0
	;
}


(************************************************************)
(* The end *)
(************************************************************)
end
			"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test include files [2]',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-2a.imi'],
		'options'    : '-imi2IMI -no-var-autoremove',
		'expectations' : [
			{'file': 'example-include-2a-regenerated.imi' , 'content' : """
var
	y, x
		: clock;

	j, i
		: rational;

	p
		: parameter;



(************************************************************)
 automaton pta1
(************************************************************)
 actions: a;

loc l1: invariant True
	when  p + 1 = x do {x := 0;}  sync a goto l1;
 end (* pta1 *)
(************************************************************)

(************************************************************)
 automaton pta2
(************************************************************)
 actions: a;

loc l1: invariant True
	when  y = 1 do {y := p;}  sync a goto l1;
 end (* pta2 *)
(************************************************************)


(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete = 
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[pta1] := l1, 
		loc[pta2] := l1,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
		j := 0, 
		i := 0
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous = 
		&  p >= 1
		& y = 0
		& x = 0
	;
}



(************************************************************)
(* The end *)
(************************************************************)
end
			"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test include files [3]',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-3a.imi'],
		'options'    : '-imi2IMI -no-var-autoremove',
		'expectations' : [
			{'file': 'example-include-3a-regenerated.imi' , 'content' : """
var
	y, x
		: clock;

	j, i
		: rational;

	p
		: parameter;

(************************************************************)
 automaton pta1
(************************************************************)
 actions: a;

loc l1: invariant True
	when  p + 1 = x do {x := 0;}  sync a goto l1;
 end (* pta1 *)
(************************************************************)


(************************************************************)
 automaton pta2
(************************************************************)
 actions: a;

loc l1: invariant True
	when  y = 1 do {y := p;}  sync a goto l1;
 end (* pta2 *)
(************************************************************)



(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete = 
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[pta1] := l1, 
		loc[pta2] := l1,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
		j := 0, 
		i := 0
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous = 
		&  p >= 1
		& y = 0
		& x = 0
	;
}

(************************************************************)
(* The end *)
(************************************************************)
end
			"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/19
		# Last modified            : 2021/07/19
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test include: non-existing included file',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-nonexisting-1.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'example-include-nonexisting-1.res' , 'content' : """
Error                                   : model parsing error
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/19
		# Last modified            : 2021/07/19
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test include: non-existing included file with funny characters',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-nonexisting-2.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'example-include-nonexisting-2.res' , 'content' : """
Error                                   : model parsing error
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/19
		# Last modified            : 2021/07/19
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test include: parsing error in included file',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-nonexisting-3a.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'example-include-nonexisting-3a.res' , 'content' : """
Error                                   : model parsing error
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------


	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/19
		# Last modified            : 2021/07/19
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test include: non-existing file in included file',
		'tags'       : 'syntax,include',
		'input_files': ['tests_include_model/example-include-nonexisting-4a.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'example-include-nonexisting-4a.res' , 'content' : """
Error                                   : model parsing error
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/06/09
		# Last modified            : 2022/06/09
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test include: functions',
		'author': 'lbinria',
		'tags':'syntax,include, function',
		'input_files': ['tests_include_model/example-include-functions-1a.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'example-include-functions-1a-regenerated.imi' , 'content' : """
var
	res, j, i
		: int;

function f() : int begin
  var i1 : int = i + 1;
  var i2 : int = (i + 1) * 2;
  return i1 + i2
end

(************************************************************)
 automaton P1
(************************************************************)
 actions: ;

loc s0: invariant True
	when True do {}  (* sync nosync_1*)  goto lend;

accepting loc lend: invariant True
 end (* P1 *)
(************************************************************)


(************************************************************)
 automaton P2
(************************************************************)
 actions: ;

loc s0: invariant True
	when f() + j = 4 do {res := f();}  (* sync nosync_2*)  goto lend;

accepting loc lend: invariant res = 3
 end (* P2 *)
(************************************************************)


(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete =
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[P1] := s0,
		loc[P2] := s0,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
		res := 0,
		j := 1,
		i := 0
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous =
		& True
	;

}


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


	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# AUTOMATIC VARIABLES REMOVAL
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		'purpose'    : 'Test discrete variable automatic removal',
		'input_files': ['testVarElim.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			# NOTE: we just parse the beginning of state 1 to check that the variables are properly removed
			{'file': 'testVarElim-statespace.states' , 'content' : """
  STATE 1:
  pta: l1, i = 0, j = 0 ==>
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test absence of discrete variable automatic removal',
		'input_files': ['testVarElim.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			# NOTE: we just parse the beginning of state 1 to check that the variables are properly removed
			{'file': 'testVarElim-statespace.states' , 'content' : """
  STATE 1:
  pta: l1, i = 0, j = 0, k = 0, l = 0 ==>
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/27
		# Last modified            : 2024/02/27
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test something',
		'tags'       : 'syntax,semantic,translation',
		'input_files': ['autoremove/detection-bool-var.imi', 'autoremove/detection-bool-var-EF.imiprop'],
		'options'    : '-states-description',
		'expectations' : [
			{'file': 'detection-bool-var.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT
		"""
			} # end result file
			,
			{'file': 'detection-bool-var.res' , 'content' : """
			Number of states                        : 2
		"""
			} # end result file
			,
			{'file': 'detection-bool-var-statespace.states' , 'content' : """
  pta: l0, i = 0 ==>
&True
		"""
			} # end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/01
		# Last modified            : 2021/07/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test no clock removal for x=y with x used in the model but not y',
		'tags' : 'auto remove',
		'input_files': ['test_var_remove.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'test_var_remove.res' , 'content' : """
Number of clocks                        : 2
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/01
		# Last modified            : 2021/07/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test correct variable removal for more complex dependencies',
		'tags' : 'auto remove',
		'input_files': ['test_var_remove_2.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'test_var_remove_2.res' , 'content' : """
Number of clocks                        : 3
		""" # TODO: add check for rational and parameter too, etc., perhaps using a -mode statespace -states-description -depth-limit 2
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/08
		# Last modified            : 2021/07/08
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test variable removal for event-recording automata',
		'input_files': ['LALSD14_AIP.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'LALSD14_AIP.res' , 'content' : """
Number of clocks                        : 4
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/08
		# Last modified            : 2021/07/08
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test variable removal for various types',
		'input_files': ['test_removal.imi'],
		'options'    : '-mode checksyntax ',
		'expectations' : [
			{'file': 'test_removal.res' , 'content' : """
Number of clocks                        : 1
		"""
			} # end result file
			,
			{'file': 'test_removal.res' , 'content' : """
Number of parameters                    : 1
Number of discrete variables            : 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# STATE SPACE GENERATION
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : TODO
		# Test since               : TODO
		# Test for IMITATOR version: TODO
		'purpose'    : 'Test the state space',
		'input_files': ['flipflop.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove', #TODO: re-do without '-no-var-autoremove'
		'expectations' : [
			{'file': 'flipflop-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  input: Input0, g1: G10011, g2: G2101, g3: G30011, g4: G410 ==>
& 5 >= s
& dG3_u >= 8
& dG4_u >= 3
& s >= 0
& ckG1 >= s
& ckG2 >= s
& ckG3 >= s
& ckG4 >= s

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 1:
  input: Input1, g1: G11011, g2: G2101, g3: G30011, g4: G410 ==>
& 12 >= s
& dG3_u >= 8
& dG4_u >= 3
& s >= 5
& ckG2 >= s
& ckG3 >= s
& ckG4 >= s
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 2:
  input: Input1, g1: G11010, g2: G2001, g3: G30011, g4: G410 ==>
& 15 >= s
& dG3_u >= 8
& dG4_u >= 3
& s >= 12
& ckG2 >= s
& ckG3 >= s
& ckG4 >= s
& s = 5 + ckG1

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 3:
  input: Input2, g1: G11110, g2: G2011, g3: G30111, g4: G410 ==>
& dG3_u + 15 >= s
& 32 >= s
& dG3_u >= 8
& dG4_u >= 3
& ckG2 >= s
& s >= 15
& ckG4 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 4:
  input: Input3, g1: G10110, g2: G2011, g3: G30111, g4: G410 ==>
& 39 >= s
& dG3_u + 15 >= s
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& ckG4 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 5:
  input: Input2, g1: G11110, g2: G2011, g3: G30110, g4: G400 ==>
& 32 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 23 + ckG4
& ckG4 >= 0
& ckG2 >= s
& dG4_u >= ckG4
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 6:
  input: Input4, g1: G10010, g2: G2001, g3: G30011, g4: G410 ==>
& dG3_u >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 39
& s = 0
& ckG1 = 0
& ckG3 = 24

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 24

  /************************************************************/
  STATE 7:
  input: Input3, g1: G10110, g2: G2011, g3: G30110, g4: G400 ==>
& 39 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 32 + ckG4
& ckG4 >= 0
& ckG2 >= s
& dG4_u >= ckG4
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 8:
  input: Input3, g1: G10110, g2: G2011, g3: G30110, g4: G400 ==>
& 39 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= ckG4
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& s >= 23 + ckG4
& ckG4 + 32 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u + dG4_u >= 17
& dG4_u >= 3
& dG3_u >= 8

  /************************************************************/
  STATE 9:
  input: Input2, g1: G11110, g2: G2011, g3: G31110, g4: G401 ==>
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 23 + ckG4
& ckG4 >= 3
& ckG2 >= s
& 32 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 10:
  input: Input4, g1: G10010, g2: G2001, g3: G30010, g4: G400 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 0
& 7 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 0

  Projection onto the parameters:
   dG3_u + dG4_u >= 24
& dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 11:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401 ==>
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& s >= 32 + ckG4
& ckG4 >= 3
& ckG2 >= s
& 39 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 12:
  input: Input4, g1: G10010, g2: G2001, g3: G30010, g4: G400 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 0

  Projection onto the parameters:
   dG4_u >= 7
& dG3_u >= 8
& dG3_u + dG4_u >= 24

  /************************************************************/
  STATE 13:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401 ==>
& 39 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u + s >= 32 + ckG4
& dG4_u >= 3
& s >= 32
& s >= 23 + ckG4
& ckG4 >= 3
& ckG2 >= s
& ckG4 + 32 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u + dG4_u >= 17
& dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 14:
  input: Input3, g1: G10110, g2: G2011, g3: G31110, g4: G401 ==>
& 39 >= s
& dG3_u + ckG4 + 15 >= s
& dG4_u >= 3
& ckG2 >= s
& s >= 32
& s >= 23 + ckG4
& ckG4 + 29 >= s
& s = 5 + ckG1
& s = 15 + ckG3

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 15:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401 ==>
& dG3_u + ckG4 >= 24
& dG4_u + 7 >= ckG4
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 24

  Projection onto the parameters:
   dG3_u + dG4_u >= 17
& dG3_u >= 8
& dG4_u >= 3

  /************************************************************/
  STATE 16:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 7
& 16 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 0

  Projection onto the parameters:
   dG4_u >= 7
& dG3_u >= 8
& dG3_u + dG4_u >= 24

  /************************************************************/
  STATE 17:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 3
& 7 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 24

  Projection onto the parameters:
   dG3_u >= 17
& dG4_u >= 3

  /************************************************************/
  STATE 18:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= ckG4
& ckG2 >= 39
& ckG4 >= 3
& 7 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 0

  Projection onto the parameters:
   dG4_u >= 3
& dG3_u >= 17
& dG3_u + dG4_u >= 24

  /************************************************************/
  STATE 19:
  input: Input4, g1: G10010, g2: G2001, g3: G31010, g4: G401 ==>
& dG3_u + ckG4 >= 24
& dG4_u >= 3
& ckG2 >= 39
& ckG4 >= 10
& 16 >= ckG4
& s = 0
& ckG1 = 0
& ckG3 = 24

  Projection onto the parameters:
   dG3_u >= 8
& dG4_u >= 3

/************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "dUp"
  s_1 -> s_2 via "qG1Down"
  s_2 -> s_3 via "ckUp"
  s_3 -> s_4 via "dDown"
  s_3 -> s_5 via "qG3Down"
  s_4 -> s_6 via "ckDown"
  s_4 -> s_7 via "qG3Down"
  s_5 -> s_8 via "dDown"
  s_5 -> s_9 via "qUp"
  s_7 -> s_10 via "ckDown"
  s_7 -> s_11 via "qUp"
  s_8 -> s_12 via "ckDown"
  s_8 -> s_13 via "qUp"
  s_9 -> s_14 via "dDown"
  s_10 -> s_18 via "qUp"
  s_11 -> s_17 via "ckDown"
  s_12 -> s_16 via "qUp"
  s_13 -> s_15 via "ckDown"
  s_14 -> s_19 via "ckDown"
			"""
			} #end statespace file
		] # end expectations
	} # end test case
	,
	##------------------------------------------------------------
	#{
		## Test version: 2.11
		## Test since  : 2019/03/07
		## Test until  : 2020/03/20
		## Reason for removal: projectresult is now in the property, so a state space cannot be projected
		#'purpose'    : 'Test state space with projection',
		#'input_files': ['testProjectP.imi'],
		#'options'    : '-mode statespace -states-description',
		#'expectations' : [
			#{'file': 'testProjectP-statespace.states' , 'content' : """
  #DESCRIPTION OF THE STATES

  #/************************************************************/
  #INITIAL
  #STATE 0:
  #pta: l1 ==>
#& pabs >= 0
#& p1 >= x
#& p3 >= 0
#& x >= 0
#& x = xabs

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 1:
  #pta: l2 ==>
#& pabs >= 0
#& p1 >= 0
#& p3 >= x
#& x >= 0
#& p1 + x = xabs

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 2:
  #pta: lfinal ==>
#& pabs >= 0
#& p3 >= 0
#& pabs = p1
#& x = 0
#& pabs = xabs

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs = p1

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 3:
  #pta: l3 ==>
#& 2 >= x
#& pabs >= 0
#& p1 >= 0
#& p3 >= 0
#& x >= 0
#& p1 + p3 + x = xabs

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 4:
  #pta: lfinal ==>
#& pabs >= p1
#& p1 >= 0
#& pabs = p1 + p3
#& x = 0
#& pabs = xabs

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs = p1 + p3

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 5:
  #pta: l4 ==>
#& 2 >= x
#& pabs >= 0
#& p1 >= 0
#& p3 >= 2
#& x >= 0
#& p1 + x + 2 = xabs

  #Projection onto the parameters:
   #p3 >= 2
#& p1 >= 0
#& pabs >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 6:
  #pta: lfinal ==>
#& pabs >= 2
#& p3 >= 2
#& pabs = 2 + p1
#& x = 0
#& pabs = xabs

  #Projection onto the parameters:
   #p3 >= 2
#& pabs >= 2
#& pabs = 2 + p1

  #Projection onto selected parameters {pabs}:
   #pabs >= 2

  #/************************************************************/
  #STATE 7:
  #pta: l4 ==>
#& 2 >= x
#& pabs >= 0
#& p1 + p3 + x + 2 >= xabs
#& p1 >= 0
#& p3 >= 0
#& x >= 0
#& xabs >= p1 + p3 + x

  #Projection onto the parameters:
   #p3 >= 0
#& p1 >= 0
#& pabs >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #/************************************************************/
  #STATE 8:
  #pta: lfinal ==>
#& p1 + p3 + 2 >= pabs
#& pabs >= p1 + p3
#& p1 >= 0
#& p3 >= 0
#& x = 0
#& pabs = xabs

  #Projection onto the parameters:
   #p1 + p3 + 2 >= pabs
#& pabs >= p1 + p3
#& p3 >= 0
#& p1 >= 0

  #Projection onto selected parameters {pabs}:
   #pabs >= 0

  #DESCRIPTION OF THE TRANSITIONS
  #s_0 -> s_1 via "a"
  #s_0 -> s_2 via "a"
  #s_1 -> s_3 via "a"
  #s_1 -> s_4 via "a"
  #s_1 -> s_5 via "b"
  #s_1 -> s_6 via "b"
  #s_3 -> s_7 via "a"
  #s_3 -> s_8 via "a"
#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/28
		# Last modified            : 2020/09/28
		# Test for IMITATOR version: 3
		'purpose'    : 'Test the state space (no float conversion)',
		'input_files': ['testFloat.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'testFloat-statespace.states' , 'content' : """
pta: l2, i = 5/4, j = 1/3
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/28
		# Last modified            : 2020/09/28
		# Test for IMITATOR version: 3
		'purpose'    : 'Test the state space (with float conversion)',
		'input_files': ['testFloat.imi'],
		'options'    : '-mode statespace -states-description -output-float -no-var-autoremove',
		'expectations' : [
		# NOTE: cut the end of the float just in case the OS doesn't represent them the same way…
			{'file': 'testFloat-statespace.states' , 'content' : """
pta: l2, i = 5/4 (~ 1.25), j = 1/3 (~ 0.3333333333
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test urgency',
		'input_files': ['testUrgency.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'testUrgency-statespace.states' , 'content' : """
		  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_0 via "a"
  s_0 -> s_1 via "b"
  s_0 -> s_2 via "b"
  s_1 -> s_3 via "c"
  s_2 -> s_4 via "b"
  s_2 -> s_4 via "c"
  s_3 -> s_1 via "b"
  s_3 -> s_3 via "a"
  s_4 -> s_4 via "a"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test synchronization model',
		'input_files': ['testSynchro.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'testSynchro-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta1: l1, pta2: l1, pta3: l1 ==>
& 3 >= x
& x >= 0

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  pta1: l1, pta2: l1, pta3: l2 ==>
& 3 >= x
& x >= 0

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 2:
  pta1: l1, pta2: l1, pta3: l3 ==>
& x >= 0

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 3:
  pta1: l2, pta2: l2, pta3: l3 ==>
& x >= 4

  Projection onto the parameters:
  True

/************************************************************/
DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "c"
  s_1 -> s_2 via "a"
  s_2 -> s_3 via "b"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test updates (printing)',
		'input_files': ['testUpdates.imi'],
		'options'    : '-imi2IMI -no-var-autoremove',
		'expectations' : [
			{'file': 'testUpdates-regenerated.imi' , 'content' : """
urgent loc idle: invariant True
	when True do {i := (3 * i - 1) / (5 * i * i); x := x + 1/2 * p - 7 * i - 1; y := y + 1;} sync a goto idle;
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test updates and inequalities printing order (printing)',
		'input_files': ['testUpdatesInequalitiesOrder.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'testUpdatesInequalitiesOrder-regenerated.imi' , 'content' : """
	when  a > 0
& b > 0
& c > 0 do {a := 0; b := 0; c := 0;}  sync a goto idle;
	when True do {a := 2; b := 3 + 3; c := 0;}  sync a goto idle;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (linear D reachable)',
		'input_files': ['linear_expressions/linear-invariant-d-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-d-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (linear PD reachable)',
		'input_files': ['linear_expressions/linear-invariant-pd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-pd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 10 > p1
& p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (linear XD reachable)',
		'input_files': ['linear_expressions/linear-invariant-xd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-xd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (linear PXD reachable)',
		'input_files': ['linear_expressions/linear-invariant-pxd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-pxd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (linear D reachable)',
		'input_files': ['linear_expressions/linear-invariant-d-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-d-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (linear PD reachable)',
		'input_files': ['linear_expressions/linear-invariant-pd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-pd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 10 > p1
& p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (linear XD reachable)',
		'input_files': ['linear_expressions/linear-invariant-xd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-xd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (linear PXD reachable)',
		'input_files': ['linear_expressions/linear-invariant-pxd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'linear-invariant-pxd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 0
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
		# Test version             : 1
		# Test since               : 2022/05/02
		# Last modified            : 2022/05/02
		# Test for IMITATOR version: 3
		'purpose'    : 'Test that a linear expression multiplying variable and coef (k*v or v*k) work correctly',
		'author': 'lbinria',
		'tags':'linear, expression, behavior',
		'input_files': ['linear_expressions/linear_expr_coef_variable.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'linear_expr_coef_variable-statespace.states' , 'content' : """
  STATE 1:
  P: lend, i1 = 19, i2 = 7, i3 = 7, i4 = 7, r = 3 ==>
&True

  Projection onto the parameters:
  True
		"""
			} # end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear D reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-d-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-d-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear PD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-pd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-pd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 10 > p1
& p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear XD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-xd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-xd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear PXD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-pxd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-pxd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear PD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-pd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-pd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear XD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-xd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-xd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in guards (non-linear PXD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-guard-pxd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-guard-pxd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear D reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-d-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-d-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear PD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-pd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-pd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 10 > p1
& p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear XD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-xd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-xd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear PXD reachable)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-pxd-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-pxd-reachable.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 0
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear PD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-pd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-pd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear XD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-xd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-xd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test expression consistency in invariants (non-linear PXD semantic error)',
		'input_files': ['nonlinear_expressions/nonlinear-invariant-pxd-semantic-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'nonlinear-invariant-pxd-semantic-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test linear expression unary minus in guard (computing)',
		'input_files': ['linear_expressions/unary-minus-linear-expression.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unary-minus-linear-expression.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test linear expression unary minus in guard (printing)',
		'input_files': ['linear_expressions/unary-minus-linear-expression.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'unary-minus-linear-expression-regenerated.imi' , 'content' : """
when  y + 7 > 2*x
& x + y = 5 + i
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2022/10/27
		## Last modified            : 2022/10/27
		## Test for IMITATOR version: 3.3
		## Author 					: lbinria
		'author': 'lbinria',
		'tags': 'parsing, arithmetic, linear, reduction',
		'purpose'    : 'Test linear expression reduction is correct',
		'input_files': ['linear_expressions/linear-expression-reduction.imi'],
		'options'    : '-imi2IMI -no-var-autoremove',
		'expectations' : [
			{'file': 'linear-expression-reduction-regenerated.imi' , 'content' : """
      x1 := 6 * x + 10 * y - 53;
      x2 := 6 * x + 10 * y - 53;
      x3 := y + 5 * x + 5;
      x4 := x - 1;
      x5 := -(6);
      x6 := 2 * x + 11;
      x7 := -(3 * x) + 23 * y + 35;
      x8 := 24 + 12 * x;
      x9 := -(12 * y) + 2 * x + 4 * z + 41;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/10/20
		## Last modified            : 2021/10/20
		## Test for IMITATOR version: 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'tags': 'parsing, arithmetic, float',
		'purpose'    : 'Test that literal floats are parsed as literal rationals',
		'input_files': ['arithmetic_expressions/literal_float.imi'],
		'options'    : '-no-var-autoremove -mode checksyntax',
		'expectations' : [
			{'file': 'literal_float.res' , 'content' : """
 ************************************************************)
------------------------------------------------------------
Number of IPTAs                         : 1
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test arithmetic expression unary minus in guard (computing)',
		'input_files': ['arithmetic_expressions/unary-minus-guard.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'unary-minus-guard.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test arithmetic expression unary minus in guard (printing)',
		'input_files': ['arithmetic_expressions/unary-minus-guard.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'unary-minus-guard-regenerated.imi' , 'content' : """
when  -(i * 2) = -2 * i
& -(i * -2) = 2 * i
& -(i / 2) = (-i) / 2
& -(i / -2) = i / 2
& -(i + i) = -2 * i
& -(i - i) = 0
& -(2 * i - i) = -i
& -(-(i)) = i
& -(-(-(i))) = i do {}  sync a goto lend;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test arithmetic expression unary minus in update (printing)',
		'input_files': ['arithmetic_expressions/unary-minus-update.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'unary-minus-update-regenerated.imi' , 'content' : """
when  i = 0 do {
  i := -(i * 2);
  i := -2 * i;
  i := -(i * -2);
  i := -(i / 2);
  i := (-i) / 2;
  i := -(i / -2);
  i := -(i + i);
  i := -(i - i);
  i := -(2 * i - i);
  i := -(-(i));
  i := -(-(-(i)));
  i := -1 + 1;
  i := -1 - 1;
}  sync a goto lend;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test arithmetic expression with parenthesis (printing)',
		'input_files': ['arithmetic_expressions/arithmetic-expr-printing-1.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'arithmetic-expr-printing-1-regenerated.imi' , 'content' : """
when  z * x / y > x do {}  sync a1 goto lend;
        when  z / (x * y) > x do {}  sync a1 goto lend;
        when  z / (x - y) > x do {}  sync a1 goto lend;
        when  z / (x + y) > x do {}  sync a1 goto lend;
        when  z * (x - y) > x do {}  sync a1 goto lend;
        when  z * (x + y) > x do {}  sync a1 goto lend;
        when  (x - y) / z > x do {}  sync a1 goto lend;
        when  (x + y) / z > x do {}  sync a1 goto lend;
        when  (x - y) * z > x do {}  sync a1 goto lend;
        when  (x + y) * z > x do {}  sync a1 goto lend;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/02/10
		## Last modified            : 2021/02/10
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test arithmetic expression without parenthesis (printing)',
		'input_files': ['arithmetic_expressions/arithmetic-expr-printing-2.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'arithmetic-expr-printing-2-regenerated.imi' , 'content' : """
when  z * x / y > x do {}  sync a1 goto lend;
        when  z / x * y > x do {}  sync a1 goto lend;
        when  z / x - y > x do {}  sync a1 goto lend;
        when  z / x + y > x do {}  sync a1 goto lend;
        when  z * x - y > x do {}  sync a1 goto lend;
        when  z * x + y > x do {}  sync a1 goto lend;
        when  x - y / z > x do {}  sync a1 goto lend;
        when  x + y / z > x do {}  sync a1 goto lend;
        when  x - y * z > x do {}  sync a1 goto lend;
        when  x + y * z > x do {}  sync a1 goto lend;
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# BEGIN : Test Boolean expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'purpose'    : 'Test Boolean True constant (computing)',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-constant-reachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-constant-reachable.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		'purpose'    : 'Test Boolean False constant (computing)',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-constant-unreachable.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-constant-unreachable.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		'purpose'    : 'Test Boolean negation (computing)',
		'tags':'boolean, computing, semantic',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-negation.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-negation.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		'purpose'    : 'Test that a Boolean variable is correctly updated (computing)',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-discrete-var-update.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-discrete-var-update.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		'purpose'    : 'Test Boolean variable update conversion to Uppaal (printing)',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-discrete-var-update.imi'],
		'options'    : '-imi2Uppaal',
		'expectations' : [
			{'file': 'bool-discrete-var-update-uppaal.xml' , 'content' : """
/* Discrete variables declarations (WARNING: these variables can be rational-valued in IMITATOR, but they become integer-valued in Uppaal) */
bool b = true;

/* User defined function declarations (WARNING: some user defined functions may not be well translated) */



/* Action declarations */



	/*------------------------------------------------------------*/
	/* Initial constraint (not interpreted by Uppaal)             */
	/*------------------------------------------------------------*/
	 /* true */
</declaration>

<template><name x="0" y="0">pta</name><declaration>// No local declaration for automaton 'pta'
</declaration>

<location id="id_pta0_loc0" x="0" y="0">
	<name x="0" y="-40">l1</name>
	<label kind="invariant" x="0" y="40"></label></location>

<location id="id_pta0_loc1" x="200" y="0">
	<name x="200" y="-40">l2</name>
	<label kind="invariant" x="200" y="40"></label></location>

<location id="id_pta0_loc2" x="400" y="0">
	<name x="400" y="-40">lend</name>
	<label kind="invariant" x="400" y="40">b</label></location>
 <init ref="id_pta0_loc0"/>

	<transition>
		<source ref="id_pta0_loc0"/>
		<target ref="id_pta0_loc1"/>
		<label kind="guard" x="100" y="40">b</label>
		<label kind="assignment" x="100" y="-40">b = false</label>
	</transition>
	<transition>
		<source ref="id_pta0_loc1"/>
		<target ref="id_pta0_loc2"/>
		<label kind="guard" x="300" y="40">(! (b))</label>
		<label kind="assignment" x="300" y="-40">b = 1 &lt; 2</label>
	</transition>
 </template>
<system>
// List one or more processes to be composed into a system.

system pta;
</system></nta>
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test that a Boolean constant is initialized with consistant type',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-constant-init-type-error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-constant-init-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test that a Boolean variable is initialized with consistent type',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-variable-init-type-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-variable-init-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/11
		## Last modified            : 2021/03/11
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a Boolean variable is updated with a consistent typed expression',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/bool-variable-update-type-error.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-variable-update-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

    #------------------------------------------------------------
    {
        ## Test version             : 1
        ## Test since               : 2021/05/12
        ## Last modified            : 2021/05/31
        ## Test for IMITATOR version: 3.1.0
        ## Author 					: lbinria
        'purpose'    : 'Test that "not" Boolean operator is effective and correct',
		'tags':'boolean, computing, semantic',
        'input_files': ['boolean_expressions/not-operator.imi'],
        'options'    : '-mode statespace -states-description',
        'expectations' : [
            {'file': 'not-operator-statespace.states' , 'content' : """
  /************************************************************/
  STATE 1:
  pta: lend, b1 = False, b2 = True ==> 
&True

  Projection onto the parameters:
  True

  /************************************************************/
		"""
             } # end result file
            ,
        ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/31
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test a complex Boolean comparison',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/complex-bool-comparison.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'complex-bool-comparison-statespace.states' , 'content' : """
  /************************************************************/
  STATE 1:
  pta: l1, b1 = True, b2 = False ==> 
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 2:
  pta: lend, b1 = True, b2 = False ==> 
&True

  Projection onto the parameters:
  True

  /************************************************************/
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/31
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test logical operator priority',
		'tags':'boolean, computing, semantic',
		'input_files': ['boolean_expressions/logical-operator-priority.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'logical-operator-priority-statespace.states' , 'content' : """
  /************************************************************/
  STATE 1:
  pta: l2, b1 = False, b2 = True ==> 
& p >= 0
& x > 5 + p

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a1"
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/23
		# Last modified            : 2024/02/23
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test Boolean operator priority',
		'tags'       : 'boolean,semantic',
		'input_files': ['boolean_expressions/bool-priorities.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-priorities.res' , 'content' : """
BEGIN CONSTRAINT
 p = 0
OR
  p = 2
OR
  p = 4
OR
  p = 6
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/23
		# Last modified            : 2024/02/23
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test Boolean and arithmetic operator priority',
		'tags'       : 'boolean,semantic',
		'input_files': ['boolean_expressions/bool-arithmetic-priorities.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-arithmetic-priorities.res' , 'content' : """
BEGIN CONSTRAINT
  p = 22
 OR
   p = 8
 OR
   p = 24
 OR
   p = 150
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/23
		# Last modified            : 2024/02/23
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test implication in Boolean expressions (model)',
		'tags'       : 'syntax,boolean',
		'input_files': ['boolean_expressions/bool-implication.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-implication.res' , 'content' : """
BEGIN CONSTRAINT
 p = 0
OR
  p = 2
OR
  p = 4
OR
  p = 6
OR
  p = 8
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/23
		# Last modified            : 2024/02/23
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test implication in Boolean expressions (property)',
		'tags'       : 'syntax,boolean',
		'input_files': ['boolean_expressions/bool-implication-property.imi', 'boolean_expressions/bool-implication-property.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-implication-property.res' , 'content' : """
BEGIN CONSTRAINT
  p_1 = 2
 & p_2 = 2
 OR
   p_1 >= 0
 & p_2 = 3
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/23
		# Last modified            : 2024/02/23
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test implication in Boolean expressions (property, mixing locations + int)',
		'tags'       : 'syntax,boolean',
		'input_files': ['boolean_expressions/bool-implication-property-int.imi', 'boolean_expressions/bool-implication-property-int.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'bool-implication-property-int.res' , 'content' : """
BEGIN CONSTRAINT
  p_1 = 2
 & p_2 = 2
 OR
   p_1 >= 0
 & p_2 = 3
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
		'purpose'    : 'Toy example for exemplifying specifications over Booleans only',
		'author':'lbinria',
		'tags':'boolean, computing, toy',
		'input_files': ['boolean_expressions/bool-exemplifying.imi', 'boolean_expressions/bool-exemplifying.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'bool-exemplifying.res' , 'content' : """
BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "plma": "l1",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "False",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "p1up",
          "transitions": [
            {
              "transition": {
                "PTA": "SBA",
                "guard": " not  (p1)",
                "updates": {
                  "p1": "True"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l1",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "True",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "p2up",
          "transitions": [
            {
              "transition": {
                "PTA": "SBA",
                "guard": " not  (p2)",
                "updates": {
                  "p2": "True"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l1",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "True",
            "p2": "True"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "p1down",
          "transitions": [
            {
              "transition": {
                "PTA": "SBA",
                "guard": "p1",
                "updates": {
                  "p1": "False"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l1",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "False",
            "p2": "True"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a1",
          "transitions": [
            {
              "transition": {
                "PTA": "plma",
                "guard": "True",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l2",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "False",
            "p2": "True"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/4",
          "action": "p2down",
          "transitions": [
            {
              "transition": {
                "PTA": "SBA",
                "guard": "p2",
                "updates": {
                  "p2": "False"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l2",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "False",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "1/4",
            "global_time": "1/4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/4",
          "action": "p1up",
          "transitions": [
            {
              "transition": {
                "PTA": "SBA",
                "guard": " not  (p1)",
                "updates": {
                  "p1": "True"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l2",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "True",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "1/2",
            "global_time": "1/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "check",
          "transitions": [
            {
              "transition": {
                "PTA": "plma",
                "guard": "p1 AND  x > 0",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l3",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "True",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "3",
          "action": "a2",
          "transitions": [
            {
              "transition": {
                "PTA": "plma",
                "guard": " not  (p2) AND  x = 3",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "plma": "l4",
            "SBA": "l1"
          },
          "discrete_variables": {
            "p1": "True",
            "p2": "False"
          },
          "continuous_variables": {
            "x": "3",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test Boolean expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test array expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'author': 'lbinria',
		'purpose'    : 'Test general behavior of array expressions',
		'tags':'array, computing, function, semantic',
		'input_files': ['array_expressions/array.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'array-statespace.states' , 'content' : """
  /************************************************************/
  INITIAL
  STATE 0:
  P: s0, i = 0, my_int_array_to_update = [1, 0], array_of_queue = [queue()], array_of_stack = [stack()], array_of_list = [list([1, 2])], super_nested = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]], nested = [[1, 2], [3, 4]], my_bin_array = [0b1001, 0b0101], my_rat_array = [0, 0, 0], my_bool_array = [False, False] ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  P: lend, i = 0, my_int_array_to_update = [1, 0], array_of_queue = [queue()], array_of_stack = [stack()], array_of_list = [list([0, 0])], super_nested = [[[1, 2], [3, 0]], [[5, 6], [0, 0]]], nested = [[1, 2], [3, 4]], my_bin_array = [0b1001, 0b0101], my_rat_array = [1, 0, 0], my_bool_array = [False, True] ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

    #------------------------------------------------------------
    {
        'author': 'lbinria',
        'purpose'    : 'Test that accessing an out of bound index raise a controlled error',
        'tags':'array, semantic, error',
        'input_files': ['array_expressions/array_out_of_bound_error.imi'],
        'options'    : '-mode statespace -states-description',
        'expectations' : [
            {'file': 'array_out_of_bound_error.res' , 'content' : """
Error                                   : index out of range
		"""
             } # end result file
            ,
        ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

	#------------------------------------------------------------
	# END : Test array expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test list expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'author': 'lbinria',
		'purpose'    : 'Test general behavior of list expressions',
		'tags':'list, computing, function, semantic',
		'input_files': ['list_expressions/list.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'list-statespace.states' , 'content' : """
  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1, list_of_queue = list([queue()]), list_of_stack = list([stack()]), list_array = [list([1]), list([2])], list_list = list([list([1])]), array_list = list([[1, 2]]), binary_word_list = list([0b1100]), bool_list = list([True]), int_list = list([1]), rational_list = list([1]) ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  pta: lend, list_of_queue = list([queue()]), list_of_stack = list([stack()]), list_array = [list([1]), list([2])], list_list = list([list([1])]), array_list = list([[1, 2]]), binary_word_list = list([0b1100]), bool_list = list([True]), int_list = list([2, 1]), rational_list = list([1]) ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test list expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test stack expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'author': 'lbinria',
		'purpose'    : 'Test general behavior of stack expressions',
		'tags':'stack, computing, function, semantic',
		'input_files': ['stack_queue_expressions/stack.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'stack-statespace.states' , 'content' : """
  pta: lend, r = 1, i = 3, s3 = stack([3, 2]), s2 = stack(), s1 = stack([2, 1]) ==>
&True

  Projection onto the parameters:
  True
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test stack expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test queue expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'author': 'lbinria',
		'purpose'    : 'Test general behavior of queue expressions',
		'tags':'queue, computing, function, semantic',
		'input_files': ['stack_queue_expressions/queue.imi'],
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'queue-statespace.states' , 'content' : """
  pta: lend, r = 1, i = 2, s3 = queue([3, 2]), s2 = queue(), s1 = queue([2, 1]) ==>
&True

  Projection onto the parameters:
  True
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test queue expressions
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test type checking
	#------------------------------------------------------------

	# BEGIN : Test type checking on constant declarations

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a constant is not initialized with a rational literal',
		'tags' : 'type checking, semantic, constants, declarations',
		'input_files': ['type_checking/constants_declarations/constant-int-divide-type-error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'constant-int-divide-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that constant declaration is type checked',
		'tags' : 'type checking, semantic, constants, declarations',
		'input_files': ['type_checking/constants_declarations/constant-declaration-bad-type-error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'constant-declaration-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	# END : Test type checking on constant declarations

	# BEGIN : Test type checking on updates

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.0.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that update of an int variable by a literal rational is forbidden',
		'tags': 'type checking, update',
		'input_files': ['type_checking/updates/update-variable-bad-type-error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'update-variable-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/04/02
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.0.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a clock isn’t updated with a bad type',
		'tags': 'type checking, update',
		'input_files': ['type_checking/updates/clock-update-type-error.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'clock-update-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	# END : Test type checking on updates


	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/03/12
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an update expression doesn’t mix different types in addition',
		'input_files': ['type_checking/expression-mixin-type-error-1.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'expression-mixin-type-error-1.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/03/12
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an update expression doesn’t mix different types in multiplication',
		'input_files': ['type_checking/expression-mixin-type-error-2.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'expression-mixin-type-error-2.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/03/12
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an update expression doesn’t mix different types in comparison',
		'input_files': ['type_checking/expression-mixin-type-error-3.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'expression-mixin-type-error-3.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/03/12
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an update expression doesn’t mix different types in "in" expression',
		'input_files': ['type_checking/expression-mixin-type-error-4.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'expression-mixin-type-error-4.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	# BEGIN : Type checking complex tests

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/27
		## Last modified            : 2021/05/27
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test type checking by making a lot of complex operations on multiple types',
		'input_files': ['type_checking/complex-type-checking.imi'],
		'tags':'type checking',
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'complex-type-checking-statespace.states' , 'content' : """
  INITIAL
  STATE 0:
  pta: l1, k = 0, j = 1, b1 = True, b2 = False, i = 0 ==>
& x >= 0
& p >= 0

  Projection onto the parameters:
   p >= 0
		"""
			 } # end result file
			,
			{'file': 'complex-type-checking-statespace.states' , 'content' : """
  STATE 23:
  pta: l2, k = 10, j = 0, b1 = False, b2 = True, i = 1 ==>
& x >= 0
& p > 10

  Projection onto the parameters:
   p > 10
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	# END : Type checking complex test

	# BEGIN : Type checking on variable initializations

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/04/14
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a variable is not initialized with a rational literal',
		'tags': 'type checking, init',
		'input_files': ['type_checking/inits/variable-int-divide-type-error.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'variable-int-divide-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/28
		## Last modified            : 2021/05/28
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a variable of type X cannot be initialized by an expression of type Y',
		'input_files': ['type_checking/inits/init-discrete-bad-type-error.imi'],
		'tags': 'type checking, init',
		'options'    : '',
		'expectations' : [
			{'file': 'init-discrete-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/04/05
		# Last modified            : 2023/04/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test undeclared variables in continuous init',
		'input_files': ['test-wrong-continuous-init-var.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'test-wrong-continuous-init-var.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/04/05
		# Last modified            : 2023/04/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test undeclared variables in discrete init',
		'input_files': ['test-wrong-discrete-init-var.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'test-wrong-discrete-init-var.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# BEGIN : Test new init state section
	#------------------------------------------------------------

# 	#------------------------------------------------------------
# 	{
# 		## Test version             : 1
# 		## Test since               : 2021/03/10
# 		## Last modified            : 2021/03/10
# 		## Test disabled            : 2021/06/01
# 		## Test for IMITATOR version: > 3.0
# 		## Author 					: lbinria
# 		'author': 'lbinria',
# 		'purpose'    : 'Test init state printing - old state zone (printing)',
# 		'tags' : 'semantic, printing, init',
# 		'input_files': ['init_state/old-init-state-printing.imi'],
# 		'options'    : '-imi2IMI',
# 		'expectations' : [
# 			{'file': 'old-init-state-printing-regenerated.imi' , 'content' : """
# var
# 	i, j
# 		: discrete;
#
#
# (************************************************************)
#  automaton pta
# (************************************************************)
#  actions: ;
#
# loc l1: invariant True
# 	when  i = 0
# & j = 0 do {}  (* sync nosync_1*)  goto lend;
#
# accepting loc lend: invariant True
#  end (* pta *)
# (************************************************************)
#
#
# (************************************************************)
#  automaton pta2
# (************************************************************)
#  actions: ;
#
# loc l2: invariant True
# 	when  i = 0
# & j = 0 do {}  (* sync nosync_2*)  goto lend2;
#
# accepting loc lend2: invariant True
#  end (* pta2 *)
# (************************************************************)
#
#
# (************************************************************)
# (* Initial state *)
# (************************************************************)
#
# init := True
# 	(*------------------------------------------------------------*)
# 	(* Initial location *)
# 	(*------------------------------------------------------------*)
# 	& loc[pta] = l1
# 	& loc[pta2] = l2
#
# 	(*------------------------------------------------------------*)
# 	(* Initial discrete assignments *)
# 	(*------------------------------------------------------------*)
# 	& i = 1
# 	& j = 2
#
# 	(*------------------------------------------------------------*)
# 	(* Initial constraint *)
# 	(*------------------------------------------------------------*)
# 	 & True
#
# ;
#
#
# (************************************************************)
# (* The end *)
# (************************************************************)
# end
# 		"""
# 			 } # end result file
# 			,
# 		] # end expectations
# 	} # end test case
# 	#------------------------------------------------------------
#
# 	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/10
		## Last modified            : 2021/06/01
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test imi2IMI for initial state v3.1',
		'tags' : 'semantic, printing, init',
		'input_files': ['init_state/new-init-state-printing.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'new-init-state-printing-regenerated.imi' , 'content' : """
var
	i, j
		: rational;

	b
		: bool;

	k, l
		: int;



(************************************************************)
 automaton pta
(************************************************************)
 actions: ;

loc l1: invariant True
	when b
& i = 0
& j = 0
& k = 0
& l = 0 do {} (* sync nosync_1*)  goto lend;

accepting loc lend: invariant True
 end (* pta *)
(************************************************************)


(************************************************************)
 automaton pta2
(************************************************************)
 actions: ;

loc l2: invariant True
	when b
& i = 0
& j = 0
& k = 0
& l = 0 do {} (* sync nosync_2*)  goto lend2;

accepting loc lend2: invariant True
 end (* pta2 *)
(************************************************************)


(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete =
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[pta] := l1,
		loc[pta2] := l2,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
		i := 1,
		j := 2,
		b := False,
		k := 0,
		l := 0
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous =
		& True
	;

}


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
		## Test version             : 1
		## Test since               : 2021/04/16
		## Last modified            : 2021/04/16
		## Test for IMITATOR version: 3.0.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init of a clock in discrete state section is forbidden - new init state (behavior)',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-clock-in-discrete-state-section-error.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'init-clock-in-discrete-state-section-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/04/16
		## Last modified            : 2021/04/16
		## Test for IMITATOR version: > 3.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init of a parameter in discrete state section is forbidden - new init state (behavior)',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-parameter-in-discrete-state-section-error.imi'],
		'options'    : '-imi2IMI',
		'expectations' : [
			{'file': 'init-parameter-in-discrete-state-section-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/09
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init of a discrete variable in discrete and continuous section at the same time is forbidden - new init state (behavior)',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-variable-at-discrete-and-continuous.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'init-variable-at-discrete-and-continuous.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

# 	#------------------------------------------------------------
# 	{
# 		## Test version             : 1
# 		## Test since               : 2021/03/04
# 		## Last modified            : 2021/05/31
# 		## Test for IMITATOR version: 3.1.0
# 		## Author 					: lbinria
# 		'purpose'    : 'Test that init of an undeclared variable is forbidden - new init state (behavior)',
# 		'tags' : 'semantic, behavior, init',
# 		'input_files': ['init_state/init-discrete-section-not-declared-error.imi'],
# 		'options'    : '',
# 		'expectations' : [
# 			{'file': 'init-discrete-section-not-declared-error.res' , 'content' : """
# Error                                   : invalid model
# 		"""
# 			 } # end result file
# 			,
# 		] # end expectations
# 	} # end test case
# 	#------------------------------------------------------------
#
# 	,


	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/04
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init of a variable with a constant is accepted - new init state (behavior)',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-variable-with-constant.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'init-variable-with-constant-statespace.states' , 'content' : """
  STATE 1:
  pta: lend, b = True ==>
&True

  Projection onto the parameters:
  True
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/27
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init of a variable with a variable is forbidden - new init state',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-variable-with-variable.imi'],
		'options'    : '-no-var-autoremove -mode statespace -states-description',
		'expectations' : [
			{'file': 'init-variable-with-variable.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/22
		# Last modified            : 2023/08/22
		# Test for IMITATOR version: 3.4-beta
		'purpose'    : 'Test that constants can be used in continuous inits, both on left and right side of inequalities',
		'tags'       : 'syntax,init',
		'input_files': ['init_state/constants-in-init.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'constants-in-init.res' , 'content' : """
Number of IPTAs                         : 1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/06/21
		## Last modified            : 2021/06/21
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that init a variable with another variable in continuous init section is forbidden',
		'tags' : 'semantic, behavior, init',
		'input_files': ['init_state/init-using-variable-in-continuous-error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'init-using-variable-in-continuous-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test new init state section
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/28
		## Last modified            : 2021/05/28
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an init expression cannot be ill-typed',
		'input_files': ['type_checking/inits/init-discrete-expression-bad-type-error.imi'],
		'tags': 'type checking, init',
		'options'    : '',
		'expectations' : [
			{'file': 'init-discrete-expression-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/28
		## Last modified            : 2021/05/28
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an init expression cannot mix different types',
		'input_files': ['type_checking/inits/init-discrete-expression-mixin-type-error.imi'],
		'tags': 'type checking, init',
		'options'    : '',
		'expectations' : [
			{'file': 'init-discrete-expression-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/28
		## Last modified            : 2021/05/28
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an init constraint on clock cannot use non rational valued variable(s) or constant(s)',
		'tags': 'type checking, init',
		'input_files': ['type_checking/inits/init-continuous-clock-type-error.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'init-continuous-clock-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/28
		## Last modified            : 2021/05/28
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an init constraint on parameter cannot use non rational valued variable(s) or constant(s)',
		'tags':'type checking, init',
		'input_files': ['type_checking/inits/init-continuous-parameter-type-error.imi'],
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'init-continuous-parameter-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/27
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test init a variable with a complex expression',
		'input_files': ['type_checking/inits/init-expression-complex.imi'],
		'tags': 'type checking, init',
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'init-expression-complex-statespace.states' , 'content' : """
  STATE 1:
  pta: lend, r2 = 7/3, b = True ==>
& 3*p > 7

  Projection onto the parameters:
   3*p > 7
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

# TODO benjamin remove, we can now use int in continuous init
# 	#------------------------------------------------------------
# 	{
# 		## Test version             : 1
# 		## Test since               : 2021/06/21
# 		## Last modified            : 2021/06/21
# 		## Test for IMITATOR version: 3.1.0
# 		## Author 					: lbinria
# 		'author': 'lbinria',
# 		'purpose'    : 'Test that init of non rational valued discrete variables in continuous init section raise an error',
# 		'input_files': ['type_checking/inits/init-non-rational-discrete-in-continuous-error.imi'],
# 		'tags': 'type checking, init',
# 		'options'    : '',
# 		'expectations' : [
# 			{'file': 'init-non-rational-discrete-in-continuous-error.res' , 'content' : """
# Error                                   : invalid model
# 		"""
# 			 } # end result file
# 			,
# 		] # end expectations
# 	} # end test case
# 	#------------------------------------------------------------
#
# 	,

	# END : Type checking on variable initializations

	# BEGIN : Type checking on conditional clauses

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/03/12
		## Last modified            : 2021/05/31
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that the use of a bad typed expression in conditional expression is forbidden',
		'input_files': ['type_checking/conditionals/conditional-expression-bad-type-error.imi'],
		'tags':'type checking, conditional',
		'options'    : '',
		'expectations' : [
			{'file': 'conditional-expression-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	# END : Type checking on conditional clauses

	# BEGIN : Type checking on guards tests

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a term cannot compute on a bad type',
		'input_files': ['type_checking/guards/term-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'term-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a term cannot mix of different types',
		'input_files': ['type_checking/guards/term-mixin-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'term-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a factor cannot mix of different types',
		'input_files': ['type_checking/guards/factor-mixin-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'factor-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a factor cannot compute on a bad type',
		'input_files': ['type_checking/guards/factor-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'factor-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a comparison cannot compute on a bad type',
		'input_files': ['type_checking/guards/compare-operator-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'compare-operator-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a comparison cannot mix different types',
		'input_files': ['type_checking/guards/compare-operator-mixin-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'compare-operator-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an "in" expression cannot compute on a bad type',
		'input_files': ['type_checking/guards/in-operator-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'in-operator-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that an "in" expression cannot mix different types',
		'input_files': ['type_checking/guards/in-operator-mixin-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'in-operator-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a logical expression cannot compute on a bad type (other than bool)',
		'input_files': ['type_checking/guards/logical-operator-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'logical-operator-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a "not" expression cannot compute on a bad type (other than bool)',
		'input_files': ['type_checking/guards/not-operator-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'not-operator-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a "rational_of_int" function cannot compute on a bad type',
		'input_files': ['type_checking/guards/rational-of-int-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'rational-of-int-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/05/25
		## Last modified            : 2021/05/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that a "rational_of_int" function cannot mix different types',
		'input_files': ['type_checking/guards/rational-of-int-mixin-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'rational-of-int-mixin-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/06/07
		## Last modified            : 2021/06/07
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that use of a rational expression as exponent in "pow" function raise an error',
		'input_files': ['type_checking/guards/pow-exponent-bad-type-error.imi'],
		'tags':'type checking',
		'options'    : '',
		'expectations' : [
			{'file': 'pow-exponent-bad-type-error.res' , 'content' : """
Error                                   : invalid model
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/08/25
		## Last modified            : 2021/08/25
		## Test for IMITATOR version: 3.1.0
		## Author 					: lbinria
		'author': 'lbinria',
		'purpose'    : 'Test that properties support type checking',
		'input_files': ['type_checking/typed-property.imi', 'type_checking/typed-property.imiprop'],
		'tags':'type checking, property',
		'options'    : '',
		'expectations' : [
			{'file': 'typed-property.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	# END : Type checking on guards tests
	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/31
		# Last modified            : 2023/07/31
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether undeclared var is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-undeclared-var.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/31
		# Last modified            : 2023/07/31
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether a clock (so far forbidden there) is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-clock.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/02
		# Last modified            : 2023/08/02
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether a parameter (so far forbidden there) is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-parameter.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/31
		# Last modified            : 2023/07/31
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether an unused clock is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-unused-clock.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/02
		# Last modified            : 2023/08/02
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether an unused parameter is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-unused-parameter.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test author              : Étienne André
		# Test since               : 2023/07/31
		# Last modified            : 2023/08/02
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test whether an unused discrete is found in property',
		'tags'       : 'property',
		'input_files': ['properties/dummy.imi', 'properties/property-unused-discrete.imiprop'],
		'options'    : '-depth-limit 2',
		'expectations' : [
			{'file': 'dummy.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/04/05
		# Last modified            : 2023/04/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test void var isn’t allowed',
		'input_files': ['test-void.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'test-void.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/04/05
		# Last modified            : 2023/04/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test void array isn’t allowed',
		'input_files': ['test-void-array.imi'],
		'options'    : '-mode checksyntax',
		'expectations' : [
			{'file': 'test-void-array.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test type checking
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test user defined function
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		## Test version             : 1
		## Test since               : 2021/06/07
		## Last modified            : 2021/06/07
		## Test for IMITATOR version: 3.1.0
		'author': 'lbinria',
		'purpose'    : 'Test that behavior of pow function is correct',
		'input_files': ['functions/pow.imi'],
		'tags': 'semantic, behavior, function',
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'pow-statespace.states' , 'content' : """
  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1, r1 = 3, r2 = 4, projected_result_1 = 0, projected_result_2 = 0, n = 0, i = 3, j = 4 ==> 
& p1 >= 0

  Projection onto the parameters:
   p1 >= 0

  /************************************************************/
  STATE 1:
  pta: l1, r1 = 3, r2 = 4, projected_result_1 = 82, projected_result_2 = 65, n = 1, i = 3, j = 4 ==> 
& p1 >= 0

  Projection onto the parameters:
   p1 >= 0

  /************************************************************/
  STATE 2:
  pta: l1, r1 = 3, r2 = 4, projected_result_1 = 82, projected_result_2 = 65, n = 2, i = 3, j = 4 ==> 
& p1 >= 0

  Projection onto the parameters:
   p1 >= 0

  /************************************************************/
  STATE 3:
  pta: lend, r1 = 3, r2 = 4, projected_result_1 = 82, projected_result_2 = 65, n = 3, i = 3, j = 4 ==> 
& p1 = 82
& p2 = 65

  Projection onto the parameters:
   p1 = 82
& p2 = 65
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 1.1
		## Test since               : 2022/07/19
		## Last modified            : 2023/08/22
		'author': 'lbinria',
		'purpose'    : 'Test all builtin functions',
		'input_files': ['functions/builtin-functions.imi'],
		'tags': 'semantic, behavior, function',
		'imitator-version': '3.4-beta',
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'builtin-functions-statespace.states' , 'content' : """
  STATE 1:
  P: lend, r = 5, r_pow = 25, q_copy = queue([11, 1]), queue_length = 2, queue_is_empty = True, queue_pop = 10, queue_top = 11, q = queue(), s_copy = stack([1, 10]), stack_length = 2, stack_is_empty = True, stack_pop = 11, stack_top = 10, s = stack(), list_length = 5, list_mem = True, list_rev = list([5, 4, 3, 2, 1]), list_cons = list([0, 1, 2, 3, 4, 5]), list_tl = list([2, 3, 4, 5]), list_hd = 1, list_is_empty = False, l = list([1, 2, 3, 4, 5]), a_length = 4, array_mem = True, array_b = [1, 2, 3, 4], aa = [1, 2, 3, 4], bfl = 0b101100, bfr = 0b001011, b = 0b1011, bsl = 0b1100, bsr = 0b0010, bla = 0b0011, blo = 0b1111, blxo = 0b1100, bln = 0b0100, i_pow = 25, i_div = 2, modulo = 1 ==>
&True
		"""
			 } # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/03/16
      ## Last modified            : 2022/03/16
      ## Test for IMITATOR version: 3.3
      'author': 'lbinria',
      'purpose'    : 'Test that evaluation order of function parameters is left to right',
      'input_files': ['functions/function_argument_eval_order.imi'],
      'tags': 'behavior, function',
      'options'    : '-mode statespace -states-description -no-var-autoremove',
      'expectations' : [
        {'file': 'function_argument_eval_order-statespace.states' , 'content' : """
    STATE 1:
    pta: lend, result = 9, s = stack() ==>
    &True

    Projection onto the parameters:
    True
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/03/16
      ## Last modified            : 2022/03/16
      'author': 'lbinria',
      'purpose'    : 'Test some behaviors on user functions',
      'input_files': ['functions/user-function-1.imi'],
      'tags': 'behavior, function',
      'imitator-version': '3.4',
      'options'    : '-mode statespace -states-description -no-var-autoremove',
      'expectations' : [
        {'file': 'user-function-1-statespace.states' , 'content' : """
  INITIAL
  STATE 0:
  P: l0, r_result = 0, lerp_result = 0, r_global = 0, r1 = 1, top_stack = 0, top_queue = 0, i_result = 0, i_result_2 = 0, a_global = [0, 0], i_global = 0, bin = 0b1011, q = queue(), s = stack(), i1 = 1 ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  P: lend, r_result = 4, lerp_result = 1583/200, r_global = 15, r1 = 1, top_stack = 2, top_queue = 0, i_result = 3, i_result_2 = 3, a_global = [0, 1], i_global = 10, bin = 0b0011, q = queue([0, 1, 2]), s = stack([2, 1, 0]), i1 = 1 ==>
&True
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/06/23
      ## Last modified            : 2022/06/23
      'author': 'lbinria',
      'purpose'    : 'Test side-effects detection in user-defined functions',
      'input_files': ['functions/function-side-effects-detect.imi'],
      'tags': 'function, inference',
      'imitator-version': '3.4',
      # verbose low for writing DETAILS in .res
      'options'    : '-no-var-autoremove -verbose low',
      'expectations' : [
        {'file': 'function-side-effects-detect.res' , 'content' : """
  "function_metas": [
    {
      "name": "f1",
      "side-effects": true
    },
    {
      "name": "f2",
      "side-effects": true
    },
    {
      "name": "f3",
      "side-effects": false
    },
    {
      "name": "f4",
      "side-effects": true
    },
    {
      "name": "f5",
      "side-effects": false
    }
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/08/23
		# Last modified            : 2023/08/23
		# Test for IMITATOR version: 3.4-beta
		'purpose'    : 'Test absence of false positive in side-effects detection in user-defined functions',
		'tags'       : 'syntax,semantic,translation',
		'input_files': ['functions/function-no-side-effects-detect.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'function-no-side-effects-detect-statespace.states' , 'content' : """
STATE 1:
  pta: l2, q = queue([4, 2, 99, 1]), i = 99
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/24
      ## Last modified            : 2023/01/24
      'author': 'lbinria',
      'purpose'    : 'Check that returning clock is forbidden',
      'input_files': ['functions/return-clock.imi'],
      'tags': 'function, clock, return',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'return-clock.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

	#-------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/03/14
		# Last modified            : 2023/03/14
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test that a non-void function can be called as a void function',
		'input_files': ['functions/functions-sideeffects-nonvoid.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'functions-sideeffects-nonvoid-statespace.states' , 'content' : """
  STATE 1:
  A: l1, my_global_i = 2, my_global_j = 4
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/24
      ## Last modified            : 2023/01/24
      'author': 'lbinria',
      'purpose'    : 'Check that returning parameter is forbidden',
      'input_files': ['functions/return-param.imi'],
      'tags': 'function, parameter, return',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'return-param.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/25
      ## Last modified            : 2023/01/25
      'author': 'lbinria',
      'purpose'    : 'Check that updating a constant is forbidden',
      'input_files': ['functions/seq-code-bloc-update-constant.imi'],
      'tags': 'code, bloc, constant, update',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'seq-code-bloc-update-constant.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/27
      ## Last modified            : 2023/01/27
      'author': 'lbinria',
      'purpose'    : 'Check that updating a parameter is forbidden',
      'input_files': ['functions/seq-code-bloc-update-parameter.imi'],
      'tags': 'code, bloc, parameter, update',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'seq-code-bloc-update-parameter.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/27
      ## Last modified            : 2023/01/27
      'author': 'lbinria',
      'purpose'    : 'Check that updating a discrete to a parameter is forbidden',
      'input_files': ['functions/seq-code-bloc-update-discrete-with-parameter.imi'],
      'tags': 'code, bloc, parameter, update',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'seq-code-bloc-update-discrete-with-parameter.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/27
      ## Last modified            : 2023/01/27
      'author': 'lbinria',
      'purpose'    : 'Check that updating a discrete with a clock is forbidden',
      'input_files': ['functions/seq-code-bloc-update-discrete-with-clock.imi'],
      'tags': 'code, bloc, parameter, update',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'seq-code-bloc-update-discrete-with-clock.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2023/01/27
      ## Last modified            : 2023/01/27
      'author': 'lbinria',
      'purpose'    : 'Check that nonlinear expressions over clocks of parameters are forbidden',
      'input_files': ['functions/seq-code-bloc-nonlinear-op-continuous.imi'],
      'tags': 'code, bloc, parameter, update',
      'imitator-version': '3.4',
      'options'    : '-no-var-autoremove',
      'expectations' : [
        {'file': 'seq-code-bloc-nonlinear-op-continuous.res' , 'content' : """
Error                                   : invalid model
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/03/13
		# Last modified            : 2023/03/13
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test functions: with/without side-effects, and clock updates allowed in functions',
		'input_files': ['functions/side-effects.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'side-effects-statespace.states' , 'content' : """
  STATE 2:
  pta: l3, t = [10, 10, 3, 4], i = 0, j = 1 ==>
& p >= 0
& x >= 10*p
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/04/05
		# Last modified            : 2023/04/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test that local variables do not duplicate discrete states',
		'input_files': ['functions/nonduplicated-states.imi'],
		'options'    : '-mode statespace -states-description',
		'expectations' : [
			{'file': 'nonduplicated-states-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  A: l1 ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  A: l2 ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
  s_0 -> s_1
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test user defined function
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test dependency graph
	#------------------------------------------------------------

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/06/29
      ## Last modified            : 2022/06/29
      'author': 'lbinria',
      'purpose'    : 'Simple cycle detection',
      'input_files': ['dependency_graph/simple-cycle-detection.imi'],
      'tags': 'behavior, function, cycle, dependency',
        'imitator-version': '3.4',
      # verbose low for writing DETAILS in .res
      'options'    : '-verbose low',
      'expectations' : [
        {'file': 'simple-cycle-detection.res' , 'content' : """
  "cycles": [
    "P -> f -> f"
  ]
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/06/29
      ## Last modified            : 2022/06/29
      'author': 'lbinria',
      'purpose'    : 'Complex cycle detection',
      'input_files': ['dependency_graph/complex-cycle-detection.imi'],
      'tags': 'behavior, function, cycle, dependency',
      'imitator-version': '3.4',
      # verbose low for writing DETAILS in .res
      'options'    : '-verbose low',
      'expectations' : [
        {'file': 'complex-cycle-detection.res' , 'content' : """
"cycles": [
    "P -> f1 -> x -> f2 -> x -> f1",
    "P -> f3 -> z -> y -> x -> f1 -> x -> f2 -> x -> f1"
  ]
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

    #------------------------------------------------------------
    {
      ## Test version             : 1
      ## Test since               : 2022/07/21
      ## Last modified            : 2022/07/21
      'author': 'lbinria',
      'purpose'    : 'Test auto-remove management on updates and user-defined functions',
      'input_files': ['dependency_graph/variable-autoremove-on-seq-update.imi'],
      'tags': 'variable, function, auto-remove, dependency',
      'imitator-version': '3.4',
      'options'    : '-mode statespace -states-description',
      'expectations' : [
        {'file': 'variable-autoremove-on-seq-update-statespace.states' , 'content' : """
P: lend, s = stack([0]), i = 2, j = 0
      """
         } # end result file
        ,
      ] # end expectations
    } # end test case
    #------------------------------------------------------------

    ,

	#------------------------------------------------------------
	# END : Test dependency graph
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'purpose'    : 'Test updates (state space)',
		'input_files': ['testUpdates.imi'],
		'options'    : '-mode statespace -depth-limit 4 -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'testUpdates-statespace.states' , 'content' : """

  /************************************************************/
  INITIAL
  STATE 0:
  autom: idle, i = 1 ==>
& p >= 0
& x = 0
& y = 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 1:
  autom: idle, i = 2/5 ==>
& p >= 0
& 5*p = 38 + 10*x
& y = 1

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 2:
  autom: idle, i = 1/4 ==>
& p >= 0
& 20*p = 131 + 20*x
& y = 2

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 3:
  autom: idle, i = -4/5 ==>
& p >= 0
& 30*p = 39 + 20*x
& y = 3

  Projection onto the parameters:
   p >= 0
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1.1
        ## Test since               : 2023/01/10
        ## Last modified            : 2023/01/10
        ## Test for IMITATOR version: 3.3
        'author': 'lbinria',
        'purpose'    : 'Evaluation order of updates (only 1 PTA)',
        'input_files': ['updates/clock-discrete-order.imi'],
		'tags': 'behavior, update, order',
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'clock-discrete-order-statespace.states' , 'content' : """
  pta: lend, i = 1, j = 1 ==>
& y >= 1
& x + 1 = y
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1.1
        ## Test since               : 2022/03/17
        ## Last modified            : 2023/01/10
        ## Test for IMITATOR version: 3.3
        'author': 'lbinria',
        'purpose'    : 'Evaluation order of updates (1+ PTA)',
        'input_files': ['updates/compound-updates-order.imi'],
		'tags': 'behavior, update, order',
		'options'    : '-mode statespace -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'compound-updates-order-statespace.states' , 'content' : """
pta1: lend, pta2: lend, pta3: lend, r1 = 0, r2 = 2, r3 = 3, top1 = 0, top2 = 1, top3 = 2, s = stack([2, 1, 0]) ==>
& x >= 6
& x = 1 + y
& x = 1 + z
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1
        ## Test since               : 2022/07/19
        ## Last modified            : 2022/07/19
        ## Test for IMITATOR version: 3.3
        'author': 'lbinria',
        'purpose'    : 'Test that constant update is forbidden',
        'input_files': ['updates/constant-update.imi'],
		'tags': 'behavior, update',
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'constant-update.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1
        ## Test since               : 2022/06/23
        ## Last modified            : 2022/06/23
        ## Test for IMITATOR version: 3.3
        'author': 'lbinria',
        'purpose'    : 'Does the model have complex updates ?',
        'input_files': ['updates/has-complex-updates-1.imi'],
		'tags': 'behavior, update',
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'has-complex-updates-1.res' , 'content' : """
Has complex updates?                    : false
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1
        ## Test since               : 2022/06/23
        ## Last modified            : 2022/06/23
        ## Test for IMITATOR version: 3.3
        'author': 'lbinria',
        'purpose'    : 'Does the model have complex updates ?',
        'input_files': ['updates/has-complex-updates-2.imi'],
		'tags': 'behavior, update',
		'options'    : '-no-var-autoremove',
		'expectations' : [
			{'file': 'has-complex-updates-2.res' , 'content' : """
Has complex updates?                    : true
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1.0
        ## Test since               : 2023/01/17
        ## Last modified            : 2023/01/17
        ## Test for IMITATOR version: 3.4
        'author': 'lbinria',
        'purpose'    : 'Check clock rewriting is correct',
        'input_files': ['updates/clock-rewriting.imi'],
		'tags': 'update, rewriting, clock',
		'options'    : '-no-var-autoremove -mode statespace -states-description -verbose high',
		'expectations' : [
			{'file': 'clock-rewriting.res' , 'content' : """
"clock_rewriting": [
    {
      "x1": {
        "expr": "0",
        "rewritten_expr": "0"
      }
    },
    {
      "x1": {
        "expr": "x1 + rational_of_int(a[1] * 2)",
        "rewritten_expr": "5 + p1 + 1 + 8"
      }
    },
    {
      "x1": {
        "expr": "x1 + p1 + rational_of_int(inc(0))",
        "rewritten_expr": "5 + p1 + 1"
      }
    },
    {
      "x1": {
        "expr": "clock_value",
        "rewritten_expr": "5"
      }
    },
    {
      "w": {
        "expr": "2 * w + rational_of_int(loc_i)",
        "rewritten_expr": "2 * (w + 5 + 6 + 7 + 8 + 9 + 10) + 100"
      }
    },
    {
      "z": {
        "expr": "z + rational_of_int(loc_j)",
        "rewritten_expr": "w + 4 + 1 + 2 + 3"
      }
    },
    {
      "z": {
        "expr": "z + rational_of_int(loc_j)",
        "rewritten_expr": "w + 4 + 1 + 2"
      }
    },
    {
      "z": {
        "expr": "z + rational_of_int(loc_j)",
        "rewritten_expr": "w + 4 + 1"
      }
    },
    {
      "z": {
        "expr": "z + rational_of_int(loc_j)",
        "rewritten_expr": "w + 4"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5 + 6 + 7 + 8 + 9 + 10"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5 + 6 + 7 + 8 + 9"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5 + 6 + 7 + 8"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5 + 6 + 7"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5 + 6"
      }
    },
    {
      "w": {
        "expr": "w + rational_of_int(i)",
        "rewritten_expr": "w + 5"
      }
    },
    {
      "y": {
        "expr": "x + z",
        "rewritten_expr": "2 * (x + 1) + 2 + w + 4"
      }
    },
    {
      "z": {
        "expr": "w + rational_of_int(i1 * i1)",
        "rewritten_expr": "w + 4"
      }
    },
    {
      "x": {
        "expr": "2 * x + rational_of_int(i1)",
        "rewritten_expr": "2 * (x + 1) + 2"
      }
    },
    {
      "x": {
        "expr": "x + 1",
        "rewritten_expr": "x + 1"
      }
    }
  ]
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
        ## Test version             : 1.0
        ## Test since               : 2023/01/17
        ## Last modified            : 2023/01/17
        ## Test for IMITATOR version: 3.4
        'author': 'lbinria',
        'purpose'    : 'Check expression reduce on clock updates',
        'input_files': ['updates/expr-reduce.imi'],
		'tags': 'update, reducing, expression',
		'options'    : '-no-var-autoremove -verbose high',
		'expectations' : [
			{'file': 'expr-reduce.res' , 'content' : """
  "expr_reducing": [
    {
      "expr": "(2 * x - 3 * y) - (4 * x - 2 * y)",
      "reduced_expr": "0 - 2 * x - 1 * y + 0",
      "simplified_expr": "-((2 * x)) - y"
    },
    {
      "expr": "(2 * x + 3 * y) - (4 * x + 2 * y)",
      "reduced_expr": "0 - 2 * x + 1 * y + 0",
      "simplified_expr": "-((2 * x)) + y"
    },
    {
      "expr": "2 * (2 * (4 * x + 2 * y) - 3 * (4 * x + 2 * y)) + 1 * x + 1 * y",
      "reduced_expr": "0 - 7 * x - 3 * y + 0",
      "simplified_expr": "-((7 * x)) - 3 * y"
    },
    {
      "expr": "2 * (a * b) + 2 * (a * b)",
      "reduced_expr": "0 + 4 * (a * b) + 0",
      "simplified_expr": "4 * (a * b)"
    },
    {
      "expr": "(2 * a * b) * (2 * c * d)",
      "reduced_expr": "0 + 0 + 0 * (c * d) + 0 * (a * b) + 4 * ((a * b) * (c * d))",
      "simplified_expr": "4 * ((a * b) * (c * d))"
    },
    {
      "expr": "2 * (-2 - r1)",
      "reduced_expr": "0 - 4 - 2 * r1",
      "simplified_expr": "-((4)) - 2 * r1"
    },
    {
      "expr": "1 / (r1 * r2)",
      "reduced_expr": "0 + 1 * (1 / (r1 * r2))",
      "simplified_expr": "(1 / (r1 * r2))"
    },
    {
      "expr": "rational_of_int(i1) / 2",
      "reduced_expr": "0 + 1/2 * (rational_of_int(i1))",
      "simplified_expr": "1/2 * rational_of_int(i1)"
    },
    {
      "expr": "2 / rational_of_int(i1)",
      "reduced_expr": "0 + 2 * (1 / (rational_of_int(i1)))",
      "simplified_expr": "2 * (1 / rational_of_int(i1))"
    },
    {
      "expr": "1 - 1 + 2 * x + 1 * x + 1 * y - 1 * y",
      "reduced_expr": "0 + 3 * x + 0",
      "simplified_expr": "3 * x"
    },
    {
      "expr": "2 * (rational_of_int(i1 * i1) + 3) + 3 * (y + 4 + 2 * (1 + 3 + 2 * y))",
      "reduced_expr": "0 + 15 * y + 2 * (rational_of_int(i1 * i1)) + 42",
      "simplified_expr": "15 * y + 2 * rational_of_int(i1 * i1) + 42"
    }
  ]
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/14
		# Last modified            : 2020/09/14
		# Test for IMITATOR version: 3
		'purpose'    : 'Test stopwatches',
		'input_files': ['testStopwatches.imi', 'testStopwatches.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testStopwatches.res' , 'content' : """
BEGIN CONSTRAINT
p = 10
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/17
		# Last modified            : 2020/09/17
		# Test for IMITATOR version: 3
		'purpose'    : 'Test flows',
        'tags':'flow, behavior',
		'input_files': ['testFlows.imi', 'testFlows.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testFlows.res' , 'content' : """
BEGIN CONSTRAINT
p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/05/02
		# Last modified            : 2022/05/02
		# Test for IMITATOR version: 3
		'purpose'    : 'Test that undefined stopwatchs raise errors in flow update',
		'author': 'lbinria',
		'tags':'clock, stopwatch, flow, error, semantic',
		'input_files': ['flow_error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'flow_error.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/05/02
		# Last modified            : 2022/05/02
		# Test for IMITATOR version: 3
		'purpose'    : 'Test that undefined stopwatchs raise errors in stop',
		'author': 'lbinria',
		'tags':'clock, stopwatch, stop, error, semantic',
		'input_files': ['stopwatch_error.imi'],
		'options'    : '',
		'expectations' : [
			{'file': 'stopwatch_error.res' , 'content' : """
Error                                   : invalid model
		"""
			} # end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test clock elimination',
		'input_files': ['loopingTAdyn.imi'],
		'options'    : '-mode statespace -dynamic-elimination -states-description -depth-limit 10', # NOTE: depth-limit is just a safety to avoid yielding a loop if the test fails!
		'expectations' : [
			{'file': 'loopingTAdyn-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l0 ==>
&True

  Projection onto the parameters:
  True

  /************************************************************/
  STATE 1:
  pta: l1 ==>
& x >= 0

  Projection onto the parameters:
  True

  /************************************************************/
DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
  s_1 -> s_1 via "a"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test EF (old version) with bad initial state',
		#'input_files': ['testEFdegenerate1.imi', 'testEFdegenerate1.imiprop'],
		#'options'    : '-merge -incl ',
		#'expectations' : [
			#{'file': 'testEFdegenerate1.res' , 'content' : """
#BEGIN CONSTRAINT
 #p2 >= 0
#& p1 >= 0
#END CONSTRAINT

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : good
#------------------------------------------------------------
#Number of states                        : 0
#Number of transitions                   : 0
#Number of computed states               : 0
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with bad initial state',
		'input_files': ['testEFdegenerate1.imi', 'testEFdegenerate1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFdegenerate1.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 0
Number of transitions                   : 0
Number of computed states               : 0
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with bad initial state (queue-based BFS)',
		'input_files': ['testEFdegenerate1.imi', 'testEFdegenerate1.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'testEFdegenerate1.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test EF (old version) with bad initial state and some constrained valuations',
		#'input_files': ['testEFdegenerate2.imi'],
		#'options'    : '-mode EFold -merge -incl ',
		#'expectations' : [
			#{'file': 'testEFdegenerate2.res' , 'content' : """
#BEGIN CONSTRAINT
 #p1 >= p2
#& p2 >= 0
#END CONSTRAINT

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : good
#------------------------------------------------------------
#Number of states                        : 0
#Number of transitions                   : 0
#Number of computed states               : 0
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with bad initial state and some constrained valuations',
		'input_files': ['testEFdegenerate2.imi', 'testEFdegenerate2.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFdegenerate2.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 0
Number of transitions                   : 0
Number of computed states               : 0
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with bad initial state and some constrained valuations (queue-based BFS)',
		'input_files': ['testEFdegenerate2.imi', 'testEFdegenerate2.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'testEFdegenerate2.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with the new syntax from 2.10.1',
		'input_files': ['fischer_2.imi', 'fischer_2.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'fischer_2.res' , 'content' : """
BEGIN CONSTRAINT
 a >= 0
& b > a
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test EF (old version) with basic safety property for Fischer:3',
		#'input_files': ['F3.imi'],
		#'options'    : '-mode EFold -merge -incl ',
		#'expectations' : [
			#{'file': 'F3.res' , 'content' : """
#BEGIN CONSTRAINT
 #Delta > delta
#& delta >= 0
 #OR
 #Delta > 2*delta
#& delta >= 0
#END CONSTRAINT
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: witness',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-witness.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF on toy example: witness (queue-based BFS)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-witness.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF on toy example: synthesis (queue-based BFS)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (syntax without parentheses)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-noparen.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (-output-prefix)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-output-prefix mypersonnalizedprefix',
		'expectations' : [
			{'file': 'mypersonnalizedprefix.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (explicit -merge yes -comparison inclusion)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-merge yes -comparison inclusion',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (-merge none)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-merge none',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (-merge none -comparison equality)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	# TODO: merge without inclusion (for now: loops forever)

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test EF on toy example: synthesis (-merge none -comparison equality -no-cumulative-pruning)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge.imiprop'],
		'options'    : '-merge none -comparison equality -no-cumulative-pruning',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state2.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state2.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state-neg.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state-neg.res' , 'content' : """
BEGIN CONSTRAINT
  0 >= 3 + p1
 & p2 + 3 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF: basic reachability property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state-neg.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state-neg.res' , 'content' : """
BEGIN CONSTRAINT
  0 >= 3 + p1
 & p2 + 3 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/01/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF: basic reachability property with disjunctive result without projection',
		'tags'       : 'semantic,projection',
		'input_files': ['basic-reachability-projection.imi', 'basic-reachability-noprojection.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'basic-reachability-projection.res' , 'content' : """
BEGIN CONSTRAINT
  p1 > 0
 & 5 > p2
 & p2 > p1
 OR
   p1 > 8
 & 9 > p2
 & p2 > p1
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
		# Test version             : 2
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF: basic reachability property with disjunctive result without projection (queue-based BFS)',
		'tags'       : 'semantic,projection',
		'input_files': ['basic-reachability-projection.imi', 'basic-reachability-noprojection.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'basic-reachability-projection.res' , 'content' : """
BEGIN CONSTRAINT
  p1 > 0
 & 5 > p2
 & p2 > p1
 OR
   p1 > 8
 & 9 > p2
 & p2 > p1
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/01/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF: basic reachability property with disjunctive result with projection',
		'tags'       : 'semantic,projection',
		'input_files': ['basic-reachability-projection.imi', 'basic-reachability-projection.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'basic-reachability-projection.res' , 'content' : """
BEGIN CONSTRAINT
  5 > p1
 & p1 > 0
 OR
   9 > p1
 & p1 > 8
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF: basic reachability property with disjunctive result with projection (queue-based BFS)',
		'tags'       : 'semantic,projection',
		'input_files': ['basic-reachability-projection.imi', 'basic-reachability-projection.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'basic-reachability-projection.res' , 'content' : """
BEGIN CONSTRAINT
  5 > p1
 & p1 > 0
 OR
   9 > p1
 & p1 > 8
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
		'purpose'    : 'Test AGnot: basic safety property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AGnot: basic safety property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AGnot: basic safety property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state2.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
p > 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AGnot: basic safety property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state2.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
p > 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AGnot: basic reachability property depending on the initial invariant',
		'input_files': ['safety/safety-initial-state-neg.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state-neg.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AGnot: basic reachability property depending on the initial invariant (queue-based BFS)',
		'input_files': ['safety/safety-initial-state-neg.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state-neg.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/01/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AGnot: basic reachability property with disjunctive result without projection',
		'tags'       : 'semantic',
		'input_files': ['safety/safety-simple.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-simple.res' , 'content' : """
  p1 >= 0
 & p2 >= 0
 & 3 > p2
 OR
   p2 >= 3
 & p1 >= p2
 & 11 >= p2
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AGnot: basic reachability property with disjunctive result without projection (queue-based BFS)',
		'tags'       : 'semantic',
		'input_files': ['safety/safety-simple.imi', 'basic-properties/synth-safety.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-simple.res' , 'content' : """
  p1 >= 0
 & p2 >= 0
 & 3 > p2
 OR
   p2 >= 3
 & p1 >= p2
 & 11 >= p2
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/01/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AGnot: basic reachability property with disjunctive result with projection',
		'tags'       : 'semantic,projection',
		'input_files': ['safety/safety-simple.imi', 'safety/safety-simple.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-simple.res' , 'content' : """
  11 >= p2
 & p2 >= 0
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AGnot: basic reachability property with disjunctive result with projection (queue-based BFS)',
		'tags'       : 'semantic,projection',
		'input_files': ['safety/safety-simple.imi', 'safety/safety-simple.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-simple.res' , 'content' : """
  11 >= p2
 & p2 >= 0
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/22
		# Last modified            : 2023/12/22
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AG on a very simple example',
		'tags'       : 'algorithm',
		'input_files': ['safety/safety-initial-state2.imi', 'safety/safety-initial-state2-AG1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
p > 5
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
		# Test version             : 2
		# Test author              : Étienne André
		# Test since               : 2023/12/22
		# Last modified            : 2024/04/12
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AG on a very simple example (queue-based BFS)',
		'tags'       : 'algorithm',
		'input_files': ['safety/safety-initial-state2.imi', 'safety/safety-initial-state2-AG1.imiprop'],
		'options'    : '-new-queue-EF',
		'expectations' : [
			{'file': 'safety-initial-state2.res' , 'content' : """
BEGIN CONSTRAINT
p > 5
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
		'purpose'    : 'Test EF with basic safety property for Fischer:3',
		'input_files': ['F3.imi', 'F3.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
delta >= Delta
    & Delta >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with basic safety property for Fischer:3 (double-directional inclusion)',
		'input_files': ['F3.imi', 'F3.imiprop'],
		'options'    : '-merge yes -comparison doubleinclusion',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
delta >= Delta
    & Delta >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with basic safety property for Fischer:3 (queueBFS exploration order)',
		'input_files': ['F3.imi', 'F3.imiprop'],
		'options'    : '-expl-order queueBFS',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
delta >= Delta
    & Delta >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with basic safety property for Fischer:3 (layerBFS exploration order) with counterexample',
		'input_files': ['F3.imi', 'F3-witness.imiprop'],
		'options'    : '-expl-order layerBFS',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
 2*delta >= Delta
& Delta >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with basic safety property for Fischer:3 (queueBFS exploration order) with counterexample',
		'input_files': ['F3.imi', 'F3-witness.imiprop'],
		'options'    : '-expl-order queueBFS',
		'expectations' : [
			{'file': 'F3.res' , 'content' : """
BEGIN CONSTRAINT
 2*delta >= Delta
& Delta >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test EF (old version) with complex safety property',
		#'input_files': ['coffeeDrinker.imi'],
		#'options'    : '-mode EFold -merge ',
		#'expectations' : [
			#{'file': 'coffeeDrinker.res' , 'content' : """
#BEGIN CONSTRAINT
 #15 >= p_add_sugar
#& p_button > 0
#& p_add_sugar + p_coffee >= 15
#& p_add_sugar >= 3*p_button
#& p_coffee > 0
 #OR
 #p_add_sugar + p_coffee >= 15
#& p_button >= 5
#& 15 >= p_add_sugar
#& p_coffee > 0
#& p_add_sugar >= 2*p_button
 #OR
 #3*p_button >= p_add_sugar + p_coffee
#& p_add_sugar >= 2*p_button
#& 15 >= p_add_sugar + p_coffee
#& p_coffee > 0
 #OR
 #5 >= p_button
#& p_button > 0
#& p_add_sugar >= 15
#& p_coffee > 0
 #OR
 #15 >= 2*p_button
#& p_button >= 5
#& p_add_sugar >= 15
#& p_coffee > 0
 #OR
 #p_add_sugar + p_coffee >= 15
#& p_add_sugar >= p_button
#& 15 >= p_add_sugar
#& p_coffee > 0
#& 2*p_button >= 15
 #OR
 #p_add_sugar >= p_button
#& p_coffee > 0
#& 15 >= p_add_sugar + p_coffee
#& 2*p_button >= p_add_sugar + p_coffee
 #OR
 #p_coffee > 0
#& 15 >= p_add_sugar
#& p_add_sugar > 0
#& p_add_sugar + p_coffee >= 15
#& p_button >= 15
 #OR
 #p_add_sugar > 0
#& 15 >= p_add_sugar + p_coffee
#& p_coffee > 0
#& p_button >= p_add_sugar + p_coffee
 #OR
 #2*p_button >= 15
#& 15 >= p_button
#& p_add_sugar >= 15
#& p_coffee > 0
 #OR
 #p_button >= 15
#& p_coffee > 0
#& p_add_sugar >= 15
#END CONSTRAINT
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with complex safety property',
		'input_files': ['coffeeDrinker.imi', 'coffeeDrinker.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'coffeeDrinker.res' , 'content' : """
BEGIN CONSTRAINT
 p_add_sugar > 0
& 2*p_button > p_add_sugar
& p_add_sugar + p_coffee > 2*p_button
& 15 > 2*p_button
OR
  p_add_sugar >= 2*p_button
& p_add_sugar + p_coffee > 3*p_button
& 3*p_button > p_add_sugar
& 5 > p_button
OR
  p_add_sugar > 0
& 2*p_button >= 15
& p_add_sugar + p_coffee > 15
& 15 > p_button
& p_button > p_add_sugar
OR
  2*p_button >= p_add_sugar + p_coffee
& p_add_sugar > 0
& p_add_sugar + p_coffee > p_button
& p_button > p_add_sugar
& 15 >= p_add_sugar + p_coffee
OR
  15 > p_add_sugar + p_coffee
& p_coffee > 0
& p_button > 0
& 5 > p_button
& p_add_sugar >= 3*p_button
END CONSTRAINT
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/02
		# Last modified            : 2021/07/02
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test EF with a parameter not used in the model (but still useful) + negative clock + negative flow',
		'tags' : 'auto remove',
		'input_files': ['test_param_unused.imi' , 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_param_unused.res' , 'content' : """
BEGIN CONSTRAINT
 14 > p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EF with complex safety property on coffee drinker with int',
		'input_files': ['coffeeDrinker-int.imi', 'coffeeDrinker.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'coffeeDrinker-int.res' , 'content' : """
BEGIN CONSTRAINT
 p_add_sugar > 0
& 2*p_button > p_add_sugar
& p_add_sugar + p_coffee > 2*p_button
& 15 > 2*p_button
OR
  p_add_sugar >= 2*p_button
& p_add_sugar + p_coffee > 3*p_button
& 3*p_button > p_add_sugar
& 5 > p_button
OR
  p_add_sugar > 0
& 2*p_button >= 15
& p_add_sugar + p_coffee > 15
& 15 > p_button
& p_button > p_add_sugar
OR
  2*p_button >= p_add_sugar + p_coffee
& p_add_sugar > 0
& p_add_sugar + p_coffee > p_button
& p_button > p_add_sugar
& 15 >= p_add_sugar + p_coffee
OR
  15 > p_add_sugar + p_coffee
& p_coffee > 0
& p_button > 0
& 5 > p_button
& p_add_sugar >= 3*p_button
END CONSTRAINT
"""
			 } #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test EF (old version) with observer + depth-limit + project-result (quite basic)',
		#'input_files': ['coffeeDrinker-within.imi'],
		#'options'    : '-mode EFold -merge -depth-limit 10 ',
		#'expectations' : [
			#{'file': 'coffeeDrinker-within.res' , 'content' : """
#BEGIN CONSTRAINT
 #p_coffee > 0
#END CONSTRAINT
#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	
	#,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/09
		# Last modified            : 2024/01/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EF with observer + depth-limit',
		'tags'       : 'semantic,projection',
		'input_files': ['coffeeDrinker.imi', 'coffeeDrinker-within.imiprop'],
		'options'    : '-depth-limit 10',
		'expectations' : [
				# WARNING (2024/01/09): this result was NOT manually checked!
			{'file': 'coffeeDrinker.res' , 'content' : """
BEGIN CONSTRAINT
  p_button > 0
 & p_add_sugar > 0
 & p_coffee > 0
 & 20 >= p_add_sugar + p_coffee
 OR
   15 > 2*p_button
 & p_add_sugar > 0
 & 2*p_button > p_add_sugar
 & p_add_sugar + p_coffee > 20
 OR
   p_add_sugar > 0
 & 15 > p_button
 & p_add_sugar + p_coffee > 20
 & p_button > p_add_sugar
 & 2*p_button >= 15
 OR
   p_add_sugar >= 2*p_button
 & p_add_sugar + p_coffee > 20
 & 3*p_button > p_add_sugar
 & 5 > p_button

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
		'purpose'    : 'Test EF with observer + depth-limit + project-result (quite basic)',
		'input_files': ['coffeeDrinker.imi', 'coffeeDrinker-within-project.imiprop'],
		'options'    : '-depth-limit 10',
		'expectations' : [
				# WARNING (2024/01/09): this result was manually checked, but only by projection from the former result
			{'file': 'coffeeDrinker.res' , 'content' : """
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
		# Test version             : 1
		# Test since               : 2020/09/04
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test EF (difference between witness and synthesis, here witness)',
		'input_files': ['testEFemptiness.imi', 'testEFemptiness-empt.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFemptiness.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/04
		# Last modified            : 2020/09/04
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test EF (difference between witness and synthesis, here synthesis)',
		'input_files': ['testEFemptiness.imi', 'testEFemptiness-synth.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFemptiness.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
 OR
 p >= 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/22
		# Last modified            : 2020/09/22
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test EF (without option -no-cumulative-pruning)',
		'input_files': ['testInclusionEF.imi', 'testInclusionEF-EF.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testInclusionEF.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 3
Number of transitions                   : 2
Number of computed states               : 3
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/22
		# Last modified            : 2020/09/22
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test EF (with option -no-cumulative-pruning)',
		'input_files': ['testInclusionEF.imi', 'testInclusionEF-EF.imiprop'],
		'options'    : '-no-cumulative-pruning',
		'expectations' : [
			{'file': 'testInclusionEF.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 14
Number of transitions                   : 13
Number of computed states               : 14
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test EF with depth-limit to check the quick reachability (-merge none)',
		'input_files': ['quick-reach.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '-merge none -depth-limit 3',
		'expectations' : [
			{'file': 'quick-reach.res' , 'content' : """
BEGIN CONSTRAINT
 2 > 4*p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	# NOTE: disabled because the FINAL state space is still correct (and the result as well); what is "incorrect" (??) is the "quick reachability" for a given depth…
	#{
		## Test version             : 1
		## Test since               : 2021/09/01
		## Last modified            : 2021/09/01
		## Test for IMITATOR version: 3.1
		#'purpose'    : 'Test EF with depth-limit to check the quick reachability (-merge)',
		#'input_files': ['quick-reach.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		#'options'    : '-merge -depth-limit 3',
		#'expectations' : [
			#{'file': 'quick-reach.res' , 'content' : """
#BEGIN CONSTRAINT
 #2 > 4*p
#& p >= 0
#END CONSTRAINT

#------------------------------------------------------------
#Constraint soundness                    : possible under-approximation
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/30
		# Test for IMITATOR version: 2.11
		'purpose'    : 'Test EFunsafe',
		'input_files': ['testEFcounterex.imi', 'testEFcounterex.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFcounterex.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
 OR
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2019/05/30
		# Test for IMITATOR version: 2.11
		'purpose'    : 'Test EFunsafe with counterexample',
		'input_files': ['testEFcounterex.imi', 'testEFcounterex-witness.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFcounterex.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (unsat on the initial state)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (unsat due to unreachability)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-2.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (True due to initial state matched)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-3.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (matching location)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-4.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (matching variable condition)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-5.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (matching variable condition (varying syntax))',
		'tags'       : 'algorithm,property,syntax',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-6.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (matching variable condition (varying syntax))',
		'tags'       : 'algorithm,property,syntax',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-7.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (matching variable condition (varying syntax))',
		'tags'       : 'algorithm,property,syntax',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-8.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (using both variables and locations)',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-9.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p >= 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (using both variables and locations (corner-point case))',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-10.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
p >= 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/12/18
		# Last modified            : 2023/12/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (using both variables and locations (false))',
		'tags'       : 'algorithm,property',
		'input_files': ['test_EU/test_EU.imi', 'test_EU/test_EU-11.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test_EU.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/29
		# Last modified            : 2024/02/29
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test EU (different reachable times)',
		'tags'       : 'algorithm,property',
		'input_files': ['timed_properties/test-timedEU.imi', 'timed_properties/test-untimedEU.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEU.res' , 'content' : """
BEGIN CONSTRAINT
  p = 1
 OR
   p = 3
 OR
   p = 4
 OR
   p = 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# BEGIN : Test timed EF
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the model',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic.imi', 'basic-properties/synth-EF-4-4-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic.res' , 'content' : """
BEGIN CONSTRAINT
 4 >= p
& p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, wrong interval',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-wronginterval.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval [closed, closed]',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-closedclosed.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 >= p
 & p >= 9
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval (open, closed]',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-openclosed.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 > p
 & p >= 9
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval [closed, open)',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-closedopen.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 >= p
 & p > 9
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval (open, open)',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-openopen.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 > p
 & p > 9
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval [closed, infinity)',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-closedinfinite.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 >= p
 & p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model, one parameter in the property, interval (open, infinity)',
		'tags'       : 'semantic',
		'input_files': ['timed_properties/test-timedEF-basic-prop.imi', 'timed_properties/test-timedEF-basic-prop-openinfinite.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop.res' , 'content' : """
BEGIN CONSTRAINT
  10 > p
 & p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/28
		# Last modified            : 2024/02/28
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: toy model with flows, one parameter in the property',
		'tags'       : 'semantic, flows',
		'input_files': ['timed_properties/test-timedEF-basic-prop-flows.imi', 'timed_properties/test-timedEF-basic-prop-closedopen.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedEF-basic-prop-flows.res' , 'content' : """
BEGIN CONSTRAINT
  14 >= p
 & p > 13
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/29
		# Last modified            : 2024/02/29
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed EF: check no exploration beyond timed interval',
		'tags'       : 'semantic,optimization',
		'input_files': ['timed_properties/test-timedEF-cutbranch.imi', 'basic-properties/synth-EF-4-4-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			# NOTE: check correctness first
			{'file': 'test-timedEF-cutbranch.res' , 'content' : """
BEGIN CONSTRAINT
p = 1
END CONSTRAINT
		"""
			} # end result file
			,
			# NOTE: check optimization
			{'file': 'test-timedEF-cutbranch.res' , 'content' : """
Number of states                        : 4
Number of transitions                   : 3
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test timed EF
	#------------------------------------------------------------

	#------------------------------------------------------------
	# BEGIN : Test timed AF/AU
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: untimed property',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property [0, infinity)',
		'tags'       : 'semantic,AF,optimization',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'basic-properties/synth-AF-accepting-0-infinity.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT
"""
			} # end result file
			,
			# NOTE: check that AF_[0, infinity) means AF
			{'file': 'test-timedAF-basic.res' , 'content' : """
Property                                : unavoidability synthesis
Algorithm                               : AF
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property [0, 10]',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-0-10.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property [2, 10]',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-2-10.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
True
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property (2, 10]',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-2-open-10.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property [2, 10)',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-2-10-open.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: timed property [10, 10]',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-10-10.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: parametric timed property [p, p]',
		'tags'       : 'semantic,AF',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-p-p.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/03/07
		# Last modified            : 2024/03/07
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test timed AF: parametric timed property [p, p+1]',
		'tags'       : 'semantic,AF,negativeparameter',
		'input_files': ['timed_properties/test-timedAF-basic.imi', 'timed_properties/AF-accepting-p-p-plus-11.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'test-timedAF-basic.res' , 'content' : """
BEGIN CONSTRAINT
 p + 1 >= 0
& 2 >= p
END CONSTRAINT
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	# END : Test timed AF/AU
	#------------------------------------------------------------

	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "if a2 then a1 has happened before"',
		'input_files': ['testPattern1.imi', 'testPattern1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern1.res' , 'content' : """
BEGIN CONSTRAINT
 2 >= p1
& p1 >= 0
& p2 >= 0
& p2 + 1 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "if a2 then a1 has happened before" on a model for 3 patterns',
		'input_files': ['testPattern2.imi', 'testPattern2-if.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern2.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
& 1 > p
OR
  p > 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a2 then a1 has happened before" on a model for 3 patterns',
		'input_files': ['testPattern2.imi', 'testPattern2-everytime.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern2.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
& 1 > p
OR
  p > 3
OR
  p > 2
& 3 > p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a2 then a1 has happened once before" on a model for 3 patterns',
		'input_files': ['testPattern2.imi', 'testPattern2-everytimeonce.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern2.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
& 1 > p
OR
  p > 5
OR
  p > 3
& 5 > p
OR
  p > 2
& 3 > p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	# WARNING: the expected result is different from the (mathematically) sound result, due to a time-lock; observer patterns are incorrect in presence of time-locks
	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "a within d"',
		'input_files': ['testPattern3.imi', 'testPattern3.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern3.res' , 'content' : """
BEGIN CONSTRAINT
 3 >= p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	# WARNING: the expected result is different from the (mathematically) sound result, due to a time-lock; observer patterns are incorrect in presence of time-locks
	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "a within d"',
		'input_files': ['testPattern4.imi', 'testPattern4.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern4.res' , 'content' : """
BEGIN CONSTRAINT
 p > 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "a within d" with a parametric d',
		'input_files': ['testPattern5.imi', 'testPattern5.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern5.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "if a2 then a1 has happened within d before"',
		'input_files': ['testPattern6.imi', 'testPattern6-if.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern6.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& 1 > p1
OR
  p1 > 5
& p2 >= 0
OR
  5 > p1
& p1 > 2
& p2 >= 0
OR
  p2 >= 2015
& p1 = 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a2 then a1 has happened within d before"',
		'input_files': ['testPattern6.imi', 'testPattern6-everytime.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern6.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& 1 > p1
OR
  p1 > 5
& p2 >= 0
OR
  p1 > 4
& p2 >= 0
& 5 > p1
OR
  p1 > 3
& p2 >= 0
& 4 > p1
OR
  p2 >= 4
& p1 = 4
OR
  p2 >= 2015
& p1 = 5
OR
  p1 > 2
& p2 >= 0
& 3 > p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a2 then a1 has happened once within d before"',
		'input_files': ['testPattern6.imi', 'testPattern6-everytimeonce.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern6.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& 1 > p1
OR
  p1 > 5
& p2 >= 0
OR
  p1 > 4
& p2 >= 0
& 5 > p1
OR
  p1 > 3
& p2 >= 0
& 4 > p1
OR
  p2 >= 4
& p1 = 4
OR
  p1 > 2
& p2 >= 0
& 3 > p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "if a1 then eventually a2 within d"',
		'input_files': ['testPattern7.imi', 'testPattern7-if.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern7.res' , 'content' : """
BEGIN CONSTRAINT
  p1 > 1
 & p2 >= p1
 OR
   p1 >= 0
 & 1 > p1
 & p2 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a1 then eventually a2 within d"',
		'input_files': ['testPattern7.imi', 'testPattern7-everytime.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern7.res' , 'content' : """
BEGIN CONSTRAINT
  p1 > 1
 & 2 > p1
 & p2 >= p1
 OR
   p2 >= 3
 & p1 >= 2
 & p2 >= p1
 OR
   p1 >= 0
 & p2 >= p1
 & 1 > p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "everytime a1 then eventually a2 within d once before next"',
		'input_files': ['testPattern7.imi', 'testPattern7-everytimeonce.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern7.res' , 'content' : """
BEGIN CONSTRAINT
  p1 > 4
 & p2 >= p1
 OR
   p1 > 1
 & p2 >= p1
 & 2 > p1
 OR
   p2 >= 3
 & p1 >= 2
 & p2 >= p1
 & 4 > p1
 OR
   p1 >= 0
 & p2 >= p1
 & 1 > p1

END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "sequence a1, …, an"',
		'input_files': ['testPattern8.imi', 'testPattern8-sequence.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern8.res' , 'content' : """
BEGIN CONSTRAINT
  4 > p
 & p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test observer pattern "always sequence a1, …, an"',
		'input_files': ['testPattern8.imi', 'testPattern8-alwayssequence.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testPattern8.res' , 'content' : """
BEGIN CONSTRAINT
  p > 3
 & 4 > p
 OR
   p >= 0
 & 3 > p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFmin on a toy example',
		'input_files': ['testEFmin.imi', 'testEFmin.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFmin.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 10
Number of transitions                   : 9
Number of computed states               : 12
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFmax on a toy example',
		'input_files': ['testEFmin.imi', 'testEFmax.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFmin.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
& 11 >= p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 10
Number of transitions                   : 9
Number of computed states               : 20
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 1',
		'input_files': ['testEFsynthminParams1.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams1.res' , 'content' : """
BEGIN CONSTRAINT
 p1 = 1
& p2 = 2
OR
  p1 = 1
& p2 = 3
OR
  p1 = 1
& p2 = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 2',
		'input_files': ['testEFsynthminParams2.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams2.res' , 'content' : """
BEGIN CONSTRAINT
 p1 = 1
& p2 = 2
OR
  p1 = 1
& p2 = 3
OR
  p1 = 1
& p2 = 4
OR
  p1 = 1
& p2 = 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 3',
		'input_files': ['testEFsynthminParams3.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams3.res' , 'content' : """
BEGIN CONSTRAINT
 3*p1 > 1
& 1 > p1
& p2 = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 4',
		'input_files': ['testEFsynthminParams4.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams4.res' , 'content' : """
BEGIN CONSTRAINT
 3*p1 > 1
& 1 > p1
& 2*p1 = p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 5',
		'input_files': ['testEFsynthminParams5.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams5.res' , 'content' : """
BEGIN CONSTRAINT
 p1 = 0
& p2 = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a clock-free toy example 6',
		'input_files': ['testEFsynthminParams6.imi', 'testEFsynthminParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthminParams6.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 0
& p1 + 1 = p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmin on a toy example',
		'input_files': ['testEFsynthmin.imi', 'testEFsynthmin.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmin.res' , 'content' : """
BEGIN CONSTRAINT
 3 >= p2
& p2 >= 2
& p1 = 3
OR
  p1 = 3
& p2 = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 1',
		'input_files': ['testEFsynthmaxParams1.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams1.res' , 'content' : """
BEGIN CONSTRAINT
 p1 = 1
& p2 = 2
OR
  p1 = 1
& p2 = 3
OR
  p1 = 1
& p2 = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 2',
		'input_files': ['testEFsynthmaxParams2.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams2.res' , 'content' : """

BEGIN CONSTRAINT
 p1 = 1
& p2 = 2
OR
  p1 = 1
& p2 = 3
OR
  p1 = 1
& p2 = 4
OR
  p1 = 1
& p2 = 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 3',
		'input_files': ['testEFsynthmaxParams3.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams3.res' , 'content' : """
BEGIN CONSTRAINT
 3*p1 > 4
& 2 > p1
& p2 = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 4',
		'input_files': ['testEFsynthmaxParams4.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams4.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& 5 > 3*p1
& 2*p1 = p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 5',
		'input_files': ['testEFsynthmaxParams5.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams5.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 1
& p2 = 3
OR
  p1 >= 2
& p2 = 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a clock-free toy example 6',
		'input_files': ['testEFsynthmaxParams6.imi', 'testEFsynthmaxParams1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmaxParams6.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= p2
& p1 > 0
& p2 >= 0
OR
  p1 >= 1
& p2 = 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		'purpose'    : 'Test EFsynthmax on a toy example',
		'input_files': ['testEFsynthmax.imi', 'testEFsynthmax.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFsynthmax.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 10
& p2 >= 0
OR
  p1 >= 3
& p2 = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/04/01
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example with only discrete',
		'input_files': ['testEFexemplify-discrete.imi', 'testEFexemplify-discrete.imiprop'],
		'options'    : '-merge none -comparison equality -no-var-autoremove',
		'expectations' : [
			{'file': 'testEFexemplify-discrete.res' , 'content' : """
BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:
  

 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {
            "i": "0"
          },
          "continuous_variables": {
            "global_time": "0"
          },
          "flows": {
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {
                  "i": "3"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {
            "i": "3"
          },
          "continuous_variables": {
            "global_time": "1/2"
          },
          "flows": {
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {
                  "i": "i + 1"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "lTarget"
          },
          "discrete_variables": {
            "i": "4"
          },
          "continuous_variables": {
            "global_time": "1"
          },
          "flows": {
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)

END RESULT

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/04/01
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example with only 1 clock',
		'input_files': ['testEFexemplify-1clock.imi', 'testEFexemplify-1clock.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFexemplify-1clock.res' , 'content' : """
BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:
  

 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
	"run": {
		"nature": "concrete",
		"valuation": null,
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "0",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "concrete",
				"duration": "2",
				"action": "(silent)",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " x = 2",
						"updates": {
							"x": "0"
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l2"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "0",
					"global_time": "2"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "3",
				"action": "(silent)",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " x = 3",
						"updates": {
							"x": "x"
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "lTarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "3",
					"global_time": "5"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)

END RESULT

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/04/01
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example with only 1 clock (+ non-1 flows, non-0 resets)',
		'input_files': ['testEFexemplify-1complexclock.imi', 'testEFexemplify-1complexclock.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFexemplify-1complexclock.res' , 'content' : """
BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:
  

 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "2",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 2",
                "updates": {
                  "x": "5"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "5",
            "global_time": "2"
          },
          "flows": {
            "x": "3",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 8",
                "updates": {
                  "x": "2 * x"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "16",
            "global_time": "3"
          },
          "flows": {
            "x": "-1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "10",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 6",
                "updates": {
                  "x": "x - 3"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "lTarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "3",
            "global_time": "13"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)

END RESULT

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/04/01
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example with 2 clocks (+ non-1 flows, non-0 resets)',
		'input_files': ['testEFexemplify-2complexclocks.imi', 'testEFexemplify-2complexclocks.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFexemplify-2complexclocks.res' , 'content' : """
BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "y": "0",
            "swap": "-1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "0",
            "swap": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "2",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 2",
                "updates": {
                  "y": "5"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "2",
            "y": "5",
            "swap": "1",
            "global_time": "2"
          },
          "flows": {
            "x": "3",
            "y": "2",
            "swap": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 5",
                "updates": {
                  "swap": "x",
                  "x": "y",
                  "y": "swap"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "7",
            "y": "5",
            "swap": "5",
            "global_time": "3"
          },
          "flows": {
            "x": "-12",
            "y": "1",
            "swap": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "(silent)",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 1",
                "updates": {
                  "y": "y - x"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "lTarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "1",
            "y": "9/2",
            "swap": "11/2",
            "global_time": "7/2"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "swap": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)

END RESULT
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/04/01
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example with 1 clock and 2 bounded signals',
		'input_files': ['testEFexemplify-2signals.imi', 'testEFexemplify-2signals.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			# WARNING: there is a lot of non-determinism, so a failure on this test does not necessarily mean the algorithm is wrong! It should then be manually inspected
			# NOTE: only quickly checked it (2022/03/31)
			{'file': 'testEFexemplify-2signals.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  

 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
	"run": {
		"nature": "concrete",
		"valuation": null,
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1", 
					"signal_1": "increasing_slow", 
					"signal_2": "increasing_slow"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "0",
					"s_1": "29/4",
					"s_2": "29/8",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"s_1": "1", 
					"s_2": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "concrete",
				"duration": "2",
				"action": "(silent)",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " s_1 > s_2 AND x = 2",
						"updates": {
							"x": "0"
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l2", 
					"signal_1": "increasing_slow", 
					"signal_2": "increasing_slow"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "0",
					"s_1": "37/4",
					"s_2": "45/8",
					"global_time": "2"
				},
				"flows": {
					"x": "1", 
					"s_1": "1", 
					"s_2": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "1",
				"action": "stabilize_1",
				"transitions": [
					{
					"transition": {
						"PTA": "signal_1",
						"guard": "True",
						"updates": {
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l2", 
					"signal_1": "stabilized", 
					"signal_2": "increasing_slow"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "1",
					"s_1": "41/4",
					"s_2": "53/8",
					"global_time": "3"
				},
				"flows": {
					"x": "1", 
					"s_1": "0", 
					"s_2": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "13/4",
				"action": "(silent)",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " x > 4 AND 10 > s_2",
						"updates": {
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l3", 
					"signal_1": "stabilized", 
					"signal_2": "increasing_slow"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "17/4",
					"s_1": "41/4",
					"s_2": "79/8",
					"global_time": "25/4"
				},
				"flows": {
					"x": "2", 
					"s_1": "0", 
					"s_2": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "83/8",
				"action": "(silent)",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " x > 20 AND s_1 > 10 AND s_1 + 10 = s_2",
						"updates": {
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "lTarget", 
					"signal_1": "stabilized", 
					"signal_2": "increasing_slow"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"x": "25",
					"s_1": "41/4",
					"s_2": "81/4",
					"global_time": "133/8"
				},
				"flows": {
					"x": "1", 
					"s_1": "0", 
					"s_2": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/02
		# Last modified            : 2022/03/02
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify without parameters: unique negative run',
		'input_files': ['testEFExemplify1neg.imi', 'testEFExemplify1neg.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplify1neg.res' , 'content' : """
(************************************************************)
 Run #2

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "specification": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a1",
          "transitions": [
            {
              "transition": {
                "PTA": "specification",
                "guard": " 2 >= x",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "specification": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "1",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "3",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "specification": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "4",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/02
		# Last modified            : 2022/03/02
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify without parameters: unique negative run (in initial location)',
		'input_files': ['testEFExemplify1neg-b.imi', 'testEFExemplify1neg.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplify1neg-b.res' , 'content' : """
(************************************************************)
 Run #2

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "specification": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "4",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "specification": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "4",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/02
		# Last modified            : 2022/03/02
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify without parameters: unique negative run + flows',
		'input_files': ['testEFExemplify1neg-c.imi', 'testEFExemplify1neg.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplify1neg-c.res' , 'content' : """
(************************************************************)
 Run #2

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "specification": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "y": "1",
            "z": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/8",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "4",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "specification": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "4",
            "y": "1/2",
            "z": "2",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/02
		# Last modified            : 2022/03/02
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify without parameters: unique negative run + flows + negative variable',
		'input_files': ['testEFExemplify1neg-d.imi', 'testEFExemplify1neg.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplify1neg-d.res' , 'content' : """
(************************************************************)
 Run #2

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "specification": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "y": "1",
            "z": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "4",
          "action": "a1"
        }
      },
      {
        "state": {
          "location": {
            "specification": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "4",
            "y": "0",
            "z": "2",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "specification": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "5",
            "y": "-1/4",
            "z": "2",
            "global_time": "5"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a1"
        }
      },
      {
        "state": {
          "location": {
            "specification": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "6",
            "y": "-1/2",
            "z": "2",
            "global_time": "6"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/10
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify without parameters: non-determinism + flows + negative variable + 2 PTAs',
		'input_files': ['testEFExemplifyNonDet.imi', 'testEFExemplifyNonDet.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplifyNonDet.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "spec1": "l1",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "y": "1",
            "z": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a1",
          "transitions": [
            {
              "transition": {
                "PTA": "spec1",
                "guard": " 4 >= x",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "1",
            "y": "3/4",
            "z": "3/2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "9",
          "action": "a2",
          "transitions": [
            {
              "transition": {
                "PTA": "spec2",
                "guard": " x >= 10",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "10",
            "y": "-3/2",
            "z": "3/2",
            "global_time": "10"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "9/2",
          "action": "a3",
          "transitions": [
            {
              "transition": {
                "PTA": "spec1",
                "guard": "True",
                "updates": {

                }
              }
            },
            {
              "transition": {
                "PTA": "spec2",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "11/2",
            "y": "3",
            "z": "3/2",
            "global_time": "29/2"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "9/2",
          "action": "a2",
          "transitions": [
            {
              "transition": {
                "PTA": "spec2",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "1",
            "y": "15/2",
            "z": "6",
            "global_time": "19"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "spec1": "l1",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "0",
            "y": "1",
            "z": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "6",
          "action": "a1"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "6",
            "y": "-1/2",
            "z": "3/2",
            "global_time": "6"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "7",
            "y": "-3/4",
            "z": "3/2",
            "global_time": "7"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a3"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "6",
            "y": "1/4",
            "z": "3/2",
            "global_time": "8"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "x": "5",
            "y": "5/4",
            "z": "5/2",
            "global_time": "9"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/15
		# Last modified            : 2022/03/15
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify with one parameter: unique negative parameter valuation',
		'input_files': ['testEFExemplify1Pneg.imi', 'testEFExemplify1Pneg.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplify1Pneg.res' , 'content' : """
(************************************************************)
 Run #2

 Valuation:
  p = 4

 Other valuations with equivalent (discrete) run:
 p = 4

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "4"
    },
    "steps": [
      {
        "state": {
          "location": {
            "specification": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a1",
          "transitions": [
            {
              "transition": {
                "PTA": "specification",
                "guard": " 2 >= x",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "specification": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "1",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "specification": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "2",
            "global_time": "2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/15
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify with one parameter: non-determinism + flows + negative variable + 2 PTAs',
		'input_files': ['testEFExemplifyPNonDet.imi', 'testEFExemplifyPNonDet.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testEFExemplifyPNonDet.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 4

 Other valuations with equivalent (discrete) run:
 p = 4

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "4"
    },
    "steps": [
      {
        "state": {
          "location": {
            "spec1": "l1",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "0",
            "y": "1",
            "z": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "4",
          "action": "a1",
          "transitions": [
            {
              "transition": {
                "PTA": "spec1",
                "guard": " p >= x AND x >= 4",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "4",
            "y": "0",
            "z": "3/2",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "6",
          "action": "a2",
          "transitions": [
            {
              "transition": {
                "PTA": "spec2",
                "guard": " x >= 10",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "10",
            "y": "-3/2",
            "z": "3/2",
            "global_time": "10"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "9/2",
          "action": "a3",
          "transitions": [
            {
              "transition": {
                "PTA": "spec1",
                "guard": "True",
                "updates": {

                }
              }
            },
            {
              "transition": {
                "PTA": "spec2",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "11/2",
            "y": "3",
            "z": "3/2",
            "global_time": "29/2"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "9/2",
          "action": "a2",
          "transitions": [
            {
              "transition": {
                "PTA": "spec2",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "1",
            "y": "15/2",
            "z": "6",
            "global_time": "19"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 p >= 0
& 4 > p

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "0"
    },
    "steps": [
      {
        "state": {
          "location": {
            "spec1": "l1",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "0",
            "y": "1",
            "z": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a1"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "y": "3/4",
            "z": "3/2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "2",
            "y": "1/2",
            "z": "3/2",
            "global_time": "2"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a3"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "y": "3/2",
            "z": "3/2",
            "global_time": "3"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "0",
            "y": "5/2",
            "z": "5/2",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #3

 Valuation:
  p = 4

 Other valuations with equivalent (discrete) run:
 p = 4

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "4"
    },
    "steps": [
      {
        "state": {
          "location": {
            "spec1": "l1",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "0",
            "y": "1",
            "z": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "6",
          "action": "a1"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "6",
            "y": "-1/2",
            "z": "3/2",
            "global_time": "6"
          },
          "flows": {
            "x": "1",
            "y": "-1/4",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "l2",
            "spec2": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "7",
            "y": "-3/4",
            "z": "3/2",
            "global_time": "7"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "0",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a3"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "6",
            "y": "1/4",
            "z": "3/2",
            "global_time": "8"
          },
          "flows": {
            "x": "-1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a2"
        }
      },
      {
        "state": {
          "location": {
            "spec1": "lT",
            "spec2": "lT"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "4",
            "x": "5",
            "y": "5/4",
            "z": "5/2",
            "global_time": "9"
          },
          "flows": {
            "x": "1",
            "y": "1",
            "z": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example',
		'input_files': ['testCounterExSimple-3.imi', 'testCounterExSimple-3.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-3.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 1/2

 Other valuations with equivalent (discrete) run:
 2*p = 1

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "1/2"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1/2",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "3",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " 2*p = 1 AND x = 3",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1/2",
            "x": "0",
            "global_time": "3"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 2*p > 1
OR
  p >= 0
& 1 > 2*p

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "0"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/03/31
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example (variant without invariant)',
		'input_files': ['testCounterExSimple-3b.imi', 'testCounterExSimple-3.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-3b.res' , 'content' : """
(************************************************************)
 Run #3

 Valuation:
  p = 1/2

 Other valuations with equivalent (discrete) run:
 2*p = 1

 Run nature: impossible run

 Run:
{
	"run": {
		"nature": "negative",
		"valuation": {
			"p": "1/2"
			},
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1/2",
					"x": "0",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "impossible",
				"duration": "4",
				"action": "a"
			}
			},
			{
			"state": {
				"location": {
					"pta": "ltarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1/2",
					"x": "4",
					"global_time": "4"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example (clock initially non-zero, strict constraints)',
		'input_files': ['testCounterExSimple-4.imi', 'testCounterExSimple-4.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-4.res' , 'content' : """
BEGIN RESULT
(************************************************************)
 Run #1


 Valuation:
  p = 4095/2

 Other valuations with equivalent (discrete) run:
 2048 > p
& p > 2047

 Run nature: valid run

 Run:
{
	"run": {
		"nature": "concrete",
		"valuation": {
			"p": "4095/2"
			},
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "4095/2",
					"x": "1",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "concrete",
				"duration": "1/2",
				"action": "a",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": "True AND  p = 2046 + x",
						"updates": {
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "ltarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "4095/2",
					"x": "3/2",
					"global_time": "1/2"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 2047 >= p
& p >= 0
OR
  p >= 2048

 Run nature: impossible run

 Run:
{
	"run": {
		"nature": "negative",
		"valuation": {
			"p": "0"
			},
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "0",
					"x": "1",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "impossible",
				"duration": "1",
				"action": "a"
			}
			},
			{
			"state": {
				"location": {
					"pta": "ltarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "0",
					"x": "2",
					"global_time": "1"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)


(************************************************************)
 Run #3

 Valuation:
  p = 4095/2

 Other valuations with equivalent (discrete) run:
 2048 > p
& p > 2047

 Run nature: impossible run

 Run:
{
	"run": {
		"nature": "negative",
		"valuation": {
			"p": "4095/2"
			},
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "4095/2",
					"x": "1",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "impossible",
				"duration": "5/4",
				"action": "a"
			}
			},
			{
			"state": {
				"location": {
					"pta": "ltarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "4095/2",
					"x": "9/4",
					"global_time": "5/4"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)
END RESULT

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example (violation possible only in the initial state)',
		'input_files': ['testCounterExSimple-5.imi', 'testCounterExSimple-5.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-5.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 1

 Other valuations with equivalent (discrete) run:
 p > 0

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "1"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/4",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "5/4",
            "global_time": "1/4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/4",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "3/2",
            "global_time": "1/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 p = 0

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "0"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "3",
            "global_time": "2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "4",
            "global_time": "3"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example (violation possible only at position 2)',
		'input_files': ['testCounterExSimple-5b.imi', 'testCounterExSimple-5b.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-5b.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 1

 Other valuations with equivalent (discrete) run:
 p > 0

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "1"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "3/2",
            "global_time": "1/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 p = 0

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "0"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "3",
            "global_time": "2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example (violation possible only at last position)',
		'input_files': ['testCounterExSimple-5c.imi', 'testCounterExSimple-5c.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-5c.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 1

 Other valuations with equivalent (discrete) run:
 p > 0

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "1"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "1",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 0

 Other valuations with equivalent (discrete) run:
 p = 0

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "0"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "1",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "0",
            "x": "2",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example (no bad parameter valuation derived)',
		'input_files': ['testCounterExSimple-6.imi', 'testCounterExSimple-6.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
				# NOTE / TODO: the NEGATIVE run is disabled so far! due to a BUG …
			{'file': 'testCounterExSimple-6.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 2

 Other valuations with equivalent (discrete) run:
 p > 1

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "2"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/02/09
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy example (no bad parameter nor clock valuation derived)',
		'input_files': ['testCounterExSimple-7.imi', 'testCounterExSimple-7.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-7.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 2

 Other valuations with equivalent (discrete) run:
 p > 1

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "2"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p": "2",
            "x": "2",
            "global_time": "1/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example (no bad parameter nor clock valuation derived)',
		'input_files': ['testCounterExSimple-8.imi', 'testCounterExSimple-8.imiprop'],
		'options'    : '-merge none -comparison equality -draw-cart',
		'expectations' : [
			{'file': 'testCounterExSimple-8.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p1 = 1
& p2 = 1/2

 Other valuations with equivalent (discrete) run:
 p1 > p2
& p2 > 0
& 6 > p2

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p1": "1",
      "p2": "1/2"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "1",
            "p2": "1/2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p2 > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "1",
            "p2": "1/2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "1",
            "p2": "1/2",
            "x": "2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p1 > p2 AND p2 + x > p1",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "1",
            "p2": "1/2",
            "x": "3",
            "global_time": "1"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p1 = 3
& p2 = 3

 Other valuations with equivalent (discrete) run:
 p1 > 0
& p2 >= p1
& 6 > p2

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p1": "3",
      "p2": "3"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "3",
            "p2": "3",
            "x": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "0",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " p2 > 0",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "3",
            "p2": "3",
            "x": "3/2",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1/2",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": "True",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "3",
            "p2": "3",
            "x": "2",
            "global_time": "1/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "ltarget"
          },
          "discrete_variables": {

          },
          "continuous_variables": {
            "p1": "3",
            "p2": "3",
            "x": "3",
            "global_time": "3/2"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
			# NOTE: Just check its existence
			{'file': 'testCounterExSimple-8_ex_2_neg.png' , 'content' : ""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test EFexemplify on a toy monitoring-style example',
		'input_files': ['testCounterExSimple-9.imi', 'testCounterExSimple-9.imiprop'],
		'options'    : '-merge none -comparison equality',
		'expectations' : [
			{'file': 'testCounterExSimple-9.res' , 'content' : """

BEGIN RESULT

(************************************************************)
 Run #1

 Valuation:
  p = 1

 Other valuations with equivalent (discrete) run:
 p >= 0

 Run nature: valid run

 Run:
{
	"run": {
		"nature": "concrete",
		"valuation": {
			"p": "1"
			},
		"steps": [
			{
			"state": {
				"location": {
					"pta": "l1"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1",
					"x": "0",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			},
			{
			"transition": {
				"nature": "concrete",
				"duration": "0",
				"action": "a",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": "True",
						"updates": {
							"x": "0"
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l2"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1",
					"x": "0",
					"global_time": "0"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "1",
				"action": "b",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " p = x",
						"updates": {
							"x": "0"
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "l3"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1",
					"x": "0",
					"global_time": "1"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}, 
			{
			"transition": {
				"nature": "concrete",
				"duration": "1",
				"action": "b",
				"transitions": [
					{
					"transition": {
						"PTA": "pta",
						"guard": " p = x",
						"updates": {
						}
					}
					}
				]
			}
			},
			{
			"state": {
				"location": {
					"pta": "ltarget"
				},
				"discrete_variables": {
				},
				"continuous_variables": {
					"p": "1",
					"x": "1",
					"global_time": "2"
				},
				"flows": {
					"x": "1", 
					"global_time": "1"
				}
			}
			}
		]
	}
}
(************************************************************)
END RESULT

"""
			} # end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example without parameter',
		'input_files': ['testCounterExSimple.imi', 'testCounterExSimple.imiprop'],
		'options'    : '-merge none -comparison equality -no-var-autoremove',
		'expectations' : [
			{'file': 'testCounterExSimple.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:


 Other valuations with equivalent (discrete) run:
True

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": null,
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {
            "d1": "1/2",
            "d2": "50"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "3",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 3",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {
            "d1": "1/2",
            "d2": "50"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "3"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 1",
                "updates": {
                  "x": "0",
                  "d1": "d1 + 2"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {
            "d1": "5/2",
            "d2": "50"
          },
          "continuous_variables": {
            "x": "0",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 1",
                "updates": {
                  "d2": "d2 / 2"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l4"
          },
          "discrete_variables": {
            "d1": "5/2",
            "d2": "25"
          },
          "continuous_variables": {
            "x": "1",
            "global_time": "5"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "7",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 8",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "lbad"
          },
          "discrete_variables": {
            "d1": "5/2",
            "d2": "25"
          },
          "continuous_variables": {
            "x": "8",
            "global_time": "12"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/?
		# Last modified            : 2022/03/31
		# Test for IMITATOR version: 3.3
		'purpose'    : 'Test EFexemplify on a toy example with parameters and discrete variables',
		'input_files': ['testCounterExSimple-2.imi', 'testCounterExSimple-2.imiprop'],
		'options'    : '-merge none -comparison equality -no-var-autoremove',
		'expectations' : [
			{'file': 'testCounterExSimple-2.res' , 'content' : """
(************************************************************)
 Run #1

 Valuation:
  p = 1/2

 Other valuations with equivalent (discrete) run:
 1 > p
& p >= 0

 Run nature: valid run

 Run:
{
  "run": {
    "nature": "concrete",
    "valuation": {
      "p": "1/2"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {
            "d1": "1/2"
          },
          "continuous_variables": {
            "p": "1/2",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "3",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 3",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {
            "d1": "1/2"
          },
          "continuous_variables": {
            "p": "1/2",
            "x": "0",
            "global_time": "3"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 1",
                "updates": {
                  "x": "0",
                  "d1": "d1 + 2"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {
            "d1": "5/2"
          },
          "continuous_variables": {
            "p": "1/2",
            "x": "0",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " 1 > p AND x = 1",
                "updates": {
                  "d1": "d1 / 2"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l4"
          },
          "discrete_variables": {
            "d1": "5/4"
          },
          "continuous_variables": {
            "p": "1/2",
            "x": "1",
            "global_time": "5"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "7",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 8",
                "updates": {

                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "lbad"
          },
          "discrete_variables": {
            "d1": "5/4"
          },
          "continuous_variables": {
            "p": "1/2",
            "x": "8",
            "global_time": "12"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)


(************************************************************)
 Run #2

 Valuation:
  p = 1

 Other valuations with equivalent (discrete) run:
 p >= 1

 Run nature: impossible run

 Run:
{
  "run": {
    "nature": "negative",
    "valuation": {
      "p": "1"
    },
    "steps": [
      {
        "state": {
          "location": {
            "pta": "l1"
          },
          "discrete_variables": {
            "d1": "1/2"
          },
          "continuous_variables": {
            "p": "1",
            "x": "0",
            "global_time": "0"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "3",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 3",
                "updates": {
                  "x": "0"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l2"
          },
          "discrete_variables": {
            "d1": "1/2"
          },
          "continuous_variables": {
            "p": "1",
            "x": "0",
            "global_time": "3"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "concrete",
          "duration": "1",
          "action": "a",
          "transitions": [
            {
              "transition": {
                "PTA": "pta",
                "guard": " x = 1",
                "updates": {
                  "x": "0",
                  "d1": "d1 + 2"
                }
              }
            }
          ]
        }
      },
      {
        "state": {
          "location": {
            "pta": "l3"
          },
          "discrete_variables": {
            "d1": "5/2"
          },
          "continuous_variables": {
            "p": "1",
            "x": "0",
            "global_time": "4"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "impossible",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "l4"
          },
          "discrete_variables": {
            "d1": "5/4"
          },
          "continuous_variables": {
            "p": "1",
            "x": "1",
            "global_time": "5"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      },
      {
        "transition": {
          "nature": "arbitrary",
          "duration": "1",
          "action": "a"
        }
      },
      {
        "state": {
          "location": {
            "pta": "lbad"
          },
          "discrete_variables": {
            "d1": "5/4"
          },
          "continuous_variables": {
            "p": "1",
            "x": "2",
            "global_time": "6"
          },
          "flows": {
            "x": "1",
            "global_time": "1"
          }
        }
      }
    ]
  }
}
(************************************************************)
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test LoopSynth: BlT09-fig1',
		'input_files': ['BlT09-fig1.imi', 'BlT09-fig1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'BlT09-fig1.res' , 'content' : """
BEGIN CONSTRAINT
 u + 2 > l
& l >= 0
& u > 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test LoopSynth: flip-flop (no loop)',
		'input_files': ['flipflop.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test LoopSynth: no loop but state met twice (BFS)',
		'tags'       : 'cycle',
		'input_files': ['cycles/visited-twice-nocycle.imi' , 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'visited-twice-nocycle.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test LoopSynth: no loop but state met twice (NDFS)',
		'tags'       : 'cycle',
		'input_files': ['cycles/visited-twice-nocycle.imi' , 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'visited-twice-nocycle.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		'purpose'    : 'Test LoopSynth: simple example with loop (BFS)',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
 10 >= p2
& p2 >= 0
& p1 >= 0
& p1 + 5 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test LoopSynth: simple example with loop (NDFS)',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
 10 >= p2
& p2 >= 0
& p1 >= 0
& p1 + 5 >= p2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with no real loop (1a) (BFS)',
		'input_files': ['testNoCycle-1a.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNoCycle-1a.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with no real loop (1a) (NDFS)',
		'input_files': ['testNoCycle-1a.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNoCycle-1a.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with no real loop (1b) (BFS)',
		'input_files': ['testNoCycle-1b.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNoCycle-1b.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with no real loop (1b) (NDFS)',
		'input_files': ['testNoCycle-1b.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNoCycle-1b.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test LoopSynth: simple example with loop for any valuation (BFS)',
		'input_files': ['deadlock/PDFC5.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p3 >= 0
& p2 >= 0
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test LoopSynth: simple example with loop for any valuation (NDFS)',
		'input_files': ['deadlock/PDFC5.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p3 >= 0
& p2 >= 0
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: witness vs. synthesis (witness) (BFS)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-loop-witness.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: witness vs. synthesis (synthesis) (BFS)',
		'input_files': ['testEFInclMerge.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: witness vs. synthesis (synthesis) (NDFS)',
		'input_files': ['testEFInclMerge.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 5 > p
& p >= 0
OR
  p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (BFS)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (NDFS)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (over-approx with -comparison inclusion)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-comparison inclusion -cycle-algo BFS',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (over-approx with -comparison inclusion -merge)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-cycle-algo BFS -comparison inclusion -merge yes',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	# TODO: do -merge only! (so far does not terminate)

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (under-approx with -depth-limit)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-cycle-algo BFS -depth-limit 3 -no-cumulative-pruning',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : depth limit (2 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/09
		# Test for IMITATOR version: 3
		'purpose'    : 'Test LoopSynth: simple example with 1 real loop (invalid with -comparison inclusion -depth-limit)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-loop.imiprop'],
		'options'    : '-cycle-algo BFS -comparison inclusion -depth-limit 3',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possibly invalid
Termination                             : depth limit (1 successor unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: witness vs. synthesis (witness)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-accloop-witness.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: witness vs. synthesis (synthesis) (BFS)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: witness vs. synthesis (synthesis) (NDFS)',
		'input_files': ['testEFInclMerge.imi', 'testEFInclMerge-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testEFInclMerge.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2046
OR
  5 > p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with no real loop (1a)',
		'input_files': ['testNoCycle-1a.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNoCycle-1a.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with no real loop (1b)',
		'input_files': ['testNoCycle-1b.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNoCycle-1b.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AccLoopSynth: no loop but state met twice (BFS)',
		'tags'       : 'cycle',
		'input_files': ['cycles/visited-twice-nocycle.imi' , 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'visited-twice-nocycle.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AccLoopSynth: no loop but state met twice (NDFS)',
		'tags'       : 'cycle',
		'input_files': ['cycles/visited-twice-nocycle.imi' , 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'visited-twice-nocycle.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth (BFS): simple example with 1 real loop',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth (NDFS): simple example with 1 real loop',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		## Test version             : 1
		## Test since               : 2020/09/09
		## Last modified            : 2020/09/10
		## Test deleted             : 2021/09/01
		## Test for IMITATOR version: 3
		#'purpose'    : 'Test AccLoopSynth: simple example with 1 real loop (syntax variant without parentheses)',
		#'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop-noparen.imiprop'],
		#'options'    : '-cycle-algo BFS',
		#'expectations' : [
			#{'file': 'exLoopIncl.res' , 'content' : """
#BEGIN CONSTRAINT
 #p = 2
#END CONSTRAINT

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : good
#------------------------------------------------------------
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with 1 real loop (over-approx with -comparison inclusion)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo BFS -comparison inclusion',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with 1 real loop (over-approx with -comparison inclusion -merge)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo BFS -comparison inclusion -merge yes',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	# TODO: do -merge only! (so far does not terminate)

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with 1 real loop (under-approx with -depth-limit)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo BFS -depth-limit 3 -no-cumulative-pruning',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : depth limit (2 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/09
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example with 1 real loop (invalid with -comparison inclusion -depth-limit)',
		'input_files': ['exLoopIncl.imi', 'exLoopIncl-accloop.imiprop'],
		'options'    : '-cycle-algo BFS -comparison inclusion -depth-limit 3',
		'expectations' : [
			{'file': 'exLoopIncl.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possibly invalid
Termination                             : depth limit (1 successor unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,

	# NOTE: test removed since there is no accepting loop synthesis + observer patterns as of version 3
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test AccLoopSynth: flipflop (no loop)',
		#'input_files': ['flipflop.imi', 'flipflop-accloop.imiprop'],
		#'options'    : '',
		#'expectations' : [
			#{'file': 'flipflop.res' , 'content' : """
#BEGIN CONSTRAINT
#False
#END CONSTRAINT

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : good
#------------------------------------------------------------
#Number of states                        : 20
#Number of transitions                   : 19
#Number of computed states               : 20
#"""
			#} #end result file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/07/22
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example 1',
		'input_files': ['testNDFS-1.imi', 'testNDFS1-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNDFS-1.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/07/22
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth: simple example 2',
		'input_files': ['testNDFS-2.imi', 'testNDFS2-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testNDFS-2.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
OR
  p = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/07/22
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth (BFS): simple example 3 (accepting for all valuations)',
		'input_files': ['deadlock/PDFC5.imi', 'deadlock/PDFC5-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p3 >= 0
& p2 >= 0
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynth (NDFS): simple example 3 (accepting for all valuations)',
		'input_files': ['deadlock/PDFC5.imi', 'deadlock/PDFC5-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p3 >= 0
& p2 >= 0
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/22
		# Last modified            : 2020/09/22
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test AccLoopSynth (BFS) (without option -no-cumulative-pruning)',
		'input_files': ['testInclusionEF.imi', 'testInclusionEF-loop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testInclusionEF.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 4
Number of transitions                   : 4
Number of computed states               : 5
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/22
		# Last modified            : 2020/09/22
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test AccLoopSynth (NDFS)',
		'input_files': ['testInclusionEF.imi', 'testInclusionEF-loop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testInclusionEF.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/22
		# Last modified            : 2020/09/22
		# Test for IMITATOR version: 3.0
		'purpose'    : 'Test AccLoopSynth (with option -no-cumulative-pruning)',
		'input_files': ['testInclusionEF.imi', 'testInclusionEF-loop.imiprop'],
		'options'    : '-cycle-algo BFS -no-cumulative-pruning',
		'expectations' : [
			{'file': 'testInclusionEF.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p
& p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 14
Number of transitions                   : 15
Number of computed states               : 16
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AccLoopSynthNDFS emptiness: flipflop (no loop)',
		'input_files': ['flipflop.imi', 'flipflop-accloop-witness.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test AccLoopSynthNDFS synthesis: flipflop (no loop)',
		'input_files': ['flipflop.imi', 'flipflop-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS: simple example with no real loop (1a)',
		'input_files': ['testNoCycle-1a.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testNoCycle-1a.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/24
		# Last modified            : 2020/09/24
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS: simple example with no real loop (1b)',
		'input_files': ['testNoCycle-1b.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testNoCycle-1b.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/07/22
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS emptiness: simple example 1',
		'input_files': ['testNDFS-1.imi', 'testNDFS1-accloop-witness.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNDFS-1.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/08/01
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS synthesis: simple example 1',
		'input_files': ['testNDFS-1.imi', 'testNDFS1-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNDFS-1.res' , 'content' : """
BEGIN CONSTRAINT
 p = 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/08/01
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS emptiness: simple example 2',
		'input_files': ['testNDFS-2.imi', 'testNDFS2-accloop-witness.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNDFS-2.res' , 'content' :
# NOTE: p = 4 would also be an acceptable result
"""
BEGIN CONSTRAINT
 p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : terminated after reaching a target state (some states may have been unexplored)
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/08/01
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test AccLoopSynthNDFS synthesis: simple example 2',
		'input_files': ['testNDFS-2.imi', 'testNDFS2-accloop.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'testNDFS-2.res' , 'content' : """
BEGIN CONSTRAINT
 p = 2
OR
  p = 1
OR
  p = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/13
		# Last modified            : 2021/07/13
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test cycle synthesis with BSID',
		'input_files': ['ALR15_fig1.imi', 'ALR15_fig1-CycleThrough.imiprop'],
		'options'    : '-cycle-algo NDFS -depth-step 1',
		'expectations' : [
			{'file': 'ALR15_fig1.res' , 'content' : """
BEGIN CONSTRAINT
 1 >= p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test AccLoopSynth with depth-limit to check the quick reachability (NDFS)',
		'input_files': ['quick-reach.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-depth-limit 3 -cycle-algo NDFS',
		'expectations' : [
			{'file': 'quick-reach.res' , 'content' : """
BEGIN CONSTRAINT
 2 > 4*p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test AccLoopSynth with depth-limit to check the quick reachability (BFS)',
		'input_files': ['quick-reach.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-depth-limit 4 -cycle-algo BFS',
		'expectations' : [
			{'file': 'quick-reach.res' , 'content' : """
BEGIN CONSTRAINT
 2 > 4*p
& p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test AccLoopSynth (BFS) on a simple example',
		'input_files': ['test-generalized-buchi.imi', 'test-generalized-buchi-cycle.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'test-generalized-buchi.res' , 'content' : """
BEGIN CONSTRAINT
 10 >= p_1
& p_1 >= 0
& p_2 >= 0
OR
  p_1 >= 0
& p_2 = 2000
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test AccLoopSynth (NDFS) on a simple example',
		'input_files': ['test-generalized-buchi.imi', 'test-generalized-buchi-cycle.imiprop'],
		'options'    : '-cycle-algo NDFS',
		'expectations' : [
			{'file': 'test-generalized-buchi.res' , 'content' : """
BEGIN CONSTRAINT
 10 >= p_1
& p_1 >= 0
& p_2 >= 0
OR
  p_1 >= 0
& p_2 = 2000
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/09/01
		# Last modified            : 2021/09/01
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test generalized acceptance condition (BFS)',
		'input_files': ['test-generalized-buchi.imi', 'test-generalized-buchi.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'test-generalized-buchi.res' , 'content' : """
BEGIN CONSTRAINT
 10 >= p_1
& p_1 >= 2
& p_2 = 2000
OR
  p_2 >= 2021
& p_1 >= 2
& 10 >= p_1
& 2046 >= p_2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# AF
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: initial accepting location',
		'tags'       : 'AF',
		'input_files': ['AF/AF-initial.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial.res' , 'content' : """
BEGIN CONSTRAINT
  10 >= p
 & p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: initial accepting location (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-initial.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial.res' , 'content' : """
BEGIN CONSTRAINT
  10 >= p
 & p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: unsatisfiable initial location',
		'tags'       : 'AF',
		'input_files': ['AF/AF-initial-unsatisfiable.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial-unsatisfiable.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: unsatisfiable initial location (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-initial-unsatisfiable.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial-unsatisfiable.res' , 'content' : """
Error                                   : unsatisfiable initial conditions
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: false (no accepting location)',
		'tags'       : 'AF',
		'input_files': ['AF/AF-false-noaccepting.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-false-noaccepting.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: false (no accepting location) (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-false-noaccepting.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-false-noaccepting.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: initial accepting location restricted by an invariant',
		'tags'       : 'AF',
		'input_files': ['AF/AF-initial-invariant.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial-invariant.res' , 'content' : """
BEGIN CONSTRAINT
 1 > p
& p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: initial accepting location restricted by an invariant (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-initial-invariant.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-initial-invariant.res' , 'content' : """
BEGIN CONSTRAINT
 1 > p
& p >= 0
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: false due to no invariant',
		'tags'       : 'AF',
		'input_files': ['AF/AF-false-noinvariant.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-false-noinvariant.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: no invariant but guard bounded from below (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-noinvariant-noupperguard.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-noinvariant-noupperguard.res' , 'content' : """
BEGIN CONSTRAINT
  p >= 0
 & 10 >= p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: no invariant but guard bounded from below',
		'tags'       : 'AF',
		'input_files': ['AF/AF-noinvariant-noupperguard.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-noinvariant-noupperguard.res' , 'content' : """
BEGIN CONSTRAINT
  p >= 0
 & 10 >= p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: no invariant but guard bounded from below (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-noinvariant-noupperguard.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-noinvariant-noupperguard.res' , 'content' : """
BEGIN CONSTRAINT
  p >= 0
 & 10 >= p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple reachability',
		'tags'       : 'AF',
		'input_files': ['AF/AF-simple.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-simple.res' , 'content' : """
BEGIN CONSTRAINT
p = 5
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple reachability (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-simple.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-simple.res' , 'content' : """
BEGIN CONSTRAINT
p = 5
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple reachability (variant with 2 parameters)',
		'tags'       : 'AF',
		'input_files': ['AF/AF-simple2.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-simple2.res' , 'content' : """
BEGIN CONSTRAINT
  p2 > 5
 & 10 >= p2
 & p1 = 5
 OR
   p2 >= 0
 & 5 > p2
 & p1 = 5
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple reachability (variant with 2 parameters)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-simple2.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-simple2.res' , 'content' : """
BEGIN CONSTRAINT
  p2 > 5
 & 10 >= p2
 & p1 = 5
 OR
   p2 >= 0
 & 5 > p2
 & p1 = 5
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with a loop',
		'tags'       : 'AF',
		'input_files': ['AF/AF-loop.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-loop.res' , 'content' : """
BEGIN CONSTRAINT
  p > 6
 & 9 > p
 OR
   p > 5
 & 6 > p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with a loop',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-loop.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-loop.res' , 'content' : """
BEGIN CONSTRAINT
  p > 6
 & 9 > p
 OR
   p > 5
 & 6 > p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with a loop for all valuations',
		'tags'       : 'AF',
		'input_files': ['AF/AF-loop-false.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-loop-false.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with a loop for all valuations',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-loop-false.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-loop-false.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/19
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with negative flows and deadlock',
		'tags'       : 'AF',
		'input_files': ['AF/AF-negflow.imi' , 'basic-properties/synth-AF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-negflow.res' , 'content' : """
BEGIN CONSTRAINT
  15 >= p1
 & p1 >= 0
 & p2 > 5 + p1
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/19
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AF: simple example with negative flows and deadlock (AU syntax)',
		'tags'       : 'AF,AU',
		'input_files': ['AF/AF-negflow.imi' , 'basic-properties/synth-A-true-U-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AF-negflow.res' , 'content' : """
BEGIN CONSTRAINT
  15 >= p1
 & p1 >= 0
 & p2 > 5 + p1
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/26
		# Last modified            : 2024/01/26
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AU: simple example',
		'tags'       : 'AU',
		'input_files': ['AF/AU-simple1.imi' , 'AF/AU-simple1.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AU-simple1.res' , 'content' : """
BEGIN CONSTRAINT
  p2 > 5
 & 10 >= p2
 & p1 = 5
 OR
   p2 > 3
 & 5 > p2
 & p1 = 5
 OR
   p2 > 2
 & 3 > p2
 & p1 = 5
 OR
   p2 >= 0
 & 2 > p2
 & p1 = 5
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
		# Test version             : 1
		# Test author              : Étienne André and Masaki Waga
		# Test since               : 2024/03/14
		# Last modified            : 2024/03/14
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AU: self-composition example',
		'tags'       : 'AU',
		'input_files': ['AF/selfcomp-AF.imi' , 'AF/selfcomp-AF.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'selfcomp-AF.res' , 'content' : """
BEGIN CONSTRAINT
  p >= 0
 & 3 > p
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/05
		# Last modified            : 2024/02/05
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AW: simple example',
		'tags'       : 'AW',
		'input_files': ['AF/AU-simple1.imi' , 'AF/AU-simple1-AW.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'AU-simple1.res' , 'content' : """
BEGIN CONSTRAINT
  p2 >= 0
 & 5 > p2
 & p1 = 5
 OR
   p2 > 5
 & 10 >= p2
 & p1 = 5

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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/02/09
		# Last modified            : 2024/02/09
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test AW: no loop over phi',
		'tags'       : 'AW',
		'input_files': ['cycles/visited-twice-nocycle.imi' , 'cycles/visited-twice-nocycle-AW.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'visited-twice-nocycle.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# ??
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/10/07
		# Last modified            : 2021/10/07
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test option: -no-global-time-clock-in-comparison (without)',
		'input_files': ['testExemplify-loop-single.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS -depth-limit 10',
		'expectations' : [
			{'file': 'testExemplify-loop-single.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		# Test version             : 1
		# Test since               : 2021/10/07
		# Last modified            : 2021/10/07
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Test option: -no-global-time-clock-in-comparison (with)',
		'input_files': ['testExemplify-loop-single.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '-cycle-algo BFS -no-global-time-clock-in-comparison -depth-limit 10',
		'expectations' : [
			{'file': 'testExemplify-loop-single.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 2
Number of transitions                   : 2
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	##------------------------------------------------------------
	#{
		## Test version             : 1
		## Test since               : 2021/09/23
		## Last modified            : 2021/09/23
		## Test for IMITATOR version: 3.2
		#'purpose'    : 'Test cycle exemplification [temporarily disabled]',
		#'input_files': ['testExemplify-loop-single.imi', 'exemplify-loop.imiprop'],
		#'options'    : '-cycle-algo BFS',
		#'expectations' : [
			#{'file': 'testExemplify-loop-single.res' , 'content' : """
#TODO
		#""" # TODO
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	##------------------------------------------------------------
	#{
		## Test version             : 1
		## Test since               : 2021/10/07
		## Last modified            : 2021/10/07
		## Test for IMITATOR version: 3.2
		#'purpose'    : 'Test cycle exemplification [temporarily disabled]',
		#'input_files': ['testExemplify-loop-single2.imi', 'exemplify-loop.imiprop'],
		#'options'    : '-cycle-algo BFS',
		#'expectations' : [
			#{'file': 'testExemplify-loop-single2.res' , 'content' : """
#TODO
		#""" # TODO
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test state predicate with `accepting` keyword and location predicate in property: EF',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-EF.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
 OR
   p = 4

END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/14
		# Last modified            : 2020/09/14
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: EF',
		'input_files': ['testEFaccepting.imi', 'basic-properties/synth-EF-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/14
		# Test for IMITATOR version: 3
		'purpose'    : 'Test empty state predicate: EF',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-EFnopred.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/14
		# Last modified            : 2020/09/14
		# Test for IMITATOR version: 3
		'purpose'    : 'Test complex state predicate: EF',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-EFcomplex.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
	p = 2
OR
	p = 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test state predicate with `accepting` keyword and location predicate in property: Loop',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
 OR
   p = 4

END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: Loop',
		'input_files': ['testEFaccepting.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/03/12
		# Last modified            : 2021/03/12
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: Loop [alias AccCycle]',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop-nopred-acccycle.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/03/12
		# Last modified            : 2021/03/12
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: Loop [alias AcceptingCycle]',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop-nopred-acceptingcycle.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/03/12
		# Last modified            : 2021/03/12
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: Loop [alias AccLoop]',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop-nopred-accloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/03/12
		# Last modified            : 2021/03/12
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: Loop [alias AcceptingLoop]',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop-nopred-acceptingloop.imiprop'],
		'options'    : '-cycle-algo BFS',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test state predicate with `accepting` keyword and location predicate in property: NDFS',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-accloop.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
 OR
   p = 4

END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test only `accepting` keyword: NDFS',
		'input_files': ['testEFaccepting.imi', 'basic-properties/synth-cyclethrough-accepting.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
  p = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/10
		# Last modified            : 2020/09/10
		# Test for IMITATOR version: 3
		'purpose'    : 'Test state predicate with `accepting` keyword and location predicate in property: PRPC',
		'input_files': ['testEFaccepting.imi', 'testEFaccepting-PRPC.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFaccepting.res' , 'content' : """
BEGIN CONSTRAINT
 2 > p
& p >= 0
OR
  4 > p
& p > 2
OR
  p > 4
<good|bad>
 p = 2
OR
  p = 4
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact <good|bad> exact
Termination                             : regular termination
Constraint nature                       : good/bad

------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: flip-flop (no loop)',
		'input_files': ['flipflop.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '-nz-method already',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: PDFC4 (loop but Zeno)',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '-nz-method already',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: PDFC4 (loop but Zeno); check',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '-nz-method check',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: PDFC4 (loop but Zeno); transform',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: CUBPTA1 (non-Zeno loop)',
		'input_files': ['CUBPTA1.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '-nz-method already',
		'expectations' : [
			{'file': 'CUBPTA1.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test NZCUB: CUBPTA2 (2 non-Zeno loops)',
		'input_files': ['CUBPTA2.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '-nz-method already',
		'expectations' : [
			{'file': 'CUBPTA2.res' , 'content' : """
BEGIN CONSTRAINT
 p2 >= 0
& p1 >= 5
OR
  p1 >= 0
& p2 >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test infinite run on one example (Zeno version)',
		'input_files': ['testInfiniteRun.imi', 'basic-properties/synth-loop.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testInfiniteRun.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test infinite run on one example (non-Zeno version)',
		'input_files': ['testInfiniteRun.imi', 'basic-properties/synth-NZcycle.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testInfiniteRun.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example with no deadlock but constrained initial state',
		'input_files': ['deadlock/PDFC0-nodeadlock-init.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'PDFC0-nodeadlock-init.res' , 'content' : """
BEGIN CONSTRAINT
 5 >= p1
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example without clocks',
		'input_files': ['deadlock/PDFC1.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-states-description',
		'expectations' : [
			{'file': 'PDFC1.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
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

  /************************************************************/
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
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2024/01/18
		# Last modified            : 2024/01/18
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test PDFC: very basic example; variant with 1 clock',
		'tags'       : 'deadlock',
		'input_files': ['deadlock/PDFC1-1clock.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'PDFC1-1clock.res' , 'content' : """
BEGIN CONSTRAINT
False
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
		'purpose'    : 'Test PDFC: very basic example (false result)',
		'input_files': ['deadlock/PDFC3.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description',
		'expectations' : [
			{'file': 'PDFC3.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
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
   p2 >= 0
& p1 >= 0

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
& p2 >= 0
& p1 >= 0
& p1 + 5 >= p2

  /************************************************************/
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
		## Test version             : 1
		## Test author              : Étienne André
		## Test since               : 2022/10/03
		## Last modified            : 2022/10/03
		## Test for IMITATOR version: 3.3
		'purpose'    : 'Test PDFC: very basic example (normal result)',
		'input_files': ['deadlock/PDFC4.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description',
		'expectations' : [
			{'file': 'PDFC4.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& p1 + 5 >= p2
& 5 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		## Test version             : 2
		## Test author              : Étienne André
		## Test since               : 2016
		## Last modified            : 2022/10/03
		## Test for IMITATOR version: 3.3
		'purpose'    : 'Test PDFC: very basic example, Zeno version (normal result)',
		'input_files': ['deadlock/PDFC4-zeno.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description',
		'expectations' : [
			{'file': 'PDFC4-zeno.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& p1 + 5 >= p2
& 5 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: very basic example (false result)',
		'input_files': ['deadlock/PDFC6.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description -no-var-autoremove',
		'expectations' : [
			{'file': 'PDFC6.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Number of states              : 2
Number of transitions         : 1
Number of computed states     : 2
"""
			} #end result file
			,
			{'file': 'PDFC6-statespace.states' , 'content' : """
  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==>
& p >= 0
& x1 >= 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==>
& p >= 0
& x1 >= 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
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
		'purpose'    : 'Test PDFC: very basic example (true result)',
		'input_files': ['deadlock/PDFC7.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description',
		'expectations' : [
			{'file': 'PDFC7.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Number of states              : 2
Number of transitions         : 2
Number of computed states     : 3
"""
			} #end result file
			,
			{'file': 'PDFC7-statespace.states' , 'content' : """

  DESCRIPTION OF THE STATES

  /************************************************************/
  INITIAL
  STATE 0:
  pta: l1 ==>
& p >= 0
& x1 >= 0

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  STATE 1:
  pta: l2 ==>
& p >= 0
& x1 >= p

  Projection onto the parameters:
   p >= 0

  /************************************************************/
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
		'purpose'    : 'Test PDFC: basic example with disjunction',
		'input_files': ['deadlock/PDFC5.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& p1 + 5 = p3
OR
  p1 >= 0
& p3 > 5 + p1
& p2 >= 5 + p1
OR
  p1 >= 0
& p1 + 5 > p3
& p3 >= 0
& p2 >= 5 + p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: basic example with disjunction (no inclusion)',
		'input_files': ['deadlock/PDFC5.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-comparison equality',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& p1 + 5 = p3
OR
  p1 >= 0
& p3 > 5 + p1
& p2 >= 5 + p1
OR
  p1 >= 0
& p1 + 5 > p3
& p3 >= 0
& p2 >= 5 + p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: basic example with disjunction (explicit inclusion)',
		'input_files': ['deadlock/PDFC5.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-comparison inclusion',
		'expectations' : [
			{'file': 'PDFC5.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 >= 0
& p1 + 5 = p3
OR
  p1 >= 0
& p3 > 5 + p1
& p2 >= 5 + p1
OR
  p1 >= 0
& p1 + 5 > p3
& p3 >= 0
& p2 >= 5 + p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: example with early termination due to false constraint',
		'input_files': ['flipflop.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : ' -states-description',
		'expectations' : [
			{'file': 'flipflop.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: example with basic backward under-approximation',
		'input_files': ['deadlock/PDFC8.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-depth-limit 5 -no-var-autoremove',
		'expectations' : [
			{'file': 'PDFC8.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 2
& 3 >= p1
OR
  p1 > 0
& 2 > p1
<good|bad>
 p1 > 3
OR
  p1 = 2
OR
  p1 = 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : possible under-approximation <good|bad> possible over-approximation
Termination                   : depth limit (1 successor unexplored)
Constraint nature             : good/bad
------------------------------------------------------------
Number of states              : 7
Number of transitions         : 7
Number of computed states     : 8
"""
			} #end result file
		] # end expectations
	} # end test case
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: example with basic backward under-approximation and exact result',
		'input_files': ['deadlock/PDFC8b.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-depth-limit 5 -no-var-autoremove', #TODO: re-do without '-no-var-autoremove'
		'expectations' : [
			{'file': 'PDFC8b.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 2
& 3 >= p1
OR
  p1 > 0
& 2 > p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : depth limit (1 successor unexplored)
Constraint nature             : good
------------------------------------------------------------
Number of states              : 7
Number of transitions         : 7
Number of computed states     : 8
"""
			} #end result file
		] # end expectations
	} # end test case

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: another example with basic backward under-approximation',
		'input_files': ['deadlock/PDFC9.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-depth-limit 5 -no-var-autoremove', #TODO: re-do without '-no-var-autoremove'
		'expectations' : [
			{'file': 'PDFC9.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 2
<good|bad>
 2 >= p1
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : possible under-approximation <good|bad> possible over-approximation
Termination                   : depth limit (1 successor unexplored)
Constraint nature             : good/bad
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: again another example with basic backward under-approximation',
		'input_files': ['deadlock/PDFC9b.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-depth-limit 5 -no-var-autoremove', #TODO: re-do without '-no-var-autoremove'
		'expectations' : [
			{'file': 'PDFC9b.res' , 'content' : """
BEGIN CONSTRAINT
 p1 > 2
<good|bad>
 2 >= p1
& p1 >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness          : possible under-approximation <good|bad> possible over-approximation
Termination                   : depth limit (1 successor unexplored)
Constraint nature             : good/bad
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
			
	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PDFC: example with negative flow',
		'input_files': ['deadlock/PDFC10-negflow.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'PDFC10-negflow.res' , 'content' : """
BEGIN CONSTRAINT
 p1 >= 0
& p2 > 5 + p1
& 15 >= p1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case

	,

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TESTING FIX TO DEADLOCK COMPUTATION
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 1',
		'input_files': ['deadlock/deadlock1.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'deadlock1.res' , 'content' : """
			BEGIN CONSTRAINT
 3 >= p
& p >= 0
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 2',
		'input_files': ['deadlock/deadlock2.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock2.res' , 'content' : """
BEGIN CONSTRAINT
 2 >= q
& p >= 0
& q >= 0
& 3 >= p
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 3',
		'input_files': ['deadlock/deadlock3.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock3.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 2
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 4',
		'input_files': ['deadlock/deadlock4.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock4.res' , 'content' : """
BEGIN CONSTRAINT
 2 >= q
& p >= 0
& q >= 0
& 2 >= p
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 5',
		'input_files': ['deadlock/deadlock5.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock5.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 2
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 6',
		'input_files': ['deadlock/deadlock6.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock6.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 4
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2023/01/23
		# Test for IMITATOR version: 3.4-beta
		'purpose'    : 'Updated Deadlock Test 7',
		'input_files': ['deadlock/deadlock7.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock7.res' , 'content' : """
BEGIN CONSTRAINT
 q >= 4
& p >= 4
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Mikael Bisgaard Dahlsen-Jensen
		# Test since               : 2022
		# Last modified            : 2022
		# Test for IMITATOR version: 3.2
		'purpose'    : 'Updated Deadlock Test 8',
		'input_files': ['deadlock/deadlock8.imi', 'basic-properties/synth-deadlock-free.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'deadlock8.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the inverse method (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop.pi0'],
		'options'    : '',
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
		# Test version             : 1
		# Test since               : 2020/09/21
		# Last modified            : 2020/09/21
		# Test for IMITATOR version: 3
		'purpose'    : 'Test the inverse method (flip-flop) #witness',
		'input_files': ['flipflop.imi', 'flipflop-witness.pi0'],
		'options'    : '',
		'expectations' : [
# NOTE: witness is not recognized by IM, therefore will raise a warning and give the same result as synthesis
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
		'purpose'    : 'Test the convex inverse method (SIMOP)',
		'input_files': ['simop.imi', 'simop.pi0'],
		'options'    : '-merge yes -no-random -comparison inclusion',
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
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion-IM.imiprop'],
		'options'    : '',
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
		'purpose'    : 'Test variants of IM: here IMconvex',
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion-IMconvex.imiprop'],
		'options'    : '',
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
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion-IMK.imiprop'],
		'options'    : '',
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
		'input_files': ['testIM-IMK-IMunion.imi', 'testIM-IMK-IMunion-IMunion.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testIM-IMK-IMunion.res' , 'content' : """
		4 >= p & p > 1
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
		'input_files': ['exVariantes.imi', 'exVariantes-IM.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
	BEGIN CONSTRAINT
 5*p1 > p2
& p2 >= 3
& p2 >= 4*p1
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
		'purpose'    : 'Test variants of IM (bis): here IMconvex',
		'input_files': ['exVariantes.imi', 'exVariantes-IMconvex.imiprop'],
		'options'    : '',
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
		'input_files': ['exVariantes.imi', 'exVariantes-IMK.imiprop'],
		'options'    : '-no-random',
		'expectations' : [
			{'file': 'exVariantes.res' , 'content' : """
 p2 > 2
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
		'purpose'    : 'Test variants of IM (bis): here IMunion',
		'input_files': ['exVariantes.imi', 'exVariantes-IMunion.imiprop'],
		'options'    : '-no-random',
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
		'purpose'    : 'Test IM on an incomplete example',
		'input_files': ['exIMnoncomplete.imi', 'exIMnoncomplete-IMconvex.imiprop'],
		'options'    : '-no-random',
		'expectations' : [
			{'file': 'exIMnoncomplete.res' , 'content' : """
BEGIN CONSTRAINT
p1 >= 0
& p2 >= 0
& p3 > p1
END CONSTRAINT		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test IMcomplete on an incomplete example',
		'input_files': ['exIMnoncomplete.imi', 'exIMnoncomplete-IM.imiprop'],
		'options'    : '-no-random',
		'expectations' : [
			{'file': 'exIMnoncomplete.res' , 'content' : """
BEGIN CONSTRAINT
     p1 >= 0
    & p2 > p1
    & p3 >= 0
    OR
      p3 > p1
    & p2 >= 0
    & p1 >= p2
END CONSTRAINT		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 2
		# Test since               : 2019/05/30
		# Test for IMITATOR version: 2.11
		'purpose'    : 'Test PRP on a very simple example',
		'input_files': ['testEFcounterex.imi', 'testEFcounterex-prp.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testEFcounterex.res' , 'content' : """
BEGIN CONSTRAINT
 p >= 0
& 1 > p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} #end result file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test PRP (old version) on a simple example (good reference valuation)',
		#'input_files': ['testPRP.imi', 'testPRP.pigood'],
		#'options'    : '-PRP  -states-description',
		#'expectations' : [
			#{'file': 'testPRP.res' , 'content' : """
#BEGIN CONSTRAINT
	#4 > p2
    #& 3 > p1
    #& p1 >= 0
    #& p2 >= 0
#END CONSTRAINT
		  #"""
			#} # end result file
			#,
			#{'file': 'testPRP-statespace.states' , 'content' : """
  #DESCRIPTION OF THE STATES

  #/************************************************************/
  #INITIAL
  #STATE 0:
  #pta: l1 ==>
#& p1 >= 0
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 1:
  #pta: l2 ==>
#& p1 >= 0
#& p2 >= 0
#& x >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 2:
  #pta: l3 ==>
#& p1 >= x
#& p2 >= 0
#& x >= 0
#& 1 >= p2
#& x = y

  #Projection onto the parameters:
   #1 >= p2
#& p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 3:
  #pta: l4 ==>
#& p1 >= p2
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p1 >= p2
#& p2 >= 0

  #/************************************************************/
  #DESCRIPTION OF THE TRANSITIONS
  #s_0 -> s_1
  #s_1 -> s_2
  #s_1 -> s_3
  #s_2 -> s_2
#"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (good reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP-good.imiprop'],
		'options'    : '-states-description',
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
   1 >= p2
& p2 >= 0
& p1 >= 0

  /************************************************************/
  STATE 3:
  pta: l4 ==>
& p1 >= p2
& p2 >= 0
& y >= 0
& x = y

  Projection onto the parameters:
   p1 >= p2
& p2 >= 0

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
  s_1 -> s_2
  s_1 -> s_3
  s_2 -> s_2
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test PRP (old version) on a simple example (bad reference valuation)',
		#'input_files': ['testPRP.imi', 'testPRP.pibad'],
		#'options'    : '-PRP  -states-description',
		#'expectations' : [
			#{'file': 'testPRP.res' , 'content' : """
		 #& p1 >= 0
#OR
  #p2 >= 0
#& p1 >= 3
		  #"""
			#} # end result file
			#,
			#{'file': 'testPRP-statespace.states' , 'content' : """
  #DESCRIPTION OF THE STATES

  #/************************************************************/
  #INITIAL
  #STATE 0:
  #pta: l1 ==>
#& p1 >= 0
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 1:
  #pta: l2 ==>
#& p1 >= 0
#& p2 >= 0
#& x >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 2:
  #pta: locBad1 ==>
#& p1 >= 0
#& p2 >= 4
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 4
#& p1 >= 0

  #/************************************************************/
  #STATE 3:
  #pta: l4 ==>
#& p1 >= p2
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p1 >= p2
#& p2 >= 0

  #/************************************************************/
  #STATE 4:
  #pta: locBad2 ==>
#& p1 >= 3
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 3

  #/************************************************************/
  #DESCRIPTION OF THE TRANSITIONS
  #s_0 -> s_1
  #s_0 -> s_2
  #s_1 -> s_3
  #s_1 -> s_4
		  #"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (bad reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP-bad.imiprop'],
		'options'    : '-states-description',
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
   p1 >= p2
& p2 >= 0

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

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
  s_0 -> s_2
  s_1 -> s_3
  s_1 -> s_4
		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test PRP (old version) on a simple example (looping reference valuation)',
		#'input_files': ['testPRP.imi', 'testPRP.piloop'],
		#'options'    : '-PRP  -depth-limit 10 -states-description',
		#'expectations' : [
			#{'file': 'testPRP.res' , 'content' : """
 #p2 >= 4
    #& p1 >= 0
    #OR
      #p2 >= 0
    #& p1 >= 3
		  #"""
			#} # end result file
			#,
			#{'file': 'testPRP-statespace.states' , 'content' : """
  #DESCRIPTION OF THE STATES

  #/************************************************************/
  #INITIAL
  #STATE 0:
  #pta: l1 ==>
#& p1 >= 0
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 1:
  #pta: l2 ==>
#& p1 >= 0
#& p2 >= 0
#& x >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 0

  #/************************************************************/
  #STATE 2:
  #pta: locBad1 ==>
#& p1 >= 0
#& p2 >= 4
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 4
#& p1 >= 0

  #/************************************************************/
  #STATE 3:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 4:
  #pta: l4 ==>
#& p1 >= p2
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p1 >= p2
#& p2 >= 0

  #/************************************************************/
  #STATE 5:
  #pta: locBad2 ==>
#& p1 >= 3
#& p2 >= 0
#& y >= 0
#& x = y

  #Projection onto the parameters:
   #p2 >= 0
#& p1 >= 3

  #/************************************************************/
  #STATE 6:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 1 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 7:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 2 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 8:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 3 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 9:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 4 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 10:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 5 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 11:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 6 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 12:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 7 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #STATE 13:
  #pta: infiniteLoop ==>
#& 1 >= x
#& p1 >= 0
#& p2 >= 5
#& x >= 0
#& x + 8 = y

  #Projection onto the parameters:
   #p2 >= 5
#& p1 >= 0

  #/************************************************************/
  #DESCRIPTION OF THE TRANSITIONS
  #s_0 -> s_1
  #s_0 -> s_2
  #s_0 -> s_3
  #s_1 -> s_4
  #s_1 -> s_5
  #s_3 -> s_6
  #s_6 -> s_7
  #s_7 -> s_8
  #s_8 -> s_9
  #s_9 -> s_10
  #s_10 -> s_11
  #s_11 -> s_12
  #s_12 -> s_13


		  #"""
			#} # end result file
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRP on a simple example (looping reference valuation)',
		'input_files': ['testPRP.imi', 'testPRP-loop.imiprop'],
		'options'    : '-depth-limit 10 -states-description',
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
   p1 >= p2
& p2 >= 0

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

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1
  s_0 -> s_2
  s_0 -> s_3
  s_1 -> s_4
  s_1 -> s_5
  s_3 -> s_6
  s_6 -> s_7
  s_7 -> s_8
  s_8 -> s_9
  s_9 -> s_10
  s_10 -> s_11
  s_11 -> s_12
  s_12 -> s_13


		  """
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode cover (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid4x4-cover.imiprop'],
		'options'    : '',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  p1 = 1
& p2 = 1

 K1:
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
(************************************************************)
 Tile #2

 Pi2:
  p1 = 2
& p2 = 1

 K2:
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
(************************************************************)
 Tile #3

 Pi3:
  p1 = 3
& p2 = 1

 K3:
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
(************************************************************)
 Tile #4

 Pi4:
  p1 = 4
& p2 = 1

 K4:
 p2 >= 1
& p1 >= 4
& 2 > p2
& 5 > p1

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
(************************************************************)
 Tile #5

 Pi5:
  p1 = 1
& p2 = 2

 K5:
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
(************************************************************)
 Tile #6

 Pi6:
  p1 = 2
& p2 = 2

 K6:
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
(************************************************************)
 Tile #7

 Pi7:
  p1 = 3
& p2 = 2

 K7:
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
(************************************************************)
 Tile #8

 Pi8:
  p1 = 4
& p2 = 2

 K8:
 p2 >= 2
& p1 >= 4
& 3 > p2
& 5 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #9

 Pi9:
  p1 = 1
& p2 = 3

 K9:
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
(************************************************************)
 Tile #10

 Pi10:
  p1 = 2
& p2 = 3

 K10:
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
(************************************************************)
 Tile #11

 Pi11:
  p1 = 3
& p2 = 3

 K11:
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
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #12

 Pi12:
  p1 = 4
& p2 = 3

 K12:
 p2 >= 3
& p1 >= 4
& 4 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #13

 Pi13:
  p1 = 1
& p2 = 4

 K13:
 p2 >= 4
& p1 >= 1
& 5 > p2
& 2 > p1

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
(************************************************************)
 Tile #14

 Pi14:
  p1 = 2
& p2 = 4

 K14:
 p2 >= 4
& p1 >= 2
& 5 > p2
& 3 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #15

 Pi15:
  p1 = 3
& p2 = 4

 K15:
 p2 >= 4
& p1 >= 3
& 5 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
 Tile #16

 Pi16:
  p1 = 4
& p2 = 4

 K16:
 p2 >= 4
& p1 >= 4
& 5 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 11
Local number of transitions   : 10
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 16
Number of tiles computed      : 16
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 8.0
Average number of transitions : 7.0

"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2020/09/04
		# Test for IMITATOR version: 3
		'purpose'    : 'Test BC in mode cover (grid) with step=2',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid4x4-cover-step2.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'testBC-grid-plain.res' , 'content' : """
------------------------------------------------------------
Number of integers in v0                : 4
Number of tiles computed                : 4
Coverage                                : integer-complete
Termination                             : regular termination
Number of unsuccessful points           : 0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,


	{
		'purpose'    : 'Test BC in mode cover (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-cover.imiprop'],
		'options'    : '',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid2-plain.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  p1 = 0
& p2 = 0

 K1:
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
(************************************************************)
 Tile #2

 Pi2:
  p1 = 2
& p2 = 0

 K2:
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
(************************************************************)
 Tile #3

 Pi3:
  p1 = 4
& p2 = 0

 K3:
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
(************************************************************)
 Tile #4

 Pi4:
  p1 = 0
& p2 = 2

 K4:
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
(************************************************************)
 Tile #5

 Pi5:
  p1 = 2
& p2 = 2

 K5:
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
(************************************************************)
 Tile #6

 Pi6:
  p1 = 4
& p2 = 2

 K6:
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
(************************************************************)
 Tile #7

 Pi7:
  p1 = 0
& p2 = 4

 K7:
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
(************************************************************)
 Tile #8

 Pi8:
  p1 = 2
& p2 = 4

 K8:
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
(************************************************************)
 Tile #9

 Pi9:
  p1 = 4
& p2 = 4

 K9:
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
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 27
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
		'purpose'    : 'Test BC in mode cover + depth-limit (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid4x4-cover.imiprop'],
		'options'    : '-depth-limit 5 ',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
 #Tile #1

 #Pi1:
  #p1 = 1
#& p2 = 1

 #K1:
 #p2 >= 1
#& p1 >= 1
#& 2 > p2
#& 2 > p1

#------------------------------------------------------------
#Constraint soundness          : possible over-approximation
#Termination                   : depth limit (1 successor unexplored)
#Constraint nature             : good
#------------------------------------------------------------
#Local number of states        : 5
#Local number of transitions   : 4
#"""
			#} # end BC file
			#,
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
 #Tile #2

 #Pi2:
  #p1 = 2
#& p2 = 1

 #K2:
 #p2 >= 1
#& p1 >= 2
#& 3 > p1

#------------------------------------------------------------
#Constraint soundness          : possible over-approximation
#Termination                   : depth limit (1 successor unexplored)
#Constraint nature             : good
#------------------------------------------------------------
#Local number of states        : 5
#Local number of transitions   : 4
#"""
			#} # end BC file
			#,
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
 #Tile #3

 #Pi3:
  #p1 = 3
#& p2 = 1

 #K3:
 #p2 >= 0
#& p1 >= 3
#& 4 > p1

#------------------------------------------------------------
#Constraint soundness          : possible over-approximation
#Termination                   : depth limit (1 successor unexplored)
#Constraint nature             : good
#------------------------------------------------------------
#Local number of states        : 5
#Local number of transitions   : 4
#"""
			#} # end BC file
			#,
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
 #Tile #4

 #Pi4:
  #p1 = 4
#& p2 = 1

 #K4:
 #p1 >= 4
#& p2 >= 0

#------------------------------------------------------------
#Constraint soundness          : possible over-approximation
#Termination                   : depth limit (1 successor unexplored)
#Constraint nature             : good
#------------------------------------------------------------
#Local number of states        : 5
#Local number of transitions   : 4
#"""
			#} # end BC file
			#,
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
 #Tile #5

 #Pi5:
  #p1 = 1
#& p2 = 2

 #K5:
 #p2 >= 2
#& p1 >= 1
#& 2 > p1

#------------------------------------------------------------
#Constraint soundness          : possible over-approximation
#Termination                   : depth limit (1 successor unexplored)
#Constraint nature             : good
#------------------------------------------------------------
#Local number of states        : 5
#Local number of transitions   : 4
#"""
			#} # end BC file
			#,
			# NOTE: actual result
			#{'file': 'testBC-grid-plain.res' , 'content' : """
#(************************************************************)
#GENERAL STATISTICS
#(************************************************************)
#------------------------------------------------------------
#Number of integers in v0      : 16
#Number of tiles computed      : 5
#Coverage                      : unknown
#Termination                   : regular termination
#Number of unsuccessful points : 11
#Average number of states      : 5.0
#Average number of transitions : 4.0
#"""
			#} # end BC file
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0                : 16
Number of tiles computed                : 0
Coverage                                : empty
Termination                             : regular termination
Number of unsuccessful points           : 16
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode cover with depth limit (JLR15)',
		'input_files': ['JLR-TACAS13.imi', 'JLR-TACAS13-cover.imiprop'],
		'options'    : '-depth-limit 10 ',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'JLR-TACAS13.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  a = 1
& b = 0

 K1:
 a > b
& b >= 0
& 2 > b
& 10 >= a

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 1
Local number of transitions             : 0
"""
			} # end BC file
			,
			{'file': 'JLR-TACAS13.res' , 'content' : """
(************************************************************)
 Tile #2

 Pi2:
  a = 3
& b = 2

 K2:
 a > b
& b >= 2
& 10 >= a

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 2
Local number of transitions             : 1
"""
			} # end BC file
			,
			{'file': 'JLR-TACAS13.res' , 'content' : """
    Number of integers in v0                : 121
    Number of tiles computed                : 2
    Coverage                                : unknown
    Termination                             : regular termination
    Number of unsuccessful points           : 119
    Average number of states                : 1.5
    Average number of transitions           : 0.5
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode sequential + depth limit (grid with loop)',
		'input_files': ['testBC-grid-plain-loop.imi', 'testBC-grid3x3-cover.imiprop'],
		'options'    : '-depth-limit 10 ',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  p1 = 2
& p2 = 1

 K1:
 p2 >= 1
& p1 >= 2
& 2 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 6
Local number of transitions             : 5
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #2

 Pi2:
  p1 = 3
& p2 = 1

 K2:
 p2 >= 1
& p1 >= 3
& 2 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 6
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #3

 Pi3:
  p1 = 1
& p2 = 2

 K3:
 p2 >= 2
& p1 >= 1
& 3 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 6
Local number of transitions             : 5
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #4

 Pi4:
  p1 = 2
& p2 = 2

 K4:
 p2 >= 2
& p1 >= 2
& 3 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 6
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #5

 Pi5:
  p1 = 3
& p2 = 2

 K5:
 p2 >= 2
& p1 >= 3
& 3 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 7
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #6

 Pi6:
  p1 = 1
& p2 = 3

 K6:
 p2 >= 3
& p1 >= 1
& 4 > p2
& 2 > p1

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 7
Local number of transitions             : 6
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
 Tile #7

 Pi7:
  p1 = 2
& p2 = 3

 K7:
 p2 >= 3
& p1 >= 2
& 4 > p2
& 3 > p1

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Local number of states                  : 8
Local number of transitions             : 7
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain-loop.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0                : 9
Number of tiles computed                : 7
Coverage                                : unknown
Termination                             : regular termination
Number of unsuccessful points           : 2
Average number of states                : 7.0
Average number of transitions           : 6.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode cover + graphical output (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-draw-cart -graphics-source',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
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
Constraint nature             : good
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
Constraint nature             : good
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
			{'file': 'flipflop_cart_points_2.txt' , 'content' : """8. 9.
8. 16.
17. 7.
17. 3.
14. 3.
8. 9.
# """
			} # end tile file
			,
			{'file': 'flipflop_cart_points_8.txt' , 'content' : """17. 31.
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
		'purpose'    : 'Test BC in mode cover with tiles limit (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-cover.imiprop'],
		'options'    : '-cart-tiles-limit 4 ',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'flipflop.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  dG3_u = 8
& dG4_u = 3

 K1:
 dG3_u >= 8
& dG4_u >= 3
& 17 > dG3_u + dG4_u

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
			{'file': 'flipflop.res' , 'content' : """
(************************************************************)
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

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 11
Local number of transitions   : 10
"""
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
(************************************************************)
 Tile #3

 Pi3:
  dG3_u = 17
& dG4_u = 3

 K3:
 dG3_u >= 17
& dG4_u >= 3
& 24 > dG3_u + dG4_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 15
Local number of transitions   : 14
"""
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
(************************************************************)
 Tile #4

 Pi4:
  dG3_u = 21
& dG4_u = 3

 K4:
 dG3_u + dG4_u >= 24
& dG4_u >= 3
& 7 > dG4_u
& 24 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 17
Local number of transitions   : 16
"""
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
------------------------------------------------------------
Number of integers in v0      : 644
Number of tiles computed      : 4
Coverage                      : unknown
Termination                   : tiles limit
Number of unsuccessful points : 10
Average number of states      : 12.7
Average number of transitions : 11.7
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode random (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid4x4-random.imiprop'],
		'options'    : '',
		'expectations' : [
			# WARNING: this algorithm is… random! hence no absolute guarantee to find the result (this said, a max_tries of 20 generally allows one to find all tiles with a good probability)
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
 p2 >= 1
& p1 >= 4
& 2 > p2
& 5 > p1

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
 p2 >= 2
& p1 >= 4
& 3 > p2
& 5 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 4
& 4 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 1
& 5 > p2
& 2 > p1

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
 p2 >= 4
& p1 >= 2
& 5 > p2
& 3 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 3
& 5 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 4
& 5 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 11
Local number of transitions   : 10
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 16
Number of tiles computed      : 16
Coverage                      : unknown
Termination                   : regular termination

"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode random (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-random.imiprop'],
		'options'    : '',
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
Number of tiles computed      : 9
Coverage                      : unknown
Termination                   : regular termination
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode random (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-random.imiprop'],
		'options'    : '',
		'expectations' : [
			# WARNING: this algorithm is… random! hence no absolute guarantee to find the result (this said, a max_tries of 200 generally allows one to find all tiles with a good probability)
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)… and to check them separately as the order is of course unknown
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
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 8
& dG3_u + dG4_u >= 24
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
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
			#{'file': 'flipflop_cart_points_2.txt' , 'content' : """14. 3.
#8. 9.
#8. 16.
#17. 7.
#17. 3.
#14. 3.
## """
			#} # end tile file
			#,
			#{'file': 'flipflop_cart_points_8.txt' , 'content' : """17. 31.
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
		'purpose'    : 'Test BC in mode random+seq',
		'input_files': ['flipflop.imi', 'flipflop-randomseq.imiprop'],
		'options'    : '',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)… and to check them separately as the order is of course unknown
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
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 8
& dG3_u + dG4_u >= 24
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
Number of integers in v0      : 644
Number of tiles computed      : 8
Coverage                      : integer-complete
Termination                   : regular termination
"""
			} # end BC file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode shuffle (grid)',
		'input_files': ['testBC-grid-plain.imi', 'testBC-grid4x4-shuffle.imiprop'],
		'options'    : '',
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
 p2 >= 1
& p1 >= 4
& 2 > p2
& 5 > p1

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
 p2 >= 2
& p1 >= 4
& 3 > p2
& 5 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 3
& p1 >= 4
& 4 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 1
& 5 > p2
& 2 > p1

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
 p2 >= 4
& p1 >= 2
& 5 > p2
& 3 > p1

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
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 3
& 5 > p2
& 4 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 10
Local number of transitions   : 9
"""
			} # end BC file
			,
			{'file': 'testBC-grid-plain.res' , 'content' : """
:
 p2 >= 4
& p1 >= 4
& 5 > p2
& 5 > p1

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
Local number of states        : 11
Local number of transitions   : 10
"""
			} # end BC file
			,
			# NOTE: actual result
			{'file': 'testBC-grid-plain.res' , 'content' : """
(************************************************************)
GENERAL STATISTICS
(************************************************************)
------------------------------------------------------------
Number of integers in v0      : 16
Number of tiles computed      : 16
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 0
Average number of states      : 8.0
Average number of transitions : 7.0
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode shuffle (grid2)',
		'input_files': ['testBC-grid2-plain.imi', 'testBC-grid5x5-shuffle.imiprop'],
		'options'    : '',
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
Number of tiles computed      : 9
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 27
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
		'purpose'    : 'Test BC in mode shuffle (flip-flop)',
		'input_files': ['flipflop.imi', 'flipflop-shuffle.imiprop'],
		'options'    : '',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)… and to check them separately as the order is of course unknown
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
			} # end BC file
			,
			{'file': 'flipflop.res' , 'content' : """
 dG3_u >= 8
& dG3_u + dG4_u >= 24
& 17 > dG3_u

------------------------------------------------------------
Constraint soundness          : exact
Termination                   : regular termination
Constraint nature             : good
------------------------------------------------------------
"""
			} # end BC file
			,
			# NOTE: the actual result
			{'file': 'flipflop.res' , 'content' : """
Number of integers in v0      : 644
Number of tiles computed      : 8
Coverage                      : integer-complete
Termination                   : regular termination
Number of unsuccessful points : 636
Average number of states      : 15.1
Average number of transitions : 14.1
"""
			} # end BC file			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test BC in mode cover (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-cover.imiprop'],
		'options'    : '-no-random ',
		'expectations' : [
			# NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #1

 Pi1:
  p1 = 0
& p2 = 0

 K1:
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
(************************************************************)
 Tile #2

 Pi2:
  p1 = 1
& p2 = 0

 K2:
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
(************************************************************)
 Tile #3

 Pi3:
  p1 = 2
& p2 = 0

 K3:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #4

 Pi4:
  p1 = 3
& p2 = 0

 K4:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #5

 Pi5:
  p1 = 0
& p2 = 1

 K5:
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
(************************************************************)
 Tile #6

 Pi6:
  p1 = 1
& p2 = 1

 K6:
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
(************************************************************)
 Tile #7

 Pi7:
  p1 = 2
& p2 = 1

 K7:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #8

 Pi8:
  p1 = 3
& p2 = 1

 K8:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #9

 Pi9:
  p1 = 0
& p2 = 2

 K9:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
(************************************************************)
 Tile #10

 Pi10:
  p1 = 2
& p2 = 2

 K10:
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
			} # end BC file
			,
			{'file': 'diffBCPRPC.res' , 'content' : """
------------------------------------------------------------
Number of integers in v0                : 25
Number of tiles computed                : 10
Coverage                                : integer-complete
Termination                             : regular termination
Number of unsuccessful points           : 15
Average number of states                : 7.4
Average number of transitions           : 10.8
"""
			} # end BC file
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test PRPC (on the case study BC vs. PRPC)',
		'input_files': ['diffBCPRPC.imi', 'diffBCPRPC-PRPC.imiprop'],
		'options'    : '-no-random ',
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
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	#{
		#'purpose'    : 'Test PRPC (old version) in mode cover with depth limit (JLR15)',
		#'input_files': ['JLR-TACAS13.imi', 'JLR-TACAS13.v0'],
		#'options'    : '-mode cover -PRP -depth-limit 10 ',
		#'expectations' : [
			## NOTE: no other way for now that checking separately the constraints (because the computation times may of course differ)
			#{'file': 'JLR-TACAS13.res' , 'content' : """
#(************************************************************)
 #Tile #1

 #Pi1:
  #a = 1
#& b = 0

 #K1:
 #2 > b
#& b >= 0
#& a > b
#& 10 >= a

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : good
#------------------------------------------------------------
#Local number of states                  : 1
#Local number of transitions             : 0
#"""
			#} # end BC file
			#,
			#{'file': 'JLR-TACAS13.res' , 'content' : """
#(************************************************************)
 #Tile #2

 #Pi2:
  #a = 0
#& b = 1

 #K2:
 #a >= 0
#& 9*b >= 2
#& 10 >= b
#& b >= a


#------------------------------------------------------------
#Constraint soundness                    : possible under-approximation
#Termination                             : depth limit (1 successor unexplored)
#Constraint nature                       : bad
#------------------------------------------------------------
#Local number of states                  : 18
#Local number of transitions             : 17
#"""
			#} # end BC file
			#,
			#{'file': 'JLR-TACAS13.res' , 'content' : """
#(************************************************************)
 #Tile #3

 #Pi3:
  #a = 3
#& b = 2

 #K3:
 #b >= 2
#& a >= 0
#& 10 >= a
#& 10 >= b

#------------------------------------------------------------
#Constraint soundness                    : exact
#Termination                             : regular termination
#Constraint nature                       : bad
#------------------------------------------------------------
#Local number of states                  : 2
#Local number of transitions             : 1
#"""
			#} # end BC file
			#,
			#{'file': 'JLR-TACAS13.res' , 'content' : """
#(************************************************************)
#GENERAL STATISTICS
#(************************************************************)
#------------------------------------------------------------
#Number of integers in v0                : 121
#Number of tiles computed                : 3
#Coverage                                : unknown
#Termination                             : regular termination
#Number of unsuccessful points           : 118
#Average number of states                : 7.0
#Average number of transitions           : 6.0
#"""
			#} # end BC file
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		'purpose'    : 'Testing state space + merging on a trivial example: -merge none',
		'input_files': ['exActionsNonPreserved.imi'],
		'options'    : '-mode statespace -states-description -merge none',
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
   2 >= p
& p >= 0

  /************************************************************/
  STATE 2:
  pta: L2 ==>
& p >= 2
& x >= 2

  Projection onto the parameters:
   p >= 2

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a"
  s_0 -> s_2 via "b"
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Testing state space + merging on a trivial example: -merge',
		'input_files': ['exActionsNonPreserved.imi'],
		'options'    : '-mode statespace -states-description -merge yes',
		'expectations' : [
			{'file': 'exActionsNonPreserved-statespace.states' , 'content' : """
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
  STATE 2:
  pta: L2 ==> 
& p >= 0
& x >= 2

  Projection onto the parameters:
   p >= 0

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_2 via "a"
  s_0 -> s_2 via "b"
  """ 
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	
	,
	
	#------------------------------------------------------------
	{
		'purpose'    : 'Testing state space + merging on a trivial example: testing no option = -merge none',
		'input_files': ['exActionsNonPreserved.imi'],
		'options'    : '-mode statespace -states-description',
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
   2 >= p
& p >= 0

  /************************************************************/
  STATE 2:
  pta: L2 ==>
& p >= 2
& x >= 2

  Projection onto the parameters:
   p >= 2

  /************************************************************/
  DESCRIPTION OF THE TRANSITIONS
  s_0 -> s_1 via "a"
  s_0 -> s_2 via "b"
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
		'options'    : '-imi2IMI',
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
 actions: t1_arr_event, t1_arr, t1_dis, t1_miss, t1_end;


loc t1_loc_idle: invariant True
	when True do {t1_urgent := 0;}  sync t1_arr_event goto t1_loc_act_event;

loc t1_loc_act_event: invariant 0 >= t1_urgent
	when  t1_urgent = 0 do {t1_c := 0; t1_d := 0;} sync t1_arr goto t1_loc_act;

loc t1_loc_act: invariant 8 >= t1_d stop{t1_c}
	when True do {} sync t1_dis goto t1_loc_exe;
	when  t1_d >= 8 do {} sync t1_miss goto t1_loc_miss;

loc t1_loc_exe: invariant 8 >= t1_d
& 2 >= t1_c
	when  t1_d >= 8
& 2 > t1_c do {}  sync t1_miss goto t1_loc_miss;
	when  t1_c = 2 do {} sync t1_end goto t1_loc_idle;

loc t1_loc_miss: invariant True
 end (* Task_t1 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t1_arr
(************************************************************)
 actions: t1_arr_event;


loc t1_arr_loc_arr: invariant  8 >= t1_arr_x
	when  t1_arr_x = 8 do {t1_arr_x := 0;}  sync t1_arr_event goto t1_arr_loc_arr;
 end (* Periodic_t1_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t2
(************************************************************)
 actions: t2_arr_event, t2_arr, t2_dis, t2_miss, t2_pre, t2_end;


loc t2_loc_idle: invariant True
	when True do {t2_urgent := 0;}  sync t2_arr_event goto t2_loc_act_event;

loc t2_loc_act_event: invariant  0 >= t2_urgent
	when  t2_urgent = 0 do {t2_c := 0; t2_d := 0;}  sync t2_arr goto t2_loc_act;

loc t2_loc_act: invariant  20 >= t2_d stop{t2_c}
	when True do {}  sync t2_dis goto t2_loc_exe;
	when  t2_d >= 20 do {}  sync t2_miss goto t2_loc_miss;

loc t2_loc_exe: invariant  20 >= t2_d
& 5 >= t2_c
	when  5 > t2_c do {}  sync t2_pre goto t2_loc_act;
	when  t2_d >= 20
& 5 > t2_c do {}  sync t2_miss goto t2_loc_miss;
	when  t2_c = 5 do {}  sync t2_end goto t2_loc_idle;

loc t2_loc_miss: invariant True
 end (* Task_t2 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t2_arr
(************************************************************)
 actions: t2_arr_event;


loc t2_arr_loc_arr: invariant  20 >= t2_arr_x
	when  t2_arr_x = 20 do {t2_arr_x := 0;}  sync t2_arr_event goto t2_arr_loc_arr;
 end (* Periodic_t2_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t3
(************************************************************)
 actions: t3_arr_event, t3_arr, t3_dis, t3_miss, t3_pre, t3_end;


loc t3_loc_idle: invariant True
	when True do {t3_urgent := 0;}  sync t3_arr_event goto t3_loc_act_event;

loc t3_loc_act_event: invariant  0 >= t3_urgent
	when  t3_urgent = 0 do {t3_c := 0; t3_d := 0;}  sync t3_arr goto t3_loc_act;

loc t3_loc_act: invariant  50 >= t3_d stop{t3_c}
	when True do {}  sync t3_dis goto t3_loc_exe;
	when  t3_d >= 50 do {}  sync t3_miss goto t3_loc_miss;

loc t3_loc_exe: invariant  50 >= t3_d
& 8 >= t3_c
	when  8 > t3_c do {}  sync t3_pre goto t3_loc_act;
	when  t3_d >= 50
& 8 > t3_c do {}  sync t3_miss goto t3_loc_miss;
	when  t3_c = 8 do {}  sync t3_end goto t3_loc_idle;

loc t3_loc_miss: invariant True
 end (* Task_t3 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t3_arr
(************************************************************)
 actions: t3_arr_event;


loc t3_arr_loc_arr: invariant  50 >= t3_arr_x
	when  t3_arr_x = 50 do {t3_arr_x := 0;}  sync t3_arr_event goto t3_arr_loc_arr;
 end (* Periodic_t3_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t4
(************************************************************)
 actions: t4_arr_event, t4_arr, t4_dis, t4_miss, t4_pre, t4_end;


loc t4_loc_idle: invariant True
	when True do {t4_urgent := 0;}  sync t4_arr_event goto t4_loc_act_event;

loc t4_loc_act_event: invariant  0 >= t4_urgent
	when  t4_urgent = 0 do {t4_c := 0; t4_d := 0;}  sync t4_arr goto t4_loc_act;

loc t4_loc_act: invariant  100 >= t4_d stop{t4_c}
	when True do {}  sync t4_dis goto t4_loc_exe;
	when  t4_d >= 100 do {}  sync t4_miss goto t4_loc_miss;

loc t4_loc_exe: invariant  t4_C >= t4_c
& 100 >= t4_d
	when  t4_C > t4_c do {}  sync t4_pre goto t4_loc_act;
	when  t4_C > t4_c
& t4_d >= 100 do {}  sync t4_miss goto t4_loc_miss;
	when  t4_C = t4_c do {}  sync t4_end goto t4_loc_idle;

loc t4_loc_miss: invariant True
 end (* Task_t4 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t4_arr
(************************************************************)
 actions: t4_arr_event;


loc t4_arr_loc_arr: invariant  100 >= t4_arr_x
	when  t4_arr_x = 100 do {t4_arr_x := 0;}  sync t4_arr_event goto t4_arr_loc_arr;
 end (* Periodic_t4_arr *)
(************************************************************)


(************************************************************)
 automaton Task_t5
(************************************************************)
 actions: t5_arr_event, t5_arr, t5_dis, t5_miss, t5_pre, t5_end;


loc t5_loc_idle: invariant True
	when True do {t5_urgent := 0;}  sync t5_arr_event goto t5_loc_act_event;

loc t5_loc_act_event: invariant  0 >= t5_urgent
	when  t5_urgent = 0 do {t5_c := 0; t5_d := 0;}  sync t5_arr goto t5_loc_act;

loc t5_loc_act: invariant  200 >= t5_d stop{t5_c}
	when True do {}  sync t5_dis goto t5_loc_exe;
	when  t5_d >= 200 do {}  sync t5_miss goto t5_loc_miss;

loc t5_loc_exe: invariant  t5_C >= t5_c
& 200 >= t5_d
	when  t5_C > t5_c do {}  sync t5_pre goto t5_loc_act;
	when  t5_C > t5_c
& t5_d >= 200 do {}  sync t5_miss goto t5_loc_miss;
	when  t5_C = t5_c do {}  sync t5_end goto t5_loc_idle;

loc t5_loc_miss: invariant True
 end (* Task_t5 *)
(************************************************************)


(************************************************************)
 automaton Periodic_t5_arr
(************************************************************)
 actions: t5_arr_event;


loc t5_arr_loc_arr: invariant  200 >= t5_arr_x
	when  t5_arr_x = 200 do {t5_arr_x := 0;}  sync t5_arr_event goto t5_arr_loc_arr;
 end (* Periodic_t5_arr *)
(************************************************************)


(************************************************************)
 automaton sched_CPU1
(************************************************************)
 actions: t1_arr, t2_arr, t3_arr, t4_arr, t5_arr, t1_dis, t2_dis, t3_dis, t4_dis, t5_dis, t5_end, t5_pre, t4_end, t4_pre, t3_end, t3_pre, t2_end, t2_pre, t1_end;


loc CPU1_loc_: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_At3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_At4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_At5;

loc CPU1_loc_At1: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1;

loc CPU1_loc_At2: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2;

loc CPU1_loc_At3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;

loc CPU1_loc_At4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;

loc CPU1_loc_At5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;

loc CPU1_loc_Rt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt5;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_At3Rt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_At4Rt5;
	when True do {CPU1_urgent := 0;}  sync t5_end goto CPU1_loc_Et5;

loc CPU1_loc_Et5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  (* sync nosync_1*)  goto CPU1_loc_stop;

loc CPU1_loc_At1Rt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At1Wt5;

loc CPU1_loc_At1Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt5;

loc CPU1_loc_At2Rt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At2Wt5;

loc CPU1_loc_At2Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt5;

loc CPU1_loc_At3Rt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At3Wt5;

loc CPU1_loc_At3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;

loc CPU1_loc_At4Rt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_pre goto CPU1_loc_At4Wt5;

loc CPU1_loc_At4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;

loc CPU1_loc_Rt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt4;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt4;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_At3Rt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_end goto CPU1_loc_Et4;

loc CPU1_loc_Et4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  (* sync nosync_2*)  goto CPU1_loc_stop;

loc CPU1_loc_At1Rt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At1Wt4;

loc CPU1_loc_At1Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt4;

loc CPU1_loc_At2Rt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At2Wt4;

loc CPU1_loc_At2Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4;

loc CPU1_loc_At3Rt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At3Wt4;

loc CPU1_loc_At3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;

loc CPU1_loc_Rt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_At3Rt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_end goto CPU1_loc_Et4Wt5;

loc CPU1_loc_Et4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;

loc CPU1_loc_At1Rt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At1Wt4Wt5;

loc CPU1_loc_At1Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt4Wt5;

loc CPU1_loc_At2Rt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At2Wt4Wt5;

loc CPU1_loc_At2Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4Wt5;

loc CPU1_loc_At3Rt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_pre goto CPU1_loc_At3Wt4Wt5;

loc CPU1_loc_At3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;

loc CPU1_loc_Rt3: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt3;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_end goto CPU1_loc_Et3;

loc CPU1_loc_Et3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  (* sync nosync_3*)  goto CPU1_loc_stop;

loc CPU1_loc_At1Rt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3;

loc CPU1_loc_At1Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3;

loc CPU1_loc_At2Rt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3;

loc CPU1_loc_At2Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3;

loc CPU1_loc_Rt3Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_end goto CPU1_loc_Et3Wt5;

loc CPU1_loc_Et3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;

loc CPU1_loc_At1Rt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt5;

loc CPU1_loc_At1Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt5;

loc CPU1_loc_At2Rt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt5;

loc CPU1_loc_At2Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt5;

loc CPU1_loc_Rt3Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_end goto CPU1_loc_Et3Wt4;

loc CPU1_loc_Et3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;

loc CPU1_loc_At1Rt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt4;

loc CPU1_loc_At1Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt4;

loc CPU1_loc_At2Rt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt4;

loc CPU1_loc_At2Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4;

loc CPU1_loc_Rt3Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_At2Rt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_end goto CPU1_loc_Et3Wt4Wt5;

loc CPU1_loc_Et3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;

loc CPU1_loc_At1Rt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At1Wt3Wt4Wt5;

loc CPU1_loc_At1Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt3Wt4Wt5;

loc CPU1_loc_At2Rt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_pre goto CPU1_loc_At2Wt3Wt4Wt5;

loc CPU1_loc_At2Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4Wt5;

loc CPU1_loc_Rt2: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt2Wt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt2Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt2Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2;

loc CPU1_loc_Et2: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  (* sync nosync_4*)  goto CPU1_loc_stop;

loc CPU1_loc_At1Rt2: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2;

loc CPU1_loc_At1Wt2: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2;

loc CPU1_loc_Rt2Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt5;

loc CPU1_loc_Et2Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;

loc CPU1_loc_At1Rt2Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt5;

loc CPU1_loc_At1Wt2Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt5;

loc CPU1_loc_Rt2Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt4;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt4;

loc CPU1_loc_Et2Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;

loc CPU1_loc_At1Rt2Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt4;

loc CPU1_loc_At1Wt2Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt4;

loc CPU1_loc_Rt2Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt4Wt5;

loc CPU1_loc_Et2Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;

loc CPU1_loc_At1Rt2Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt4Wt5;

loc CPU1_loc_At1Wt2Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt4Wt5;

loc CPU1_loc_Rt2Wt3: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt3;

loc CPU1_loc_Et2Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;

loc CPU1_loc_At1Rt2Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3;

loc CPU1_loc_At1Wt2Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3;

loc CPU1_loc_Rt2Wt3Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt3Wt5;

loc CPU1_loc_Et2Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;

loc CPU1_loc_At1Rt2Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt5;

loc CPU1_loc_At1Wt2Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt5;

loc CPU1_loc_Rt2Wt3Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt3Wt4;

loc CPU1_loc_Et2Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;

loc CPU1_loc_At1Rt2Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt4;

loc CPU1_loc_At1Wt2Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt4;

loc CPU1_loc_Rt2Wt3Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_arr goto CPU1_loc_At1Rt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t2_end goto CPU1_loc_Et2Wt3Wt4Wt5;

loc CPU1_loc_Et2Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;

loc CPU1_loc_At1Rt2Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_pre goto CPU1_loc_At1Wt2Wt3Wt4Wt5;

loc CPU1_loc_At1Wt2Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t1_dis goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;

loc CPU1_loc_Rt1: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1;

loc CPU1_loc_Et1: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  (* sync nosync_5*)  goto CPU1_loc_stop;

loc CPU1_loc_Rt1Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt5;

loc CPU1_loc_Et1Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t5_dis goto CPU1_loc_Rt5;

loc CPU1_loc_Rt1Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt4;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt4;

loc CPU1_loc_Et1Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4;

loc CPU1_loc_Rt1Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt4Wt5;

loc CPU1_loc_Et1Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t4_dis goto CPU1_loc_Rt4Wt5;

loc CPU1_loc_Rt1Wt3: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt3;

loc CPU1_loc_Et1Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3;

loc CPU1_loc_Rt1Wt3Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt3Wt5;

loc CPU1_loc_Et1Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt5;

loc CPU1_loc_Rt1Wt3Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt3Wt4;

loc CPU1_loc_Et1Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4;

loc CPU1_loc_Rt1Wt3Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t2_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt3Wt4Wt5;

loc CPU1_loc_Et1Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t3_dis goto CPU1_loc_Rt3Wt4Wt5;

loc CPU1_loc_Rt1Wt2: invariant True
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2;

loc CPU1_loc_Et1Wt2: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2;

loc CPU1_loc_Rt1Wt2Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt5;

loc CPU1_loc_Et1Wt2Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt5;

loc CPU1_loc_Rt1Wt2Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt4;

loc CPU1_loc_Et1Wt2Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4;

loc CPU1_loc_Rt1Wt2Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t3_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt4Wt5;

loc CPU1_loc_Et1Wt2Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt4Wt5;

loc CPU1_loc_Rt1Wt2Wt3: invariant True
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt3Wt4;
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt3Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt3;

loc CPU1_loc_Et1Wt2Wt3: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3;

loc CPU1_loc_Rt1Wt2Wt3Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t4_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt5;

loc CPU1_loc_Et1Wt2Wt3Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt5;

loc CPU1_loc_Rt1Wt2Wt3Wt4: invariant True
	when True do {CPU1_urgent := 0;}  sync t5_arr goto CPU1_loc_Rt1Wt2Wt3Wt4Wt5;
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt4;

loc CPU1_loc_Et1Wt2Wt3Wt4: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4;

loc CPU1_loc_Rt1Wt2Wt3Wt4Wt5: invariant True
	when True do {CPU1_urgent := 0;}  sync t1_end goto CPU1_loc_Et1Wt2Wt3Wt4Wt5;

loc CPU1_loc_Et1Wt2Wt3Wt4Wt5: invariant  0 >= CPU1_urgent
	when  CPU1_urgent = 0 do {}  sync t2_dis goto CPU1_loc_Rt2Wt3Wt4Wt5;

loc CPU1_loc_stop: invariant True
 end (* sched_CPU1 *)
(************************************************************)


(************************************************************)
 automaton OBS_dline
(************************************************************)
 actions: t1_miss, t2_miss, t3_miss, t4_miss, t5_miss;


loc dline_loc_nomiss: invariant True
	when True do {t1_d := 0;}  sync t1_miss goto dline_loc_miss;
	when True do {t1_d := 0;}  sync t2_miss goto dline_loc_miss;
	when True do {t1_d := 0;}  sync t3_miss goto dline_loc_miss;
	when True do {t1_d := 0;}  sync t4_miss goto dline_loc_miss;
	when True do {t1_d := 0;}  sync t5_miss goto dline_loc_miss;

loc dline_loc_miss: invariant  t1_d = 0 stop{t1_d}
 end (* OBS_dline *)
(************************************************************)

(************************************************************)
(* Initial state *)
(************************************************************)

init := {

	discrete = 
		(*------------------------------------------------------------*)
		(* Initial location *)
		(*------------------------------------------------------------*)
		loc[Task_t1] := t1_loc_idle, 
		loc[Periodic_t1_arr] := t1_arr_loc_arr, 
		loc[Task_t2] := t2_loc_idle, 
		loc[Periodic_t2_arr] := t2_arr_loc_arr, 
		loc[Task_t3] := t3_loc_idle, 
		loc[Periodic_t3_arr] := t3_arr_loc_arr, 
		loc[Task_t4] := t4_loc_idle, 
		loc[Periodic_t4_arr] := t4_arr_loc_arr, 
		loc[Task_t5] := t5_loc_idle, 
		loc[Periodic_t5_arr] := t5_arr_loc_arr, 
		loc[sched_CPU1] := CPU1_loc_, 
		loc[OBS_dline] := dline_loc_nomiss,
		(*------------------------------------------------------------*)
		(* Initial discrete variables assignments *)
		(*------------------------------------------------------------*)
	;

	(*------------------------------------------------------------*)
	(* Initial continuous constraint *)
	(*------------------------------------------------------------*)
	continuous = 
		& t5_C >= 10
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
}


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
		'purpose'    : 'Test the graphical state space generation (no details)',
		'input_files': ['CUBPTA1.imi'],
		'options'    : '-mode statespace -draw-statespace undetailed -graphics-source',
		'expectations' : [
			{'file': 'CUBPTA1-statespace.dot' , 'content' : """
  s_0 -> s_1 [label="a"];
  s_1 -> s_2 [label="a"];
  s_2 -> s_3 [label="a"];
  s_3 -> s_2 [label="a"];

/* Initial state */
  s_init [shape=none, label="init"];
  s_init -> s_0;

/* Colors */

  s_0 [color=blue, style=filled];
  s_1 [color=cyan, style=filled];
  s_2 [color=blue, style=filled];
  s_3 [color=cyan, style=filled];
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the graphical state space generation (normal)',
		'input_files': ['CUBPTA1.imi'],
		'options'    : '-mode statespace -draw-statespace normal -graphics-source',
		'expectations' : [
			{'file': 'CUBPTA1-statespace.dot' , 'content' : """
  s_0 -> s_1 [label="a"];
  s_1 -> s_2 [label="a"];
  s_2 -> s_3 [label="a"];
  s_3 -> s_2 [label="a"];

/* Initial state */
  s_init [shape=none, label="init"];
  s_init -> s_0;

/* Colors */

  s_0[fillcolor=blue, style=filled, shape=Mrecord, label="s_0|{pta : l1}"];
  s_1[fillcolor=cyan, style=filled, shape=Mrecord, label="s_1|{pta : l2}"];
  s_2[fillcolor=blue, style=filled, shape=Mrecord, label="s_2|{pta : l1}"];
  s_3[fillcolor=cyan, style=filled, shape=Mrecord, label="s_3|{pta : l2}"];
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	{
		'purpose'    : 'Test the graphical state space generation (verbose)',
		'input_files': ['CUBPTA1.imi'],
		'options'    : '-mode statespace -draw-statespace full -graphics-source',
		'expectations' : [
			{'file': 'CUBPTA1-statespace.dot' , 'content' : """
  s_0 -> s_1 [label="a"];
  s_1 -> s_2 [label="a"];
  s_2 -> s_3 [label="a"];
  s_3 -> s_2 [label="a"];

/* Initial state */
  s_init [shape=none, label="init"];
  s_init -> s_0;

/* Colors */

  s_0[fillcolor=blue, style=filled, shape=Mrecord, label="s_0|{pta : l1}|{ p1 \>= x \\n \& p2 \>= 0 \\n \& x \>= 0 \\n \& x = y| p2 \>= 0 \\n \& p1 \>= 0}"];
  s_1[fillcolor=cyan, style=filled, shape=Mrecord, label="s_1|{pta : l2}|{ p1 + x \>= y \\n \& p2 \>= y \\n \& x \>= 0 \\n \& y \>= x| p2 \>= 0 \\n \& p1 \>= 0}"];
  s_2[fillcolor=blue, style=filled, shape=Mrecord, label="s_2|{pta : l1}|{ p1 \>= x \\n \& p2 \>= 1 \\n \& x \>= 0 \\n \& x = y| p2 \>= 1 \\n \& p1 \>= 0}"];
  s_3[fillcolor=cyan, style=filled, shape=Mrecord, label="s_3|{pta : l2}|{ p1 + x \>= y \\n \& p2 \>= y \\n \& p2 \>= 1 \\n \& x \>= 0 \\n \& y \>= x| p1 \>= 0 \\n \& p2 \>= 1}"];
		"""
			} # end result file
			# NOTE (ÉA, 2018/06/05): I had to manually replace '\n' with '\\n' to make this test pass
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	#{
		## Test version: 2.11
		## Test since  : 2019/03/07
		## Test until  : 2020/04/10
		#'purpose'    : 'Test the graphical state space generation (verbose with projection)',
		#'input_files': ['CUBPTA1project.imi'],
		#'options'    : '-mode statespace -draw-statespace full -graphics-source',
		#'expectations' : [
			#{'file': 'CUBPTA1project-statespace.dot' , 'content' : """
  #s_0 -> s_1 [label="a"];
  #s_1 -> s_2 [label="a"];
  #s_2 -> s_3 [label="a"];
  #s_3 -> s_2 [label="a"];

#/* Initial state */
  #s_init [shape=none, label="init"];
  #s_init -> s_0;

#/* Colors */

  #s_0[fillcolor=blue, style=filled, shape=Mrecord, label="s_0|{pta : l1}|{ p1 \>= x \\n \& p2 \>= 0 \\n \& x \>= 0 \\n \& x = y| p2 \>= 0 \\n \& p1 \>= 0| p2 \>= 0}"];
  #s_1[fillcolor=yellow, style=filled, shape=Mrecord, label="s_1|{pta : l2}|{ p1 + x \>= y \\n \& p2 \>= y \\n \& x \>= 0 \\n \& y \>= x| p2 \>= 0 \\n \& p1 \>= 0| p2 \>= 0}"];
  #s_2[fillcolor=blue, style=filled, shape=Mrecord, label="s_2|{pta : l1}|{ p1 \>= x \\n \& p2 \>= 1 \\n \& x \>= 0 \\n \& x = y| p2 \>= 1 \\n \& p1 \>= 0| p2 \>= 1}"];
  #s_3[fillcolor=yellow, style=filled, shape=Mrecord, label="s_3|{pta : l2}|{ p1 + x \>= y \\n \& p2 \>= y \\n \& p2 \>= 1 \\n \& x \>= 0 \\n \& y \>= x| p1 \>= 0 \\n \& p2 \>= 1| p2 \>= 1}"];
		#"""
			#} # end result file
			## NOTE (ÉA, 2019/03/07): I had to manually replace '\n' with '\\n' to make this test pass
			#,
		#] # end expectations
	#} # end test case
	##------------------------------------------------------------

	#,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test translation to HyTech',
		'input_files': ['flipflop.imi'],
		'options'    : '-imi2HyTech -no-var-autoremove', #TODO: re-do without '-no-var-autoremove'
		'expectations' : [
			{'file': 'flipflop.hy' , 'content' : """
 -- Created to be compatible with 'hytech-v1.04f-Linux_static'
 --************************************************************
var
	s, ckG1, ckG2, ckG3, ckG4
		: clock;

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
	when  ckG4 >= 3 do {} sync qDown goto G410;

loc G410: while  ckG4 >= 0 wait{}
	when True do {ckG4' = 0}  sync qG3Down goto G400;

loc G400: while  dG4_u >= ckG4 wait{}
	when True do {}  sync qG3Up goto G410;
	when  ckG4 >= 3 do {} sync qUp goto G401;
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

	------------------------------------------------------------
	-- Initial constraint
	------------------------------------------------------------
	 &  dG4_u >= 3
& ckG1 >= 0
& ckG2 >= 0
& ckG3 >= 0
& ckG4 >= 0
& dG3_u >= 8
& s = 0
;


--************************************************************
--* The end
--************************************************************

"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,
	
	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/05
		# Last modified            : 2021/07/05
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test translation to HyTech even when a (useless, non-existing) property file is passed',
		'input_files': ['flipflop.imi', 'nonexistingfile.imiprop'],
		'options'    : '-imi2HyTech',
		'expectations' : [
			{'file': 'flipflop.hy' , 'content' : """
var
	s, ckG1, ckG2, ckG3, ckG4
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test translation to TikZ with uncontrollable actions',
		'tags'       : 'syntax,translation,controllable',
		'input_files': ['parametric_timed_games/include_controllable_actions.imi'],
		'options'    : '-imi2TikZ',
		'expectations' : [
			# WARNING: one has to manually replace \ with \\
			{'file': 'include_controllable_actions.tex' , 'content' : """
		\\path (l1) edge[] node{\\begin{tabular}{@{} c @{\ } c@{} }
		& $ \\styleclock{x} = 1$\\\\
		 & $\\styleact{a}$\\\\
		 & $x:=1/2 * \\styleparam{p}$\\\\%
		\\end{tabular}} (l1);

		\\path (l1) edge[] node{\\begin{tabular}{@{} c @{\ } c@{} }
		& $ \\styleclock{x} = 1$\\\\
		 & $\\styleact{b}$\\\\
		 & $x:=1$\\\\%
		\\end{tabular}} (l1);

		\\path (l1) edge[uncontrollable] node{\\begin{tabular}{@{} c @{\ } c@{} }
		& $ \\styleclock{x} = 1$\\\\
		 & $\\styleact{c}$\\\\
		 & $x:=2$\\\\%
		\\end{tabular}} (l1);
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/09
		# Last modified            : 2021/07/09
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test call for export to dot', # NOTE: no check is made concerning the validity of this export!
		'input_files': ['flipflop.imi'],
		'options'    : '-imi2DOT',
		'expectations' : [
			{'file': 'flipflop.dot' , 'content' : """
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test author              : Étienne André
		# Test since               : 2023/07/11
		# Last modified            : 2023/07/11
		# Test for IMITATOR version: 3.4
		'purpose'    : 'Test export to dot with controllable actions',
		'tags'       : 'syntax,translation',
		'input_files': ['parametric_timed_games/include_controllable_actions.imi'],
		'options'    : '-imi2PDF -graphics-source',
		'expectations' : [
			{'file': 'include_controllable_actions-pta.dot' , 'content' : """
/**************************************************/
/* automaton pta1 */
/**************************************************/
 init0[shape=none, label="pta1"];
 init0 -> s_0_0;

s_0_0[fillcolor="#b3e2cd", style=filled, fontsize=16, label="l1|{True}"];
	s_0_0 -> s_0_0 [penwidth=3, color=blue, label=" x = 1\\na\\n  x := 1/2 * p;"];
	s_0_0 -> s_0_0 [penwidth=3, color=cyan, label=" x = 1\\nb\\n  x := 1;"];
	s_0_0 -> s_0_0 [style=dashed, label=" x = 1\\nc\\n  x := 2;"];
/**************************************************/


/**************************************************/
/* automaton pta2 */
/**************************************************/
 init1[shape=none, label="pta2"];
 init1 -> s_1_0;

s_1_0[fillcolor="#b3e2cd", style=filled, fontsize=16, label="l1|{True}"];
	s_1_0 -> s_1_0 [penwidth=3, color=blue, label=" x = 1\\na\\n  x := 1/2 * p;"];
	s_1_0 -> s_1_0 [penwidth=3, color=cyan, label=" x = 1\\nb\\n  x := 1;"];
	s_1_0 -> s_1_0 [style=dotted, color=gray40, style=dashed, label="  x := 0;"];
		"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/09
		# Last modified            : 2021/07/09
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test call for export to JPG', # NOTE: no check is made concerning the validity of this export!
		'input_files': ['flipflop.imi'],
		'options'    : '-imi2JPG -graphics-source',
		'expectations' : [
			{'file': 'flipflop-pta.dot' , 'content' : """
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/09
		# Last modified            : 2021/07/09
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test call for export to PDF', # NOTE: no check is made concerning the validity of this export!
		'input_files': ['flipflop.imi'],
		'options'    : '-imi2PDF -graphics-source',
		'expectations' : [
			{'file': 'flipflop-pta.dot' , 'content' : """
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2021/07/09
		# Last modified            : 2021/07/09
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test call for export to PNG', # NOTE: no check is made concerning the validity of this export!
		'input_files': ['flipflop.imi'],
		'options'    : '-imi2PNG -graphics-source',
		'expectations' : [
			{'file': 'flipflop-pta.dot' , 'content' : """
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'Test translation to Uppaal with synchronization model',
		'input_files': ['testSynchroUppaal.imi'],
		'options'    : '-imi2Uppaal',
		'expectations' : [
			{'file': 'testSynchroUppaal-uppaal.xml' , 'content' : """
/* Clocks declarations */
clock x;

/* Discrete variable declarations needed to encode IMITATOR's strong broadcast into Uppaal */
int nb__a = 3;

/* User defined function declarations (WARNING: some user defined functions may not be well translated) */



/* Action declarations */
broadcast chan a; /* This action is used in 3 automata: IMITATOR uses strong broadcast semantics, while Uppaal uses broadcast semantics; the correctness is ensured thanks to variable 'nb__a' */
chan b;
broadcast chan c;

	/*------------------------------------------------------------*/
	/* Initial constraint (not interpreted by Uppaal)             */
	/*------------------------------------------------------------*/
	 /*  x == 0 */
</declaration>

<template><name x="0" y="0">pta1</name><declaration>// No local declaration for automaton 'pta1'
</declaration>

<location id="id_pta0_loc0" x="0" y="0">
	<name x="0" y="-40">l1</name>
	<label kind="invariant" x="0" y="40">nb__a == 3</label></location>

<location id="id_pta0_loc1" x="200" y="0">
	<name x="200" y="-40">l2</name>
	<label kind="invariant" x="200" y="40">nb__a == 3</label></location>
 <init ref="id_pta0_loc0"/>

	<transition>
		<source ref="id_pta0_loc0"/>
		<target ref="id_pta0_loc0"/>
		<label kind="synchronisation" x="0" y="80">a!</label>
		<label kind="guard" x="0" y="40"> x == 3</label>
		<label kind="assignment" x="0" y="-40">x = 0, nb__a = 1</label>
	</transition>
	<transition>
		<source ref="id_pta0_loc0"/>
		<target ref="id_pta0_loc1"/>
		<label kind="synchronisation" x="100" y="80">b!</label>
		<label kind="guard" x="100" y="40"> x == 4</label>

	</transition>
 </template>


<template><name x="1" y="1">pta2</name><declaration>// No local declaration for automaton 'pta2'
</declaration>

<location id="id_pta1_loc0" x="0" y="0">
	<name x="0" y="-40">l1</name>
	<label kind="invariant" x="0" y="40">nb__a == 3</label></location>

<location id="id_pta1_loc1" x="200" y="0">
	<name x="200" y="-40">l2</name>
	<label kind="invariant" x="200" y="40">nb__a == 3</label></location>
 <init ref="id_pta1_loc0"/>

	<transition>
		<source ref="id_pta1_loc0"/>
		<target ref="id_pta1_loc0"/>
		<label kind="synchronisation" x="0" y="80">a?</label>
		<label kind="guard" x="0" y="40"></label>
		<label kind="assignment" x="0" y="-40">x = 0, nb__a = nb__a + 1</label>
	</transition>
	<transition>
		<source ref="id_pta1_loc0"/>
		<target ref="id_pta1_loc1"/>
		<label kind="synchronisation" x="100" y="80">b?</label>
		<label kind="guard" x="100" y="40"></label>

	</transition>
 </template>


<template><name x="2" y="2">pta3</name><declaration>// No local declaration for automaton 'pta3'
</declaration>

<location id="id_pta2_loc0" x="0" y="0">
	<name x="0" y="-40">l1</name>
	<label kind="invariant" x="0" y="40"> 3 &gt;= x &amp;&amp; nb__a == 3</label></location>

<location id="id_pta2_loc1" x="200" y="0">
	<name x="200" y="-40">l2</name>
	<label kind="invariant" x="200" y="40"> 3 &gt;= x &amp;&amp; nb__a == 3</label></location>

<location id="id_pta2_loc2" x="400" y="0">
	<name x="400" y="-40">l3</name>
	<label kind="invariant" x="400" y="40">nb__a == 3</label></location>
 <init ref="id_pta2_loc0"/>

	<transition>
		<source ref="id_pta2_loc0"/>
		<target ref="id_pta2_loc1"/>
		<label kind="synchronisation" x="100" y="80">c!</label>
		<label kind="guard" x="100" y="40"> x == 3</label>
		<label kind="assignment" x="100" y="-40">x = 0</label>
	</transition>
	<transition>
		<source ref="id_pta2_loc1"/>
		<target ref="id_pta2_loc2"/>
		<label kind="synchronisation" x="300" y="80">a?</label>
		<label kind="guard" x="300" y="40"></label>
		<label kind="assignment" x="300" y="-40">x = 0, nb__a = nb__a + 1</label>
	</transition>
 </template>
<system>
// List one or more processes to be composed into a system.

system pta1, pta2, pta3;
</system></nta>
	"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		# Test version             : 1
		# Test since               : 2022/02/04
		# Last modified            : 2022/02/04
		# Test for IMITATOR version: 3.1
		'purpose'    : 'Test translation to Jani',
		'author': 'lbinria',
		'tags' : 'translation, jani',
		'input_files': ['translations/jani.imi'],
		'options'    : '-imi2Jani',
		'expectations' : [
			{'file': 'jani.jani' , 'content' : """
	"functions":
	[

	]
	,
	"actions":
	[

	]
	,
	"variables":
	[

		{
			"name":"i",
			"type":"real",
			"initial-value":6
		}
		,

		{
			"name":"r",
			"type":"real",
			"initial-value":2
		}

	]
	,
	"properties":
	[

	]
	,
	"automata":
	[

		{
			"name": "pta",
			"locations":
			[

				{
					"name": "l1"
				}
				,

				{
					"name": "lend"
				}

			]
			,
			"initial-locations":
			[
				"l1"
			]
			,
			"edges":
			[

				{
					"location":"l1",
					"action":"nosync_1",
					"guard":
					{
						"exp":
						{
							"op":"=",
							"left":
							{
								"op":"/",
								"left":
								{
									"op":"-",
									"left":
									{
										"op":"+",
										"left":"i",
										"right":1
									}
									,
									"right":2
								}
								,
								"right":2
							}
							,
							"right":
							{
								"op":"/",
								"left":
								{
									"op":"+",
									"left":
									{
										"op":"*",
										"left":"r",
										"right":2
									}
									,
									"right":1
								}
								,
								"right":2
							}

						}

					}
					,
					"destinations":
					[

						{
							"location":"lend",
							"assignments":
							[

								{
									"ref":"i",
									"value":
									{
										"op":"+",
										"left":
										{
											"op":"*",
											"left":5,
											"right":3
										}
										,
										"right":
										{
											"op":"pow",
											"left":"r",
											"right":2
										}

									}

								}

							]

						}

					]

				}

			]

		}

	]
	,
	"system":
	{
		"elements":
		[

			{
				"automaton": "pta"
			}

		]
		,
		"syncs":
		[

		]

	}

}
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'FMTV challenge: Test EF with project-result -verbose mute',
		'input_files': ['fmtv1A1-v2.imi', 'fmtv1A1-v2-EF.imiprop'],
		'options'    : '',
		'expectations' : [
			{'file': 'fmtv1A1-v2.res' , 'content' : """
BEGIN CONSTRAINT
 18126 >= 125*e2e
& e2e >= 63
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'FMTV challenge: Test EFmin',
		'input_files': ['fmtv1A1-v2.imi', 'fmtv1A1-v2-min.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'fmtv1A1-v2.res' , 'content' : """
BEGIN CONSTRAINT
 e2e >= 63
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	#------------------------------------------------------------
	{
		'purpose'    : 'FMTV challenge: Test EFmax',
		'input_files': ['fmtv1A1-v2.imi', 'fmtv1A1-v2-max.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'fmtv1A1-v2.res' , 'content' : """
BEGIN CONSTRAINT
 e2e >= 0
& 18126 >= 125*e2e
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	#------------------------------------------------------------

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 1',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy01.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy01.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,
    
	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 2',
        'tags':'ptg',        
		'input_files': ['parametric_timed_games/uppaal_converted/toy02.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy02.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,
    
	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 3',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy03.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy03.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,
    
	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 4',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy04.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy04.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,
    
	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 5',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy05.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy05.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]
	
	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 6',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy06.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy06.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 7',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy07.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy07.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 8',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy08.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy08.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 9',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy09.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy09.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 10',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy10.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy10.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 11',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy11.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy11.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 12',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy12.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy12.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 13',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy13.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy13.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 14',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy14.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy14.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 15',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy15.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy15.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis on UPPAAl examples (no parameters) 16',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/uppaal_converted/toy16.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'toy16.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Check that we cannot force uncontrolled actions',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/actions_test.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'actions_test.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 1',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg1.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg1.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 2',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg2.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg2.res' , 'content' : """
BEGIN CONSTRAINT
p > 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 3',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg3.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg3.res' , 'content' : """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 4',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg4.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg4.res' , 'content' : """
BEGIN CONSTRAINT
2 > p
 & p >= 0
 & q >= 5 + p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 5',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg5.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg5.res' , 'content' : """
BEGIN CONSTRAINT
5 >= p
 & p > 0
 & q >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Simple parametric timed game 6',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/simple_ptg6.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'simple_ptg6.res' , 'content' : """
BEGIN CONSTRAINT
5 >= p
 & p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 1',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated1.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated1.res' , 'content' : """
BEGIN CONSTRAINT
5 > p
 & p >= 1
 & q >= p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 2',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated2.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated2.res' , 'content' : """
BEGIN CONSTRAINT
p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 3',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated3.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated3.res' , 'content' : """
BEGIN CONSTRAINT
p >= 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 4',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated4.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated4.res' , 'content' : """
BEGIN CONSTRAINT
10 > p
 & p >= 1
 & q >= p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 5',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated5.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated5.res' , 'content' : """
BEGIN CONSTRAINT
5 >= p
 & p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 6',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated6.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated6.res' , 'content' : """
BEGIN CONSTRAINT
10 >= p
 & p >= 1
 & q >= p
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: PTG written by GPT4 7',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/gpt4_generated/generated7.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'generated7.res' , 'content' : """
BEGIN CONSTRAINT
4 > p
 & p >= 1
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Multiple Automata',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/multiple_automata.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'multiple_automata.res' , 'content' : """
BEGIN CONSTRAINT
 1 >= p
 & p >= 0
 & q = 2
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Complete synthesis test (witness)',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/complete_synthesis_test.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'complete_synthesis_test.res' , 'content' : """
BEGIN CONSTRAINT
6 >= p & p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Complete synthesis test (synthesis)',
        'tags':'ptg',
		'input_files': ['parametric_timed_games/complete_synthesis_test.imi', 'basic-properties/synth-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'complete_synthesis_test.res' , 'content' : """
BEGIN CONSTRAINT
p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Losing state propagation (simple, witness)',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/losing_state_prop_witness_simple.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute -PTG-propagate',
		'expectations' : [
			{'file': 'losing_state_prop_witness_simple.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]
	
	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Losing state propagation (complex, witness)',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/losing_state_prop_witness_complex.imi', 'basic-properties/witness-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute -PTG-propagate',
		'expectations' : [
			{'file': 'losing_state_prop_witness_complex.res' , 'content' : """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Losing state propagation (simple, synthesis)',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/losing_state_prop_synth.imi', 'basic-properties/synth-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute -PTG-propagate',
		'expectations' : [
			{'file': 'losing_state_prop_synth.res' , 'content' : """
BEGIN CONSTRAINT
p >= 3
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Test Cumulative Pruning',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/cumulative_pruning.imi', 'basic-properties/synth-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'cumulative_pruning.res' , 'content' : """
BEGIN CONSTRAINT
p >= 5
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Test Coverage Pruning (Lose)',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/coverage_pruning_lose.imi', 'basic-properties/synth-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'coverage_pruning_lose.res' , 'content' : """
BEGIN CONSTRAINT
	5 >= p & p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

	,

	##------------------------------------------------------------
	{
		'purpose'    : 'Test parametric timed games synthesis: Test Coverage Pruning (Win)',
        "tags": "ptg",
		'input_files': ['parametric_timed_games/coverage_pruning_win.imi', 'basic-properties/synth-parametric-win-strat-accepting.imiprop'],
		'options'    : '-verbose mute',
		'expectations' : [
			{'file': 'coverage_pruning_win.res' , 'content' : """
BEGIN CONSTRAINT
	1 > p & p >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
"""
			} # end result file
			,
		] # end expectations
	} # end test case
	##------------------------------------------------------------]

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
    # TESTING TEMPLATES
    {
        'purpose'      : 'Test correct instantiation of templates (FischerPS08-2)',
        'input_files'  : ['templates/FischerPS08-2.imi', 'templates/FischerPS08-AGnot.imiprop'],
        'options'      : '-verbose mute',
        'expectations' : [{
            'file'   : 'FischerPS08-2.res',
            'content': """
BEGIN CONSTRAINT
 delta >= 0
& Delta >= delta
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 38
Number of transitions                   : 47
Number of computed states               : 48
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (Pipeline_KP12_2_3)',
        'input_files'  : ['templates/Pipeline_KP12_2_3.imi', 'templates/Pipeline_KP12_2_3-EF.imiprop'],
        'options'      : '-verbose mute -depth-limit=9',
        'expectations' : [{
            'file'   : 'Pipeline_KP12_2_3.res',
            'content': """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : depth limit (12 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 47
Number of transitions                   : 70
Number of computed states               : 71
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (SLAF14_5)',
        'input_files'  : ['templates/SLAF14_5.imi', 'templates/SLAF14_5-AGnot.imiprop'],
        'options'      : '-verbose mute -depth-limit=9',
        'expectations' : [{
            'file'   : 'SLAF14_5.res',
            'content': """
BEGIN CONSTRAINT
 t5_C >= 10
& t4_C >= 10
& 50 >= t4_C
& 50 >= t5_C
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : depth limit (100 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 492
Number of transitions                   : 1300
Number of computed states               : 1301
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (category12_vulnerable_conditional-selfcomp)',
        'input_files'  : ['templates/category12_vulnerable_conditional-selfcomp.imi', 'templates/category12_vulnerable_conditional-selfcomp.imiprop'],
        'options'      : '-verbose mute -depth-limit=10',
        'expectations' : [{
            'file'   : 'category12_vulnerable_conditional-selfcomp.res',
            'content': """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : depth limit (68 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 255
Number of transitions                   : 432
Number of computed states               : 433
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (category14_not_vulnerable-selfcomp)',
        'input_files'  : ['templates/category14_not_vulnerable-selfcomp.imi', 'templates/category14_not_vulnerable-selfcomp.imiprop'],
        'options'      : '-verbose mute -depth-limit=9',
        'expectations' : [{
            'file'   : 'category14_not_vulnerable-selfcomp.res',
            'content': """
BEGIN CONSTRAINT
False
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible under-approximation
Termination                             : depth limit (9 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 67
Number of transitions                   : 116
Number of computed states               : 117
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (fischerHRSV02_3)',
        'input_files'  : ['templates/fischerHRSV02_3.imi', 'templates/fischerHRSV02_3-AGnot.imiprop'],
        'options'      : '-verbose mute -depth-limit=9',
        'expectations' : [{
            'file'   : 'fischerHRSV02_3.res',
            'content': """
BEGIN CONSTRAINT
 max_delay > min_delay
& min_delay >= 0
& max_rw > min_rw
& min_rw >= 0
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : possible over-approximation
Termination                             : depth limit (422 successors unexplored)
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 914
Number of transitions                   : 1476
Number of computed states               : 1477
"""
        }]
    }
    ,
    {
        'purpose'      : 'Test correct instantiation of templates (fischer_orig)',
        'input_files'  : ['templates/fischer_orig.imi', 'templates/fischer_orig.imiprop'],
        'options'      : '-verbose mute',
        'expectations' : [{
            'file'   : 'fischer_orig.res',
            'content': """
BEGIN CONSTRAINT
True
END CONSTRAINT

------------------------------------------------------------
Constraint soundness                    : exact
Termination                             : regular termination
Constraint nature                       : good
------------------------------------------------------------
Number of states                        : 25
Number of transitions                   : 36
Number of computed states               : 37
"""
        }]
    }
    ,
### THE END
]
