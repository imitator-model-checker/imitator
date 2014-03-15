#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#                     Testing script
#                     (Testing data)
#
# Testing data descripion:
#     sample test data for testing the test =)
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2013/01/23
# Last modified: 2013/01/23
#************************************************************


#************************************************************
# BINARIES
#************************************************************

# Binaries to execute, with options
binaries = [
		{'version': '2.51' , 'options' : '-merge'}
		,
		{'version': 'unstable' , 'options' : ''}
]

#************************************************************
# COMMON OPTIONS
#************************************************************

# Options common for all case studies
common_options = ''



#************************************************************
# LIST OF EXAMPLES
#************************************************************
examples = [
	{'files' : ['Train/Train1PTA.imi' , 'Train/Train1PTA.pi0'] , 'options' : ''}
	,
	{'files' : ['Train/Train1PTA.imi' , 'Train/Train1PTA.pi0'] , 'options' : ''}
]



##************************************************************
## STANDARD CODE
##************************************************************
## NOTE: I couldn't find a way to get the previous variables global (otherwise this code is useless)
#def get_examples():
	#return examples
#def get_binaries():
	#return binaries
#def get_common_options():
	#return common_options
