#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#                     Testing script
#                     (Testing data)
#
# Testing data descripion:
#     cartography algorithm, testing improved algorithm
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
		{'version': '2.51' , 'options' : ''}
		,
		{'version': 'unstable' , 'options' : ''}
]

#************************************************************
# COMMON OPTIONS
#************************************************************

# Options common for all case studies
common_options = '-mode cover'



#************************************************************
# LIST OF EXAMPLES
#************************************************************
examples = [
	{'files' : ['AndOr/AndOr.imi' , 'AndOr/AndOr.v0 '] , 'options' : ''}
	,
	{'files' : ['Flipflop/flipflop.imi' , 'Flipflop/flipflop.v0'] , 'options' : ''}
	,
	{'files' : ['SRlatch/SRlatch.imi' , 'SRlatch/SRlatch.v0'] , 'options' : ''}
	,
	#{'files' : ['RCP/RCP.imi' , 'RCP/RCP.v0'] , 'options' : ''}
	#,
	{'files' : ['CSMACD/csmacdPrism_2p.imi' , 'CSMACD/csmacdPrism_2p.v0'] , 'options' : ''}
]

