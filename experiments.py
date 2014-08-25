#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#               Launch and interprete experiments
#
# Etienne Andre
# Nguyen Hoang Gia
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/25
# Last modified: 2014/08/25
#************************************************************


import os
#from time import gmtime, strftime

#************************************************************
# CONSTANTS
#************************************************************
imitator_bin = "bin/imitator"
path_to_case_studies = "examples/"
extension_imi = '.imi'
extension_v0 = '.v0'


#************************************************************
# EXPERIMENTS
#************************************************************
# Options valid for all experiments
common_options = '-mode cover'

# List of models to consider
models = [
	'Flipflop/flipflop',
	'Scheduling/example-a50-j0',
]

# List of options to consider for each model
distributed_options = [
	'-distributed random10',
	'-distributed random20',
	'-distributed sequential',
	'-distributed shuffle',
]

# Number of nodes
nb_nodes = 8

#************************************************************
# FUNCTIONS
#************************************************************
# Lauch an experiment, and store the result
def launch_and_store(command):
	print "***** Launching experiment '" + command + "'"
	
	# Launch experiment
	os.system(command)

	# Parse result and store
	# TODO

	# etc.
	# TODO

#************************************************************
# LAUNCH EXPERIMENTS
#************************************************************
# Loop on models
for model in models:
	print "************************************************************"
	print "*** EXPERIMENT '" + model + "'"
	print "************************************************************"
	
	generic_command = imitator_bin  + ' ' + path_to_case_studies + model + extension_imi + ' ' + path_to_case_studies + model + extension_v0  + ' ' + common_options
	
	# First: no distribution
	command = generic_command
	launch_and_store(command)
	
	# Second: loop on distributed options
	mpi_command = 'mpiexec -n ' + str(nb_nodes)
	for distributed_option in distributed_options:
		command = mpi_command + ' ' + generic_command + ' ' + distributed_option
		launch_and_store(command)



#************************************************************
# PROCESS AND PRINT RESULTS
#************************************************************
# TODO



#************************************************************
# THE END
#************************************************************

exit(0)
