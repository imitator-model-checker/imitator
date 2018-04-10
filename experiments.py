#!/usr/bin/python
# ************************************************************
#
#                       IMITATOR
#
#               Launch and interpret experiments
#
# Etienne Andre
# Nguyen Hoang Gia
#
# Laboratoire d'Informatique de Paris Nord
# Université Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/25
# Last modified: 2014/08/25
# ************************************************************


from __future__ import print_function

import os

# ************************************************************
# CONSTANTS
# ************************************************************
imitator_bin = "bin/imitator"
path_to_case_studies = "examples/"
extension_imi = '.imi'
extension_v0 = '.v0'

# ************************************************************
# EXPERIMENTS
# ************************************************************
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


# ************************************************************
# FUNCTIONS
# ************************************************************
# Launch an experiment, and store the result
def launch_and_store(command):
    print("***** Launching experiment '%s'" % command)

    # Launch experiment
    os.system(command)


# Parse result and store
# TODO

# etc.
# TODO

# ************************************************************
# LAUNCH EXPERIMENTS
# ************************************************************

generic_command_fmt = '{bin} {path}{model}{extension_imi} {path}{model}{extension_v0} {options}'
mpi_command = 'mpiexec -n ' + str(nb_nodes)

# Loop on models
for model in models:
    print("************************************************************")
    print("*** EXPERIMENT '%s'" % model)
    print("************************************************************")

    # First: no distribution
    generic_command = generic_command_fmt.format(bin=imitator_bin,
                                                 path=path_to_case_studies,
                                                 extension_imi=extension_imi,
                                                 model=model,
                                                 extension_v0=extension_v0,
                                                 options=common_options)
    launch_and_store(generic_command)

    # Second: loop on distributed options
    for distributed_option in distributed_options:
        command = '{} {} {}'.format(mpi_command, generic_command, distributed_option)
        launch_and_store(command)

# ************************************************************
# PROCESS AND PRINT RESULTS
# ************************************************************
# TODO

exit(0)
