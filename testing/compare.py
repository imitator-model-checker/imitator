#!/usr/bin/python
# ************************************************************
#
#                       IMITATOR
#
# Script to compare performances between options/versions
# (ongoing work)
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Université Paris 13, Sorbonne Paris Cite, France
# Created      : 2012/05/??
# Last modified: 2013/01/23
# ************************************************************


# ************************************************************
# MODULES
# ************************************************************
from __future__ import print_function
import datetime
import os
import sys

from testdata_bc_improved import binaries, examples, common_options


# ************************************************************
# GENERAL CONFIGURATION
# ************************************************************

# Root path to the main IMITATOR directory
IMITATOR_PATH = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# Path to the example directory
EXAMPLE_PATH = os.path.join(IMITATOR_PATH, 'examples/')
# Path to the binary directory
BINARY_PATH = os.path.join(IMITATOR_PATH, 'bin/')
# Root name for the binaries
BINARY_NAME = 'IMITATOR'


# ************************************************************
# DATA MODULES
# ************************************************************
# Import ONE AND ONLY ONE set of data at a time
# WARNING: very very ugly (but couldn't find better)

# import testdata_sample
# binaries = testdata_sample.binaries
# examples = testdata_sample.examples
# common_options = testdata_sample.common_options

# binaries = testdata_bc_improved.binaries
# examples = testdata_bc_improved.examples
# common_options = testdata_bc_improved.common_options

print(dir())

# TODO: check that 'files' is defined for each example
# TODO: check that 'version' is defined for each binary

print('(No test carried out on imported data)')


# ************************************************************
# FUNCTIONS
# ************************************************************
def make_binary(binary_version):
    return os.path.join(BINARY_PATH, BINARY_NAME) + binary_version


def make_file(file_name):
    return os.path.join(EXAMPLE_PATH, file_name)


def fail_with(text):
    print('Fatal error!')
    print(text)
    sys.exit(0)


def print_warning(text):
    print(' *** Warning: %s' % text)


def print_error(text):
    print(' *** Error: %s' % text)


# ************************************************************
# STARTING SCRIPT
# ************************************************************

# print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
header = """
############################################################
 IMITATOR TESTER
 v0.1

 Etienne Andre
 LIPN, Universite Paris 13, Sorbonne Paris Cite, France
############################################################
Starting testing...
"""

print(header)
now = datetime.datetime.now()
print(now.strftime("%A %d. %B %Y %H:%M:%S %z"))


# ************************************************************
# CHECK THE EXISTENCE OF BINARIES
# ************************************************************
all_files_ok = True
for binary in binaries:
    binary_version = binary['version']
    file = make_binary(binary_version)
    if not os.path.exists(file):
        print_error('Binary %s does not exist' % file)
        all_files_ok = False

if not all_files_ok:
    fail_with('Binaries were not found')


# ************************************************************
# LAUNCH EXAMPLES
# ************************************************************
for example in examples:
    # Initialize the string for the proper call
    example_call = ''
    # Add each file
    for file in example['files']:
        example_call += make_file(file) + ' '

    # Print something
    print('\n************************************************************')
    print(' EXAMPLE %s \n' % example_call)

    # Add the option (if any)
    if 'options' in example:
        example_call += example['options']

    # Iterate on versions
    for binary in binaries:
        binary_version = binary['version']
        print('*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-')
        print(' Version %s' % binary_version)

        # Handle options
        options = common_options
        if 'options' in binary:
            options += ' ' + binary['options']

        cmd = make_binary(binary_version) + ' ' + example_call + options
        print(' >> %s' % cmd)

        os.system(cmd)  # returns the exit status


# ************************************************************
# THE END
# ************************************************************

print('\n...The end of IMITATOR TESTER!')
