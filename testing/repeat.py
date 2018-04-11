#!/usr/bin/python
# -*- coding: utf-8 -*-
# ************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13, Sorbonne Paris Cité (France)
# 
# Script description: small script to detect a rare exception in the random generator initialization
# 
# File contributors : Étienne André
# Created           : 2016/03/23
# Last modified     : 2016/03/23
# ************************************************************


# ************************************************************
# MODULES
# ************************************************************
from __future__ import print_function
import datetime
import os
import sys
import subprocess

# ************************************************************
# GENERAL CONFIGURATION
# ************************************************************

# Root path to the main IMITATOR root directory
IMITATOR_PATH = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# Path to the binary directory
BINARY_PATH = os.path.join(IMITATOR_PATH, 'bin/')
# Path to the example directory
EXAMPLE_PATH = os.path.join(IMITATOR_PATH, 'testing/testcases/')

# Name for the non-distributed binary to test
BINARY_NAME = 'imitator'


# ************************************************************
# FUNCTIONS
# ************************************************************
def make_binary(binary):
    return os.path.join(BINARY_PATH, binary)


def make_file(file_name):
    return os.path.join(EXAMPLE_PATH, file_name)


orig_stdout = sys.stdout
logfile = open('/dev/null', 'w')


# sys.stdout = logfile

# ************************************************************
# MAIN TESTING FUNCTION
# ************************************************************

def test(nb_times):
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # CHECK FOR THE EXISTENCE OF BINARIES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    binary = make_binary(BINARY_NAME)
    # if not os.path.exists(binary):
        # fail_with('Binary ' + binary + ' does not exist')
        # all_files_ok = False

    print('# TESTING BINARY ' + BINARY_NAME)

    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    # TEST CASES
    # *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
    for i in range(1, nb_times):

        # Do not print each step
        if i % 25 == 0:
            print(i)

        cmd = [binary, make_file('flipflop.imi'), make_file('flipflop.v0'), '-mode random1']

        result = subprocess.call(cmd, stdout=logfile)  # stderr=logfile
        if result != 0:
            print('...oops! Something went wrong, it seems.')
            print(' '.join(cmd))
            sys.exit(1)


# ************************************************************
# STARTING SCRIPT
# ************************************************************

# print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
header="""
############################################################
 Script                                               v0.1
 
 Étienne André
 LIPN, Université Paris 13, Sorbonne Paris Cité (France)
'############################################################
"""
now = datetime.datetime.now()
print(now.strftime("%A %d. %B %Y %H:%M:%S %z"))

# ************************************************************
# TESTING
# ************************************************************
test(100000)

# ************************************************************
# THE END
# ************************************************************

print('\n...The end!')

sys.exit(0)
