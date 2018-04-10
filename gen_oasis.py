#!/usr/bin/python
# -*- coding: utf-8 -*-

# ************************************************************
#
#                       IMITATOR
#
# Create the _oasis file for IMITATOR
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Université Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/18
# Last modified: 2014/08/18
# ************************************************************


# This script copies oasis-config into _oasis


# ************************************************************
# IMPORTS
# ************************************************************
from __future__ import print_function


# ************************************************************
# CONSTANTS
# ************************************************************
# Files
input_file_path = 'oasis-config'
output_file_path = '_oasis'


# WARNING: the rest of this file is identical to gen_oasis_distr.py

header = """\
############################################################
# WARNING! This file has been automatically generated; do not modify it!
# Modify {} instead 
############################################################

""".format(input_file_path)


def write_to_oasis(input_file, output_file):
    # Open file
    with open(input_file) as old_file, open(output_file, 'w') as new_file:

        # Add header
        new_file.write(header)

        # Copy file
        new_file.write(old_file.read())

        print('Successfully generated file %s.' % output_file_path)


# ************************************************************
# GO
# ************************************************************
if __name__ == '__main__':
    write_to_oasis(input_file_path, output_file_path)
