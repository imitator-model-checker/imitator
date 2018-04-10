#!/usr/bin/python
# -*- coding: utf-8 -*-
# ************************************************************
#
#                       IMITATOR
#
# Create a file PaTATOR.ml for a distributed version
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Université Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/18
# Last modified: 2016/03/17
# ************************************************************


# This script copies IMITATOR.ml into IMITATORdistr.ml and comments out the call to PaTATOR


# ************************************************************
# IMPORTS
# ************************************************************
from __future__ import print_function
import re


# ************************************************************
# CONSTANTS
# ************************************************************
# Files
input_file_path = 'src/IMITATOR.ml'
output_file_path = 'src/PaTATOR.ml'

# Patterns in IMITATOR.ml
pattern_begin = r'(\(\*\(\* \*\* \*\*\* \*\*\*\* \*\*\*\*\* \*\*\*\*\*\*    BEGIN FORK PaTATOR    \*\*\*\*\*\* \*\*\*\*\* \*\*\*\* \*\*\* \*\* \*\))'
pattern_end = r'(\(\* \*\* \*\*\* \*\*\*\* \*\*\*\*\* \*\*\*\*\*\*    END FORK PaTATOR    \*\*\*\*\*\* \*\*\*\*\* \*\*\*\* \*\*\* \*\* \*\)\*\))'

# Header of the resulting file
header = """
(*****************************************************************
 WARNING! This file has been automatically generated; do not modify it!
 ****************************************************************)
 
 
"""

# ************************************************************
# GO
# ************************************************************
print("\nGenerating file '%s'..." % output_file_path)


with open(input_file_path, 'r') as old_file, open(output_file_path, 'w') as new_file:

    # Read imitator.ml
    old_text = old_file.read()

    # Add header
    new_file.write(header)

    # ************************************************************
    # Look for begin pattern
    # ************************************************************
    new_text, found_pattern = re.subn(pattern_begin, r"\1*)", old_text)
    if not found_pattern:
        print("Begin pattern not found in '%s'! Aborting." % input_file_path)
        exit(1)

    # ************************************************************
    # Look for end pattern
    # ************************************************************
    text, found_pattern = re.subn(pattern_end, r"(*\1", new_text)
    if not found_pattern:
        print('End pattern not found in %s! Aborting.' % input_file_path)
        exit(1)

    new_file.write(text)

    print('Successfully generated file %s' % output_file_path)


