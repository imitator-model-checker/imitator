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


# ************************************************************
# FUNCTIONS
# ************************************************************
def find_pattern(read_file, write_file, pattern, new_pattern):
    found_pattern = False
    for line in read_file:
        if pattern in line:
            # print 'Begin pattern found in ' + input_file_path + '.'
            write_file.write(line.replace(pattern, new_pattern))
            found_pattern = True
            break
        else:
            write_file.write(line)

    # Check if pattern was found
    return found_pattern


# ************************************************************
# CONSTANTS
# ************************************************************
# Files
input_file_path = 'src/IMITATOR.ml'
output_file_path = 'src/PaTATOR.ml'

# Patterns in IMITATOR.ml
pattern_begin = '(*(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)'
pattern_end = '(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)*)'


# ************************************************************
# GO
# ************************************************************
print("\nGenerating file '%s'..." % output_file_path)


# Header of the resulting file
header = """
(*****************************************************************
 WARNING! This file has been automatically generated; do not modify it!
 ****************************************************************)
 
 
"""


with open(input_file_path, 'r') as old_file, open(output_file_path, 'w') as new_file:

    # Add header
    new_file.write(header)

    # ************************************************************
    # Look for begin pattern
    # ************************************************************
    found_begin_pattern = find_pattern(old_file, new_file, pattern_begin, pattern_begin + '*)')
    if not found_begin_pattern:
        print("Begin pattern not found in '%s'! Aborting." % input_file_path)
        exit(1)

    # ************************************************************
    # Look for end pattern
    # ************************************************************
    found_end_pattern = find_pattern(old_file, new_file, pattern_end, '(*' + pattern_end)
    if not found_end_pattern:
        print('End pattern not found in %s! Aborting.' % input_file_path)
        exit(1)

    new_file.write(old_file.read())

    print('Successfully generated file %s' % output_file_path)


