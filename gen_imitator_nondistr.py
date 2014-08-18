#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
# Create a file IMITATOR-nondistr.ml for a non-distributed version
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/18
# Last modified: 2014/08/18
#************************************************************


# This script copies IMITATOR.ml into IMITATOR-distr.ml and comments out the call to PaTATOR


#************************************************************
# IMPORTS
#************************************************************
from os import close


#************************************************************
# CONSTANTS
#************************************************************
# Files
input_file_path = 'src/IMITATOR.ml'
output_file_path = 'src/IMITATOR-nondistr.ml'

# Patterns in IMITATOR.ml
pattern_begin = '(* ** *** **** ***** ******    BEGIN FORK PaTATOR    ****** ***** **** *** ** *)'
pattern_end = '(* ** *** **** ***** ******    END FORK PaTATOR    ****** ***** **** *** ** *)'


#************************************************************
# GO
#************************************************************
# Open file
old_file = open(input_file_path)

# Create new file
#print 'Creating file ' + output_file_path + '.'
new_file = open(output_file_path, 'w')

# Add header
new_file.write("\
(*****************************************************************\n\
 WARNING! This file has been automatically generated; do not modify it!\n\
 ****************************************************************)\n\n")

#************************************************************
# Look for begin pattern
#************************************************************
found_begin_pattern = False
for line in old_file:
	if pattern_begin in line:
		#print 'Begin pattern found in ' + input_file_path + '.'
		new_file.write(line.replace(pattern_begin, pattern_begin + '(*'))
		found_begin_pattern = True
		break
	else:
		new_file.write(line)

# Check if pattern was found
if not found_begin_pattern:
	print 'Begin pattern not found in ' + input_file_path + '! Aborting.'
	exit(1)

#************************************************************
# Look for end pattern
#************************************************************
found_end_pattern = False
for line in old_file:
	if pattern_end in line:
		#print 'End pattern found in ' + input_file_path + '.'
		new_file.write(line.replace(pattern_end, '*)' + pattern_end))
		found_end_pattern = True
		break
	else:
		new_file.write(line)

# Check if pattern was found
if not found_end_pattern:
	print 'End pattern not found in ' + input_file_path + '! Aborting.'
	exit(1)

#************************************************************
# Copy the remaining lines
#************************************************************
for line in old_file:
	new_file.write(line)

#************************************************************
# Close
#************************************************************
new_file.close()
old_file.close()

print 'Successfully generated file ' + output_file_path + '.'


