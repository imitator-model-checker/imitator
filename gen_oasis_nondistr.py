#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
# Create the _oasis file for IMITATOR
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2014/08/18
# Last modified: 2014/08/18
#************************************************************


# This script copies oasis-config-nondistr into _oasis


#************************************************************
# IMPORTS
#************************************************************
from os import close


#************************************************************
# CONSTANTS
#************************************************************
# Files
input_file_path = 'oasis-config-nondistr'
output_file_path = '_oasis'


# WARNING: the rest of this file is identical to gen_oasis.py


#************************************************************
# GO
#************************************************************
# Open file
old_file = open(input_file_path)

# Create new file
new_file = open(output_file_path, 'w')

# Add header
new_file.write("\
############################################################\n\
# WARNING! This file has been automatically generated; do not modify it!\n\
# Modify " + input_file_path + " instead \n\
############################################################\n\n")


#************************************************************
# Copy
#************************************************************
for line in old_file:
	new_file.write(line)

#************************************************************
# Close
#************************************************************
new_file.close()
old_file.close()

print 'Successfully generated file ' + output_file_path + '.'

