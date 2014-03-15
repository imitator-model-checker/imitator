#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#               Increment the build number
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2013/05/22
# Last modified: 2013/09/25
#************************************************************


import os
from time import gmtime, strftime

#************************************************************
# CONSTANTS
#************************************************************
build_number_file_name = "build_number.txt"

print "Incrementing build number..."



#************************************************************
# GET CURRENT BUILD NUMBER
#************************************************************
# Open file in read mode
file_handler = open(build_number_file_name)

# Read content
content = file_handler.read()

# Close file
file_handler.close()

# Convert to int
current_build = int(content)


#************************************************************
# INCREMENTS BUILD NUMBER FOR NEXT TIME
#************************************************************

# Open file in write mode
file_handler = open(build_number_file_name, "w")

# Add + 1
file_handler.write(str(current_build + 1))

# Close file
file_handler.close()

print "Current build number: " + (str) (current_build)

exit(0)
