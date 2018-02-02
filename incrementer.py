#!/usr/bin/python
# -*- coding: utf-8 -*-

# ************************************************************
#
#                       IMITATOR
#
#               Increment the build number
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : 2013/05/22
# Last modified: 2016/03/14
# ************************************************************


from __future__ import print_function
import os

#************************************************************
# CONSTANTS
#************************************************************
# The file containing the build number
build_number_file_name = "build_number.txt"

# The empty file (not synchronized with the IMITATOR repository) acting as a flag: if it exists, the build number is
# incremented, otherwise it is not
flag_file_name = "iamadeveloper"


# ************************************************************
# CHECK WHETHER THE BUILD NUMBER SHOULD BE INCREMENTED
# ************************************************************
if not os.path.isfile(flag_file_name):
    print("File '%s' not found: build number kept unchanged." % flag_file_name)
    exit(0)


# ************************************************************
# GET CURRENT BUILD NUMBER
# ************************************************************
print("Incrementing build number...")

# Open file in read mode
with open(build_number_file_name) as file_handler:

    # Read content
    content = file_handler.read()

    # Convert to int
    current_build = int(content)


# ************************************************************
# INCREMENTS BUILD NUMBER FOR NEXT TIME
# ************************************************************

# Open file in write mode
with open(build_number_file_name, "w") as file_handler:

    # Add + 1
    file_handler.write(str(current_build + 1))


print("Current build number: %s" % content)

exit(0)
