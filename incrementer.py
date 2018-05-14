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
# Université Paris 13, Sorbonne Paris Cite, France
# Created      : 2013/05/22
# Last modified: 2016/03/14
# ************************************************************


from __future__ import print_function

import os

# ************************************************************
# CONSTANTS
# ************************************************************
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
# INCREMENTS BUILD NUMBER FOR NEXT TIME
# ************************************************************
print("Incrementing build number...")

# Open file in read mode
with open(build_number_file_name, 'r+') as file_handler:

    # Get current build number content
    content = file_handler.read()

    # Convert to int
    current_build = int(content)

    file_handler.seek(0)

    # Increments build number for next time
    file_handler.write(str(current_build + 1))

    print("Current build number: %s" % content)

exit(0)
