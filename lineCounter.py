########################################################
#   Laboratoire Specification et Verification
#
#   Line counter script
#
#   Author: Etienne ANDRE
#
#   Created       : 2009/04/06
#   Last modified : 2014/09/24
########################################################

########################################################
# IMPORTED MODULES
########################################################
from __future__ import print_function

import os
import re

# Counter of lines
counter = 0

# Number of files
nb_files = 0

# Relative path
path = "./src/"  # os.getcwd() +

print(" Counting lines in all files in %s" % path)

# Iterate on the files in the "src" directory
for file in os.listdir(path):
    file = os.path.join(path, file)

    # Only consider files
    if os.path.isfile(file):

        # Count lines
        nb_lines = open(file, 'r').read().count('\n') + 1
        counter += nb_lines
        nb_files += 1

        # Print on screen
        print(" File {}: {} lines".format(file, nb_lines))

# Print final result
print("IMITATOR contains {} lines of code in {} files.".format(counter, nb_files))
