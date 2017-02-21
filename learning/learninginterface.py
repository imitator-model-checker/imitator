#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
#
# Script to handle interface between IMITATOR and the learning tool
#
# Étienne André
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cité, France
#
# Created      : 2016/08/04
# Last modified: 2016/08/04
#************************************************************


#************************************************************
# MODULES
#************************************************************
import sys



#************************************************************
# CONSTANTS
#************************************************************


#************************************************************
# USEFUL METHODS
#************************************************************
def abort(reason):
	raise SystemExit('Fatal error!\n' + reason + "\n*** Aborting")


#************************************************************
# Retrieve arguments
#************************************************************
if len(sys.argv) <> 4:
	abort("Exactly 3 arguments are expected")

original_model = sys.argv[1]
new_model_name = sys.argv[2]
pi0 = sys.argv[3]

# DEBUG: recall arguments
print original_model
print new_model_name
print pi0





# So far, do nothing :-)

#************************************************************
# THE END
#************************************************************

exit(0)
