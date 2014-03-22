#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#               Increment the build number
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2013/09/26
# Last modified: 2014/03/22
#************************************************************


import os
from time import gmtime, strftime

#************************************************************
# CONSTANTS
#************************************************************
build_number_file_name = "build_number.txt"
ml_file_name = "src/BuildInfo.ml"
mli_file_name = "src/BuildInfo.mli"


print "Python is now handling build information..."


#************************************************************
# GET CURRENT BUILD TIME
#************************************************************
current_build_date = strftime("%Y-%m-%d %H:%M:%S", gmtime()) + " UTC"
# Just for generation date
date_str = strftime("%Y-%m-%d", gmtime())
year_str = strftime("%Y", gmtime())


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

# Close file
file_handler.close()

#************************************************************
# CREATES OCAML FILES
#************************************************************
def write_to_file(file_name, content):
	# Open and create if non existing
	if os.path.exists(file_name):
		file_handler = file(file_name, "r+")
	else:
		file_handler = file(file_name, "w")
	# Write content
	file_handler.write(content)
	# Close
	file_handler.close()

# .ml
write_to_file(ml_file_name, """
(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        python script
 * 
 * Automatically generated: """ + date_str + """
 *
 ****************************************************************)
 
let build_number = \"""" + str (current_build) + """\"
let build_time = \"""" + current_build_date + """\"
let build_year = \"""" + year_str + """\"

""")


# .mli
write_to_file(mli_file_name, """
(*****************************************************************
 *
 *                       IMITATOR
 * 
 * Laboratoire Specification et Verification (ENS Cachan & CNRS, France)
 * Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 * 
 * Author:        python script
 * 
 * Automatically generated: """ + date_str + """
 *
 ****************************************************************)
 
val build_number : string
val build_time : string
val build_year : string
""")


print "Files '" + ml_file_name + "' and '" + mli_file_name + "' successfully generated."

exit(0)
