########################################################
########################################################
##   Laboratoire Specification et Verification
##
##   Line counter script
##
##   Author: Etienne ANDRE
##
##   Created       : 06/04/2009
##   Last modified : 06/04/2009
########################################################
######################################################## 

########################################################
## IMPORTED MODULES
########################################################
import os, re


# Counter of lines
counter = 0
# Number of files
nb_files = 0
# Iterate on the files of the current directory
for file in os.listdir('.'):
	# Only consider files
	if os.path.isfile(file):
		# Open file
		file_object	= open(file, 'r')
		string_file	= file_object.read()

		# Count lines
		nb_lines	= len(re.findall("\n", string_file)) + 1
		counter		= counter + nb_lines
		nb_files	= nb_files + 1
		
		# Print on screen
		print " File " + file + ": " + str(nb_lines) + " lines"

# Print final result
print "IMPERATOR contains " + str(counter) + " lines of code in " + str(nb_files) + " files."
