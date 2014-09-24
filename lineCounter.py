########################################################
########################################################
##   Laboratoire Specification et Verification
##
##   Line counter script
##
##   Author: Etienne ANDRE
##
##   Created       : 2009/04/06
##   Last modified : 2009/09/24
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
# Relative path
path = "./src/" # os.getcwd() + 

print " Counting lines in all files in " + path

# Iterate on the files in the "src" directory
for file in os.listdir(path):
	file = path + file
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
print "IMITATOR contains " + str(counter) + " lines of code in " + str(nb_files) + " files."
