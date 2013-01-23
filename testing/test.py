#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#                     Testing script
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
# Created      : ??/05/2012
# Last modified: 23/01/2013
#************************************************************


#************************************************************
# MODULES
#************************************************************
import time
import datetime
import os
import sys


#************************************************************
# GENERAL CONFIGURATION
#************************************************************

# Root path to the main IMITATOR directory
IMITATOR_PATH = ''
# Path to the example directory
EXAMPLE_PATH = IMITATOR_PATH + 'examples/'
# Path to the binary directory
BINARY_PATH = IMITATOR_PATH + 'bin/'
# Root name for the binaries
BINARY_NAME = 'IMITATOR'


#************************************************************
# DATA MODULES
#************************************************************
# Import ONE AND ONLY ONE set of data at a time
# WARNING: very very ugly (but couldn't find better)
import testdata_sample
binaries = testdata_sample.binaries
examples = testdata_sample.examples
common_options = testdata_sample.common_options

print dir()

# TODO: check that 'files' is defined for each example
# TODO: check that 'version' is defined for each binary

print '(No test carried out on imported data)'


#************************************************************
# FUNCTIONS
#************************************************************
def make_binary(binary_version) :
	return BINARY_PATH + BINARY_NAME + binary_version

def make_file(file_name) :
	return EXAMPLE_PATH + file_name

def fail_with(text) :
	print 'Fatal error!'
	print text
	sys.exit(0)

def print_warning(text) :
	print ' *** Warning: ' + text

def print_error(text) :
	print ' *** Error: ' + text


#************************************************************
# STARTING SCRIPT
#************************************************************

#print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print '############################################################'
print ' IMITATOR TESTER'
print ' v0.1'
print ''
print ' Etienne Andre'
print ' LIPN, Universite Paris 13, Sorbonne Paris Cite, France'
print '############################################################'
print 'Starting testing...'
now = datetime.datetime.now()
print now.strftime("%A %d. %B %Y %H:%M:%S %z")


#************************************************************
# CHECK THE EXISTENCE OF BINARIES
#************************************************************
all_files_ok = True
for binary in binaries:
	binary_version = binary['version']
	file = make_binary(binary_version)
	if not os.path.exists(file) :
		print_error('Binary ' + file + ' does not exist')
		all_files_ok = False
if not all_files_ok:
	fail_with('Binaries were not found')


#************************************************************
# LAUNCH EXAMPLES
#************************************************************
for example in examples:
	# Initialize the string for the proper call
	example_call = ''
	# Add each file
	for file in example['files']:
		example_call += make_file(file) + ' '
	
	# Print something
	print ''
	print '************************************************************'
	print ' EXAMPLE ' + example_call
	print ''

	# Add the option (if any)
	if 'options' in example:
		example_call += example['options']
	
	# Iterate on versions
	for binary in binaries:
		binary_version = binary['version']
		print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
		print ' Version ' + binary_version
		
		# Handle options
		options = common_options
		if 'options' in binary:
			options += ' ' + binary['options']
		
		cmd = make_binary(binary_version) + ' ' + example_call + options
		print ' >> ' + cmd
		
		os.system(cmd) # returns the exit status



#************************************************************
# THE END
#************************************************************

print ''
print '...The end of IMITATOR TESTER!'
