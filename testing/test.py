#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13, Sorbonne Paris Cité (France)
# 
# Script description: Script for regression tests
# 
# File contributors : Étienne André
# Created           : 2012/05/??
# Last modified     : 2015/10/23
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

# Root path to the main IMITATOR root directory
IMITATOR_PATH = ''
# Path to the example directory
EXAMPLE_PATH = IMITATOR_PATH + 'testing/testcases/'
# Path to the binary directory
BINARY_PATH = IMITATOR_PATH + 'bin/'
# Name for the binary to test
BINARY_NAME = 'imitator'



#************************************************************
# FUNCTIONS
#************************************************************
def make_binary() :
	return BINARY_PATH + BINARY_NAME

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
# IMPORTING THE TESTS CONTENT
#************************************************************
import regression_tests_data
tests = regression_tests_data.tests


#************************************************************
# STARTING SCRIPT
#************************************************************

#print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print '############################################################'
print ' IMITATOR TESTER'
print ' v0.1'
print ''
print ' Étienne André'
print ' LIPN, Université Paris 13, Sorbonne Paris Cité (France)'
print '############################################################'
print 'Starting testing...'
now = datetime.datetime.now()
print now.strftime("%A %d. %B %Y %H:%M:%S %z")


#************************************************************
# CHECK FOR THE EXISTENCE OF BINARIES
#************************************************************
file = make_binary()
if not os.path.exists(file) :
	fail_with('Binary ' + file + ' does not exist')
	all_files_ok = False


#************************************************************
# TEST CASES
#************************************************************
test_id = 1
# TODO => should not print but save to log..
for test_case in tests:
	# Print something
	print ''
	print '************************************************************'
	print ' TEST CASE ' + str(test_id)
	print ' purpose : ' + test_case['purpose']
	print ''

	# Initialize the string for the proper call
	cmd = make_binary()
	
	# Add all input files to the command
	for each_file in test_case['input_files']:
		cmd += ' ' + make_file(each_file)
		# TODO: test for existence of files (just in case)
	
	# Add options
	cmd += ' ' + test_case['options']

	# Print something
	print ' command : ' + cmd

	# Launch!
	os.system(cmd) # returns the exit status
	
	# Check the expectations
	expectation_id = 1
	for expectation in test_case['expectations']:
		# Build file
		output_file = make_file(expectation['file'])
		
		test_expectation_id = str(test_id) + '.' + str(expectation_id)
		
		# Check existence of the output file
		if not os.path.exists(output_file):
			print ' File ' + output_file + ' does not exist! Test ' + test_expectation_id + ' failed.'
		else:
			# Read file
			with open (output_file, "r") as myfile:
				# Get the content
				content = myfile.read()
				# Replace all whitespace characters (space, tab, newline, and so on) with a single space
				content = ' '.join(content.split())
				print "\n\n\n" + content + "\n\n\n"
				
				# Replace all whitespace characters (space, tab, newline, and so on) with a single space
				expected_content = ' '.join(expectation['content'].split())
				print "\n\n\n" + expected_content + "\n\n\n"
				
				# Look for the expected content
				position = content.find(expected_content)
				
				if position >= 0:
					print 'Test ' + test_expectation_id + ' passed.'
				else:
					print 'Test ' + test_expectation_id + ' failed.'
		
		# Increment the expectation id
		expectation_id = expectation_id + 1

	# Increment the test id
	test_id = test_id + 1


#************************************************************
# THE END
#************************************************************

print ''
print '...The end of IMITATOR TESTER!'
