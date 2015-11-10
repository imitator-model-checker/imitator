#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13, Sorbonne Paris Cité (France)
# 
# Script description: TESTATOR (script for regression tests)
# 
# File contributors : Étienne André
# Created           : 2012/05/??
# Last modified     : 2015/11/10
#************************************************************


#************************************************************
# MODULES
#************************************************************
import time
import datetime
import os
import sys
import subprocess



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
# Log file
LOGFILE = IMITATOR_PATH + 'testing/tests.log'


#************************************************************
# BY DEFAULT: ALL TO LOG FILE
#************************************************************
orig_stdout = sys.stdout
logfile = file(LOGFILE, 'w')
sys.stdout = logfile


#************************************************************
# FUNCTIONS
#************************************************************
def make_binary() :
	return BINARY_PATH + BINARY_NAME

def make_file(file_name) :
	return EXAMPLE_PATH + file_name

def fail_with(text) :
	print_to_log('Fatal error!')
	print_to_log(text)
	sys.exit(0)

def print_warning(text) :
	print_to_log(' *** Warning: ' + text)

def print_error(text) :
	print_to_log(' *** Error: ' + text)


# Print text to log file
def print_to_log(content):
	print content

def print_to_screen(content):
	# Revert stdout
	sys.stdout = orig_stdout
	# Print
	print content
	# Put back stdout to log file
	sys.stdout = logfile

# Print text both to log file and to screen
# NOTE: can probably do better...
def print_to_screen_and_log(content):
	# Print to log
	print_to_log(content)
	# Also print to screen
	print_to_screen(content)



#************************************************************
# IMPORTING THE TESTS CONTENT
#************************************************************
import regression_tests_data
tests = regression_tests_data.tests


#************************************************************
# STARTING SCRIPT
#************************************************************

#print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print_to_screen_and_log('############################################################')
print_to_screen_and_log(' TESTATOR                                              v0.1')
print_to_screen_and_log('')
print_to_screen_and_log(' Étienne André')
print_to_screen_and_log(' LIPN, Université Paris 13, Sorbonne Paris Cité (France)')
print_to_screen_and_log('############################################################')
now = datetime.datetime.now()
print_to_screen_and_log(now.strftime("%A %d. %B %Y %H:%M:%S %z"))


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
# Id for benchmarks
benchmark_id = 1
# Number of passed benchmarks
passed_benchmarks = 0

# Id for test case
test_case_id = 1
# Number of passed test cases
passed_test_cases = 0

for test_case in tests:
	# Initially everything is ok
	passed = True

	# Print something
	print_to_log('')
	print_to_log('************************************************************')
	print_to_log(' BENCHMARK ' + str(benchmark_id))
	print_to_log(' purpose : ' + test_case['purpose'])
	print_to_log('')
	print_to_screen(' Benchmark ' + str(benchmark_id) + "...")

	# Add the path to all input files
	cmd_inputs = []
	for each_file in test_case['input_files']:
		cmd_inputs += [make_file(each_file)]
		# TODO: test for existence of files (just in case)
	
	# Prepare the command (using a list form)
	cmd = [make_binary()] + cmd_inputs + (test_case['options']).split()
	
	# Print the command
	print_to_log(' command : ' + ' '.join(cmd))

	# Launch!
	#os.system(cmd)
	# BUG here: the output is printed AT THE TOP OF logfile, instead of at the right location... anyway I will try to fix that later (FIXME)
	subprocess.call(cmd, stdout=logfile, stderr=logfile)
	
	# Check the expectations
	expectation_id = 1
	for expectation in test_case['expectations']:
		# Build file
		output_file = make_file(expectation['file'])
		
		test_expectation_id = str(benchmark_id) + '.' + str(expectation_id)
		
		# Check existence of the output file
		if not os.path.exists(output_file):
			print_to_log(' File ' + output_file + ' does not exist! Test ' + test_expectation_id + ' failed.')
			passed = False
		else:
			# Read file
			with open (output_file, "r") as myfile:
				# Get the content
				content = myfile.read()
				# Replace all whitespace characters (space, tab, newline, and so on) with a single space
				content = ' '.join(content.split())
				
				# Replace all whitespace characters (space, tab, newline, and so on) with a single space
				expected_content = ' '.join(expectation['content'].split())
				
				# Look for the expected content
				position = content.find(expected_content)
				
				if position >= 0:
					print_to_log(' Test ' + test_expectation_id + ' passed.')
					passed_test_cases += 1
				else:
					passed = False
					print_to_log(' Test ' + test_expectation_id + ' failed!')
					print_to_log("\n" + '*** Expected content for this test:')
					print_to_log("\n" + expected_content + "\n\n")
					print_to_log('*** Content found:')
					print_to_log("\n" + content + "\n\n")
			# Close file
			#close (myfile)
			# Delete file
			os.remove(output_file)
			# TODO: remove all expected output files AFTER the end of the test case; otherwise, if several tests are made in the same file, it won't work

		# Increment the expectation id
		expectation_id += 1
		
		# One more test case
		test_case_id += 1
		
	# If all test cases passed, increment the number of passed benchmarks
	if passed:
		passed_benchmarks += 1
	else:
		print_to_screen("FAILED!")

	# Increment the benchmark id
	benchmark_id += 1


#************************************************************
# THE END
#************************************************************
print_to_log('')
print_to_log('************************************************************')

# NOTE: ugly...
total_benchmarks = benchmark_id - 1
total_test_cases = test_case_id - 1

if total_benchmarks == passed_benchmarks and total_test_cases == passed_test_cases:
	print_to_screen_and_log('All benchmarks (' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ') passed successfully.')
	print_to_screen_and_log('All test cases (' + str(passed_test_cases) + '/' + str(total_test_cases) + ') passed successfully.')
else:
	print_to_screen_and_log('WARNING! Some tests failed.')
	print_to_screen_and_log('' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks passed successfully.')
	print_to_screen_and_log('' + str(total_benchmarks - passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks failed.')
	print_to_screen_and_log('' + str(passed_test_cases) + '/' + str(total_test_cases) + ' test cases passed successfully.')
	print_to_screen_and_log('' + str(total_test_cases - passed_test_cases) + '/' + str(total_test_cases) + ' test cases failed.')

print_to_screen('(See ' + LOGFILE + ' for details.)')

print_to_log('')
print_to_screen_and_log('...The end of TESTATOR!')
