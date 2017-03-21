#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13 (France)
# 
# Script description: TESTATOR (script for non-regression tests)
# 
# File contributors : Étienne André
# Created           : 2012/05/??
# Last modified     : 2017/02/10
#************************************************************


#************************************************************
# MODULES
#************************************************************
import time
import datetime
import os
import sys
import subprocess


# To output colored text
class bcolors:
	ERROR	= '\033[91m'
	BOLD	= '\033[1m'
	GOOD	= '\033[1;32;40m'
	NORMAL	= '\033[0m'
	WARNING	= '\033[93m'


#************************************************************
# GENERAL CONFIGURATION
#************************************************************

# Root path to the main IMITATOR root directory
IMITATOR_PATH = ''
# Path to the example directory
EXAMPLE_PATH = IMITATOR_PATH + 'testing/testcases/'
# Path to the binary directory
BINARY_PATH = IMITATOR_PATH + 'bin/'

# Name for the non-distributed binary to test
BINARY_NAME = 'imitator'
# Log file for the non-distributed binary
LOGFILE = IMITATOR_PATH + 'testing/tests.log'

# Name for the distributed binary to test
DISTRIBUTED_BINARY_NAME = 'patator'
# Log file for the distributed binary
DISTRIBUTED_LOGFILE = IMITATOR_PATH + 'testing/testsdistr.log'


#************************************************************
# BY DEFAULT: ALL TO LOG FILE
#************************************************************
orig_stdout = sys.stdout
logfile = file(LOGFILE, 'w')
sys.stdout = logfile


#************************************************************
# FUNCTIONS
#************************************************************
def make_binary(binary) :
	return BINARY_PATH + binary

def make_file(file_name) :
	return EXAMPLE_PATH + file_name

def fail_with(text) :
	print_to_log('Fatal error!')
	print_to_log(text)
	sys.exit(1)

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
# NOTE: can probably do better…
def print_to_screen_and_log(content):
	# Print to log
	print_to_log(content)
	# Also print to screen
	print_to_screen(content)



#************************************************************
# MAIN TESTING FUNCTION
#************************************************************

def test(binary_name, tests, logfile, logfile_name):
	
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# CHECK FOR THE EXISTENCE OF BINARIES
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	binary = make_binary(binary_name)
	if not os.path.exists(binary) :
		fail_with('Binary ' + binary + ' does not exist')
		all_files_ok = False
		
	print_to_screen(bcolors.BOLD + '# TESTING BINARY ' + binary_name + bcolors.NORMAL)

	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# TEST CASES
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
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
		print_to_log('')
		print_to_log('############################################################')
		print_to_log(' BENCHMARK ' + str(benchmark_id))
		print_to_log(' purpose: ' + test_case['purpose'])
		print_to_log('')
		print_to_screen(' Benchmark ' + str(benchmark_id) + ": " + test_case['purpose'] + "…")

		# Add the path to all input files
		cmd_inputs = []
		for each_file in test_case['input_files']:
			cmd_inputs += [make_file(each_file)]
			# TODO: test for existence of files (just in case)
		
		
		#------------------------------------------------------------
		# NOTE: complicated 'if' in case of distributed…
		cmd = ''
		
		# Case 1: distributed: binary = mpiexec, options = all the rest including IMITATOR binary
		if(test_case.has_key('nb_nodes') and test_case['nb_nodes'] > 1):
			# Add the mpiexecc if distributed
			cmd = ['mpiexec'] + ['-n'] + [str(test_case['nb_nodes'])] + [binary] + cmd_inputs + (test_case['options']).split()
		# Case 2: non-distributed: binary = IMITATOR, options = all the rest
		else:
			# Prepare the command (using a list form)
			cmd = [binary] + cmd_inputs + (test_case['options']).split()
		#------------------------------------------------------------
		
		# Print the command
		print_to_log(' command : ' + ' '.join(cmd))

		# Launch!
		#os.system(cmd)
		# NOTE: flushing avoids to mix between results of IMITATOR, and text printed by this script
		logfile.flush()
		subprocess.call(cmd, stdout=logfile, stderr=logfile)
		logfile.flush()
		
		# Files to remove
		files_to_remove = []
		
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
					original_content = myfile.read()
					# Replace all whitespace characters (space, tab, newline, and so on) with a single space
					content = ' '.join(original_content.split())
					
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
						print_to_log("\n" + expectation['content'] + "\n\n")
						print_to_log('*** Content found:')
						print_to_log("\n" + original_content + "\n\n")
				# Close file
				#close (myfile)
				
				# Add file to list of files to remove if not already present
				if not output_file in files_to_remove:
					files_to_remove.append(output_file)

			# Increment the expectation id
			expectation_id += 1
			
			# One more test case
			test_case_id += 1
			
		# Remove all output files
		for my_file in files_to_remove:
			os.remove(my_file)
		
		# If all test cases passed, increment the number of passed benchmarks
		if passed:
			passed_benchmarks += 1
		else:
			print_to_screen(bcolors.ERROR + "FAILED!" + bcolors.NORMAL)

		# Increment the benchmark id
		benchmark_id += 1


	
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	# THE END
	#*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
	print_to_log('')
	print_to_log('')
	print_to_log('############################################################')

	# NOTE: ugly…
	total_benchmarks = benchmark_id - 1
	total_test_cases = test_case_id - 1

	if total_benchmarks == passed_benchmarks and total_test_cases == passed_test_cases:
		print_to_screen_and_log('All benchmarks (' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ') passed successfully.')
		print_to_screen_and_log('All test cases (' + str(passed_test_cases) + '/' + str(total_test_cases) + ') passed successfully.')
	else:
		print_to_screen(bcolors.WARNING + 'WARNING! Some tests failed.' + bcolors.NORMAL)
		print_to_log('WARNING! Some tests failed.')
		
		if(passed_benchmarks == total_benchmarks):
			print_to_screen(bcolors.GOOD + '' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks passed successfully.' + bcolors.NORMAL)
		else:
			print_to_screen(bcolors.WARNING + '' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks passed successfully.' + bcolors.NORMAL)
		print_to_log('' + str(passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks passed successfully.')
	
		if passed_benchmarks < total_benchmarks:
			print_to_screen(bcolors.ERROR + '' + str(total_benchmarks - passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks failed.' + bcolors.NORMAL)
		else:
			print_to_screen('' + str(total_benchmarks - passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks failed.')
		print_to_log('' + str(total_benchmarks - passed_benchmarks) + '/' + str(total_benchmarks) + ' benchmarks failed.')
		
		if passed_test_cases == total_test_cases:
			print_to_screen(bcolors.GOOD + '' + str(passed_test_cases) + '/' + str(total_test_cases) + ' test cases passed successfully.' + bcolors.NORMAL)
		else:
			print_to_screen(bcolors.WARNING + '' + str(passed_test_cases) + '/' + str(total_test_cases) + ' test cases passed successfully.' + bcolors.NORMAL)
		print_to_log('' + str(passed_test_cases) + '/' + str(total_test_cases) + ' test cases passed successfully.')
	
		if passed_test_cases < total_test_cases:
			print_to_screen(bcolors.ERROR + '' + str(total_test_cases - passed_test_cases) + '/' + str(total_test_cases) + ' test cases failed.' + bcolors.NORMAL)
		else:
			print_to_screen('' + str(total_test_cases - passed_test_cases) + '/' + str(total_test_cases) + ' test cases failed.')
		print_to_log('' + str(total_test_cases - passed_test_cases) + '/' + str(total_test_cases) + ' test cases failed.')

	print_to_screen('(See ' + logfile_name + ' for details.)')



#************************************************************
# STARTING SCRIPT
#************************************************************

#print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print_to_screen_and_log('############################################################')
print_to_screen(bcolors.BOLD + ' TESTATOR' + bcolors.NORMAL + '                                              v0.1')
print_to_log(' TESTATOR' + '                                              v0.1')
print_to_screen_and_log('')
print_to_screen_and_log(' Étienne André')
print_to_screen_and_log(' LIPN, Université Paris 13 (France)')
print_to_screen_and_log('############################################################')
now = datetime.datetime.now()
print_to_screen_and_log(now.strftime("%A %d. %B %Y %H:%M:%S %z"))



#************************************************************
# 1. TESTING IMITATOR
#************************************************************

# IMPORTING THE TESTS CONTENT
import regression_tests_data
tests = regression_tests_data.tests

print_to_screen('')

test(BINARY_NAME, tests, logfile, LOGFILE)


#************************************************************
# 2. TESTING PATATOR
#************************************************************

# SETTING LOGS
logfile = file(DISTRIBUTED_LOGFILE, 'w')

# IMPORTING THE TESTS CONTENT
import regression_tests_data_distr
tests_distr = regression_tests_data_distr.tests_distr

print_to_screen('')

test(DISTRIBUTED_BINARY_NAME, tests_distr + tests, logfile, DISTRIBUTED_LOGFILE)


#************************************************************
# THE END
#************************************************************

print_to_screen_and_log('')
print_to_screen_and_log('…The end of TESTATOR!')

sys.exit(0)
