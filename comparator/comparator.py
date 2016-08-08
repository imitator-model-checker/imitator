#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13, Sorbonne Paris Cité (France)
# 
# Script description: COMPARATOR (script for non-regression tests)
# 
# File contributors : Étienne André
# Created           : 2016/08/08
# Last modified     : 2016/08/08
#************************************************************


#************************************************************
# MODULES
#************************************************************
import re
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
BENCHMARKS_PATH = IMITATOR_PATH + 'benchmarks/'
# Path to the binary directory
BINARY_PATH = IMITATOR_PATH + 'bin/'

# Name for the non-distributed binary to test
BINARY_NAME = 'imitator'
# Log file for the non-distributed binary
LOGFILE = IMITATOR_PATH + 'comparator/comparator.log'
# Result files prefix
RESULT_FILES_PATH = IMITATOR_PATH + 'comparator/results/'


#************************************************************
# BY DEFAULT: ALL TO LOG FILE
#************************************************************
orig_stdout = sys.stdout
logfile = file(LOGFILE, 'w')
sys.stdout = logfile


#************************************************************
# VERSIONS, OPTIONS AND SYNTAX
#************************************************************

#------------------------------------------------------------
# Versions
#------------------------------------------------------------
# NOTE: really ugly to manually assign a value…
V_2_5	= 1
V_2_6_1	= 2
V_2_7_3	= 3
V_2_8	= 4

#------------------------------------------------------------
# Options
#------------------------------------------------------------
# NOTE: really ugly to manually assign a value…
OPT_INCLUSION			= 1
OPT_MERGING				= 2
OPT_OUTPUT_PREFIX		= 3
OPT_OUTPUT_RES			= 4
OPT_OUTPUT_TRACE_SET	= 5

UNDEFINED_SYNTAX = -1

# Critical options, i.e., without which the analysis shall not be run; in other words, if a version does not implement an option required by the analysis and belonging to this list, the benchmark is not run
critical_options = [OPT_MERGING]

# Mainly needed for printing and info purpose
option_names = {
	OPT_INCLUSION			: 'inclusion',
	OPT_MERGING				: 'merging',
	OPT_OUTPUT_PREFIX		: 'output-prefix',
	OPT_OUTPUT_RES			: 'output-result',
	OPT_OUTPUT_TRACE_SET	: 'output-trace-set',
}

#------------------------------------------------------------
# Binaries
#------------------------------------------------------------

versions = {
	#------------------------------------------------------------
	V_2_5 : {
		'version_name'		: '2.5',
		'binary'			: 'imitator25',
		'syntax':
			{
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-with-merging',
			OPT_OUTPUT_PREFIX		: '-log-prefix',
			OPT_OUTPUT_RES			: UNDEFINED_SYNTAX,
			OPT_OUTPUT_TRACE_SET	: '-with-dot',
			},
		'files_suffix'			: '_2_5',
	},
	#------------------------------------------------------------
	V_2_6_1 : {
		'version_name'		: '2.6.1',
		'binary'			: 'imitator261',
		'syntax':
			{
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_OUTPUT_PREFIX		: '-log-prefix',
			OPT_OUTPUT_RES			: UNDEFINED_SYNTAX,
			OPT_OUTPUT_TRACE_SET	: '-with-dot',
			},
		'files_suffix'			: '_2_6_1',
	},
	#------------------------------------------------------------
	V_2_7_3 : {
		'version_name'		: '2.7.3',
		'binary'			: 'imitator273',
		'syntax':
			{
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_OUTPUT_PREFIX		: '-output-prefix',
			OPT_OUTPUT_RES			: '-output-result',
			OPT_OUTPUT_TRACE_SET	: '-output-trace-set',
			},
		'files_suffix'			: '_2_7_3',
	},
	#------------------------------------------------------------
	V_2_8 : {
		'version_name'		: '2.8',
		'binary'			: 'imitator',
		'syntax':
			{
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_OUTPUT_PREFIX		: '-output-prefix',
			OPT_OUTPUT_RES			: '-output-result',
			OPT_OUTPUT_TRACE_SET	: '-output-trace-set',
			},
		'files_suffix'			: '_2_8',
	},
}



#************************************************************
# FUNCTIONS
#************************************************************
ANALYSIS_FAILED = -1


def make_binary(binary) :
	return BINARY_PATH + binary

def make_file(file_name) :
	return BENCHMARKS_PATH + file_name

def make_log_file(benchmark, version):
	return RESULT_FILES_PATH + benchmark['log_prefix'] +  versions[version]['files_suffix'] + ".benchlog"

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
# NOTE: can probably do better...
def print_to_screen_and_log(content):
	# Print to log
	print_to_log(content)
	# Also print to screen
	print_to_screen(content)



# Function to retrieve the computation time depending on the benchmark
def get_computation_time(benchmark, version):
	if version == V_2_5 or version == V_2_6_1 or version == V_2_7_3:
		# TODO: check if files exist
		# Open log file
		log_file = make_log_file(benchmark, version)
		
		# "Inverse method successfully finished after 0.048 second."
		pattern = re.compile("successfully finished after (\d*\.\d*) second")

		for i, line in enumerate(open(log_file)):
			for match in re.finditer(pattern, line):
				#print_to_screen('Found on line %s: %s' % (i+1, match.groups()))
				return match.groups()[0]
		
		print_error("Time not found for benchmark " + benchmark['name'] + " with version " + versions[version]['version_name'])
		return ANALYSIS_FAILED
	
	if version == V_2_8:
		# Open res file
		res_file = RESULT_FILES_PATH + benchmark['log_prefix'] +  versions[version]['files_suffix'] + ".res"
	
		# Pattern: Computation time                        : 0.041 second
		pattern = re.compile("Computation time\s*:\s*(\d*\.\d*) second")

		for i, line in enumerate(open(res_file)):
			for match in re.finditer(pattern, line):
				#print_to_screen('Found on line %s: %s' % (i+1, match.groups()))
				return match.groups()[0]
		
		print_error("Time not found for benchmark " + benchmark['name'] + " with version " + versions[version]['version_name'])
		return ANALYSIS_FAILED


#************************************************************
# MAIN RUNNING FUNCTION
#************************************************************

# Global result
results = {}

def run(benchmark, versions_to_test, logfile):
	
	# Print something
	print_to_screen_and_log('')
	print_to_log('')
	print_to_log('############################################################')
	print_to_screen_and_log(' BENCHMARK ' + benchmark['name'])
	
	# Create the row in the results array
	results[benchmark['log_prefix']] = {}
	
	# Prepare command
	for version in versions_to_test:
		
		print_to_log('')

		# If a critical option is not defined for this version, do not run
		to_run = True
		for option in benchmark['options']:
			if option in critical_options and versions[version]['syntax'][option] == UNDEFINED_SYNTAX:
				print_to_screen_and_log(' Option ' + option_names[option] + ' not defined for version ' + versions[version]['version_name'] + '!')
				to_run = False
		if not to_run:
			print_to_screen_and_log(' Skip version ' + versions[version]['version_name'] + '')
			# Store result
			results[benchmark['log_prefix']][version] = ANALYSIS_FAILED
		else:
			
			# Create the binary
			binary = make_binary(versions[version]['binary'])
			
			# Create the options
			options_str_list = []
			for option in benchmark['options']:
				# Only add option if syntax defined for this version
				if versions[version]['syntax'][option] != UNDEFINED_SYNTAX:
					# Add the option with the correct syntax
					options_str_list.append(versions[version]['syntax'][option])
			# Add the option to redirect log files to the dedicated dir
			options_str_list.extend([versions[version]['syntax'][OPT_OUTPUT_PREFIX] , RESULT_FILES_PATH + benchmark['log_prefix'] + versions[version]['files_suffix'] ])
					
			# Add the path to all input files
			cmd_inputs = []
			for each_file in benchmark['input_files']:
				cmd_inputs += [make_file(each_file)]
				# TODO: test for existence of files (just in case)
			
			#------------------------------------------------------------
			# NOTE: complicated 'if' in case of distributed...
			cmd = ''
			
			# Case 1: distributed: binary = mpiexec, options = all the rest including IMITATOR binary
			if(benchmark.has_key('nb_nodes') and benchmark['nb_nodes'] > 1):
				# Add the mpiexecc if distributed
				cmd = ['mpiexec'] + ['-n'] + [str(benchmark['nb_nodes'])] + [binary] + cmd_inputs + options_str_list
			# Case 2: non-distributed: binary = IMITATOR, options = all the rest
			else:
				# Prepare the command (using a list form)
				cmd = [binary] + cmd_inputs + options_str_list
			#------------------------------------------------------------
			
			# Print the command
			print_to_screen_and_log(' command: ' + ' '.join(cmd))
			
			# Create dedicated log file
			version_log_file = file(make_log_file(benchmark, version), 'w')
			version_err_file = file(RESULT_FILES_PATH + benchmark['log_prefix'] +  versions[version]['files_suffix'] + ".bencherr", 'w')

			# NOTE: flushing avoids to mix between results of IMITATOR, and text printed by this script
			logfile.flush()
			subprocess.call(cmd, stdout=version_log_file, stderr=version_err_file)
			logfile.flush()
			
			# Retrieve the computation time
			time = get_computation_time(benchmark, version)
			
			# Store result
			results[benchmark['log_prefix']][version] = time
	

			
			# TODO: test whether the termination is ok
		

def print_results(versions_to_test):
	for benchmark_id, result in results.iteritems():
		# Create text line
		line = benchmark_id + ": "
		
		for version in versions_to_test:
			line = line + str(result[version]) + "; "
		
		print_to_screen(line)


#************************************************************
# RUN!
#************************************************************

all_versions = [V_2_5, V_2_6_1, V_2_7_3, V_2_8]

# IMPORTING THE BENCHMARKS CONTENT
import comparator_data
tests = comparator_data.data

print_to_screen('')

for test in tests:
	run(test, all_versions, logfile)

print_results(all_versions)

#************************************************************
# THE END
#************************************************************

print_to_screen_and_log('')
print_to_screen_and_log('...The end of COMPARATOR!')

sys.exit(0)
