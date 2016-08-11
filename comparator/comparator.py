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
# Last modified     : 2016/08/11
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
# Result files prefix
RESULT_FILES_PATH = IMITATOR_PATH + 'comparator/results/'

orig_stdout = sys.stdout


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
OPT_DISTR_SUBDOMAIN		= 1
OPT_INCLUSION			= 2
OPT_MERGING				= 3
OPT_MODE_COVER			= 4
OPT_OUTPUT_PREFIX		= 5
OPT_OUTPUT_RES			= 6
OPT_OUTPUT_TRACE_SET	= 7
OPT_PRP					= 8


UNDEFINED_SYNTAX = -1

# Critical options, i.e., without which the analysis shall not be run; in other words, if a version does not implement an option required by the analysis and belonging to this list, the benchmark is not run
critical_options = [OPT_DISTR_SUBDOMAIN , OPT_MERGING , OPT_PRP]

# Mainly needed for printing and info purpose
option_names = {
	OPT_DISTR_SUBDOMAIN		: 'distr:subdomain',
	OPT_INCLUSION			: 'inclusion',
	OPT_MERGING				: 'merging',
	OPT_MODE_COVER			: 'mode:cover',
	OPT_OUTPUT_PREFIX		: 'output-prefix',
	OPT_OUTPUT_RES			: 'output-result',
	OPT_OUTPUT_TRACE_SET	: 'output-trace-set',
	OPT_PRP					: 'PRP',
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
			OPT_DISTR_SUBDOMAIN		: UNDEFINED_SYNTAX,
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-with-merging',
			OPT_MODE_COVER			: '-mode cover',
			OPT_OUTPUT_PREFIX		: '-log-prefix',
			OPT_OUTPUT_RES			: UNDEFINED_SYNTAX,
			OPT_OUTPUT_TRACE_SET	: '-with-dot',
			OPT_PRP					: UNDEFINED_SYNTAX,
			},
		'files_suffix'			: '_2_5',
	},
	#------------------------------------------------------------
	V_2_6_1 : {
		'version_name'		: '2.6.1',
		'binary'			: 'imitator261',
		'syntax':
			{
			OPT_DISTR_SUBDOMAIN		: UNDEFINED_SYNTAX,
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_MODE_COVER			: '-mode cover',
			OPT_OUTPUT_PREFIX		: '-log-prefix',
			OPT_OUTPUT_RES			: UNDEFINED_SYNTAX,
			OPT_OUTPUT_TRACE_SET	: '-with-dot',
			OPT_PRP					: UNDEFINED_SYNTAX,
			},
		'files_suffix'			: '_2_6_1',
	},
	#------------------------------------------------------------
	V_2_7_3 : {
		'version_name'		: '2.7.3',
		'binary'			: 'imitator273',
		'binary_dist'		: 'patator273',
		'syntax':
			{
			OPT_DISTR_SUBDOMAIN		: '-distributed dynamic',
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_MODE_COVER			: '-mode cover',
			OPT_OUTPUT_PREFIX		: '-output-prefix',
			OPT_OUTPUT_RES			: '-output-result',
			OPT_OUTPUT_TRACE_SET	: '-output-trace-set',
			OPT_PRP					: '-PRP',
			},
		'files_suffix'			: '_2_7_3',
	},
	#------------------------------------------------------------
	V_2_8 : {
		'version_name'		: '2.8',
		'binary'			: 'imitator',
		'binary_dist'		: 'patator',
		'syntax':
			{
			OPT_DISTR_SUBDOMAIN		: '-distributed dynamic',
			OPT_INCLUSION			: '-incl',
			OPT_MERGING				: '-merge',
			OPT_MODE_COVER			: '-mode cover',
			OPT_OUTPUT_PREFIX		: '-output-prefix',
			OPT_OUTPUT_RES			: '-output-result',
			OPT_OUTPUT_TRACE_SET	: '-output-trace-set',
			OPT_PRP					: '-PRP',
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
	print_to_screen('Fatal error!')
	print_to_screen(text)
	sys.exit(1)

def print_warning(text) :
	print_to_screen(' *** Warning: ' + text)

def print_error(text) :
	print_to_screen(' *** Error: ' + text)

def print_to_screen(content):
	# Print
	print content


# Function to retrieve the computation time depending on the benchmark
def get_computation_time(benchmark, version, cartography_mode):
	
	# NOTE: special case for v2.7.3 for BC as the .res file already contains the stats line (unfortunately not for other modes)
	if cartography_mode and version == V_2_7_3:
		# TODO: check if files exist
		# Open res file
		# NOTE: remark the "_cart" suffix…
		res_file = RESULT_FILES_PATH + benchmark['log_prefix'] +  versions[version]['files_suffix'] + "_cart.res"
		
		# "Stats    :  4 6 2 0 3321 8.68716406823 3"
		pattern = re.compile("Stats    :  \d+ \d+ \d+ \d+ \d+ (\d*\.\d*) \d+")

		for i, line in enumerate(open(res_file)):
			for match in re.finditer(pattern, line):
				return match.groups()[0]
		
		print_error("Time not found for benchmark " + benchmark['name'] + " (cartography mode) with version " + versions[version]['version_name'])
		return ANALYSIS_FAILED
	
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
		pattern = re.compile("Total computation time\s*:\s*(\d*\.\d*) second")

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

def run(benchmark, versions_to_test):
	
	# Print something
	print_to_screen('')
	print_to_screen('############################################################')
	print_to_screen(' BENCHMARK ' + benchmark['name'])
	
	# Create the row in the results array
	results[benchmark['log_prefix']] = {}
	
	# Check if distributed
	distributed = (benchmark.has_key('nb_nodes') and benchmark['nb_nodes'] > 1)
	
	
	# Prepare command
	for version in versions_to_test:
		
		# If a critical option is not defined for this version or the binary is not defined, do not run
		to_run = True
		
		
		# Create the binary
		
		# Case non-distributed:
		binary = make_binary(versions[version]['binary'])
		# Case distributed:
		if distributed:
			# First check that the distributed binary exists:
			if versions[version].has_key('binary_dist'):
				binary = make_binary(versions[version]['binary_dist'])
			else:
				print_warning('Distributed binary not defined for version ' + versions[version]['version_name'] + '!')
				to_run = False
		
		
		# Check that all options are defined
		
		for option in benchmark['options']:
			if option in critical_options and versions[version]['syntax'][option] == UNDEFINED_SYNTAX:
				print_warning('Option ' + option_names[option] + ' not defined for version ' + versions[version]['version_name'] + '!')
				to_run = False
		
		
		# Stop if should not run this version
		
		if not to_run:
			print_warning('Skip version ' + versions[version]['version_name'] + '')
			# Store result
			results[benchmark['log_prefix']][version] = ANALYSIS_FAILED
		else:
			
			
			# Create the options
			options_str_list = []
			for option in benchmark['options']:
				# Only add option if syntax defined for this version
				if versions[version]['syntax'][option] != UNDEFINED_SYNTAX:
					# Add the option with the correct syntax
					options_str_list.extend((versions[version]['syntax'][option]).split())
			# Add the option to redirect log files to the dedicated dir
			options_str_list.extend([versions[version]['syntax'][OPT_OUTPUT_PREFIX] , RESULT_FILES_PATH + benchmark['log_prefix'] + versions[version]['files_suffix'] ])
			
			# Find input files (that might have been redefined for this specific version)
			input_files = benchmark['input_files']
			# Input files refined using key 'input_files_v' and version
			if 'input_files_v' in benchmark.keys() and version in benchmark['input_files_v'].keys():
				input_files = benchmark['input_files_v'][version]
			
			# Add the path to all input files
			cmd_inputs = []
			for each_file in input_files:
				cmd_inputs += [make_file(each_file)]
				# TODO: test for existence of files (just in case)
			
			#------------------------------------------------------------
			# NOTE: complicated 'if' in case of distributed...
			cmd = ''
			
			# Case 1: distributed: binary = mpiexec, options = all the rest including IMITATOR binary
			if distributed:
				# Add the mpiexecc if distributed
				cmd = ['mpiexec'] + ['-n'] + [str(benchmark['nb_nodes'])] + [binary] + cmd_inputs + options_str_list
			# Case 2: non-distributed: binary = IMITATOR, options = all the rest
			else:
				# Prepare the command (using a list form)
				cmd = [binary] + cmd_inputs + options_str_list
			#------------------------------------------------------------
			
			# Print the command
			print_to_screen(' command: ' + ' '.join(cmd))
			
			# Create dedicated log file
			version_log_file = file(make_log_file(benchmark, version), 'w')
			version_err_file = file(RESULT_FILES_PATH + benchmark['log_prefix'] +  versions[version]['files_suffix'] + ".bencherr", 'w')

			# NOTE: flushing avoids to mix between results of IMITATOR, and text printed by this script
			#stdout.flush()
			subprocess.call(cmd, stdout=version_log_file, stderr=version_err_file)
			#stdout.flush()
			
			# Retrieve the computation time
			# HACK: need to take the cartography mode into consideration (for v.2.7.3 at least)
			cartography_mode = False
			if OPT_MODE_COVER in benchmark['options']:
				cartography_mode = True
			time = get_computation_time(benchmark, version, cartography_mode)
			
			# Store result
			results[benchmark['log_prefix']][version] = time
	

			
			# TODO: test whether the termination is ok
		

def print_results(versions_to_test):
	# Print something
	print_to_screen('')
	print_to_screen('############################################################')
	print_to_screen(' RESULTS')

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
	run(test, all_versions)

print_results(all_versions)

#************************************************************
# THE END
#************************************************************

print_to_screen('')
print_to_screen('...The end of COMPARATOR!')

sys.exit(0)
