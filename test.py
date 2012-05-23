#!/usr/bin/python
#************************************************************
#
#                      IMITATOR II
#
#                     Testing script
#
# Etienne ANDRE
# Laboratoire d'Informatique de Paris Nord
# Universite Paris XIII, France
#
#************************************************************


#************************************************************
# MODULES
#************************************************************
import time
import datetime

#************************************************************
# GENERAL CONFIGURATION
#************************************************************

# Root for the example directory
EXAMPLE_PATH = 'examples/'
# Root for the binary directory
BINARY_PATH = 'bin/'
# Root name for the binaries
BINARY_NAME = 'IMITATOR'

# Options common for all case studies
common_options = '-no-dot -no-log'

# Options for each version
options_per_version = {}
options_per_version['2.40'] = '-merge'

# Binaries to execute
binaries = ['2.40', '2.42']


#************************************************************
# LIST OF EXAMPLES
#************************************************************
examples = []
examples.append(EXAMPLE_PATH + 'Train/Train1PTA.imi ' + EXAMPLE_PATH + 'Train/Train1PTA.pi0')


#************************************************************
# STARTING SCRIPT
#************************************************************

#print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
print '############################################################'
print ' IMITATOR TESTER'
print ' v0.1'
print ' LIPN, Universite Paris XIII'
print '############################################################'
print 'Starting testing...'
now = datetime.datetime.now()
print now.strftime("%A %d. %B %Y %H:%M:%S %z")

for example in examples:
	print ''
	print '************************************************************'
	print ' EXAMPLE ' + example
	print ''
	for binary in binaries:
		print '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
		print ' Version ' + binary
		
		# Handle options
		options = common_options
		if binary in options_per_version:
			options += ' ' + options_per_version[binary]
		
		cmd = BINARY_PATH + BINARY_NAME + binary + ' ' + example + ' ' + options
		print ' >> ' + cmd
		
		#cmd = 
		#commands.getoutput(cmd)




print '...The end!'
