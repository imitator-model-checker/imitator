#!/usr/bin/python
#************************************************************
#
#                       IMITATOR
#
#               Parse result of IMITATOR for the FMTV challenge
#
# Etienne Andre
#
# Laboratoire d'Informatique de Paris Nord
# Universite Paris 13, Sorbonne Paris Cite, France
#
# Created      : 2015/04/21
# Last modified: 2015/10/30
#************************************************************


#************************************************************
# PACKAGES
#************************************************************
import re
import StringIO
import sys

#************************************************************
# CONSTANTS
#************************************************************
#file_name = 'fmtv1.res'
#file_name = 'fmtv1-b.res'
#file_name = 'fmtv1A1.res'
#var_name = 'e2e'
#var_name = 'D'

# HACK
INFINITY = 99999999



#************************************************************
# GLOBAL VARIABLES
#************************************************************

# Option to write all intervals in approx 
WRITE_INTERVALS = True
#WRITE_INTERVALS = False

file_name = ''
var_name = ''

global_max = 0
global_max_frac = ''
global_min = INFINITY
global_min_frac = ''


#************************************************************
# FUNCTIONS
#************************************************************

#names = ('15673>125*e2e', '375*e2e>=38371', '6003>=50*e2e', '125*e2e>=12541', 'ploplplop')

def update_min_max(ineq_str):
	global global_max, global_max_frac, global_min, global_min_frac
	
	#print "Handling " + ineq_str + "..."
	
	# Compute min/max for this interval
	int_min = INFINITY
	int_max = 0
	
	for ineq_str in pair_str:
		
		# Case MAX : n >= m * e2e
		m = re.match("((?P<num>\d+)((?P<op>>|>=))((?P<denum>\d+)\*)?" + var_name + ")", ineq_str)
		if m:
			#print "\nGroups ="
			#print m.groups()
			new_max_num = m.group('num')
			new_max_op = m.group('op')
			# HACK
			if(m.group('denum') <> None):
				new_max_denum = m.group('denum')
			else:
				new_max_denum = '1'
			
			new_max = float(new_max_num) / float(new_max_denum)
			int_max = new_max
			
			if new_max > global_max:
				print "New max found : " + str(new_max)
				# Update
				global_max = new_max
				global_max_frac = new_max_num + '/' + new_max_denum + new_max_op + var_name
		else:
			# Case MIN : m * e2e >= n
			m = re.match("(((?P<denum>\d+)\*)?" + var_name + "((?P<op>>|>=))(?P<num>\d+))", ineq_str)
			if m:
				new_min_num = m.group('num')
				new_min_op = m.group('op')
				# HACK
				if(m.group('denum') <> None):
					new_min_denum = m.group('denum')
				else:
					new_min_denum = '1'
				
				new_min = float(new_min_num) / float(new_min_denum)
				int_min = new_min
		
				if new_min < global_min:
					print "New min found : " + str(new_min)
					# Update
					global_min = new_min
					global_min_frac = var_name + new_min_op + new_min_num + '/' + new_min_denum
			else:
				print "*** ERROR: cannot parse " + ineq_str
	
	# end foreach
	
	if WRITE_INTERVALS:
		if int_min > 0 and int_max < INFINITY:
			print "Interval [" + str(int_min) + "," + str(int_max) + "]"
		else:
			print "*** Badly formatted interval: '" + ineq_str + "'"


#************************************************************
# CHECK ARGUMENTS
#************************************************************
if len(sys.argv) <> 3:
	print " *** ERROR! Expected two arguments for the file name."
	exit(1)
else:
	file_name = sys.argv[1]
	var_name = sys.argv[2]

#************************************************************
# OPEN FILE
#************************************************************
print "Starting result interpreter"

file_content = open(file_name, 'r').read()


#************************************************************
# REMOVE THE HEADER
#************************************************************
print "Removing header..."
# NOTE: UGLY!
file_parts = file_content.split('*)\n\n')
result = file_parts[1]

print "Removing white spaces..."
result = re.sub('\s+', '', result)



#************************************************************
# GET THE POSSIBLE INTERVALS
#************************************************************
print "Splitting intervals..."
intervals_str = result.split('OR')
#print intervals_str

intervals = []

print "Parsing inequalities..."
for interval_str in intervals_str:
	pair_str = interval_str.split('&')
	update_min_max(pair_str)


#************************************************************
# THE END
#************************************************************

print "\nFinished!"
print "File " + file_name
print "\n" + var_name + " in [" + str(global_min) + "," + str(global_max) + "]"
print ""
print global_max_frac
print global_min_frac
