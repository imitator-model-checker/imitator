#!/usr/bin/python
# coding=utf8


#import string


def write_bc(station, bc, last):
	bcmax = pow (2 , (bc+1))
	print "\n(* Considering case with bc = " + str(bc) + ", hence from 0 to " + str(bcmax - 1) + " (i.e., 2^(bc+1) - 1) *)"
	
	print "loc Collide" + str(station) + "_" + str(bc) + ": while x" + str(station) + " <= 0 wait {}"

	for x in range(0, bcmax):
		print "\twhen True sync prob" + str(station) + " goto Wait" + str(station) + "_" + str(bc) + "_" + str(x) + ";"
		
	next_bc = bc + 1
	if last:
		next_bc = bc
	next_loc = "Collide" + str(station) + "_" + str(next_bc)
	
	for x in range(0, bcmax):
		print "\nloc Wait" + str(station) + "_" + str(bc) + "_" + str(x) + ": while x" + str(station) + " <= " + str(x) + " timeslot wait {}"
		print "\twhen x" + str(station) + " = " + str(x) + " timeslot sync busy" + str(station) + " do {x" + str(station) + "' = 0} goto " + next_loc + ";"
		print "\twhen x" + str(station) + " = " + str(x) + " timeslot sync send" + str(station) + " do {x" + str(station) + "' = 0} goto Transmit" + str(station) + ";"


def write_program_for_station(station, bcmax):
	print "\n\n\n(* STARTING AUTOMATED PROGRAM FOR CSMA/CD FOR STATION " + str(station) + " WITH BCMAX = " + str(bcmax) + " *)"
	# All cases but the last one
	for bc in range(1, bcmax):
		write_bc(station, bc, False)
	# Last case
	write_bc(station, bcmax, True)
	print "\n(* END OF AUTOMATED PROGRAM FOR CSMA/CD *)\n"


def write_program(bcmax):
	write_program_for_station(1, bcmax)
	write_program_for_station(2, bcmax)

write_program(10)
