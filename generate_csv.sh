#!/bin/bash

###########################################
# Script for generating experiments table #
# Author: Laure Petrucci                  #
# Version: 1.0                            #
# Date: 13/0ç/2019                        #
###########################################

function usage {
	if [ $1 -ne 2 ] 
	then echo -e "\033[1;31musage\033[0m: $0 results_filename table_filename"
		exit
	fi
}

# main part of the script

usage $#
exp_dir="tests/acceptingExamples"
log_file=$1
one_result="tmp_result.txt"
rm -f $log_file
output_file=$2
rm -f $output_file
echo ";;;;No layers;;;With layers;;;" > $output_file
echo "Model;L;X;P;c;s;t;c;s;t" >> $output_file
for f in BRP coffee \
		critical-region critical-region4 F3 F4 FDDI4 FischerAHV93 flipflop fmtv1A1-v2 \
		fmtv1A3-v2 JLR-TACAS13 \
		lynch lynch5 \
		Pipeline-KP12-2-3 Pipeline-KP12-2-5 Pipeline-KP12-3-3 \
		RCP Sched2.100.0 \
		Sched2.100.2 \
		Sched2.50.0 \
		Sched2.50.2 simop \
		spsmall tgcTogether2 \
		WFAS-BBLS15-det
	do bin/imitator -mode checksyntax -verbose low $exp_dir/$f.imi > $one_result
		# copy the output to the results file
		cat $one_result >> $log_file
		# first column = benchmark file name
		echo -n "$f;" >> $output_file
		# columns 2 to 4 = L, X, P
		echo -n `grep -e locations $one_result | cut -d, -f2,5,7 | sed -e 's/^ //' \
			| sed -e 's/ locations, /\;/' | sed -e 's/ clock variables, /\;/' \
			| sed -e 's/ parameters/\;/' | sed -e 's/ parameter/\;/' ` >> $output_file
		# synthesis without layers
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFSsub -time-limit 1800 $exp_dir/$f.imi > $one_result
		cat $one_result >> $log_file
		# columns 5 to 7 = cycles, states, time (without layers)
		echo -n `grep "cycles found" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`";">> $output_file
		echo -n `grep "processed states" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`";" >> $output_file
		echo -n `grep "completed after" $one_result | sed -e 's/\[NDFS\] State space exploration completed after //' \
			| sed -e 's/ seconds.//' | sed -e 's/ second.//'  | sed -e 's/\[0m/;/'` >> $output_file
		# synthesis with layers
		bin/imitator -mode AccLoopSynthNDFS -explOrder layerNDFSsub -time-limit 1800 $exp_dir/$f.imi > $one_result
		cat $one_result >> $log_file
		# columns 8 to 10 = cycles, states, time (with layers)
		echo -n `grep "cycles found" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`";" >> $output_file
		echo -n `grep "processed states" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`";" >> $output_file
		echo `grep "completed after" $one_result | sed -e 's/\[NDFS\] State space exploration completed after //' \
			| sed -e 's/ seconds.//' | sed -e 's/ second.//' | sed -e 's/\[0m/;/' ` >> $output_file
	done
	rm -f $one_result