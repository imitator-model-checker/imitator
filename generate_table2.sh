#!/bin/bash

###########################################
# Script for generating experiments table #
# Author: Laure Petrucci                  #
# Version: 1.0                            #
# Date: 04/10/2019                        #
###########################################

function usage {
	if [ $1 -ne 2 ] 
	then echo -e "\033[1;31musage\033[0m: $0 timeout table_filename"
		exit
	fi
}

function process_results {
	echo -n `grep "cycles found" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`' & '>> $output_file
	echo -n `grep "processed states" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`' & ' >> $output_file
	echo -n `grep "completed after" $one_result | sed -e 's/\[NDFS\] State space exploration completed after //' \
		| sed -e 's/ seconds.//' | sed -e 's/ second.//'  | sed -e 's/\[0m//'` >> $output_file	
}

# main part of the script

usage $#

timeout=$1
exp_dir="tests/acceptingExamples"
one_result="tmp_result.txt"
output_file=$2
rm -f $output_file
echo '\begin{table}[pht]' > $output_file
echo '\centering' >> $output_file
# first table
echo '\begin{tabular}{||l||c|c|c||*{6}{r|r|r||}}' >> $output_file
echo '\hline\hline' >> $output_file
echo '\rowcolor{cyan} & & & & \multicolumn{3}{c||}{No strategy} & \multicolumn{3}{c||}{Subsumption} & \multicolumn{3}{c||}{Ordering} & \multicolumn{3}{c||}{Lookahead} & \multicolumn{3}{c||}{Order+Look} & \multicolumn{3}{c||}{O + L + Sub} \\' >> $output_file
echo '\rowcolor{cyan}\multirow{-2}{*}{Model} & \multirow{-2}{*}{L} & \multirow{-2}{*}{X} & \multirow{-2}{*}{P} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} & \multicolumn{1}{c|}{c} & \multicolumn{1}{c|}{s} & \multicolumn{1}{c||}{t} \\' >> $output_file
echo '\hline\hline' >> $output_file
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
		# first column = benchmark file name
		echo -n "$f & " >> $output_file
		# columns 2 to 4 = L, X, P
		echo -n `grep -e locations $one_result | cut -d, -f2,5,7 | sed -e 's/^ //' \
			| sed -e 's/ locations, / \& /' | sed -e 's/ clock variables, / \& /' \
			| sed -e 's/ parameters/ \&/' | sed -e 's/ parameter/ \& /' ` >> $output_file
# first table
	# no strategy
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFS -no-lookahead -no-acceptfirst -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo ' & ' >> $output_file
	# subsumption
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFSsub -no-lookahead -no-acceptfirst -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo ' & ' >> $output_file
	# ordering
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFS -no-lookahead -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo ' & ' >> $output_file
	# lookahead
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFS -no-acceptfirst -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo ' & ' >> $output_file
	# lookahead + ordering
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFS -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo ' & ' >> $output_file
	# lookahead + ordering + subsumption
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFSsub -time-limit $timeout $exp_dir/$f.imi > $one_result
		process_results
		echo '\\' >> $output_file
	done
echo '\hline\hline' >> $output_file
echo '\end{tabular}' >> $output_file
echo '\caption{Comparing strategies for collecting NDFS without layers}' >> $output_file
echo '\label{tbl:exp:nolayer}' >> $output_file
echo '\end{table}' >> $output_file
rm -f $one_result