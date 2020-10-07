#!/bin/bash

################################################
# Script for generating experiments table      #
# with strategies							   #
# Author: Laure Petrucci                       #
# Version: 3.1                                 #
# Date: 2020-10-07                             #
################################################

function usage {
	echo -e "\033[1;31musage\033[0m: $0 [-h | [-g] [-t timeout] -o table_filename [-S | -i input_models]]"
}

function help {
	echo -e ""
	usage
	echo -e "\nExecutes the experiments on all models. The result is written in the file specified with the \033[1m-o\033[0m option"
	echo -e "\n\033[1m-h\033[0m\t\t\tThis help"
	echo -e "\n\033[1m-g\033[0m\t\t\tUse the green colour"
	echo -e "\n\033[1m-t timeout\033[0m\t\tUses a specified value for the timeout (in seconds) \033[4m[default: 90]\033[0m"
	echo -e "\n\033[1m-o table_filename\033[0m\tOutputs the results in a csv file (with separator ;) named \033[4mtable_filename\033[0m"
	echo -e "\n\033[1m-S\033[0m\t\t\tUses a subset of the models"
	echo -e "\n\033[1m-i input_models\033[0m\t\tOnly the models in \033[4minput_models\033[0m are used"
	exit
}

function process_results {
	# find maximal depth
	echo -n `grep "depth actually" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`' ;'>> $output_file
	# minimal depth at which a cycle was found
	min_depth=`grep "Minimum depth" $one_result | cut -d: -f2 | sed -e 's/\[0m//'`
	if [ -z "$min_depth" ]
	then echo -n " --- ; " >> $output_file
	else echo -n "$min_depth ; " >> $output_file
	fi
	echo -n `grep "cycles found" $one_result | grep -v Total | cut -d: -f2 | sed -e 's/\[0m//'`' ; '>> $output_file
	echo -n `grep "processed states" $one_result | grep -v Total | cut -d: -f2 | sed -e 's/\[0m//'`' ; ' >> $output_file
	exceeds_time=`grep "under-approximation" $one_result`
	if [ -z "$exceeds_time" ]
	then echo -n `grep "completed after" $one_result | sed -e 's/\[Cycle (NDFS)\] State space exploration completed after //' \
		| sed -e 's/ seconds.//' | sed -e 's/ second.//'  | sed -e 's/\[0m//'` >> $output_file
	else echo -n "TO" >> $output_file
	fi
}

# main part of the script

# get the options
green="-no-green" # no green colour by default
timeout=120 # 2 minutes by default
output_file=
exp_dir="tests/acceptingExamples"
input_files="BRP coffee \
		critical-region critical-region4 F3 F4 FDDI4 FischerAHV93 flipflop fmtv1A1-v2 \
		fmtv1A3-v2 JLR-TACAS13 \
		lynch lynch5 \
		Pipeline-KP12-2-3 Pipeline-KP12-2-5 Pipeline-KP12-3-3 \
		RCP Sched2.100.0 \
		Sched2.100.2 \
		Sched2.50.0 \
		Sched2.50.2 simop \
		spsmall tgcTogether2 \
		WFAS-BBLS15-det"
while getopts "ght:o:Si:" opt; do
case $opt in
	g) green="" ;;
	h) help ;;
	t) timeout=$OPTARG ;;
	o) output_file=$OPTARG ;;
	S) input_files="critical-region critical-region4 F3 F4 FDDI4 FischerAHV93 flipflop fmtv1A1-v2 \
		lynch lynch5 \
		Pipeline-KP12-2-3 \
		RCP Sched2.100.0 \
		Sched2.50.0 \
		spsmall tgcTogether2" ;;
	i) input_files=$OPTARG ;;
esac
done

if [ -z "$output_file" ]
then usage
	exit
fi

extension=".${output_file##*.}"
one_result="`basename $output_file $extension`.tmp"
rm -f $one_result
rm -f $output_file
# print the command line that was used
echo $0 $* > $output_file
# table
echo ' ; ; ; ; Subsumption ; ; ; ; ; Lookahead ; ; ; ; ; Lookahead + Acceptfirst ; ; ; ; ; S + L + A ; ; ; ; ; S + L + A + layers; ; ; ;' >> $output_file
echo 'Model ; L ; X ; P ; d ; m ; c ; s ; t ; d ; m ; c ; s ; t ; d ; m ; c ; s ; t ; d ; m ; c ; s ; t ; d ; m ; c ; s ; t ; d ; m ; c ; s ; t' >> $output_file
for f in $input_files
do echo -e "Running experiments for model \033[1;31m$f\033[0m"
	bin/imitator -mode checksyntax -verbose low $exp_dir/$f.imi > $one_result 2> /dev/null
	# first column = benchmark file name
	echo -n "$f ; " >> $output_file
	# columns 2 to 4 = L, X, P
	echo -n `grep -e locations $one_result | cut -d, -f2,5,7 | sed -e 's/^ //' \
		| sed -e 's/ locations, / \; /' | sed -e 's/ clock variables, / \; /' \
		| sed -e 's/ parameters/ \; /' | sed -e 's/ parameter/ \; /' ` >> $output_file
# subsumption
	echo -e "\tsubsumption only"
	bin/imitator $green -no-lookahead -no-acceptfirst -time-limit $timeout $exp_dir/$f.imi $exp_dir/accepting.imiprop > $one_result 2> /dev/null
	process_results
	echo -n ' ; ' >> $output_file
# lookahead
	echo -e "\tlookahead only"
	bin/imitator $green -no-acceptfirst -no-subsumption -time-limit $timeout $exp_dir/$f.imi $exp_dir/accepting.imiprop > $one_result 2> /dev/null
	process_results
	echo -n ' ; ' >> $output_file
# lookahead + accepfirst
	echo -e "\tlookahead and acceptfirst"
	bin/imitator $green -no-subsumption -time-limit $timeout $exp_dir/$f.imi $exp_dir/accepting.imiprop > $one_result 2> /dev/null
	process_results
	echo -n ' ; ' >> $output_file
# subsumption + lookahead + accepfirst
	echo -e "\tsubsumption, lookahead and acceptfirst"
	bin/imitator $green -time-limit $timeout $exp_dir/$f.imi $exp_dir/accepting.imiprop > $one_result 2> /dev/null
	process_results
	echo -n ' ; ' >> $output_file
# subsumption + lookahead + accepfirst + layers
	echo -e "\tsubsumption, lookahead, acceptfirst and layers"
	bin/imitator -layer $green -time-limit $timeout $exp_dir/$f.imi $exp_dir/accepting.imiprop > $one_result 2> /dev/null
	process_results
	echo '' >> $output_file
done
rm -f $one_result