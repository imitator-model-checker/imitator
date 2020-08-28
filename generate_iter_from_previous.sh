#!/bin/bash

###########################################
# Script for generating experiments table #
# for iterative deepening with initial    #
# depth from previous experiments         #
# Author: Laure Petrucci                  #
# Version: 1.0                            #
# Date: 2020-08-28                        #
###########################################

function usage {
	echo -e "\033[1;31musage\033[0m: $0 [-h | [-t timeout] -o table_filename [-S | -i input_models]]"
}

function help {
	echo -e ""
	usage
	echo -e "\nExecutes the experiments on all models. The result is written in the file specified with the \033[1m-o\033[0m option"
	echo -e "\n\033[1m-h\033[0m\t\t\tThis help"
	echo -e "\n\033[1m-t timeout\033[0m\t\tUses a specified value for the timeout (in seconds) \033[4m[default: 90]\033[0m"
	echo -e "\n\033[1m-o table_filename\033[0m\tOutputs the results in a csv file (with separator ;) named \033[4mtable_filename\033[0m"
	echo -e "\n\033[1m-S\033[0m\t\t\tUses a subset of the models"
	exit
}

function process_results {
	# find maximal depth
	# echo -n `grep "until depth" $one_result | tail -n 1 | cut -d' ' -f5 `' ; '>> $output_file
	echo -n `grep "depth actually" $one_result | tail -n 1 | cut -d: -f2 | sed -e 's/\[0m//'`' ;'>> $output_file
	# minimal depth at which a cycle was found
	min_depth=`grep "Minimum depth" $one_result | tail -n 1 | cut -d: -f2 | sed -e 's/\[0m//'`
	if [ -z "$min_depth" ]
	then echo -n " --- ; " >> $output_file
	else echo -n "$min_depth ; " >> $output_file
	fi
	# total number of cycles found
	echo -n `grep "cycles found" $one_result | grep Total | tail -n 1 | cut -d: -f2 | sed -e 's/\[0m//'`' ; '>> $output_file
	# total number of processed states
	echo -n `grep "processed states" $one_result | grep Total | tail -n 1 | cut -d: -f2 | sed -e 's/\[0m//'`' ; ' >> $output_file
	# time of computation
	in_time=`grep "exact" $one_result`
	if [ -z "$in_time" ]
	then echo -n "TO" >> $output_file
	else echo -n `grep "completed after" $one_result | tail -n 1 | sed -e 's/\[NDFS\] State space exploration completed after //' \
		| sed -e 's/ seconds.//' | sed -e 's/ second.//'  | sed -e 's/\[0m//'` >> $output_file
	fi
}

# main part of the script

# get the options
timeout=90 # 1.5 minute by default
output_file=
depth_step=5

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
input_depths="12 4 \
		3 5 0 0 32 3 5 13 \
		13 1 \
		3 5 \
		32 44 59 \
		8 2 \
		2 \
		6 \
		6 13 \
		25 13 \
		3"

while getopts "ht:o:S" opt; do
case $opt in
	h) help ;;
	t) timeout=$OPTARG ;;
	o) output_file=$OPTARG ;;
	S) input_files="critical-region critical-region4 F3 F4 FDDI4 FischerAHV93 flipflop fmtv1A1-v2 \
		lynch lynch5 \
		Pipeline-KP12-2-3 \
		RCP Sched2.100.0 \
		Sched2.50.0 \
		spsmall tgcTogether2"
	   input_depths="3 5 0 0 32 3 5 13 \
		3 5 \
		32 44 59 \
		8 2 \
		6 \
		25 13" ;;
esac
done

if [ -z "$output_file" ]
then usage
	exit
fi

extension=".${output_file##*.}"
one_result="`basename $2 $extension`.tmp"
rm -f $one_result
rm -f $output_file
# convert input to arrays
files=( $input_files )
depths=( $input_depths )
input_len=${#files[@]}
echo "$input_len files"
# table with iterative depth from previous experiments
echo ' ; ; ; ; No layers ; ; ; ; ; ; Layers ; ; ; ; ;' >> $output_file
echo 'Model ; L ; X ; P ; i ; d ; m ; c ; s ; t ; i ; d ; m ; c ; s ; t' >> $output_file
for ((i=0;i<input_len;i++))
	do echo -e "Running experiments for model \033[1;31m${files[$i]}\033[0m from depth \033[1;31m${depths[$i]}\033[0m"
		bin/imitator -mode checksyntax -verbose low $exp_dir/${files[$i]}.imi > $one_result 2> /dev/null
		# first column = benchmark file name
		echo -n "${files[$i]} ; " >> $output_file
		# columns 2 to 4 = L, X, P
		echo -n `grep -e locations $one_result | cut -d, -f2,5,7 | sed -e 's/^ //' \
			| sed -e 's/ locations, / \; /' | sed -e 's/ clock variables, / \; /' \
			| sed -e 's/ parameters/ \; /' | sed -e 's/ parameter/ \; /' ` >> $output_file
	# no layers
		echo -e "\twithout layers"
		echo -n " ${depths[$i]} ; " >> $output_file
		bin/imitator -mode AccLoopSynthNDFS -explOrder NDFSsub -time-limit $timeout -depth-init ${depths[$i]} -step $depth_step $exp_dir/${files[$i]}.imi > $one_result 2> /dev/null
		process_results
		echo -n ' ;' >> $output_file
	# layers
		echo -e "\twith layers"
		echo -n " ${depths[$i]} ; " >> $output_file
		bin/imitator -mode AccLoopSynthNDFS -explOrder layerNDFSsub -time-limit $timeout -depth-init ${depths[$i]} -step $depth_step $exp_dir/${files[$i]}.imi > $one_result 2> /dev/null
		process_results
		echo '' >> $output_file
	done
rm -f $one_result