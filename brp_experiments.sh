#!/bin/bash

##############################
# Script for BRP experiments #
# Author: Laure Petrucci     #
# Version: 1.1               #
# Date: 2020-01-25           #
##############################

function execute_imi {
	len="$((${#1} + 16))"
	echo -n -e "\e[103m" ; printf " %${len}s" ; echo -e "\e[0m"
	echo -e "\e[103m  bin/imitator $1  \e[0m"
	echo -n -e "\e[103m" ; printf " %${len}s" ; echo -e "\e[0m"
	bin/imitator $1
}

exp_dir="tests/BRP"

# experiments of section 6.1
echo -e '\e[42m                              \e[0m'
echo -e '\e[42m  Experiments of section 6.1  \e[0m'
echo -e '\e[42m                              \e[0m'

execute_imi "-mergeq -comparison inclusion $exp_dir/brp_Channels.imi $exp_dir/brp_Channels.imiprop"
execute_imi "-mergeq -comparison inclusion $exp_dir/brp_RC.imi $exp_dir/brp_RC.imiprop"
execute_imi "-mergeq -comparison inclusion $exp_dir/brp_RC_MAX3.imi $exp_dir/brp_RC.imiprop"
execute_imi "-mergeq -comparison inclusion $exp_dir/brp_RC_MAX4.imi $exp_dir/brp_RC.imiprop"
# LP: commented out the next line that takes too long
# execute_imi "-mergeq -comparison inclusion $exp_dir/brp_RC_MAX20.imi $exp_dir/brp_RC.imiprop"

# experiments of section 6.2
echo -e '\e[42m                              \e[0m'
echo -e '\e[42m  Experiments of section 6.2  \e[0m'
echo -e '\e[42m                              \e[0m'
execute_imi "$exp_dir/brp_RC.imi $exp_dir/accepting.imiprop -depth-limit=20"
execute_imi "$exp_dir/brp_RC_R1.imi $exp_dir/accepting.imiprop -depth-init=10 -depth-step=5"
#LP: strange results for the next two lines
# execute_imi "$exp_dir/brp_RC_loop20.imi $exp_dir/accepting.imiprop -depth-limit=40"
# execute_imi "$exp_dir/brp_RC_loop40.imi $exp_dir/accepting.imiprop -depth-limit=45"

# experiments of section 6.3
echo -e '\e[42m                              \e[0m'
echo -e '\e[42m  Experiments of section 6.3  \e[0m'
echo -e '\e[42m                              \e[0m'
execute_imi "-no-subsumption -comparison inclusion $exp_dir/brp_GF_S_in_RC.imi $exp_dir/accepting.imiprop"
execute_imi "-no-subsumption -comparison inclusion $exp_dir/brp_GF_S_in_RC2.imi $exp_dir/accepting.imiprop"
execute_imi "$exp_dir/brp_GSinFSnok.imi $exp_dir/accepting_one.imiprop"
execute_imi "$exp_dir/brp_GSinFSnok.imi $exp_dir/accepting.imiprop"
execute_imi "-no-subsumption -comparison inclusion $exp_dir/brp_GSinFSdk.imi $exp_dir/accepting.imiprop"
