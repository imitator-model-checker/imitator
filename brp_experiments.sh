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

execute_imi "-mode EF -mergeq -incl $exp_dir/brp_Channels.imi"
execute_imi "-mode EF -mergeq -incl $exp_dir/brp_RC.imi"
execute_imi "-mode EF -mergeq -incl $exp_dir/brp_RC_MAX3.imi"
execute_imi "-mode EF -mergeq -incl $exp_dir/brp_RC_MAX4.imi"
execute_imi "-mode EF -mergeq -incl $exp_dir/brp_RC_MAX20.imi"

# experiments of section 6.2
echo -e '\e[42m                              \e[0m'
echo -e '\e[42m  Experiments of section 6.2  \e[0m'
echo -e '\e[42m                              \e[0m'
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC.imi -depth-limit=20"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC_loop20.imi -depth-limit=40"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC_loop40.imi -depth-limit=45"

# experiments of section 6.3
echo -e '\e[42m                              \e[0m'
echo -e '\e[42m  Experiments of section 6.3  \e[0m'
echo -e '\e[42m                              \e[0m'
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GF_S_in_RC.imi -incl"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GF_S_in_RC2.imi -incl"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFSsub -counterexample $exp_dir/brp_GSinFSnok.imi"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_GSinFSnok.imi"
execute_imi "-mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GSinFSdk.imi -incl"