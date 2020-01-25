#!/bin/bash

##############################
# Script for BRP experiments #
# Author: Laure Petrucci     #
# Version: 1.0               #
# Date: 04/10/2019           #
##############################

exp_dir="tests/BRP"
# experiments of section 6.1
echo -e '\e[42mExperiments of section 6.1\e[0m'
bin/imitator -mode EF -mergeq -incl $exp_dir/brp_Channels.imi
bin/imitator -mode EF -mergeq -incl $exp_dir/brp_RC.imi
bin/imitator -mode EF -mergeq -incl $exp_dir/brp_RC_MAX3.imi
bin/imitator -mode EF -mergeq -incl $exp_dir/brp_RC_MAX4.imi
bin/imitator -mode EF -mergeq -incl $exp_dir/brp_RC_MAX20.imi
# experiments of section 6.2
echo -e '\e[42mExperiments of section 6.2\e[0m'
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC.imi -depth-limit=20
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC_loop20.imi -depth-limit=40
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_RC_loop40.imi -depth-limit=45
# experiments of section 6.3
echo -e '\e[42mExperiments of section 6.3\e[0m'
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GF_S_in_RC.imi -incl
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GF_S_in_RC2.imi -incl
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFSsub -counterexample $exp_dir/brp_GSinFSnok.imi
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFSsub $exp_dir/brp_GSinFSnok.imi
bin/imitator -mode AccLoopSynthNDFS -explOrder=NDFS $exp_dir/brp_GSinFSdk.imi -incl