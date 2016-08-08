#!/usr/bin/python
# -*- coding: utf-8 -*-
#************************************************************
#
#                       IMITATOR
# 
# LIPN, Université Paris 13, Sorbonne Paris Cité (France)
# 
# Script description: Data for COMPARATOR
# 
# File contributors : Étienne André
# Created           : 2016/08/08
# Last modified     : 2016/08/08
#************************************************************


#************************************************************
# THE BENCHMARKS TO BE COMPARED
#************************************************************

#------------------------------------------------------------
# Options
#------------------------------------------------------------
# HACK: copied these constants from comparator.py
# NOTE: really ugly to manually assign a value…
OPT_MERGING				= 1
OPT_OUTPUT_PREFIX		= 2
OPT_OUTPUT_RES			= 3
OPT_OUTPUT_TRACE_SET	= 4

data = [
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'Flip-flop circuit',
		'log_prefix'		: 'flipflop_im',
		'input_files'		: ['Flipflop/flipflop.imi', 'Flipflop/flipflop.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'the benchmark name',
		#'log_prefix'		: 'the_benchmark_prefix',
		#'input_files'		: ['file.imi'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
]
