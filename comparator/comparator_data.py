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


#------------------------------------------------------------
# Options
#------------------------------------------------------------
# HACK: copied these constants from comparator.py
# NOTE: really ugly to manually assign a value…
OPT_INCLUSION			= 1
OPT_MERGING				= 2
OPT_OUTPUT_PREFIX		= 3
OPT_OUTPUT_RES			= 4
OPT_OUTPUT_TRACE_SET	= 5


#************************************************************
# THE BENCHMARKS TO BE COMPARED
#************************************************************

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
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'LA02_2',
		'log_prefix'		: 'LA02_2_im',
		'input_files'		: ['Scheduling/LA02_2.imi', 'Scheduling/LA02_2.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'concurent_tasks_chain',
		'log_prefix'		: 'concurent_tasks_chain_im',
		'input_files'		: ['Scheduling/concurent_tasks_chain.imi', 'Scheduling/concurent_tasks_chain.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'generic_fp',
		'log_prefix'		: 'generic_fp_im',
		'input_files'		: ['Scheduling/generic_fp.imi', 'Scheduling/generic_fp.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'full_cpr08',
		#'log_prefix'		: 'full_cpr08_im',
		#'input_files'		: ['Scheduling/full_cpr08.imi', 'Scheduling/full_cpr08.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'hppr10_audio',
		'log_prefix'		: 'hppr10_audio_im',
		'input_files'		: ['Scheduling/hppr10_audio.imi', 'Scheduling/hppr10_audio.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'astrium_basic_thermal_fp',
		'log_prefix'		: 'astrium_basic_thermal_fp_im',
		'input_files'		: ['Scheduling/astrium_basic_thermal_fp.imi', 'Scheduling/astrium_basic_thermal_fp.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'am02',
		'log_prefix'		: 'am02_im',
		'input_files'		: ['Scheduling/am02.imi', 'Scheduling/am02.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET, OPT_INCLUSION],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'Bounded retransmission protocol',
		'log_prefix'		: 'BRP_im',
		'input_files'		: ['BRP/brp.imi', 'BRP/brp.pi0'],
		'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	}
	# END benchmark
	#------------------------------------------------------------
	,
	#------------------------------------------------------------
	# BEGIN benchmark
	{
		'name'				: 'Root contention protocol',
		'log_prefix'		: 'RCP_im',
		'input_files'		: ['RCP/RCP.imi', 'RCP/RCP.pi0'],
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
