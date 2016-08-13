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
# Last modified     : 2016/08/13
#************************************************************


#------------------------------------------------------------
# Versions
#------------------------------------------------------------
# HACK: copied these constants from comparator.py
# NOTE: really ugly to manually assign a value…
V_2_5	= 1
V_2_6_1	= 2
V_2_7_3	= 3
V_2_8	= 4

#------------------------------------------------------------
# Options
#------------------------------------------------------------
# HACK: copied these constants from comparator.py
# NOTE: really ugly to manually assign a value…
OPT_DISTR_SUBDOMAIN		= 1
OPT_INCLUSION			= 2
OPT_MERGING				= 3
OPT_MODE_COVER			= 4
OPT_OUTPUT_CART			= 5
OPT_OUTPUT_PREFIX		= 6
OPT_OUTPUT_RES			= 7
OPT_OUTPUT_TRACE_SET	= 8
OPT_PRP					= 9


#************************************************************
# THE BENCHMARKS TO BE COMPARED
#************************************************************

data = [
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Flip-flop circuit',
		#'log_prefix'		: 'flipflop_im',
		#'input_files'		: ['Flipflop/flipflop.imi', 'Flipflop/flipflop.pi0'],
		#'input_files_v'		: {V_2_5 : ['Flipflop/flipflop_v_2_5.imi', 'Flipflop/flipflop.pi0'] },
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
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
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'concurent_tasks_chain',
		#'log_prefix'		: 'concurent_tasks_chain_im',
		#'input_files'		: ['Scheduling/concurent_tasks_chain.imi', 'Scheduling/concurent_tasks_chain.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'generic_fp',
		#'log_prefix'		: 'generic_fp_im',
		#'input_files'		: ['Scheduling/generic_fp.imi', 'Scheduling/generic_fp.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	###------------------------------------------------------------
	### BEGIN benchmark
	##{
		##'name'				: 'full_cpr08',
		##'log_prefix'		: 'full_cpr08_im',
		##'input_files'		: ['Scheduling/full_cpr08.imi', 'Scheduling/full_cpr08.pi0'],
		##'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	##}
	### END benchmark
	###------------------------------------------------------------
	##,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'hppr10_audio',
		#'log_prefix'		: 'hppr10_audio_im',
		#'input_files'		: ['Scheduling/hppr10_audio.imi', 'Scheduling/hppr10_audio.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'astrium_basic_thermal_fp',
		#'log_prefix'		: 'astrium_basic_thermal_fp_im',
		#'input_files'		: ['Scheduling/astrium_basic_thermal_fp.imi', 'Scheduling/astrium_basic_thermal_fp.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'am02',
		#'log_prefix'		: 'am02_im',
		#'input_files'		: ['Scheduling/am02.imi', 'Scheduling/am02.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET, OPT_INCLUSION],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Bounded retransmission protocol',
		#'log_prefix'		: 'BRP_im',
		#'input_files'		: ['BRP/brp.imi', 'BRP/brp.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Root contention protocol',
		#'log_prefix'		: 'RCP_im',
		#'input_files'		: ['RCP/RCP.imi', 'RCP/RCP.pi0'],
		#'options'			: [OPT_MERGING, OPT_OUTPUT_RES, OPT_OUTPUT_TRACE_SET],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	
	
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Flip-flop circuit',
		#'nb_nodes'			: 4,
		#'log_prefix'		: 'flipflop_bc_distr_4',
		#'input_files'		: ['Flipflop/flipflop.imi', 'Flipflop/flipflop.v0'],
		#'options'			: [OPT_MODE_COVER, OPT_OUTPUT_RES, OPT_DISTR_SUBDOMAIN],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'SPSMALL distributed(4)',
		#'nb_nodes'			: 4,
		#'log_prefix'		: 'spsmall_distr_4',
		#'input_files'		: ['Valmem/spsmall.imi', 'Valmem/spsmall.v0'],
		#'options'			: [OPT_MERGING, OPT_MODE_COVER, OPT_OUTPUT_RES, OPT_DISTR_SUBDOMAIN, OPT_PRP],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Sched2.50.0 distributed(4)',
		#'nb_nodes'			: 4,
		#'log_prefix'		: 'sched2_50_0_prpc_distr_4',
		#'input_files'		: ['Scheduling/non-preemptive-scheduling/Sched2.50.0.imi', 'Scheduling/non-preemptive-scheduling/Sched2.50.0.v0'],
		#'options'			: [OPT_MERGING, OPT_MODE_COVER, OPT_OUTPUT_RES, OPT_DISTR_SUBDOMAIN, OPT_PRP],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
	##------------------------------------------------------------
	## BEGIN benchmark
	#{
		#'name'				: 'Sched2.100.0 distributed(4)',
		#'nb_nodes'			: 4,
		#'log_prefix'		: 'sched2_100_0_prpc_distr_4',
		#'input_files'		: ['Scheduling/non-preemptive-scheduling/Sched2.100.0.imi', 'Scheduling/non-preemptive-scheduling/Sched2.100.0.v0'],
		#'options'			: [OPT_MERGING, OPT_MODE_COVER, OPT_OUTPUT_RES, OPT_DISTR_SUBDOMAIN, OPT_PRP],
	#}
	## END benchmark
	##------------------------------------------------------------
	#,
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
