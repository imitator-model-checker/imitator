#!/usr/bin/python

import json
import sys
import argparse

import time
from datetime import date



import itertools

def string_to_int(s):
	int_s = 0;
	for i in range(0,len(s)) :
		#print s[len(s)-1-i]
		#print 2**i
		#print int(s[i])
		int_s = 2**i*int(s[len(s)-1-i]) + int_s
	return int_s

def norm_loc(loc,n) :
	while len(loc) != n :
		loc = '0' + loc
	return loc

getBin = lambda x: x >= 0 and str(bin(x))[2:] or "-" + str(bin(x))[3:]

def kbits(n, k):
    result = []
    for bits in itertools.combinations(range(n), k):
        s = ['0'] * n
        for bit in bits:
            s[bit] = '1'
        result.append(''.join(s))
    return result
    
 
class CPU :
    def __init__(self, ii, tt) :
        self.id = int(ii)
        self.type = tt

    def __str__(self) :
        return "CPU(id=%d, type=%s)" % (self.id, self.type)
    def __repr__(self) :
        return self.__str__()

class Task :
    def __init__(self, ii, ww, pp, ss, cpu) :
        self.id = int(ii)
        self.wcet = int(ww)
        self.prio = int(pp)
        self.seq = int(ss)
        self.cpu = int(cpu)

    def __str__(self) :
        return "TASK(id=%d, wcet=%d, prio=%d, cpu=%d, seq=%d)" % (self.id, self.wcet, self.prio, self.cpu, self.seq)
    def __repr__(self) :
        return self.__str__()

class Pipeline :
    def __init__(self, ii, pp, dd) :
        self.id = int(ii)
        self.period = int(pp)
        self.dline = int(dd)
        self.task_list = []
        
    def __str__(self) :
        return "PIPELINE(id=%d, period=%d, dline=%d, tasks=%s)" % (self.id, self.period, self.dline, self.task_list)
    def __repr__(self) :
        return self.__str__()


def create_cpus(value) :
    cpulist = [];
    for i in value :
        cpu = CPU(i["id"], i["type"])
        cpulist.append(cpu)
    return cpulist

def create_pipelines(value) :
    plist = []
    for i in value :
        p1 = Pipeline(i["id"], i["period"], i["dline"])
        tlist = i["TASKS"]
        for x in tlist :
            task = Task(x["id"], x["wcet"], x["prio"], x["seq"], x["cpu"])
            p1.task_list.append(task)
        plist.append(p1)
    return plist


def get_scheduler(t, cpulist) :
    for cpu in cpulist :
        if t.cpu == cpu.id : 
            if cpu.type == "preemptive" : return "Fixed_Priority_policy"
            elif cpu.type == "nonpreemptive" : return "Non_Preemptible_FP_policy"
    return "none"

def create_mast_file(cpu_list, pipeline_list) :
    print "-- Real-Time System Model\n" 
    print "Model ("
    print "    Model_Name => MyModel,"
    print "    Model_Date => " + date.today().strftime("%Y-%m-%d") + ");\n"
    print "-- Resources\n"
    for cpu in cpu_list :
        print "Processing_Resource (" 
        if cpu.type == "preemptive" :
            print "    Type => Fixed_Priority_Processor,"
        elif cpu.type == "nonpreemptive" :
            print "    Type => Fixed_Priority_Network,"
        print "    Name => CPU_" + str(cpu.id) + ","
        if cpu.type == "preemptive" :
            print "    Max_Priority => 100,"
            print "    Min_Priority => 1,"
            print "    Max_Interrupt_Priority => 101,"
            print "    Min_Interrupt_Priority  => 101,"
            print "    Worst_Context_Switch => 0,"
            print "    Avg_Context_Switch => 0,"
            print "    Best_Context_Switch => 0,"
            print "    System_Timer            =>"  
            print "        (Type               => Ticker,"
            print "         Worst_Overhead     => 0,"
            print "         Period             => 1)"
        else :
            print "    Packet_Worst_Overhead   => 0,"
            print "    Packet_Avg_Overhead     => 0,"
            print "    Packet_Best_Overhead    => 0,"
            print "    Max_Packet_Transmission_Time => 100,"
            print "    Min_Packet_Transmission_Time => 100"

        print ");\n"

    # Now, for each task one operation
    for p in pipeline_list :
        for t in p.task_list :
            print "Operation ("
            print "    Type     => Simple,"
            print "    Name     => Proc_" + str(p.id) + "_" + str(t.id) + "," 
            print "    Worst_Case_Execution_Time  => " + str(t.wcet) + ","
            print "    Avg_Case_Execution_Time    => " + str(t.wcet) + ","
            print "    Best_Case_Execution_Time   => " + str(t.wcet) + ");\n"

    # Now, one scheduling server per each task 
    for p in pipeline_list :
        for t in p.task_list :
            print "Scheduling_Server ("
            print "    Type => Fixed_Priority,"
            print "    Name => T_" + str(p.id) + "_" + str(t.id) + ","
            print "    Server_Sched_Parameters 	=> ("
            print "        Type => " + get_scheduler(t, cpu_list) + ","
            print "        The_Priority => " + str(t.prio) + ","
            print "        Preassigned => Yes),"
            print "    Server_Processing_Resource => CPU_" + str(t.cpu) + ");\n" 


    print "-- Transactions" 
    for p in pipeline_list :
        print "Transaction ("
        print "    Type => Regular,"
        print "    Name => Trans_" + str(p.id) + ","
        print "    External_Events => ("
        print "        (Type    => Periodic,"
        print "         Name   => EP" + str(p.id) + ","
        print "         Period => " + str(p.period) + ","
        print "         Max_Jitter => 0,"
        print "         Phase => 0)),"
        print "    Internal_Events => ("
        for t in p.task_list :
            print "        (Type  => Regular,"
            if t.seq != (len(p.task_list) - 1) :
                print "         Name  => e" + str(p.id) + str(t.seq) + "),"
            else :
                print "         Name  => Exit" + str(p.id) + ","
                print "         Timing_Requirements => ("
                print "             Type => Hard_Global_Deadline,"
                print "             Deadline => " + str(p.dline) + ","
                print "             Referenced_Event => EP" + str(p.id) + " ))),"
        print "    Event_Handlers => (" 
        for t in p.task_list :
            print "          (Type => Activity,"
            if t.seq == 0 :
                print "           Input_Event    => EP" + str(p.id)  + ","
            else :
                print "           Input_Event    => e" + str(p.id) + str((t.seq-1)) +  ","

            if t.seq == (len(p.task_list) - 1) :
                print "           Output_Event   => Exit" + str(p.id) + ","
            else :
                print "           Output_Event   => e" + str(p.id) + str(t.seq) +  ","
            print "           Activity_Operation => Proc_" + str(p.id) + "_" + str(t.id) + ","
            print "           Activity_Server => T_" + str(p.id) + "_" + str(t.id), 
            if t.seq == (len(p.task_list) - 1) :
                print ")"
            else :
                print "),"
        print "    )"
        print ");"
    return

def create_imitator_file(cpu_list, pipeline_list,input_file_name, tid_list) :
	print("EHI!!")
	print(tid_list)


	imitator_file_name = ""
	v0_file_name = ""
	flag = 0;
	for letter in input_file_name :
		if letter is '.':
			flag = 1
		if flag == 0 :
			imitator_file_name = imitator_file_name + letter
	v0_file_name = imitator_file_name + ".v0"
	imitator_file_name = imitator_file_name + ".imi"
	file = open(imitator_file_name,"w");
	# Write down all the variables
	file.write("var \n");
	count = 0
	for pipeline in pipeline_list :
		file.write("\tt_asap_{0}:clock;\n".format(pipeline.id));
		file.write("\tt_pipeline_{0}:clock;\n".format(pipeline.id));
		file.write("\tT_pipeline_{0}={1}:parameter;\n".format(pipeline.id,pipeline.period));
		file.write("\tD_pipeline_{0}={1}:parameter;\n".format(pipeline.id,pipeline.dline));
		for task in pipeline.task_list :
			file.write("\tt_task_{0}:clock;\n".format(task.id));
			if task.id in tid_list :
				file.write("\tC_task_{0}:parameter;\n".format(task.id));
			else :
				file.write("\tC_task_{0}={1}:parameter;\n".format(task.id,task.wcet));				
			file.write("\tToken_{0}:discrete;\n".format(task.id));
	file.write("\n\n");
	#The observer
	file.write("automaton observer\n".format(pipeline.id));
	file.write("\tsynclabs:")
	i=0;
	for cpu in cpu_list :
		if i==0:
			file.write("DEADLINE_MISSED_{0}".format(cpu.id))
			i=i+1;
		else:
			file.write(",DEADLINE_MISSED_{0}".format(cpu.id))
	file.write(";\n\n")
	file.write("\nloc ObserverOK: invariant True\n")
	for cpu in cpu_list :
		file.write("\t\twhen True sync DEADLINE_MISSED_{0} goto ObserverNOK;\n".format(cpu.id));
	file.write("\nloc ObserverNOK: invariant True\n")
	file.write("\nend\n\n")
		
	#Time to deal with the pipelines
	for pipeline in pipeline_list :
		# Write down the pipeline starters
		#file.write("automaton pipeline_starter_{0}\n".format(pipeline.id));
		#file.write("\t synclabs: start_pipeline_{0};\n\n".format(pipeline.id));
		#file.write("\t loc starter: invariant t_starter_pipeline_{0} <= T_pipeline_{0}\n".format(pipeline.id));
		#file.write("\t\t when t_starter_pipeline_{0} = T_pipeline_{0} sync start_pipeline_{0} do ".format(pipeline.id));
		#file.write("{t_starter_pipeline_");
		#file.write("{0}\ := 0".format(pipeline.id));
		#file.write("} goto starter;\n");
		#file.write("end\n\n");
		#Write down the pipelines
		file.write("automaton pipeline_{0}\n".format(pipeline.id));
		file.write("\tsynclabs:pipeline_restart_{0}".format(pipeline.id));
		i=0;
		for task in pipeline.task_list :
			#if i==0 :
				#file.write("task_{0}_act,task_{0}_done".format(task.id))
				#i = i+1
			#else:
			file.write(",task_{0}_act,task_{0}_done".format(task.id))

		file.write(";\n\n");
		#file.write("\tloc P{0}_1: invariant True\n".format(pipeline.id));
		#file.write("\t\t when True sync start_pipeline_{0} do ".format(pipeline.id));
		#file.write("{t_pipeline_");
		#file.write("{0}\ := 0".format(pipeline.id));
		#file.write("}");
		#file.write("goto P{0}_2;\n".format(pipeline.id));
		i=0
		count = 0
		for task in pipeline.task_list :
			i = i +1;
			count = count + 1;
			for cpu in cpu_list:
				if cpu.id == task.cpu:
					if cpu.type == 'nonpreemptive':
						flag = 0;
					else:
						flag = 1;
			#if flag ==1:
			file.write("\tloc P{0}_{1}: invariant t_asap_{0} <= 0\n".format(pipeline.id,i));
			file.write("\t\t when t_asap_{1}=0 sync task_{0}_act goto P{1}_{2};\n".format(task.id,pipeline.id,i+1));
			#else:
				#file.write("\tloc P{0}_{1}: invariant t_pipeline_{0} <= D_pipeline_{0}\n".format(pipeline.id,i));
				#file.write("\t\t when True sync task_{0}_act goto P{1}_{2};\n".format(task.id,pipeline.id,i+1));
			
			i=i+1;
			file.write("\tloc P{0}_{1}: invariant t_pipeline_{0} <= D_pipeline_{0}\n".format(pipeline.id,i));
			#if count == len(pipeline.task_list) :
			#file.write("\t\t when True sync task_{0}_done do {t_asap_{1} := 0} goto P{1}_1;\n".format(task.id,pipeline.id));
			#else: 
			file.write("\t\t when t_pipeline_{1} <= D_pipeline_{1} sync task_{0}_done do".format(task.id,pipeline.id));
			file.write(" {");
			file.write("t_asap_{0}\ := 0".format(pipeline.id));
			file.write("} ");
			file.write("goto P{0}_{1};\n".format(pipeline.id,i+1));
			#file.write("\t\t when t_pipeline_{0} = D_pipeline_{0} + 1 sync DEADLINE_MISS_{0} goto DEADLINE_MISSED;\n".format(pipeline.id));
			
		file.write("\tloc P{0}_{1}: invariant t_pipeline_{0} <= T_pipeline_{0}\n".format(pipeline.id,i+1));
		file.write("\t\t when t_pipeline_{0} = T_pipeline_{0} do".format(pipeline.id));
		file.write(" {");
		file.write("t_pipeline_{0}\ := 0,t_asap_{0}\ := 0".format(pipeline.id));
		file.write("} ");
		file.write("sync pipeline_restart_{1} goto P{1}_{2};\n".format(task.id,pipeline.id,1));
		#file.write("\tloc DEADLINE_MISSED: invariant True\n");
		file.write("end\n\n");

	#We need CPUs in here
	for cpu in cpu_list :
		if cpu.type == "preemptive" :
			file.write("automaton proc_{0}\n".format(cpu.id))
			file.write("\t synclabs: DEADLINE_MISSED_{0}, ".format(cpu.id))
			task_on_cpu_list = []
			for pipeline in pipeline_list :
				for task in pipeline.task_list :
					if task.cpu == cpu.id :
						task_on_cpu_list.append(task)
			i=0
			for task in task_on_cpu_list:
				if i==0 :
					file.write("task_{0}_act,task_{0}_done".format(task.id))
					i = i+1
				else:
					file.write(",task_{0}_act,task_{0}_done".format(task.id))
			file.write(";\n\n")
			file.write("\tloc DEADLINE_MISS: invariant True\n");

			n=len(task_on_cpu_list)
			for i in range(0,n+1) :
				list_loc = kbits(n,i)
				for loc in list_loc :
					file.write("\tloc proc_{0}_{1}: invariant ".format(task.cpu,loc))
					task_running = -1;
					max_prio = -1;
					for i in range(0,n) :
						if ((loc[i] is "1")&(task_on_cpu_list[i].prio > max_prio)) :
							max_prio = task_on_cpu_list[i].prio
							task_running = task_on_cpu_list[i].id
					if task_running == -1 :
						file.write("True ")
					else :
						file.write("t_task_{0} <= C_task_{0} ".format(task_running))
					file.write("stop {")
					compt = 0
					for task in task_on_cpu_list :
						if ((compt == 0)&(task.id != task_running)) :
							file.write("t_task_{0}".format(task.id))
							compt = compt + 1
						elif task.id != task_running: 
							file.write(",t_task_{0}".format(task.id))
					file.write("}\n")
					if task_running == -1 :
						compt = 0;
						#for pipeline in pipeline_list:
							#file.write("\t\twhen t_pipeline_{1} = D_pipeline_{1} sync DEADLINE_MISSED goto DEADLINE_MISS;\n".format(task.id,pipeline.id));
						for task in task_on_cpu_list :
							compt = compt +1;
							file.write("\t\twhen True sync task_{0}_act do ".format(task.id))
							file.write("{")
							file.write("t_task_{0}\ := 0".format(task.id))
							file.write("} goto ")
							file.write("proc_{0}_{1};\n".format(task.cpu,norm_loc(str(getBin(string_to_int(loc)+2**(n-compt))),n)))
					else :
						compt = 0;
						#for pipeline in pipeline_list:
						for i in range(0,n) :
							if ((loc[i] is "1")):
								for pipeline in pipeline_list:
									if task_on_cpu_list[i] in pipeline.task_list:
										file.write("\t\twhen t_task_{0} < C_task_{0} & t_pipeline_{1} = D_pipeline_{1} sync DEADLINE_MISSED_{2} goto DEADLINE_MISS;\n".format(task_on_cpu_list[i].id,pipeline.id,cpu.id));
						for task in task_on_cpu_list :
							compt = compt +1;
							if loc[compt-1] is "0" :
								file.write("\t\twhen t_task_{1} < C_task_{1} sync task_{0}_act do ".format(task.id,task_running))
								file.write("{")
								file.write("t_task_{0}\ := 0".format(task.id))
								file.write("} goto ")
								file.write("proc_{0}_{1};\n".format(task.cpu,norm_loc(str(getBin(string_to_int(loc)+2**(n-compt))),n)))
							else :
								file.write("\t\twhen t_task_{0} = C_task_{0} sync task_{0}_done goto ".format(task.id))
								file.write("proc_{0}_{1};\n".format(task.cpu,norm_loc(str(getBin(string_to_int(loc)-2**(n-compt))),n)))
			file.write("end\n\n")
		else:
			file.write("automaton proc_{0}\n".format(cpu.id))
			file.write("\t synclabs: DEADLINE_MISSED_{0},".format(cpu.id))
			task_on_cpu_list = []
			for pipeline in pipeline_list :
				for task in pipeline.task_list :
					if task.cpu == cpu.id :
						task_on_cpu_list.append(task)
			i=0
			for task in task_on_cpu_list:
				if i==0 :
					file.write("task_{0}_act,task_{0}_done".format(task.id))
					i = i+1
				else:
					file.write(",task_{0}_act,task_{0}_done".format(task.id))
			file.write(";\n\n")
			file.write("\t loc idle: invariant True stop");
			file.write("{");
			compt = 0
			for task in task_on_cpu_list :
				if ((compt == 0)&(task.id != task_running)) :
					file.write("t_task_{0}".format(task.id))
					compt = compt + 1
				elif task.id != task_running: 
					file.write(",t_task_{0}".format(task.id))
			file.write("}\n")
			#for pipeline in pipeline_list:
				#file.write("\t\twhen t_pipeline_{1} = D_pipeline_{1} sync DEADLINE_MISSED_{1} goto DEADLINE_MISS;\n".format(task.id,pipeline.id));
			for task in task_on_cpu_list :
				file.write("\t\twhen True sync task_{0}_act do".format(task.id));
				file.write("{Token_");
				file.write("{0}\ := 1".format(task.id));
				file.write("}");
				file.write("goto t_{0}_running;\n".format(task.id));
			file.write("\n");
			file.write("\tloc DEADLINE_MISS: invariant True\n");
			for task in task_on_cpu_list :
				file.write("\tloc t_{0}_running: invariant t_task_{0} <= C_task_{0} ".format(task.id));
				task_running = task.id;
				file.write("stop {")
				compt = 0
				for task2 in task_on_cpu_list :
					if ((compt == 0)&(task2.id != task_running)) :
						file.write("t_task_{0}".format(task2.id))
						compt = compt + 1
					elif task2.id != task_running: 
						file.write(",t_task_{0}".format(task2.id))
				file.write("}\n")
				for pipeline in pipeline_list:
					if (task in pipeline.task_list) :
						file.write("\t\twhen t_task_{0} < C_task_{0} & t_pipeline_{1} = D_pipeline_{1} sync DEADLINE_MISSED_{2} goto DEADLINE_MISS;\n".format(task.id,pipeline.id,cpu.id));
				for task2 in task_on_cpu_list :
					file.write("\t\t when t_task_{0} <= 0 sync task_{1}_act do".format(task.id,task2.id));
					file.write("{Token_");
					file.write("{0}\ := 1".format(task2.id));
					file.write("}");
					if (task.prio > task2.prio):
						file.write("goto t_{0}_running;\n".format(task.id));
					else:
						file.write("goto t_{0}_running;\n".format(task2.id));
					file.write("\t\t when t_task_{0} > 0 sync task_{1}_act do".format(task.id,task2.id));
					file.write("{Token_");
					file.write("{0}\ := 1".format(task2.id));
					file.write("}");
					file.write("goto t_{0}_running;\n".format(task.id));
				n = len(task_on_cpu_list)-1;
				for i in range(0,n+1) :
					list_loc = kbits(n,i)
					for loc in list_loc :
						file.write("\t\twhen t_task_{0} = C_task_{0}".format(task.id));
						j=0;
						for task2 in task_on_cpu_list:
							if (task2.id != task.id):
								file.write(" & Token_{0}={1} ".format(task2.id,loc[j]));
								j = j + 1;
						max_prio = 0;
						task_running = -1;
						k=0;
						
						for task2 in task_on_cpu_list:
							if (task2.id != task.id):
								if (task2.prio > max_prio)&(loc[k] == "1"):
									max_prio = task2.prio
									task_running = task2.id
								k = k + 1;
							
						file.write("sync task_{0}_done do".format(task.id));
						file.write("{");
						
						if (task_running != -1):
							file.write("t_task_{0}\ := 0,".format(task_running));
						file.write("Token_{0} := 0".format(task.id));
						file.write("}");
						if (task_running != -1):
							file.write(" goto t_{0}_running;\n".format(task_running));
						else: 
							file.write(" goto idle;\n".format(task_running));
			file.write("end\n\n");
					
	# Write down the init region
	#file.write("var init:region;\n")
	file.write("init := loc[observer] = ObserverOK &\n")
	#The locations
	for cpu in cpu_list :
		task_on_cpu_list = []
		for pipeline in pipeline_list :
			for task in pipeline.task_list :
				if task.cpu == cpu.id :
					task_on_cpu_list.append(task)
		n=len(task_on_cpu_list)
		if cpu.type == "preemptive":
			file.write("\tloc[proc_{0}]= proc_{0}_{1} &\n".format(cpu.id,norm_loc("0",n)));
		else:
			file.write("\tloc[proc_{0}]= idle &\n".format(cpu.id));
	for pipeline in pipeline_list :
		#file.write("\tloc[pipeline_starter_{0}]=starter &\n".format(pipeline.id));
		file.write("\tloc[pipeline_{0}]=P{0}_1 &\n".format(pipeline.id));
	#The clocks
	for pipeline in pipeline_list :
		file.write("\tt_asap_{0} = 0 &\n".format(pipeline.id))
		file.write("\tt_pipeline_{0} = 0 &\n".format(pipeline.id))
		for task in pipeline.task_list :
			file.write("\tt_task_{0} = 0 & \n".format(task.id));
	#The parameters
	for pipeline in pipeline_list :
		#file.write("\tT_pipeline_{0} = {1} &\n".format(pipeline.id,pipeline.period))
		#file.write("\tD_pipeline_{0} = {1} &\n".format(pipeline.id,pipeline.dline))
		for task in pipeline.task_list :
			#file.write("\tC_task_{0} = {1} & \n".format(task.id,task.wcet));
			file.write("\tToken_{0} = 0 & \n".format(task.id));
	file.write("\tTrue;")
	#file.write("\nbad := exists_location loc[observer] = ObserverNOK;\n");

	v0_file = open(v0_file_name,"w");
	#The parameters
	compt_p = 0;
	length_p = len(pipeline_list)
	counter = 0
	for pipeline in pipeline_list :
		compt_p = compt_p +1
		#v0_file.write("\tT_pipeline_{0} = {1} &\n".format(pipeline.id,pipeline.period))
		#v0_file.write("\tD_pipeline_{0} = {1} &\n".format(pipeline.id,pipeline.dline))
		compt_t = 0
		length_t = len(pipeline.task_list)
		for task in pipeline.task_list :
			compt_t = compt_t + 1
			if task.id in tid_list : 
				counter = counter + 1
				if (counter == 2) :
					v0_file.write("\tC_task_{0} = 1..{1}".format(task.id,task.wcet));
				else :
					v0_file.write("\tC_task_{0} = 1..{1} & \n".format(task.id,task.wcet));
	return


def main() :
    
    parser = argparse.ArgumentParser(description='Converts from JSON to a mast file or to an imitator file')
    parser.add_argument("inputfile", help="The JSON file to process")
    parser.add_argument("var1", help="Index of the first variable to process")
    parser.add_argument("var2", help="Index of the last variable to process")
    parser.add_argument("--mast",  help="Produce mast output (default)", action="store_true")
    parser.add_argument("--imitator",  help="Produce imitator output", action="store_true")
    parser.add_argument("--rtscan",  help="Produce rtscan output", action="store_true")
    
    args = parser.parse_args()
    
    file = open(args.inputfile)
    json_data = file.read() 
    data = json.loads(json_data)

    for k,v in data.iteritems() :
        if k == "CPUS" :
            cpu_list = create_cpus(v)

        if k == "PIPELINES" :
            pipeline_list = create_pipelines(v)
        
    # print cpu_list;
    # print "OK -------------------\n"
    # print pipeline_list;
    # print "OK -------------------\n"

# Now that data has been read, we can produce output
    if args.mast:
        create_mast_file(cpu_list, pipeline_list)
    elif args.imitator:
        create_imitator_file(cpu_list, pipeline_list,args.inputfile, [int(args.var1), int(args.var2)])
    elif args.rtscan:
        create_rtscan_file(cpu_list, pipeline_list)

#    print "Completed"


if __name__ == "__main__" :
    main()
