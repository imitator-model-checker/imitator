#!/usr/bin/python

"""
    This script generates the Hierarchical Scheduling Composition (HSC)
    model in the form of IMITATOR input. 

    Copyright (C) 2013 Youcheng Sun <y.sun@sssup.it> and Giuseppe Lipari. 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.

"""

import copy
import json
import argparse

class SERVER :
    def __init__(self, pp, qq) :
        self.P = pp
        self.Q = qq

    def __str__(self) :
        return "SERVER(P=%s, Q=%s)" % (self.P, self.Q)
    def __repr__(self) :
        return self.__str__()

class TASKS :
    def __init__(self, ii, oo, cc, tt, dd) :
        self.id = ii
        self.O = oo
        self.C = cc
        self.T = tt
        self.D = dd

    def __str__(self) :
        return "TASKS(id=%s, O=%s, C=%s, T=%s, D=%s)" % (self.id, self.O, self.C, self.T, self.D)
    def __repr__(self) :
        return self.__str__()

"""
   Generates the starting part of the file
"""
def gen_clocks(out, ntasks) :
    out.write("var\n")
    out.write("    ")
    for i in range(1, ntasks+1) :
        out.write("p"+str(i)+", c"+str(i)+", d"+str(i)+", ")
    out.write(" x,y: clock;\n")
              
"""
   Generates the parameter part. It needs to be called for each task
"""
def gen_task_parms(out, task) :
    out.write("    ")
    out.write("O" + str(task.id) + " = " + str(task.O) +", ")
    out.write("T" + str(task.id) + " = " + str(task.T) +", ")
    out.write("C" + str(task.id) + " = " + str(task.C) +", ")
    out.write("D" + str(task.id) + " = " + str(task.D) +", ")

""" 
    Writes the server parameters
""" 
def gen_server_parms(out, server) :
    out.write("\n    ")
    out.write("P = " + str(server.P) + ", Q = " + str(server.Q))
    out.write(": parameter;\n\n")                  

"""
   Generates the arrival automatons
"""
def gen_automaton(out, taskid) :
    i = str(taskid)
    out.write("automaton act{0}\n".format(i))
    out.write("synclabs : r{0};\n".format(i))
    out.write("loc wait_for_offset{0} : while p{0}<=O{0} wait \n".format(i))
    out.write("    when p{0}=O{0} sync r{0} do {{ p{0}'= 0 }} goto wait_for_period{0};\n".format(i))
    out.write("loc wait_for_period{0} : while p{0}<=T{0} wait \n".format(i))
    out.write("    when p{0}=T{0} sync r{0} do {{ p{0}'= 0 }} goto wait_for_period{0};\n\n".format(i))
    out.write("end\n\n")


##
## Generates all possible combination of arrivals of ntasks
## basically a list of 0, 1, with 2^ntasks elements
##
def generate_arrival_list(ntasks) :
    if ntasks == 1 : 
        return [ [0], [1] ] 
    else :
        l = generate_arrival_list(ntasks - 1)
        l2 = []
        for x in l :
            y = copy.copy(x)
            x.append(0)
            y.append(1)
            l2.append(x)
            l2.append(y)
        return l2



## returns the name of the state 
## which is taskId + R if running, or W if waiting
## (x or f must be added in front)
def build_state(l) :
    state = ""
    flag = True
    for i, e in enumerate(l) :
        if e == 1 :
            if flag == True :
                state = state + str(i+1) + "R"
                flag = False
            else :
                state = state + str(i+1) + "W"
    return state

##
## Build the invariant for a state
##
def build_invariant(l, fl = True) :
    inv = ""
    for i, e in enumerate(l):
        if e == 1 :
            if inv != "" :
                inv = inv + " & "
            if fl :
                inv = inv + "c{0}<=C{0} & d{0}<=D{0}".format(str(i+1))
            else :
                inv = inv + "c{0}<C{0} & d{0}<D{0}".format(str(i+1))
    return inv


##
## The clocks to be stopped in an executing state 
##
def build_exec_stop(l, fl=True) :
    stop = ""
    flag = True
    for i, e in enumerate(l):
        if e == 1 :
            if (flag and fl) == True:
                flag = False
            else :
                if stop != "" :
                    stop = stop + ", "
                stop = stop + "c{0}".format(str(i+1))
    return stop

##
## Counts the number of active tasks (the number of 1s)
##    
def count_active(l) :
    s = 0
    for e in l :
        s = s + e
    return s

##
## Returns the index of the first active task
##
def find_first(l) :
    for i, e in enumerate(l) :
        if e == 1 :
            return i+1
    

#
# Removes the first 1
#
def remove_first(l) :
    l2 = copy.copy(l)
    for i, e in enumerate(l2) :
        if e == 1 :
            l2[i] = 0
            return l2
    return l2

##
## The list of indexes (in strings) of the low priority tasks
##
def low_prio_list(l) :
    l2 = remove_first(l)
    sl = []
    for i, e in enumerate(l2): 
        if e == 1 :
            sl.append(str(i+1))

    return sl

##
## The list of non active tasks
##
def new_arrivals_list(l) :
    sl = []
    for i, e in enumerate(l): 
        if e == 0 :
            sl.append(i)

    return sl
    
##
## Set the i-th bith to 1
##
def set_arrival(l, i) :
    l2 = copy.copy(l)
    l2[i] = 1
    return l2


"""
   Generates the scheduling automaton
"""
def gen_sched(out, ntasks) :    
    out.write("automaton sched\n\n")
    out.write("synclabs : ")
    for i in range(1, ntasks+1) :
        id = str(i)
        out.write("r{0}, ".format(id))
    out.write(" empty, go, frozen;\n\n");

    out.write("loc idle: while True wait\n");
    for i in range(1, ntasks+1) :
        id = str(i)
        out.write("    when True  sync r{0} do {{c{0}'=0, d{0}'=0}} goto f{0}R;\n".format(id))
    out.write("\n");

    alist = generate_arrival_list(ntasks)
    alist = alist[1:]
    
    for l in alist :
        ## build the x state
        state = "x" + build_state(l)            
        print "State : " + state

        out.write("\nloc {0} : while {1}".format(state, 
                                                 build_invariant(l)))
        s = build_exec_stop(l)
        if s != "" :
            out.write(" stop {{ {0} }}".format(s))
        out.write(" wait\n")
        ## first edge: the task completes execution
        first = str(find_first(l))
        if count_active(l) == 1 :
            label = "sync empty"
            newstate = "idle"
        else:
            label = ""
            newstate = "x" + build_state(remove_first(l))
        
        out.write("    when c{0} = C{0} {1} do {{}} goto {2};\n".
                  format(first,
                         label, 
                         newstate));

        ## second set of edges: deadline misses
        out.write("    when c{0}<C{0} & d{0}=D{0} do {{}} goto error;\n".
                  format(first))
        for x in low_prio_list(l) :
            out.write("    when d{0}=D{0} do {{}} goto error;\n".
                      format(x))

        ## another set of edges: arrival of new jobs
        for arr_index in new_arrivals_list(l) : 
            reset = "c{0}'=0, d{0}'=0".format(str(arr_index+1))
            newstate = "x" + build_state(set_arrival(l, arr_index))
            out.write("    when {0} sync {1} do {{ {2} }} goto {3};\n".
                      format(build_invariant(l, False),
                             "r"+str(arr_index+1),
                             reset,
                             newstate))
                                                                             
        ## next, the frozen edge
        out.write("    when {0} sync frozen do {{}} goto {1};\n".
                  format(build_invariant(l, False),
                         "f" + build_state(l)))

        

        ## now, let us build the f state
        state = "f" + build_state(l)            
        print "State : " + state

        out.write("\nloc {0} : while {1}".format(state, 
                                                 build_invariant(l)))
        s = build_exec_stop(l, False)
        if s != "" :
            out.write(" stop {{ {0} }}".format(s))
        out.write(" wait\n")

        ## second set of edges: deadline misses
        first = str(find_first(l))
        out.write("    when c{0}<C{0} & d{0}=D{0} do {{}} goto error;\n".
                  format(first))
        for x in low_prio_list(l) :
            out.write("    when d{0}=D{0} do {{}} goto error;\n".
                      format(x))
        
        ## another set of edges: arrival of new jobs
        for arr_index in new_arrivals_list(l) : 
            reset = "c{0}'=0, d{0}'=0".format(str(arr_index+1))
            newstate = "f" + build_state(set_arrival(l, arr_index))
            out.write("    when {0} sync {1} do {{ {2} }} goto {3};\n".
                      format(build_invariant(l, False),
                             "r"+str(arr_index+1),
                             reset,
                             newstate))

        ## next, the frozen edge
        out.write("    when {0} sync go do {{}} goto {1};\n".
                  format(build_invariant(l),
                         "x" + build_state(l)))
    out.write("\n loc error: while True wait {}\n\n")
    out.write("end\n\n")


"""
   Generate the server automaton
"""
def gen_server(out, ntasks, server) :
    out.write("automaton server\n\n")
    
    out.write("synclabs: ")
    for i in range(1, ntasks+1) :
        if i != 1 :
            out.write(", ")
        out.write("r{0}".format(str(i)))
    out.write(", empty, go, frozen;\n")

    out.write("loc idle: while True stop {x,y} wait{}\n")
    for i in range(1, ntasks+1) :
        out.write("    when True sync r{0} do {{ x'=0, y'=0 }} goto active;\n".
                  format(str(i)))
    out.write("loc active: while y-x <= P-Q stop{x} wait {}\n")
    for i in range(1, ntasks+1) :
        out.write("    when True sync r{0} do {{ }} goto active;\n".
                  format(str(i)))
    out.write("    when True sync go do {} goto executing;\n")
    
    out.write("loc executing: while x <= Q wait {}\n")
    for i in range(1, ntasks+1) :
        out.write("    when True sync r{0} do {{ }} goto executing;\n".
                  format(str(i)))
    out.write("    when x = Q sync frozen do {} goto recharging;\n")
    out.write("    when {0}*x >= {1}*y sync empty do {{  }} goto empty;\n".
              format(str(server.P), str(server.Q)))
    out.write("    when {0}*x < {1}*y sync empty  do {{ x'=0,y'=0 }} goto idle;\n".
              format(str(server.P), str(server.Q)))
    out.write("    when x < Q sync frozen do { } goto active;\n");
    
    out.write("loc recharging: while y <= P stop {x} wait {}\n")
    for i in range(1, ntasks+1) :
        out.write("    when True sync r{0} do {{ }} goto recharging;\n".
                  format(str(i)))
    out.write("    when y = P do {x'=0,y'=0} goto active;\n")
    
    out.write("loc empty: while {0}*x >= {1}*y stop {{ x }} wait {{}}\n".
              format(str(server.P), str(server.Q)))
    for i in range(1, ntasks+1) :
        out.write("    when True sync r{0} do {{ }} goto active;\n".
                  format(str(i)))
    out.write("    when {0}*x = {1}*y goto idle;\n".
              format(str(server.P), str(server.Q)))
    out.write("end\n\n")


def gen_init(out, ntasks) :
    out.write("var init: region;\n")
    out.write("init := \n")
    for i in range(1, ntasks+1) :
        out.write("    loc[act{0}] = wait_for_offset{0} & p{0}=0 &\n".
                  format(str(i)))
    out.write("    loc[sched] = idle & ")
    for i in range(1, ntasks+1) :
        out.write("c{0}=0 & d{0} = 0 & ".
                  format(str(i)))
    out.write("\n    loc[server] = idle &x=0 &y=0;\n")
    

def create_tasks(value) :
    tasks = []
    for i in value :
        task = TASKS(i["id"], i["O"], i["C"], i["T"], i["D"])
        tasks.append(task)
    return tasks

def create_server(value) :
    for i in value :
        server = SERVER(i["P"], i["Q"])
    return server

def main() : 

    parser = argparse.ArgumentParser(description='Given a periodic server and its tasks described in a JSON file, this script builds the HSC model in the form of IMITATOR input.')
    parser.add_argument("inputfile", help="The JSON file to process")     
    parser.add_argument("outputfile", help="The output IMITATOR file")     
    args = parser.parse_args()

    fin = open(args.inputfile)
    json_data = fin.read()
    data = json.loads(json_data)

    for k,v in data.iteritems() :
        if k == "SERVER" :
           s = create_server(v)
        if k == "TASKS" :
           tasks = create_tasks(v)

    fin.close()

    fout = open(args.outputfile, "w")
    n = len(tasks)

    gen_clocks(fout, n)

    for i in xrange(n) :
        gen_task_parms(fout, tasks[i])

    gen_server_parms(fout, s)

    for i in xrange(n) :
        gen_automaton(fout, i+1)

    gen_sched(fout, n)
    gen_server(fout, n, s)
    gen_init(fout, n)

    fout.close()

if __name__ == "__main__" :
    main()
