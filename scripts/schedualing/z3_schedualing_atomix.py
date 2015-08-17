 #Copyright (c) Microsoft Corporation
 #All rights reserved.

 #Licensed under the Apache License, Version 2.0 (the ""License""); you
 #may not use this file except in compliance with the License. You may
 #obtain a copy of the License at

 #http://www.apache.org/licenses/LICENSE-2.0

 #THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR
 #CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT
 #LIMITATION ANY IMPLIED WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR
 #A PARTICULAR PURPOSE, MERCHANTABLITY OR NON-INFRINGEMENT.

 #See the Apache Version 2.0 License for specific language governing
 #permissions and limitations under the License.

import sys
from z3 import *
import time
import matplotlib.pyplot as plt

# Variable descriptions:
# d - the max delay allowed
# n - the number of atoms -- we optimize this
# Tmax - the maximum period time allowed when scheduling loops -- we optimize this
# G - G[i][j] = -n if j is dependent on i n iteration in the past. G[i][j] = 0 if j is normaly dependent on i. G[i][j] = 1 if no dependency
# D - inf
# m - number of cores
# p - p[i][k] = the time to run atom i on core k
# q - q[k][t] = communication time between cores k and t 
def main(input_file,timestamp_solver = True):
    init("libz3.dll")
    n = 4
#    n = 25
    d = 4
#    d = 7200
    Tmax = 4
    #(i == n - 1) 
    G = [[  1 for i in range(n)] for j in range(n) ]
    G[0][1] = -1
    G[1][3] = -1
    G[1][2] = 0
    G[1][3] = 0
#    G[0][1] = G[1][2] = G[2][3] = G[3][4] = 0
#    G[4][5] = G[5][6] = G[6][7] = G[6][8] = 0
#    G[6][9] = G[6][10] = G[7][11] = G[8][12] = 0
#    G[9][13] = G[10][14] = G[11][15] = G[12][16] = 0
#    G[13][17] = G[14][18] = G[15][19] = G[16][20] = 0
#    G[17][21] = G[18][22] = G[19][23] = G[20][23] = 0
#    G[21][23] = G[22][23] = G[23][24] = 0
    D = 2 * d
    m=2
#    m = 9;				#%3 dsps, 4 vcps, 2 fcps
    p = [[1,1],[2,2],[1,1],[2,2]]
    q = [[0,0],[0,0]]
#    p = [
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [400,	400,	400,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [600,	600,	600,	D,	D,	D,	D,	D,	D],
#         [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [D,	D,	D,	4000,	4000,	4000,	4000,	D,	D],	
#	      [D,	D,	D,	4000,	4000,	4000,	4000,	D,	D],
#	      [D,	D,	D,	4000,	4000,	4000,	4000,	D,	D],
#	      [D,	D,	D,	4000,	4000,	4000,	4000,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [200,	200,	200,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	      [100,	100,	100,	D,	D,	D,	D,	D,	D]]
#    q = [
#            [0,	200,	200,	100,	100,	100,	100,	100,	100],
#	        [200,	0,	200,	100,	100,	100,	100,	100,	100],
#	        [200,	200,	0,	100,	100,	100,	100,	100,	100],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D],
#	        [100,	100,	100,	D,	D,	D,	D,	D,	D]]
    #ret = genUnrollLoop(n,p,G,2)
    #n = ret['n']
    #p = ret['p']
    #G = ret['G']
    print G
    s = genLoopSchedSolver(n,m,p,d,D,G,q,Tmax)
    start = time.clock()
    print(s.check())
    end = time.clock()
    if timestamp_solver:
        print "Solution found in %.8lf seconds" % (end - start)
    model = s.model()
    list = []
    for t in model.decls():
        if not "z3" in str(t):
            list.append("%s = %s" % (t,model[t]))
    list.sort()
    for t in list:
        print(t)
    plotSchedual(m,n,p,q,G,d,model)

# simple binary search algorithm to find best d
def schedStateSimple(state,q,m):
    G = state.dependecies
    p = state.runningtimes
    d = state.maxdelay
    D = 2 * d
    n = state.numberofatoms
    epsilon = state.epsilon
    delta = 1
    max = d
    min = 0
    corrent = max
    s = genSingleSchedSolver(n,m,p,d,D,G,q)
    if str(s.check() != "sat"):
           return -1
    else:
        d = d/2
    while(epsilon < delta):
        s = genSingleSchedSolver(n,m,p,d,D,G,q)
        if str(s.check() == "sat"):
            corrent = d
            max = d
        else:
            min = d
        d = (max - min) /2
        if max < min:
            break
        epsilon *= 0.5
    genSingleSchedSolver(n,m,p,d,D,G,q)
    return parseResults(n,p,s.model())

#the smt problem for a simple state schedule
def genSingleSchedSolver(n,m,p,d,D,G,q): 
    #s = SolverFor('QF_LIA')
    s = Solver()
    x = RealVector('x',n) # consider making bit vector
    y = []; # define y_i_j
    for i in range(n):
        y.append([])
        for j in range(m):
            if p[i][j] < D:
                y[i].append(Bool('y__%s__%s' % (i,j)))
            else:
                y[i].append(False)
    for i in range(n):  
       # add a costraint that each atom is alocated to at least one core,
       # no need to add a constraint that each atom is allocated to only 
       # one core because in the optimal solution this will never happen anyway
       s.add(Or(y[i]))
       flag = False  
       for j in range(n):
           if i != j:
               flag = flag or (G[j][i] == 0)
               if G[i][j] == 0: # if i and j are dependent C[i] < x[j]
                   for k in range(m):
                       for t in range(m):
                           if (y[i][k] != False) and (y[j][t] != False):
                               # this constraint makes sure ordering between dependent atoms is kept
                               # and takes care of communication costs between the atoms
                               s.add(Implies(And(y[i][k],y[j][t]), x[j] - x[i] >= p[i][k] + q[k][t])) 
               else:
                   for k in range(m):
                       if (y[i][k] != False) and (y[j][k] != False):
                           # this constraint makes sure that no core is over booked
                           s.add(Implies(And(y[i][k],y[j][k]),Or(x[j] - x[i] >= p[i][k], x[i] - x[j] >= p[j][k])))
       if not flag:
           # if the Atom is not dependent on any other atom just need to make sure
           # it is not schedualed in the past
           s.add(x[i] >= 0) # x_i >= 0
       for k in range(m):
           if y[i][k] != False:
               s.add(Implies(y[i][k],x[i] + p[i][k] <= d))
    return s

def parseResults(n,p,model):
    core_alloc = [0 for i in range(n)]
    start_times = [0 for i in range(n)]
    end_times = [0 for i in range(n)]
    proc_time = [0 for i in range(n)]
    for t in model.decls():
       if 'y' in str(t):
           imp = str(model[t])
           if  imp == 'True':
               temp = str(t).split('__')
               core_num = int(temp[2])
               atom_num = int(temp[1])
               #if atom_num == n-1:
                #    continue
               start = float(eval(str(model[Real('x__'+str(atom_num))])))
               core_alloc[atom_num] = core_num
               start_times[atom_num] = start
               proc_time[atom_num] = p[atom_num][core_num]
               end_times[atom_num] = start + p[atom_num][core_num]
    return { 'core_alloc':core_alloc, 'start_times':start_times, 'end_times':end_times, 'proc_time':proc_time}

def plotSchedual(m,n,p,q,G,d,model):
    y_name = []
    y_data = []
    y_ticks = []
    face_color = [()]
    atom_label = [()]
    for i in range(m):
        y_name.append('Core ' + str(i))
        y_data.append([])
        y_ticks.append((i+1) * 10 + 5)
        face_color.append(())
        atom_label.append(())
    fig, ax = plt.subplots()
    res = parseResults(n,p,model)
    calloc = res['core_alloc']
    stimes = res['start_times']
    etimes = res['end_times']
    ptimes = res['proc_time']
    for i in range(n):
        com_time = 0
        max_end = 0
        flag = False
        for j in range(n):
            if G[j][i] == 0:
                flag = True 
                if etimes[j] > max_end:
                    max_end = etimes[j]
                    com_time = q[calloc[j]][calloc[i]]
        if flag:
            face_color[calloc[i]] += tuple(['red'])
            atom_label[calloc[i]] += tuple(['tr'])
            tup = (stimes[i] - com_time,com_time)
            y_data[calloc[i]].append(tup)
        face_color[calloc[i]] += tuple(['blue'])
        atom_label[calloc[i]] += tuple([str(i)])
        tup = (stimes[i],ptimes[i])
        y_data[calloc[i]].append(tup)
    for i in range(m):
        ax.broken_barh(y_data[i],(10 * (i+1),9), facecolors=face_color[i], label=atom_label[i])
    ax.set_ylim(5,(m+1) * 10 + 5)
    ax.set_xlim(-d,2 * d)
    ax.set_yticks(y_ticks)
    ax.set_yticklabels(y_name)
    ax.grid(True)
    ax.set_xlabel('Time')
    plt.show()

# smt problem for loop scheduling
def genLoopSchedSolver(n,m,p,d,D,G,q,Tmax): 
    #s = SolverFor('QF_LIA')
    s = Solver()
    x = RealVector('x',n) # consider making bit vector
    T = Real('T') 
    s.add(T <= Tmax)
    y = []; # define y_i_j
    for i in range(n):
        y.append([])
        for j in range(m):
            if p[i][j] < D:
                y[i].append(Bool('y__%s__%s' % (i,j)))
            else:
                y[i].append(False)
    s.add(T > 0)
    for i in range(n):
        flag = False   
        for j in range(n):
            if i != j:
                if G[j][i] <= 0:
                    flag = flag or (G[j][i] == 0)
                    for k in range(m):
                            for t in range(m):
                                if (y[i][k] != False) and (y[j][t] != False): 
                                    s.add(Implies(And(y[i][k],y[j][t]),x[i] >= x[j] + p[j][t] + q[t][k] + G[j][i] * T))
                else:
                    for k in range(m):
                       if (y[i][k] != False) and (y[j][k] != False):
                           s.add(Implies(And(y[i][k],y[j][k]),Or(And(x[j] - x[i] >= p[i][k],x[i] + T - x[j] >= p[j][k]), And(x[i] - x[j] >= p[j][k],x[j] + T - x[i] >= p[i][k]))))
        if not flag:
            s.add(x[i] >= 0)      
        s.add(Or(y[i]))
        for k in range(m):
            if y[i][k] != False:
                s.add(Implies(y[i][k],x[i] + p[i][k] <= d))
    return s

def genUnrollLoop(n,p,G,count):
    Nnew = n * count
    Gnew = [[1 for i in range(Nnew)] for j in range(Nnew)]
    for i in range(n):
        for j in range(n):
            if G[i][j] <= 0:
                for k in range(-G[i][j],count):
                    t = i + (k + G[i][j]) * n
                    s = j + k * n
                    Gnew[t][s] = 0 
    return {'G': Gnew, 'p': p * count, 'n': Nnew}

# scheduling one state based on the schedule of a previous state !!!not complete!!!
def combineSched(core_alloc, start_times, end_times, dec_eval_time, n,m,p,d,D,G,q):
    #s = SolverFor('QF_LIA')
    s = Solver()
    x = RealVector('x',n) # consider making bit vector
    n_t = len(start_time)
    y = []; # define y_i_j
    for i in range(n):
        y.append([])
        for j in range(m):
            if p[i][j] < D:
                y[i].append(Bool('y__%s__%s' % (i,j)))
            else:
                y[i].append(False)
    for i in range(n_t):
        if end_time[i] - dec_eval_time:
            y_t = [(j == core_alloc[i]) for j in range(m)]
            y[n+i] = y_t
            if start_time[i] - dec_eval_time > 0:
                x.append(start_time[i] - dec_eval_time)
                p.append([((end_time[i] - start_time[i]) if (j == core_alloc[i]) else  D) for j in range(m)])
            else:
                x.append(0)
                p.append([((end_time[i] - dec_eval_time) if (j == core_alloc[i]) else  D) for j in range(m)])
    for i in range(n):  
       # add a costraint that each atom is alocated to at least one core,
       # no need to add a constraint that each atom is allocated to only 
       # one core because in the optimal solution this will never happen anyway
       s.add(Or(y[i]))
       flag = False  
       for j in range(n):
           if i != j:
               flag = flag or (G[j][i] == 0)
               if G[i][j] == 0: # if i and j are dependent C[i] < x[j]
                   for k in range(m):
                       for t in range(m):
                           if (y[i][k] != False) and (y[j][t] != False):
                               # this constraint makes sure ordering between dependent atoms is kept
                               # and takes care of communication costs between the atoms
                               s.add(Implies(And(y[i][k],y[j][t]), x[j] - x[i] >= p[i][k] + q[k][t])) 
               else:
                   for k in range(m):
                       if (y[i][k] != False) and (y[j][k] != False):
                           # this constraint makes sure that no core is over booked
                           s.add(Implies(And(y[i][k],y[j][k]),Or(x[j] - x[i] >= p[i][k], x[i] - x[j] >= p[j][k])))
       if not flag:
           # if the Atom is not dependent on any other atom just need to make sure
           # it is not schedualed in the past
           s.add(x[i] >= 0) # x_i >= 0
       for k in range(m):
           if y[i][k] != False:
               s.add(Implies(y[i][k],x[i] + p[i][k] <= d))
    return s

# smt problem with more complex dependencies like Read read or write write, !!!deprecated!!!
def genSingleSchedSolver_complex_dependencies(n,m,p,d,D,G,Grr,Gww,q): 
    #s = SolverFor('QF_LIA')
    s = Solver()
    x = RealVector('x',n) # consider making bit vector
    y = []; # define y_i_j
    for i in range(n):
        y.append([])
        for j in range(m):
            if p[i][j] < D:
                y[i].append(Bool('y__%s__%s' % (i,j)))
            else:
                y[i].append(False)
    for i in range(n):  
       # add a costraint that each atom is alocated to at least one core,
       # no need to add a constraint that each atom is allocated to only 
       # one core because in the optimal solution this will never happen anyway
       s.add(Or(y[i]))
       flag = False  
       for j in range(n):
           if i != j:
               flag = flag or (G[j][i] == 0)
               if G[i][j] == 0: # if i and j are dependent C[i] < x[j]
                   for k in range(m):
                       for t in range(m):
                           if (y[i][k] != False) and (y[j][t] != False):
                               # this constraint makes sure ordering between dependent atoms is kept
                               # and takes care of communication costs between the atoms
                               s.add(Implies(And(y[i][k],y[j][t]), x[j] - x[i] >= p[i][k] + q[k][t])) 
               elif Grr[i][j] == 0:
                   for k in range(m):
                       for t in range(m):
                           if (y[i][k] != False) and (y[j][t] != False):
                               # this constraint makes sure ordering between dependent atoms is kept
                               # and takes care of communication costs between the atoms
                               s.add(Implies(And(y[i][k],y[j][t]), x[j] >= x[i] - q[k][t])) 
                       if (y[i][k] != False) and (y[j][k] != False):
                          # this constraint makes sure that no core is over booked
                          s.add(Implies(And(y[i][k],y[j][k]),Or(x[j] - x[i] >= p[i][k], x[i] - x[j] >= p[j][k])))
               elif Gww[i][j] == 0:
                   for k in range(m):
                       for t in range(m):
                           if (y[i][k] != False) and (y[j][t] != False):
                               # this constraint makes sure ordering between dependent atoms is kept
                               # and takes care of communication costs between the atoms
                               s.add(Implies(And(y[i][k],y[j][t]), x[j] + p[j][t] >= x[i] + p[i][k] + q[k][t])) 
                       if (y[i][k] != False) and (y[j][k] != False):
                          # this constraint makes sure that no core is over booked
                          s.add(Implies(And(y[i][k],y[j][k]),Or(x[j] - x[i] >= p[i][k], x[i] - x[j] >= p[j][k])))
                
               else:
                   for k in range(m):
                       if (y[i][k] != False) and (y[j][k] != False):
                           # this constraint makes sure that no core is over booked
                           s.add(Implies(And(y[i][k],y[j][k]),Or(x[j] - x[i] >= p[i][k], x[i] - x[j] >= p[j][k])))
       if not flag:
           # if the Atom is not dependent on any other atom just need to make sure
           # it is not schedualed in the past
           s.add(x[i] >= 0) # x_i >= 0
       for k in range(m):
           if y[i][k] != False:
               s.add(Implies(y[i][k],x[i] + p[i][k] <= d))
    return s


if __name__ == "__main__":
  main(True, True)