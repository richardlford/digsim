#!/usr/bin/python3
import os
from pprint import pprint as pp

# cwd = list(os.path.split(os.getcwd()))[1]
# pp(cwd)
cwd = "build01"
canon = []
with open("../../f77/" + cwd + "/output.dat","r") as ifile:
    for line in ifile:
        canon.append(tuple(map(float,line.split())))
belief = []
with open("output.dat","r") as ifile:
    for line in ifile:
        belief.append(tuple(map(float,line.split())))

diffs = []
for i,line in enumerate(canon):
    for j,x in enumerate(line):
        diff = round(x - belief[i][j],4)
        if diff > 0.0:
            diffs.append((i+1,j+1,diff))
if len(diffs):
    print("Differences")
    pp(diffs)
else:
    print("No Differences")

