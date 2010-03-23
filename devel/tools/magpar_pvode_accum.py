"""Given the name of a pvode log file produced by magpar (extension is
.log_pvode, can be ommitted when calling this script), this script
creates a new file (extension is .log_pvode.corrected) which
accumulates the entries in the columns. Magpar starts counting from
zero again every now and then (which I expect happens when the |m|
becomes too large, and they re-initialise the timestepper). Hans
14/12/2007
"""

import sys

filename = sys.argv[1]

if filename[-len(".log_pvode"):] != ".log_pvode":
    filename = filename + ".log_pvode"



print "Reading %s" % filename
lines = open(filename,'r').readlines()

print "Processing %s" % filename
data = map(lambda a : map(float,a.split()),lines[4:])

data_corrected = [data[0]]

for k in range(1,len(data)):
    this_line = []
    for i in range(len(data[k])):
        if data[k][i] < data[k-1][i]: #this is a jump
            this_line.append(data_corrected[k-1][i] + data[k][i])
        else:
            this_line.append(data_corrected[k-1][i] + data[k][i]-data[k-1][i])
    data_corrected.append(this_line)


outfilename= filename+'.corrected'
print "Writing %s" % outfilename

f=open(outfilename,'w')
map(f.write,lines[0:3])
for line in data_corrected:
    f.write('%16f   ' % line[0])
    for i in range(1,len(line)):
        f.write('%4d ' % line[i])
    f.write('\n')
f.close()
        


