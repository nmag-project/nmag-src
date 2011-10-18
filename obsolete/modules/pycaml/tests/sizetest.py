import ocaml

import time

starttime = None

def tic():
    global starttime
    starttime = time.time()

def toc():
    runtime = time.time()-starttime
    print "--> %5.2f seconds" % runtime
    return runtime


n = 100000

print "This is the overall problem:"

print "Create %d floats in Python" % n,;tic(); a=map(float,range(n));toc()

print "Create %d floats in Ocaml" % n,; tic (); a=ocaml.float_array(n); toc()

print "Create %d (int,float) in Ocaml" % n,; tic (); a=ocaml.intfloattuple_array(n); toc()


print "Now gather data more systematically..."

data = []

for n in [10,100,1000,int(1e5),int(2e5),int(3e5),int(4e5),int(5e5)]:
    print
    print "Working on n=%d..." % n
    datarow = []
    datarow.append(n)
    tic();  a=map(float,range(n))  ;datarow.append(toc())
    tic (); a=ocaml.float_array(n) ; datarow.append(toc())
    tic (); a=ocaml.intfloattuple_array(n); datarow.append(toc())
    data.append(datarow)

f=open('sizetestresults.txt','w')
for row in data:
    f.write("%d " % row[0])
    for item in row[1:]:
        f.write("%f " % item)
    f.write("\n")
f.close()
    
