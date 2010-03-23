"""Given the name of a log file (run simulation with "--loglevel info2"), this program
will search for 'Memory reports' in the document and plot memory consumption against
run time."""


import sys,string

inputfilename = sys.argv[1]

lines = open(inputfilename,'r').readlines()

token = "Memory report:"
tmplines = filter( lambda line: token in line, lines)
lines=tmplines
tmplines = map( lambda line: line.split("T=")[1], lines)
lines=tmplines
tmplines = map( lambda line: line.replace('VMEM=',';;'), lines)
lines=tmplines

tmplines = map( lambda line: line.replace('KB RSS=',';;'), lines)
lines=tmplines
tmplines = map( lambda line: line.replace('KB',';;'), lines)
lines=tmplines

bitlines = map( lambda line: line.split(';;') , lines)

def tmp_convert( line ):
    #print "enterting tmp_convert. line='%s', len(line)=%d" % (line, len(line))
    time, rss, vmem, tag = line
    bits = [float(time),int(rss),int(vmem),'"'+tag.strip()+'"']
    return bits

tmpbitlines = map( tmp_convert, bitlines)

bitlines = tmpbitlines



for line in bitlines:
    for item in line:
        print item,
    print





