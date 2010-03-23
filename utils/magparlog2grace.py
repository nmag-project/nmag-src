import optparse,sys

def parse_args(argvs):

    doc = """There are two modes to use this program:

    (i) you only provide the filename of a magpar-log file, and one
    row for each available column will be displayed. Each row is
    preceeded by a column id (just an integer).

    (ii) you provide the filename and one or more coulmn ids. The
    program will return all the data from these colums. One line per
    line of data in the magpar-log file will be sent to stdout."""
    
    usage="usage: %prog [options] magparlogfile [C1] [C2] [C3] ...\n\n(C) University of Southampton, United Kingdom, 2006.\n"\
           +doc
    version="$Header$"

    parser = optparse.OptionParser(usage=usage,version=version)

    parser.add_option("--Ms",help="Multiply Mx, My, Mz with this number (magpar uses normalised \vec{M})",action="store",dest="Ms",type='float')

    parser.add_option("--ts",help="Multiply time by tscale (magpar uses nano seconds as unit for time)",action="store",dest="tscale",type='float')

    (options, arguments) = parser.parse_args(argvs)

    return options,arguments

def print_table_of_contents(infilename):
    lines=map( lambda a: a[:-1], open(infilename,'r').readlines())

    line1=lines[1].split() #ids
    keys=lines[2].split() #ids
    units=lines[3].split() #ids
    firstval=lines[4].split() #ids

    assert len(keys)==len(units),'number of keys and units differs. Confused.'
    assert len(keys)==len(firstval),'number of keys and values differs. Confused.'

    #data starts in lines[4]


    print "%3s -> %s %s %20s" % ('id', 'dataname'.center(10),'unit'.center(10),'first value')
    for i in range(len(keys)):
        if units[i]=='-': unit = ''
        else: unit=units[i]
        print "%3d -> %s %s %s" % (i, keys[i].center(10),unit.center(10),firstval[i])


def print_contents(infilename,col_ids_strings,options):
    lines=map( lambda a: a[:-1], open(infilename,'r').readlines())
    ids = map(int, col_ids_strings)
    
    keys=lines[2].split() #ids
    units=lines[3].split() #ids

    print "#",
    for id in ids:
        print keys[id].center(11)+" ",
    print "\n#",
    for id in ids:
        print units[id].center(11)+" ",
    print

    for line in lines[4:]:
        bits = line.split()
        for id in ids:
            value_str = bits[id]
            if options.Ms:
                if keys[id] in ['Mx','My','Mz']:
                    value_str = "%12g" % (float(bits[id])*options.Ms)

            if options.tscale:
                if keys[id] in ['time']:
                    value_str = "%12g" % (float(bits[id])*options.tscale)
                    
            print value_str,
        print
    
options,arguments = parse_args(sys.argv)

if len(arguments) == 1:
    print "Try '-h' for help"
    sys.exit(1)

infilename = arguments[1]

if len(arguments) == 2:
    print_table_of_contents(infilename)
else: #more than the filename given
    col_ids_strings = arguments[2:]
    print_contents(infilename,col_ids_strings,options)










