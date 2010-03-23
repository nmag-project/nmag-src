#!/usr/bin/env python

#-----------------------------------------------------
#
# odt_grace.py
#
# Richard Boardman, Hans Fangohr
# (c) 2002 University of Southampton
#
# Parses an OOMMF .odt file and allows selective
# output of columns. Ideal for sending to applications
# such as xmgrace
#
#-----------------------------------------------------
#
# $Header$
#
#-----------------------------------------------------

import sys, os, math, string, commands

def split_odt_header(line):
    """idea: substitue all spaces in expressions braced by curly braces"""
    status="closed"
    new_line = ""
    for i in range(len(line)):
        #keep status up to date
        if line[i]=="{":
            status="open"
        if line[i]=="}":
            status="closed"

        #do the work
        if line[i]==" " and status=="open":
            new_line = new_line + "_"
        else:
            new_line = new_line + line[i]
        
    #split line starting from second character (get rid of "#")
    split = string.split(new_line[1:])

    #and check whether first entries are labels of row rather than entries
    if split[0]=="Units:" or split[0]=="Columns:":
        return split[1:]
    else:
        return split

def split_line(line):
    return string.split(line[1:])

def hunt_normalisation():
    # first attempt... use the name of the odt file without extension
    # as a seed for hunting an omf file
    odt_file_root = sys.argv[1].split('.')
    position = 0
    #print odt_file_root[0]
    # execute a system ls command
    #print os.execv("ls", ["", ""])
    candidates = commands.getoutput("ls -r --sort=time " + odt_file_root[0] + "*.omf")
    candidates_list = candidates.split('\n')
    candidates_list.sort() # sort again because ls is time only...
    #print candidates_list
    candidates = candidates_list[0]
    if candidates.split(" ")[0] == 'ls:':
        # assume error
        # then go onto the next test
        pass
    else:
        # snaffle and read
        # return results
        print "# normalising from", candidates.split('\n')[position]
        return snaffle_magmag(candidates.split('\n')[position])
    # try any omf lying around the place...
    odt_file_root = odt_file_root[0].split('/')
    candidates = commands.getoutput("ls -r --sort=time " + odt_file_root[0] + "/*.omf")
    candidates_list = candidates.split('\n')
    candidates_list.sort()
    candidates = candidates_list[0]
    if candidates.split(" ")[0] == 'ls:':
        # assume error, so return 1
        print "# normalising against nothing"
        return 1
    else:
        # run with it        
        #print snaffle_magmag(candidates.split(' ')[0])
        print "# normalising from", candidates.split('\n')[position]
        return snaffle_magmag(candidates.split('\n')[position])
    #print candidates

def snaffle_magmag(input_file):
    results = float(commands.getoutput("ovf2vtk " + input_file + " " + input_file + ".tmp | grep magmag | awk '{print $6}'"))
    rem_res = commands.getoutput("rm " + input_file + ".tmp.vtk")
    return results
    #print results.split("\n")

# main program here

if len(sys.argv) < 2:
    print "I would really like some command-line arguments"
    print ""
    print "For example, use:"
    print ""
    print "   ", sys.argv[0], "myfile.odt"
    print ""
    print "to list the columns in a numbered fashion"
    print ""
    print "If you know the columns you want, you can use:"
    print ""
    print "   ", sys.argv[0], "myfile.odt 1 3 8 5"
    print ""
    print "or whatever the column numbers are to output these values to stdout"
    print ""
    print "Note that any number of columns can be used."
    print "There is also another easier way to use", sys.argv[0]
    print ""
    print "   ", sys.argv[0], "myfile.odt Simulation_time Mx My Mz"
    print ""
    print "For each name given (Simulation_time, Mx, ...) the heading"
    print "which contains it will be selected."

    sys.exit(1)

fin_name = sys.argv[1]
#fout_name = sys.argv[2]

#1st open data file File_IN fin
fin = open(fin_name,"r");

#read all files
inlines = fin.readlines()

# grab a tester...
test_line = split_odt_header(inlines[5])

# grab the headers
heads = split_odt_header(inlines[3])

# grab the units
units = split_odt_header(inlines[4])

should_normalise = 0 # set to 0 to turn normalisation off

if len(sys.argv) == 1+1:
    print "There are", len(heads), "headings and", len(test_line), "value entries per line -", len(inlines), "lines in total; examples and units given in following text:"
    # output headers
    for i in range(len(heads)):
        print i, ":", heads[i], test_line[i], units[i]
    # hunt for normalisations
    # hunt_normalisation()

columns_to_output = []

if len(sys.argv) > 1+1:
    # MF addition: numbers are not always the same!
    # It is just annoying to do every time:
    #   odt_grace.py file.odt,
    #   look at the right numbers
    #   odt_grace.py file.odt number1 number2 etc.
    # I want something easier:
    #   odt_grace.py file.odt Simulation_time Mx My Mz
    def parse_column_specification(specification):
        if specification.isdigit():
            return float(specification)
        else:
            num_matches = 0
            for i in range(len(heads)):
                if specification in heads[i]:
                    column = i
                    num_matches += 1
            if num_matches == 1: return column
            if num_matches < 1:
                print "Cannot find any heading containing the string '%s'!" % specification
                sys.exit(1)
            else:
                print "The following headings contain the string '%s':" % specification
                for i in range(len(heads)):
                    if specification in heads[i]:
                        print "%d: %s" % (i, heads[i])
                sys.exit(0)

    # hunt for normalisations
    normalisation = 0
    if should_normalise == 1:
        normalisation = hunt_normalisation()
    print "#", normalisation, "value for normalisation"
    # steal the remaining command-line args
    print "#",
    for arg in range(len(sys.argv)-2):
        columns_to_output.append(parse_column_specification(sys.argv[arg+2]))
        print heads[int(columns_to_output[arg])],
        
    print ""
    for i in range(len(inlines)-1):
        # output x, y1, y2, y3 ... yn                    
        if i > 4:
            this_line = split_odt_header(inlines[i])
            for j in range(len(columns_to_output)):
                print this_line[int(columns_to_output[j])],
            print ""
            
#-----------------------------------------------------
#
# $Log$
# Revision 1.3  2007/02/28 19:24:40  mf
# Improvement.
#
# Revision 1.2  2007-02-08 13:02:47  mf
# This version of odt_grace.py can recognize part of the field names.
# This allows to write more robust scripts/makefiles to extract data.
#
# Revision 1.1  2006-10-04 15:59:33  fangohr
# More work on demag test.
#
# Revision 1.3  2006-10-04 10:40:49  fangohr
# Moved negative sign of exchange field diff op from user provided constant
# A to diff-op string.
#
# If negative A is used, an exception will be thrown.
#
# Revision 1.2  2006-09-29 15:45:28  fangohr
# Increased damping to 0.5 (from 0.2) to obtain results quicker ...
# (also removed Zeeman field from exchange.mif)
#
# Revision 1.1  2006-09-26 16:10:09  fangohr
# First attemp to teach OOMMF about a exchange-only simulation. Notworking
# yet: magnetisation points everywhere in the same (bizarre) direction.
#
# Revision 1.2  2003-03-05 13:42:50  rpb01r
# Added support for normalisation imported from ovf2vtk v1.8+ - this can be overwritten by altering a flag in the odt_grace.py file
#
# Revision 1.1  2003/02/08 14:38:10  rpb01r
# Parses an OOMMF .odt file and allows selective output of columns. Ideal for sending to applications such as xmgrace
#
#
#-----------------------------------------------------
