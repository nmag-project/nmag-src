import os,sys

__doc__="""%s FILENAME [-rpath RPATH]

This programe takes the FILENAME (optionally with path) of a dynamically
linked executable and
(i)  finds all libraries that this file is linked to
     (using ldd) and
(ii) finds (recursively) all libraries that these libraries may link to.

If the switch '-rpath' is provided, then the rpath in the executable will
be set to the value given in RPATH. (This needs to be an absolute path
and has to be the location of the 'libs' directory on the target system.)

It then creates a subdirectory 'libs' and copies all the libraries
into the libs directory.



For the executable to find its libraries, you need to set the rpath to
the location of the binaries on the new system. If this is not done by
this script (using the -rpath switch) then it needs to be done
later. Suppose the executable is called a.out, then the rpath is
changed as follows:

chrpath -r /path/to/the/libs a.out

(The chrpath executable can be obtained from the source at this page:
http://directory.fsf.org/all/chrpath.html)

Fischbacher & Fangohr, October 2006""" % (sys.argv[0].upper())

if len(sys.argv) < 2:
    print __doc__
    sys.exit(1)

def find_dynamically_linked_libraries(name, warnings_issued = {}, libs_done={}):
    """returns list with filename strings of all libraries that
    are linked to the executable/library given as 'name'

    Note: Abuse warnings_issued and libs_done arguments as
    'static variables' that don't change between different calls
    to this function.
    """

    libs_list = []

    libs=os.popen('ldd '+name).readlines()
    
    for lib in libs:
        bits = lib.split("=>")

        if len(bits)<2:
            #print "Igoring: %s" % lib,
            continue #not a library
        
        library = bits[1].split()[0]

        #check that this is a file
        if os.path.exists(library):

            #is this a new library?
            if not library in libs_done.keys():
                libs_list += [library]
                print "progress: Adding to library list '%s'" % library 

                #recursively check that this library doesn't link
                #to any other libraries
                print "checking included libs for %s..." % library
                libs_list += find_dynamically_linked_libraries(library)
                libs_done[library]=None
                print "...done (%s)" % library
        else: #file doesn't exist
            msg = "Warning: ignoring '%s'" % (library)

            #print warning messages for the same error only once
            if not msg in warnings_issued.keys():
                warnings_issued[msg]=None
                print msg

    return libs_list



assert len(sys.argv)>1,"You need to provide the name of executable"

bin_name = sys.argv[1]
bin_name_base = os.path.basename(bin_name)

libs=find_dynamically_linked_libraries(bin_name)
os.system('mkdir libs')
for lib in libs:
    os.system("cp -v %s libs" % lib) 

#os.system("cp -v %s libs/%s" % (bin_name,bin_name_base))

if len(sys.argv)>2:
    if sys.argv[2]=='-rpath': #set rpath for user
        if len(sys.argv)>=3:
            rpath = reduce(lambda a,b : a+' '+b,sys.argv[3:],'')[1:]
            print "Setting rpath in executable %s to '%s'" % (bin_name,rpath)
            ret = os.system( "chrpath %s -r %s" % (bin_name,rpath) )
            if ret != 0:
                print "Warning: error has occured in call to chrpath"
                print "         Is the executable %s writable for us?" % bin_name

print "All done."


    
