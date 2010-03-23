import os,sys

path_ldso = '/lib/ld-linux.so.2'

__doc__="""%s FILENAME

This programe takes the FILENAME (optionally with path) of a dynamically
linked executable and
(i)  finds all libraries that this file is linked to
     (using ldd) and
(ii) finds (recursively) all libraries that these libraries may link to.

It then creates a subdirectory 'libs' and copies
- all the libraries
- the dynamic linker program (%s) and
- the executable itself
into the libs directory.

Finally, it creates a shell script with name FILENAME.sh to
execute FILENAME using only the libraries in libs
(by setting the LD_LIBRARY_PATH).

In theory, this should allow to convert any dynamically linked
executable into a collection of files that can be copied to
any other linux machine (assuming it is the same architecture)
and to be executed there. (It is thus not necessary to compile
the code on the other systems.)

Fischbacher & Fangohr, August 2006""" % (sys.argv[0].upper(),path_ldso)

if len(sys.argv) < 2:
    print __doc__
    sys.exit(1)

assert os.path.exists(path_ldso),"Can't locate ld-linux; please correct location in script"

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
for lib in libs+[path_ldso]:
    os.system("cp -v %s libs" % lib) 

os.system("cp -v %s libs/%s" % (bin_name,bin_name_base))

script = "#!/bin/sh\nLD_LIBRARY_PATH=`pwd`/libs /lib/ld-linux.so.2 libs/%s\n" % (bin_name_base)

script_name = bin_name_base+".sh"

f=open(script_name,'w')
f.write(script)
f.close()
print "Run 'sh %s' to run the executable (using only libraries in ./libs)" % script_name 


    
