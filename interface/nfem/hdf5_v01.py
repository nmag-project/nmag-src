"""hdf5.py

TODO:

-document me

-split into different files for different fileformats?


"""

__docformat__='restructuredtext'

import os
import types
import string
import logging

import numpy

import nfem.fields
from nfem_exceptions import NfemUserError
import nsim.snippets
from nsim.si_units import SI
import nsim.timings
import nmesh

import ocaml


log = logging.getLogger('nfem')


def pytables_version():
    import tables
    version_string = tables.__version__
    return map(int,version_string.split('.'))

def pytables_master_version():
    return pytables_version()[0]


def pytables_file_format_version(fh):

    file_format_version = fh.root._v_attrs.PYTABLES_FORMAT_VERSION

    log.debug("Pytables version of file %s is %s" % (fh.filename,file_format_version))

    return map(int,file_format_version.split('.'))


def importtables():
    version = '1.3.2'
    try:
        import tables
    except ImportError:
        msg = "You need to install pytables http://www.pytables.org/ (version %s or above)." % version
        raise ImportError,msg

    if tables.__version__ < version:
        raise ImportError("Have found pytables version %s but we need "
                          "version %s or above." % (tables.__version__,
                                                    version))

    log.debug("Using pytables version %s" % tables.__version__)

    return tables


def my_tables_createCArray(fileh, where, name, shape, AtomClass, filters, title):
    """There has been a change in the naming conventions in pytables when going from
    version 1.0 to version 2.0
    (see http://www.pytables.org/moin/ReleaseNotes/Migrating%20To%202.x),
    so we need wrapper functions like this one"""

    if pytables_master_version() == 1:
        atom = AtomClass(shape = shape)
        ca = fileh.createCArray(where, name, shape, atom,
                                filters=filters, title=title)
    else:
        atom = AtomClass()
        ca = fileh.createCArray(where, name, atom, shape,
                                filters=filters, title=title)
    return ca

def my_tables_import_filenode():
    """There has been a change in the naming conventions in pytables when going from
    version 1.0 to version 2.0
    (see http://www.pytables.org/moin/ReleaseNotes/Migrating%20To%202.x)
    """
    if pytables_master_version() == 1:
        from tables.nodes import FileNode
        return FileNode
    else:
        from tables.nodes import filenode
        return filenode

tables = importtables()
FileNode = my_tables_import_filenode()




#The h5 file keeps the spatially resolved fields, and for every time step we save any field, we also
#save all the average data. To avoid computing this twice (for the ndt file, say), we always check
#whether the average data is in the h5 file already, before we append it.
#
#To do this more efficiently, we keep a list of time steps in that file in the following global
#list variable:
timesteps_in_h5_file = []
#This reduces the runtime of the bigbar example by about 2 percent. Hans 25/07/2007

#We have also tried not compressing the data -- this has virtually no effect on the speed with which
#we write.


#When creating (compressed) tables, we need to declare how many characters
#at most we'd like to store in each column. We err on the large side as
#compressed zero-bytes don't take any space. However, if necessary, this number
#can be changed here:
c_dofnamelength = 128
c_fieldnamelength = c_dofnamelength
c_unitstringlength = 128
c_stringvalue = 128  #for example localtime string
c_dof_maxind = 64



"""TODO:

-unify metada
 - use the same table for averages and fieds

(-add metadata as required:
 - add field whenever we see it the first time)

 - have retrieve function to take last configuration


"""

def tagfile(f,filetype,version):
    """Add 'tag' in /etc/filetype and /etc/fileversion.

    We use this to identify the different types of h5 files we have.

    So far these are

    nmesh (only the mesh: points, simplices, simplex regions are compulsory)

    nsimdata (several time dependend fields+plus nmesh data structures)

    """

    version = str(version)
    filetype= str(filetype)

    if type(f) == types.StringType: #filename being given:
        fh = open_pytables_file(f,'a')
    else:
        fh = f

    if not hasattr(fh.root,'etc'):
        etcgroup = fh.createGroup("/", 'etc', 'Configuration and version data')

    if not hasattr(fh.root.etc,'filetype'):
        fh.createArray("/etc", 'filetype', [filetype],title='data file type')
        log.debug("Have tagged file %s with filetype '%s'" % (fh.filename,filetype))
    else:
        checktag_filetype(f,filetype)

    if not hasattr(fh.root.etc,'fileversion'):
        fh.createArray("/etc", 'fileversion', [version],title='data file type version')
        log.debug("Have tagged file %s with version %s" % (fh.filename,version))
    else:
        checktag_version(f,version)

    if type(f) == types.StringType: #was given string, need to close file
        close_pytables_file(fh)


def checktag_version(f,version):
    version = str(version)
    version_okay = False
    fileversion = f.root.etc.fileversion[0]
    log.debug("checktag_version, filename=%s, file version=%s, required version=%s" % (f.filename,fileversion,version))
    if fileversion == version:
        version_okay = True
    else:
        version_okay = False
        #Delegate error reporting to calling level
        #raise NfemUserError,"File is of version %s but you try to use with code for version %s" % (f.root.etc.fileversion[0],version)
    return version_okay

def checktag_filetype(f,filetype):
    filetype = str(filetype)
    filetype_okay = False
    filefiletype=f.root.etc.filetype[0]
    log.debug("checktag_filetype, filename=%s, filetype=%s, required filetype=%s" % (f.filename,filefiletype,filetype))
    if  filefiletype == filetype:
        filetype_okay = True
    else:
        filetype_okay = False
        #raise NfemUserError,"File %s is of type '%s' but you try to use it as '%s'" % (f.filename,f.root.etc.filetype[0],filetype)

    return filetype_okay

def checktag(f,filetype,version,alt=[]):
    """alt can be a list of 2-tuples that provide alternative combinations of filetype and version that will be accepted"""

    types_versions = [(filetype,version)]+alt

    okay = [False]*len(types_versions)

    okay[0] =checktag_version(f,version) and checktag_filetype(f,filetype)

    for i,type_version in enumerate( types_versions ):
        filetype,version = type_version
        okay[i] = checktag_version(f,version) and checktag_filetype(f,filetype)

    if True in okay:
        return okay.index(True)
    else:
        msg =  "\n\tFile %s is of type '%s' and version '%s'" % (f.filename,f.root.etc.filetype[0],f.root.etc.fileversion[0])
        msg += "\n\tTypes that your reading code might accept are '%s'" % map( lambda a:a[0], types_versions)
        msg += "\n\tVersions that your reading code might accept are '%s'" % map( lambda a:a[1], types_versions)
        raise NfemUserError,msg

def insert_file(h5fh,where,name,title,filename):
    fnode=FileNode.newNode(h5fh,where=where,name=name,title=title)
    fnode.write(open(filename,'r').read())

    import time
    fnode.attrs.creationdate=time.asctime()
    fnode.attrs.path=os.path.abspath(filename)
    import socket
    fnode.attrs.host=socket.getfqdn()

    fnode.close()



##################################################

_pytables_open_file_dict = {}

def open_pytables_file(filename,mode='r'):

    if not filename in _pytables_open_file_dict:
        _pytables_open_file_dict[filename] = 0 #currently open 0 times

    cur_open = _pytables_open_file_dict[filename]

    log.log(15,"PYTABLES: About to open file %s in mode '%s', currently open %d times" % (filename,mode,cur_open))
    callers = nsim.snippets.get_callers_string().split('\n')[1:]
    log.debug("PYTABLES: open_pytables_file caller stack:\n" + "\n".join(callers))

    if cur_open > 0:
        log.warn("PYTABLES: Warning: file '%s' has been opened more than once" % filename)

    fh = tables.openFile(filename,mode)

    _pytables_open_file_dict[filename] += 1

    return fh


def close_pytables_file(fh):
    filename = fh.filename
    _pytables_open_file_dict[filename] -= 1

    log.log(15,"PYTABLES: Closing file '%s'" % filename)
    fh.close()

    still_open = _pytables_open_file_dict[filename]

    if still_open != 0:
        print "PYTABLES: Warning: file %s has been opened another %d times" % (filename,still_open)


##################################################

def create_file(filename,mode='w'):
    f=open_pytables_file(filename,mode)
    close_pytables_file(f)

def save_simulation_files(filename,files_to_include=[]):
    f=open_pytables_file(filename,'r+')
    f.createGroup(f.root,'sim',title='Simulation code and configuration files')
    for fname in files_to_include:
        fname=fname
        insert_file(f,where="/sim",name=fname,\
                    title="The content of file '%s'" % fname,\
                    filename=fname)

    f.createGroup(f.root,'log',title='Log files')
    close_pytables_file(f)



def fieldname_by_subfieldname(fh, subfieldname):
    metadata = fh.root.etc.metadatafields.cols[:]

    subfieldnames = map(lambda a : a [0], metadata)

    if not subfieldname in subfieldnames:
        raise NameError,"No subfield '%s' in file '%s'. "\
              "Know subfields are '%s'" % \
              (subfieldname,fh.filename,subfieldnames)

    fieldnames = map(lambda a : a [1], metadata)

    fieldname = fieldnames[ subfieldnames.index(subfieldname) ]

    return fieldname

def get_row(f,fieldname,id):
    """For a given field name and id, return the row"""
    ids = map(int,f.getNode(f.root.data.fields,fieldname).col('id').tolist())
    return ids.index(id)

def get_rows(f, fieldname):
    """For a given field name, return an array rs which can be indexed in the
    following way: 'rs[i]' is the row corresponding to id i."""
    return map(int, f.getNode(f.root.data.fields,fieldname).col('id').tolist())

def get_ids_for_field(f,fieldname):
    log.info("Getting ids for field %s" % fieldname)
    return f.getNode(f.root.data.fields,fieldname).col('id')

def get_names_of_saved_fields(f):
    fieldnames=[]
    node = f.root.data.fields
    for field in node:
        fieldnames.append(field.name)
    return fieldnames

def unique(mylist):
    unique_list = []
    for elem in mylist:
        if elem in unique_list:
            pass
        else:
            unique_list.append(elem)
    return unique_list


def get_saved_ids_by_fieldname(f):
    fieldlist = get_names_of_saved_fields(f)

    saved_ids_by_fieldname = {}

    for field in fieldlist:
        saved_ids_by_fieldname[field] = get_ids_for_field(f,field)

    return saved_ids_by_fieldname

def get_saved_fields_by_id(f):
    fieldlist = get_names_of_saved_fields(f)

    saved_ids_by_fieldname = {}

    all_ids = []

    for field in fieldlist:
        saved_ids_by_fieldname[field] = get_ids_for_field(f,field)
        #also record these ids for the global id list
        all_ids = all_ids + saved_ids_by_fieldname[field].tolist()

    #make id list unique
    all_ids = unique(all_ids)
    all_ids.sort()

    fields_by_id = {}

    #go through all ids and assemble associated list of fields
    for id in all_ids:
        fields_by_id[id]=[]
        for fieldname in fieldlist:
            if id in saved_ids_by_fieldname[fieldname]:
                fields_by_id[id].append(fieldname)

    debug_msg = ""
    for id in all_ids:
        debug_msg +="\nid=%d, fields=%s" % (id,fields_by_id[id])

    log.debug("get_saved_fields_by_id: %s" % debug_msg)

    return fields_by_id






def tables_floatCol(floattype):

    if floattype == '64':
        return tables.Float64Col
    elif floattype == '32':
        return tables.Float32Col
    else:
        raise UserError,"only know float types '32' and '64' but got '%s' from you" % floattype

def tables_floatAtom(floattype):

    if floattype == '64':
        return tables.Float64Atom
    elif floattype == '32':
        return tables.Float32Atom
    else:
        raise UserError,"only know float types '32' and '64' but got '%s' from you" % floattype

#----------------------------------------------------------------------

def _get_data_index_by_dofsite(h5fh,dofname):
    """returns dictionary with dofsite as key, and integer index i as value.
    The dofsite is stored at the i-th column in the table for this dof."""

    checktag(h5fh,'nsimdata','0.1')

    table = h5fh.getNode('/mesh','dofsites')

    numpy_indices = table[0].field(dofname)

    indices=map(tuple,numpy_indices.tolist())
    data_index_by_site = dict( map(None,indices,range(len(indices))))
    return data_index_by_site

def _get_dofsite_by_index(h5fh,dofname):
    """because index is just range(len(dofsites)), this is a list"""

    checktag(h5fh,'nsimdata','0.1')

    table = h5fh.getNode('/mesh','dofsites')

    #from IPython.Shell import IPShellEmbed
    #ipshell = IPShellEmbed([])
    #ipshell()




    if pytables_master_version() == 1:
        numpy_indices = table[0].field(dofname)
    else:
        numpy_indices = table.read(start=0,stop=1,field=dofname)[0]



    return numpy_indices

    #indices=map(tuple,numpy_indices.tolist())
    #return indices

def get_su_units(f):
    """Get the SimulationUnits object from file f"""
    checktag(f,'nsimdata','0.1')

    from nsim.su_units import SimulationUnits
    su=eval(f.root.etc.metadatafields.attrs.simulationunits_repr)
    return su

def get_units_by_dofname(f):
    checktag(f,'nsimdata','0.1')
    table = f.root.etc.metadatafields

    # Pytables 2.0 seems to return a 128 character string (because that
    # is the full length of the string),i.e. "E_Anis_Py [snip]", rather
    # than only the non-white space characters,i.e. "E_Anis_Py"). We
    # thus strip the white space off from the right.
    mydict = dict([(x['dofname'].rstrip(), x['unit'])
                   for x in table.iterrows()])
    return mydict

def get_time_unit(f):
    checktag(f,'nsimdata','0.1')

    #we always store seconds, so don't need to actually have that in the file
    return SI(1,'s')

def get_mesh_unit(f):
    """Retrieve the units in which the mesh coordinates are expressed."""
    checktag(f,'nsimdata','0.1')
    si_conversion_factor = f.root.mesh.scale.sifactor.read()
    return SI(si_conversion_factor,'m')

def get_maxind_by_dofname(f):
    checktag(f,'nsimdata','0.1')
    table = f.root.etc.metadatafields

    #Pytables 2.0 seems to return a 128 character string (because that
    #is the full length of the string),i.e. "E_Anis_Py [snip]", rather
    #than only the non-white space characters,i.e. "E_Anis_Py"). We
    #thus strip the white space off from the right.

    mydict = dict( [ (string.rstrip(x['dofname']),string.rstrip(x['maxind'])) for x in table.iterrows()] )

    #from IPython.Shell import IPShellEmbed
    #ipshell = IPShellEmbed([])
    #ipshell()

    return mydict

def get_available_fields(f):
    checktag(f,'nsimdata','0.1')
    fieldnames = f.root.etc.metadatafields[:].field("fieldname").tolist()
    dofnames = f.root.etc.metadatafields[:].field("dofname").tolist()
    return fieldnames,dofnames


timer1 = nsim.timings.Timer('hdf5: append averages')

append_averages_timings = {}
append_averages_timings['all'] = 0
append_averages_timings['populate_row'] = 0
append_averages_timings['append_and_flush'] = 0
append_averages_timings['append'] = 0
append_averages_timings['table_flush'] = 0
append_averages_timings['file_flush'] = 0
append_averages_timings['file_close'] = 0
append_averages_timings['file_open'] = 0
append_averages_timings['create_meta_data_and_table'] = 0

def append_averages(filename,names,values,units,floattype='32'):

    timer1.start('all')

    timer1.start('file_open')
    f=open_pytables_file(filename,'r+')
    timer1.stop('file_open')

    checktag(f,'nsimdata','0.1')

    if hasattr(f.root.etc,'metadataaverages'):
        #Check that fields provided are already known.
        known_names = f.root.etc.metadataaverages.cols.name[:]

        for name in names:
            if not name in known_names:
                log.error( "Known names are %s" % str(known_names))
                log.error( "Known names for averaged data are %s" % known_names)
                raise NfemUserError, "This file (%s) has been created without '%s'. It cannot be added now." % (filename,name)

    else: #create metadata table


        timer1.start('create_meta_data_and_table')

        #Create table with metadataaverages, keeping the names of the doftypes,
        #fields, the units and maxind data
        metadataaveragestablestruct = {}
        metadataaveragestablestruct['name'] = tables.StringCol(c_fieldnamelength)
        metadataaveragestablestruct['unit'] = tables.StringCol(c_unitstringlength)

        #create table
        myfilter = tables.Filters(complib='zlib',complevel=5)
        table = f.createTable(f.root.etc,'metadataaverages',metadataaveragestablestruct,\
                              title='Metadataaverages on fields and dofs', filters=myfilter)
        table.flush()


        #Now populate this, one row per subfield name
        row=table.row

        for name,unit in map(None,names,units):
            row['name']=name
            row['unit']=repr(unit)

            if type(unit) == types.StringType: #i.e. not SI object but, eg, 'deg'
                pass
            elif unit.value != 1.0:
                log.warn("You are using '%s' as the unit for average %s values" % (name,str(unit)))
                log.warn("We'll let you, but you deviate from the convention")
                log.warn("to save all data in SI units.")
                raise ValueError,"Tried to save data with non-SI unit. Stopping for now"
            row.append()
        table.flush()

        timer1.stop('create_meta_data_and_table')
    #now we have added the metadata


    #check whether the right subgroups exit already
    if not hasattr(f.root,'data'):
        timer1.start('create_meta_data_and_table')
        datanode = f.createGroup('/','data','Simulation data')
        log.debug("Adding /data")
        timer1.stop('create_meta_data_and_table')

    if not hasattr(f.root.data,'averages'):
        #fieldsnode = f.createGroup('/data','averages','nsim fields spactially averaged')
        log.debug("Adding /data/averages")

        timer1.start('create_meta_data_and_table')
        tablestruct = {}

        #create table
        for name,unit,value in map(None,names,units,values):
            #create table if necessary

            log.debug("Creating /data/averages-table. Have "+\
                      "received name=%s and type=%s (value=%s)"\
                      % (name,str(type(value)),str(value)))

            if type(value) == types.FloatType:
                tablestruct[name]= tables_floatCol('64')(shape=1)
            elif type(value) == types.IntType:
                tablestruct[name]= tables.Int64Col()
            elif type(value) == types.StringType:
                tablestruct[name]=tables.StringCol(c_stringvalue)
            elif type(value) == types.ListType:
                nd_value = numpy.array(value)
                tablestruct[name]=tables_floatCol('64')(shape=nd_value.shape)
            else:
                msg = "name = %s has value %s which is of type %s" % \
                      (name,str(value),str(type(value)))
                msg +="\n Don't know how to handle this type"
                raise ValueError,msg

        myfilter = tables.Filters(complib='zlib',complevel=5)

        #from IPython.Shell import IPShellEmbed
        #ipshell = IPShellEmbed([])
        #ipshell()

        table = f.createTable(f.root.data,'averages',tablestruct,\
                              title='Spatial averages of fields', filters=myfilter)

        table.flush()

        timer1.stop('create_meta_data_and_table')

    #Now populate this, one row per dof

    timer1.start('populate_row')

    table = f.getNode(f.root.data,'averages')

    row=table.row

    step = None

    for name,value,unit in map(None,names,values,units):
        log.debug("About to add %s with value %s to /data/averages in %s" %\
                  (name,str(value),f.filename))
        row[name]=value

	#find step
	if name=='step':
	    step = value

    timer1.stop('populate_row')

    timer1.start('append_and_flush')

    timer1.start('append')
    row.append()
    timer1.stop('append')

    timer1.start('table_flush')
    table.flush()
    timer1.stop('table_flush')

    if step == None:
	raise StandardError,"Internal Error: 'step' was not in names. Names are %s" % names
    timesteps_in_h5_file.append(step)

    timer1.start('file_flush')
    f.flush()
    timer1.stop('file_flush')

    timer1.stop('append_and_flush')

    timer1.start('file_close')
    close_pytables_file(f)
    timer1.stop('file_close')

    timer1.stop('all')


# Timings for writing field data to hdf5 files
timer2 = nsim.timings.Timer("hdf5: append fields")

def append_fields(filename,fields_by_name,all_fields_by_name,time_si,step,stage,id,simulation_units,floattype='32'):
    """The field provided here is a raw-ocaml field, the filename is the filename
    of the hdf5 file.

    We check wether we have
    - the _dat.hd file. If not, create and populate with metadata:
       - dofsites for all fields (for this we need the all_fields entry)
       - shapes, fields and units for all subfields

    - the mesh (if not, we stop)

    - the sites of the dof (important for higher order basis functions)
      - if not, we'll save those

         (difficulty: to get good compression, I need to have all sites in the same table.
         This means I need to create the table completely, before I write the first data to it.)

    - the table for the field
      - if not, create

    - then append field data to table

    - we write the fields in SI units

    - positions (in the mesh data) are written in simulation units (add length scale to this,TODO Hans (fangohr 27/03/2007))

    :Parameters:

      `filename` : string
        The filename to which the field(s) should be appended

      `fields_by_name` : dictionary
        keys are field names, the values are tuples of (raw-ocaml
        field,fieldunit). This is the list of fields that we are meant
        to save.

      `all_fields_by_name` : dictionary
        keys are field names, the values are tuples of (raw-ocaml
        field,fieldunit). This is the list of all fields of the
        simulation. It will only be used to save the metadata for all
        fields when the file is created. This is new (fangohr
        28/05/2008), and alllows to add any fields later on in any
        order.

      `time_si` : SI object
        SI object representing the current simulation time

      `step` : int (or long int)
        The current step number

      `stage` : int
        The current stage (for hysteris loop)

      `simulation_units`: Simulation Units object
        To store the simulations unit used for this run

      `floattype`: string
        Either ``32`` (default) or ``64`` depending on how accurate the data
        should be stored in the hdf5 file.

    :Returns:

       `None`

    """
    timer2.start('all')

    myfloatCol = tables_floatCol(floattype)


    #if step==2160 or True:
    #    print "Next line might fail, step=%d" % step
    #    import nmag
    #    nmag.ipython()

    timer2.start('f_open')
    f = open_pytables_file(filename, 'r+')
    timer2.stop('f_open')

    timer2.start('tagfile')
    tagfile(f,'nsimdata','0.1')
    timer2.stop('tagfile')

    if not hasattr(f.root,'mesh'):
        raise IOError,"hdf5 file '%s' doesn't have mesh. Refuse to add data" % filename

    if hasattr(f.root.etc,'metadatafields'):
        timer2.start('get_metadata_from_file')

        #Check that fields provided are already known.
        known_dofnames = f.root.etc.metadatafields.cols.dofname[:]
        known_fields =  f.root.etc.metadatafields.cols.fieldname[:]

        for fieldname in fields_by_name.keys():
            if not fieldname in known_fields:
                log.error( "Known fields are %s" % known_fields)
                log.error( "Known dofs are %s" % known_dofnames)
                raise NfemUserError, "This file (%s) has been created without field '%s'. It cannot be added now. (Hint: if this is bothering you, try to save all fields in the beginning of the simulation.)" % (filename,fieldname)

        timer2.stop('get_metadata_from_file')

    else: #create metadatafields entry

        #This table keeps has entries of the type::

        #  In [7]: f.root.etc.metadatafields[0]
        #  Out[7]: ('E_demag_Co', 'E_demag', '[]', "SI(1,['m',-1.0,'kg',1.0,'s',-2.0])")
        #
        #  In [8]: f.root.etc.metadatafields[1]
        #  Out[8]: ('E_demag_Py', 'E_demag', '[]', "SI(1,['m',-1.0,'kg',1.0,'s',-2.0])")
        #
        #  In [9]: f.root.etc.metadatafields[2]
        #  Out[9]: ('phi', 'phi', '[]', "SI(1,['A',1.0])")

        #so we have easy access to the subfields, their fields, shape, and SI base units.

        timer2.start('write_metadata_to_file')

        #Create table with metadatafields, keeping the names of the doftypes,
        #fields, the units and maxind data
        metadatafieldstablestruct = {}
        metadatafieldstablestruct['fieldname'] = tables.StringCol(c_fieldnamelength)
        metadatafieldstablestruct['dofname'] = tables.StringCol(c_dofnamelength)
        metadatafieldstablestruct['unit'] = tables.StringCol(c_unitstringlength)
        metadatafieldstablestruct['maxind'] = tables.StringCol(c_dof_maxind)

        #create table
        myfilter = tables.Filters(complib='zlib',complevel=5)
        table = f.createTable(f.root.etc,'metadatafields',metadatafieldstablestruct,\
                              title='Metadatafields on fields and dofs', filters=myfilter)

        table.attrs.simulationunits_str = str(simulation_units)
        table.attrs.simulationunits_repr = repr(simulation_units)

        table.flush()


        #Now populate this, one row per dof
        row=table.row

        log.debug("append_field(): Creating table root/etc/metadatafieldswith")

        for fieldname,(field,units) in all_fields_by_name.items():

            #get dofs and maxinds:
            dofs_maxind = nfem.data_doftypes(field)
            log.log(15, "subfields are %s"  % dofs_maxind)
            for subfieldname,shape in dofs_maxind:
                row['fieldname']=fieldname
                row['dofname']=subfieldname
                row['unit']=repr(units)
                row['maxind']=repr(shape)
                row.append()
                log.debug("append_field(): Adding row for subfieldname %s, shape=%s, unit=%s" %\
                          (subfieldname, str(shape), str(units)))
        table.flush()

        timer2.stop('write_metadata_to_file')


    #Each dof can be defined on a particular subset of the mesh nodes
    #or (for higher order basis functions, at intermediate
    #positions. These are encoded as (1,), (2,) etc for first order
    #shape functions, and (1,1), (1,2), (2,2), ... for second order
    #functions. We call those tuples 'dofsites'.
    #
    #For each dof, we therefore need to make sure that when we store
    #actual data (say a vector) in column i in the table for this
    #field, that we know to which dofsite this belongs.
    #
    #We thus need a mapping from dofsite to i.
    #
    #We have furthermore no guarantee that the data in the python-dof
    #object comes in any particular order. We thus order each dof so
    #that the data in the h5-table for that field corresponds to that
    #of the dofsites in /mesh/dofsites (i.e. we use the mapping
    #i->dofsites here).
    #
    #In other words, for each dof with n data, i is increasing from 0
    #to n-1 (for every timestep=row).  /mesh/dofsites stores a mapping
    #of i to dofsite tuples.
    #

    #do we know already which dof is defined on which sites?
    if not hasattr(f.root.mesh,'dofsites'):
        timer2.start('write_dofsites_to_file')

        #Create table with dofsites
        tablestruct = {}

        log.debug("Creating table struct for sites")

        for fieldname,(field,unit) in all_fields_by_name.items():
            #pyfield = pyfields_by_name[fieldname]
            for subfieldname in nfem.subfieldnames(field):
                sites, pos, shape, sitevols = ocaml.mwe_subfield_metadata(field,subfieldname)

                log.debug("Adding column for subfield %s" % repr(subfieldname))

                #for subfieldname,dof in map(None,pyfield.dof_names,pyfield.dofs):
                #    sites = numpy.array(dof.site)
                #    tablestruct[subfieldname]= tables.Int32Atom(shape=sites.shape)
                sites = numpy.array(sites)
                if pytables_master_version() == 1:
                    tablestruct[subfieldname]= tables.Int32Atom(shape=sites.shape)
                else:
                    tablestruct[subfieldname]= tables.Int32Col(shape=sites.shape)


        log.debug("table struct for sites is %s" % repr(tablestruct))

        myfilter = tables.Filters(complib='zlib',complevel=5)

        table = f.createTable(f.root.mesh, 'dofsites',tablestruct,\
                              title='Sites for all degrees of freedom', filters=myfilter)

        table.flush()

        #this table has only one row, populate it now
        row=table.row

        #Write data to table
        for fieldname,(field,unit) in fields_by_name.items():
            #pyfield = pyfields_by_name[fieldname]
            for subfieldname in nfem.subfieldnames(field):
                log.debug("Adding dofsite data for subfield %s" % subfieldname)
                sites, pos, shape, sitevols = ocaml.mwe_subfield_metadata(field,subfieldname)

                dofsites = numpy.array(sites)
                row[subfieldname]=dofsites

        row.append()
        table.flush()
        log.debug("Written dof site table")

        timer2.stop('write_dofsites_to_file')


    #check whether the right subgroups exit already
    if not hasattr(f.root,'data'):
        datanode = f.createGroup('/','data','Simulation data')
        log.debug("Adding /data")

    if not hasattr(f.root.data,'fields'):
        fieldsnode = f.createGroup('/data','fields','nsim fields')
        log.debug("Adding /data/fields")

    #Now create arrays for fields, and save data for fields
    for fieldname,(field,unit) in fields_by_name.items():

        timer2.start('create_field_table_has_field')
        has_field = hasattr(f.root.data.fields,fieldname)
        timer2.stop('create_field_table_has_field')

        #create table if necessary
        if not has_field:
            timer2.start('create_field_table')
            log.debug("Table /root/data/fields/%s missing, will create now" % fieldname)
            #table doesn't exist yet, need to create
            tablestruct = {}

            if pytables_master_version() == 1:
                shape = 1
            else:
                shape = ()

            tablestruct['time']= tables_floatCol('64')(shape=shape) #always save time with 8 byte
            tablestruct['id']= tables.Int64Col(shape=shape)
            tablestruct['step']= tables.Int64Col(shape=shape)
            tablestruct['stage']= tables.Int64Col(shape=shape)
            for subfieldname in nfem.subfieldnames(field):
                data = numpy.array(ocaml.mwe_subfield_data(field,subfieldname))
                log.debug("Adding subfield (=col) %s with shape %s" % (subfieldname,data.shape))
                tablestruct[subfieldname]=myfloatCol(shape=data.shape)

            log.debug("Table struct for field %s is %s" % (fieldname,tablestruct))

            myfilter = tables.Filters(complib='zlib',complevel=5)
            log.debug("About to call createTable (/root/data/fields/%s)" % fieldname)
            table = f.createTable(f.root.data.fields, fieldname, \
                                  tablestruct,"Field "+fieldname,
                                  filters=myfilter)
            log.debug("Done calling createTable (/root/data/fields/%s)" % fieldname)

            log.debug("Adding /data/fields/%s table" % fieldname)

            table.flush()

            timer2.stop('create_field_table')

    #writing the actual data
    timer2.start('write_field_data_loop')
    for fieldname,(field,units) in fields_by_name.items():
        log.debug("Adding data for field %s" % fieldname)
        #pyfield = pyfields_by_name[fieldname]
        table = f.getNode(f.root.data.fields,fieldname)

        #At this stage we know that the table exists, we 'just' need to add data to it

        tabdata = table.row
        tabdata['time']=time_si.value
        tabdata['id']=id
        tabdata['step']=step
        tabdata['stage']=stage

        for subfieldname in nfem.subfieldnames(field):
            timer2.start('get_field_data_from_ocaml')
            data = numpy.array(ocaml.mwe_subfield_data(field,subfieldname))
            timer2.stop('get_field_data_from_ocaml')

            #scale correctly (to be SI)
            x = simulation_units.conversion_factor_of(units).value
            log.debug("Conversion factor (SU->SI) for writing subfield '%s' is %g (units=%s)" % (subfieldname,x,units.dens_str()))
            dofdata = data * x
            tabdata[subfieldname] = dofdata

        timer2.start('table_append')
        tabdata.append()
        timer2.stop('table_append')

        timer2.start('table_flush')
        table.flush()
        timer2.stop('table_flush')

    timer2.stop('write_field_data_loop')


    timer2.start('f_flush')
    f.flush()
    timer2.stop('f_flush')

    timer2.start('f_close')
    close_pytables_file(f)
    timer2.stop('f_close')

    timer2.stop('all')


def get_pos_from_dofsites(fh,dofsites):
    """Given a list of dofsites (as numpy array), this returns a list
    of positions to which these correspond (as numpy array)."""

    "Do this initially in for loop -- can go to faster matrix notation later if necessary."

    checktag(fh,'nsimdata','0.1')

    meshpositions = fh.root.mesh.points.read()

    #dimensionality of positions:
    dim = len(meshpositions[0])

    #number of sites
    nsites = len(dofsites)

    #output data
    dofpositions = numpy.zeros((nsites,dim),numpy.float64)

    for i,dofsite in enumerate(dofsites):
        nodes = []
        for siteid in dofsite:
            nodes.append(meshpositions[siteid])
        #work out position:
        nodes = numpy.array(nodes)
        p = numpy.zeros(nodes[0].shape,numpy.float64)
        for node in nodes:
            p += node


        p /= len(nodes)

        dofpositions[i] = p
    return dofpositions

timer3 = nsim.timings.Timer('hdf5.get_counters_for_field')

def get_counters_for_field(f, field):
    try:
        fieldtable = f.getNode(f.root.data.fields,field)

    except tables.NoSuchNodeError, msg:
        log.error("No field '%s' in file '%s'." % (field, f.filename))
        log.debug("Original error message (pytables) is '%s'" % msg)
        fieldlist = nfem.hdf5_v01.get_names_of_saved_fields(f)
        log.error("Available fields are %s" % str(fieldlist))
        import sys
        sys.exit(1)

    #time_units = eval(getattr(fieldtable.attrs,"time_units"))
    timer3.start('x')
    fieldtable_cols = fieldtable.cols

    times = fieldtable_cols.time[:]
    rows = range(len(times))
    steps = fieldtable_cols.step[:]
    timer3.stop('x')

    try:
        uids = fieldtable.cols.id[:]

    except AttributeError,msg:
	log.error("Your h5 file does not have an id coloumn. It must be incredibly old. Can't proceed. Sorry.")
        import sys
        sys.exit(1)

    stages = fieldtable.cols.stage[:]

    assert len(steps)==len(times), "Internal error"
    assert len(steps)==len(rows), "Internal error"
    assert len(steps)==len(uids), "Internal error"
    assert len(steps)==len(stages), "Internal error"

    return {'row': rows,
            'id': uids,
            'stage': stages,
            'step': steps,
            'time': times}

def get_row_stage_step_time_for_field_and_id(f, field, id):
    counters = get_counters_for_field(f, field)
    rows, uids, stages, steps, times = \
      [counters[name] for name in ['row', 'id', 'stage', 'step', 'time']]

    # Find that line that contains the required id
    try:
        row_index = uids.tolist().index(id)

    except ValueError, msg:
        log.debug("Original error message was '%s'" % msg)
        raise ValueError("\nCan't find id=%d for field '%s' in file '%s'"
                         % (id,field,f.filename)
                         + "\nUse 'nmagpp --idlist --fields %s %s' to get an "
                         "overview of available ids" % (field, f.filename))

    return (rows[row_index], stages[row_index],
            steps[row_index], times[row_index])


# experimental code: to be deleted soon

def get_row_from_id_dict(f, field):
    """Returns a dictionary mapping row to id for the given file and field."""

    try:
        fields_tb = f.getNode(f.root.data.fields, field)

    except tables.NoSuchNodeError, msg:
        log.error("No field '%s' in file '%s' (original error '%s')"
                  % (field, f.filename, msg))
        import sys
        sys.exit(1)

    try:
        id = fieldtable.cols.id[:]

    except:
        log.error("Your h5 file does not have an id column. It may have "
                  "been generated with an old (unsupported) version of Nmag "
                  "or may be simply corrupted.")
        import sys
        sys.exit(1)

    id_dict = {}
    for row, id in enumerate(ids):
      id_dict[row] = id
    return id_dict

def get_dof_data_rows(fh, field, dofname, row_begin, row_end):
    """Fetch one or more rows of a subfield from hdf5 file field table

    :Returns:
    """
    checktag(fh, 'nsimdata', '0.1')

    table = fh.getNode(fh.root.data.fields, field)
    dofnames = [colname for colname in table.colnames
                        if not colname in ['time','step','stage','id']]
    log.debug("get_dof_data_rows: field=%s dofname=%s rows=%s to %s"
              % (str(field), dofname, row_begin, row_end))

    assert dofname in dofnames, \
      "This field %s doesn't have dof %s in file (dofs are %s)" \
      % (field, dofname, dofnames)

    if pytables_master_version() == 1:
        assert False, "Not implemented, yet!"
        dofdata = table[row].field(dofname)

    else:
        dofdata = table.read(start=row_begin, stop=row_end, field=dofname)

    dofsites = _get_dofsite_by_index(fh, dofname)

    dofpos = get_pos_from_dofsites(fh, dofsites)

    return dofpos, dofdata, dofsites
# end of experimental code

def get_dof_row_data(fh,field,dofname,row):
    """Fetch one row of a subfield from hdf5 file field table

    :Returns:
    """
    checktag(fh,'nsimdata','0.1')

    table = fh.getNode(fh.root.data.fields,field)
    dofnames = [colname for colname in table.colnames if not colname in ['time','step','stage','id']]
    log.debug("get_dof_row_data: field=%s dofname=%s row=%s"
              % (str(field), dofname, row))

    assert dofname in dofnames, "This field %s doesn't have dof %s in file (dofs are %s)" % (field,dofname,dofnames)

    log.debug("get_dof_row_data: filename=%s, field=%s, dofname=%s, row=%s" % \
              (fh.filename,field,dofname,row))


    if pytables_master_version() == 1:
        dofdata = table[row].field(dofname)

    else:
        dofdata = table.read(start=row,stop=row+1,field=dofname)[0]

    dofsites = _get_dofsite_by_index(fh,dofname)

    dofpos = get_pos_from_dofsites(fh,dofsites)

    return dofpos, dofdata, dofsites




def get_dof_row_data_in_meshpoint_order(fh,field,dofname,row):
    """
    Fetch one row of a first-order-basis function subfield from table
    and return in mesh point order.

    Because not each dof is defined on all mesh points, the return
    numpy array may contain lots of zeros where the dof is not
    defined. The length of the returned is the same as the number of
    mesh points.

    We need this for writing vtk files (with nmagpp).

    :Parameters:
      `fh` : filehandler
        for hf5 file

      `field` : string
        the fieldname

      `dofname` : string
        the subfield name

      `row` : integer
        row in the field table that contains the requested data
    """


    def numpy_inv_take(data,target_indices,target_array):
        """Re-order data in ``data``.

        :Parameters:
          `data` - numpy array
            providing the data to be re-ordered

          `target_indices` - numpy array
            providing a list of integers. The integer in the first row is the
            index at which this value will be stored in the target_array

          `targetarray` - numpy array
            this array mush have n rows (where n is the number of mesh points) and then further
            dimensions to keep the dof data for each site. I.e. for a scalar dof, this is just
            a vector with n rows. For a 3d vector doc, ``target_array`` has n rows and 3 columns.

        This function carries out the operation that would be inverse of numpy's ``take``. This makes
        only sense for 1st order basis functions.

        """

        if max(target_indices)>len(target_array)-1:
            print "Something wrong here: max(target_indices)=%d>len(target_array)-1=%d-1" %\
                  (max(target_indices),len(target_array)-1)
            print " type(target_indices)     ", type(target_indices)
            print " type(target_indices[0])  ", type(target_indices[0])
            print " target_indices           ", target_indices
            print " target_indices[0]        ", target_indices[0]

            raise ValueError,"Internal problem: data is shorter than target indices values"


        # from IPython.Shell import IPShellEmbed
        # ipshell = IPShellEmbed([])
        # ipshell()

        is_right_type = type(target_indices[0]) in [types.IntType,numpy.typeDict['int'],numpy.int32]

        if not is_right_type:
            raise ValueError,"target_indices[0] == %s is not of IntType/numpy.int"\
                  "(but of type '%s'. Are you trying to"\
                  "sort data with a 2nd or higher order basis function? This doesn't work."\
                  % (target_indices[0],type(target_indices[0]))

        assert len(target_indices)==len(data),"target_indices and data have different lengths (%d!=%d)" \
               % (len(target_indices),len(data))

        for i in range(len(target_indices)):
            target_index = target_indices[i]
            target_array[target_index] = data[i]
            #print "stuffind data from index ",target_index," into position ",i
            #print "target_array[target_index]=",target_array[target_index]
        return target_array



    #how big is the mesh
    npoints = len(fh.root.mesh.points)

    #what is the dimension of the dofname (i.e. scalar, vector, tensor, etc):
    metadatafields = fh.root.etc.metadatafields

    if pytables_master_version() == 1:
        tmp = [tablerow['maxind'] for tablerow in metadatafields.where(metadatafields.cols.dofname == dofname)]
    else:
        required_dofname = dofname

        version = pytables_file_format_version(fh)[0]

        #if pytables version of data file is 1.0:
        if version == 1: #see Hans' email in pytables mailing list from 2 July 2008
            tmp = []
            for tr in metadatafields.iterrows():
                if tr['dofname'].rstrip() == required_dofname:
                    tmp.append(tr['maxind'])
        else: #this is the normal case for pytables files >= 2 when read with pytables >= 2
           tmp = [tablerow['maxind'] for tablerow in metadatafields.where('dofname == required_dofname')]

    assert len(tmp) == 1, "len(tmp)==%d -- this indicates a non-unique dofname in /etc/metadatafields. Error"

    maxindstr = tmp[0]

    log.debug("maxind for dofname %s is %s" % (dofname,maxindstr))

    dofdatashape = tuple([npoints]+eval(maxindstr))

    log.debug("dofdata (on all mesh points) has shape %s" % str(dofdatashape))

    #create empty data array:
    target_array= numpy.zeros(dofdatashape,numpy.float)

    dofpos, dofdata, dofsites = get_dof_row_data(fh,field,dofname,row)

    shapefunctionorder = len(dofsites[0])
    if  shapefunctionorder != 1:
        raise ValueError,"Can only return points in mesh order for linear basis functions"\
              ", but order seems to be %d" % shapefunctionerdor

    numpy_data = numpy_inv_take(dofdata,dofsites[:,0],target_array)

    return dofpos, numpy_data, dofsites



#----------------------------------------------------------------------



def add_mesh(file, mesh, mesh_unit_length):
    """
    Add mesh data structure to open hdf5 file.

    Raises error if /mesh exists already in that file.

    :Parameters:

      `file` : file handler or (string)
        The hdf5 file to which the mesh will be added. Either a file handler
        for the open file object or the filename ``(string)`` of the file can
        be provided.

      `mesh` : Python mesh object
        The Python object that contains the mesh

      `unit_length` : SI object or None
        The SI object that defines what a length of 1.0 in mesh coordinates
        corresponds to in the real world.

        If we save the mesh from the mesher (using this function), then
        we do not want to add a unit_length scale (so this parameter is optional).
    """

    if type(file) == tables.File:            # Open tables file handler
        f = file

    elif type(file) == types.StringType:     # Filename
        f = open_pytables_file(file, 'r+')

    meshgroup = f.createGroup("/", 'mesh', 'Mesh data')

    # C-array or not C-array:
    #
    # Experiment: bar30_30_100.nmesh (688k ) -> compressed carray (110k), -
    # -> array(246k) (this is just binary, can't be compressed)
    # -> uncompressed table -> 250k,
    # -> compressed table -> 114k
    #
    # So the C-array is slightly better, even though regions and simplices are
    # not compressed together.
    use_c_array = True
    if use_c_array:
        filter = tables.Filters(complevel=5, complib="zlib")

        # Add points
        pos_shape = (len(mesh.points), mesh.dim)
        pos_chunk = tables.Float64Atom(shape=pos_shape)

        pos_c_array = \
          my_tables_createCArray(f, meshgroup, 'points', pos_shape,
                                 tables.Float64Atom, filters=filter,
                                 title='Positions of mesh nodes (=points)')

        pos_c_array[:] = numpy.array(mesh.points, dtype=numpy.float64)

        # Compression note: I have tested different integer data types for
        # simplex indices and region indices. Between tables.Int16Bit and
        # tables.Int32BitAtom there is less than 1% difference in file size
        # (using zlib compression level 5) for a file with 2000 points and
        # 10000 simplices.  For simplicity, we only use Int32Bit at the moment.
        # (fangohr 19/01/2007)

        # Add simplices
        simp_dim = mesh.dim + 1 # Simplices are n+1 dimensional
        simplex_shape = (len(mesh.simplices), simp_dim)
        simplex_chunk = tables.Int32Atom(shape=simplex_shape)

        simplex_c_array = \
          my_tables_createCArray(f,meshgroup, 'simplices',
                                 simplex_shape, tables.Int32Atom, filter,
                                 ('Indices of nodes (starting from zero). '
                                  'Each row is one simplex.'))

        simplex_c_array[:,:] = numpy.array(mesh.simplices,dtype=numpy.int32)

        # Add simplicex regions
        simplexregion_shape = (len(mesh.simplicesregions),)

        simplexregion_chunk = tables.Int32Atom(shape=simplexregion_shape)

        simplexregion_c_array = \
          my_tables_createCArray(f, meshgroup, 'simplicesregions',
                                 simplexregion_shape, tables.Int32Atom,
                                 filter, 'Region ids (one for each simplex).')

        simplexregion_c_array[:] = numpy.array(mesh.simplicesregions,dtype=numpy.int32)

        # Add permutation info
        permutation = mesh.permutation
        if permutation != None:
            permutation_shape = (len(mesh.points),)
            permutation_c_array = \
              my_tables_createCArray(f, meshgroup, 'permutation',
                                     permutation_shape, tables.Int32Atom,
                                     filter,
                                     ('Permutation of sites with respect to '
                                      'the original (non-distributed) mesh'))
            permutation_c_array[:] = \
              numpy.array(permutation, dtype=numpy.int32)

	# Add periodic points 
        # NOTE: each line for periodic points may have a different number of
	# indices: a corner may have 4 or 8 equivalent points (in 2d or 3d) but
	# a point on the side may just have one periodic repeat.

	# We convert this into a CArray (which supports compression). We thus
        # need to convert the list of lists of irregular length into a regular
        # matrix before we can convert to a numpy array.
	if mesh.periodicpointindices != []:
	    # Need to find maximum length:
	    max_len = max(map(len, mesh.periodicpointindices))

	    periodicpointindices_shape = (len(mesh.periodicpointindices), max_len)

	    # We use -1 as the token for None
	    periodicpointindices_data = \
                numpy.zeros(periodicpointindices_shape, dtype=numpy.int32) - 1

	    # Populate numpy array with list-data
	    for i, data in enumerate(mesh.periodicpointindices):
		periodicpointindices_data[i, 0:len(data)] = data

	    periodicpointindices_c_array = \
              my_tables_createCArray(f, meshgroup, 'periodicpointindices',
                                     periodicpointindices_shape,
                                     tables.Int32Atom,
                                     title=('Indices of periodic points (one '
                                            'line for each indipendent point, '
                                            '-1=None).'),
                                     filters=filter)

	    periodicpointindices_c_array[:] = periodicpointindices_data

    else:
        #Tests have shown that using c-arrays is somewhat smaller than using tables.
        #Therefore , we should not need the next chunk of code. However, we may decide
        #to change our mind on this later, so I'll leave it in for now. (Tables
        #maybe better for larger meshes.)
        raise NotImplementedError("If we start writing tables, we need to "
                                  "modify reading routines")

        filter = tables.Filters(complevel=5, complib="zlib")

        #Create table
        tablestruct = {}

        npoints = numpy.array(mesh.points)
        nsimplices = numpy.array(mesh.simplices)
        nsimplicesregions = numpy.array(mesh.simplicesregions)

        myfloatCol = tables.Float64Col
        tablestruct['points']=myfloatCol(shape=npoints.shape)
        tablestruct['simplices']=tables.Int32Atom(shape=nsimplices.shape)
        tablestruct['simplicesregions']=tables.Int32Atom(shape=nsimplicesregions.shape)

        table = f.createTable(f.root, 'mesh', tablestruct,"nfem Mesh in table",expectedrows=1)#,filters=filter)

        tabdata = table.row
        tabdata['points']=npoints
        tabdata['simplices']=nsimplices
        tabdata['simplicesregions']=nsimplicesregions
        tabdata.append()
        table.flush()

	#Note: adding of periodic points has not been done in this table mode. (fangohr 04/11/2007)
	#
	#I suspect we will never change to storing the positions in tables: the gain is small
	#and the overhead of changing now (and providing backwards compatible reading routines)
	#may well not justify the effort.


    #Add lengthscale if given
    if mesh_unit_length:
        #check type
        if not isinstance(mesh_unit_length,SI):
            raise NsimValueError,"mesh_unit_length (%s) is of type %s but needs to be SI object" %\
                  (mesh_unit_length,type(mesh_unit_length))
        #check whether this is for meters
        if not mesh_unit_length.is_compatible_with(SI('m')):
            raise NsimUserError,"Unit length provided for mesh (%s) is not compatible with '%s'" %\
                  (mesh_unit_length,SI('m'))

        #all tests done. Add data.
        lengthgroup = f.createGroup("/mesh", 'scale', 'Mesh coordinate length scale group')
        f.createArray('/mesh/scale','sifactor',\
                      mesh_unit_length.value,\
                      title='Multiply mesh coordinates with this number to obtain meters')

        f.createArray('/mesh/scale','unit',repr(mesh_unit_length),\
                      title='SI object for coordinate to position conversion')

        log.log(15,"Added mesh_unit_length scale (%s) to %s:/mesh/scale" %\
                  (mesh_unit_length,f.filename))
    else:
        log.log(15,"Not saving mesh_unit_length (non given)")

    if type(file)==types.StringType: #we were given the filename, so we should close the file
        close_pytables_file(f)

    return None




def add_attribute(filename,leaf,name,data):
    """Allows adding pytable-attributes (basically any python object) to leaf nodes"""
    log.log(15,"About to write to %s/%s/%s, data is '%s' " % (filename,leaf,name,str(data)))
    fh = open_pytables_file(filename,'r+')
    fh.setNodeAttr(leaf,name,data)
    close_pytables_file(fh)
    return None

def get_attribute(filename,leaf,name):
    """Allows reading pytable-attributes from leaf nodes"""
    fh = open_pytables_file(filename,'r')
    data = fh.getNodeAttr(leaf,name)
    log.log(15,"Have just read %s/%s/%s = %s" % (filename,leaf,name,str(data)))
    close_pytables_file(fh)

    return data


def average_data_has_step(filename,step):

    #because we don't save the average data (for performance reasons) in the h5 file, this function will
    #effectively always return False. I make this explicit now to make it faster.
    return False #Fangohr, 11/11/2008

    global timesteps_in_h5_file #make it clear to Python that we want to use this global list

    log.debug("average_data_has_step(filename=%s,step=%d): Entering. timesteps_in_h5_file=%s" \
	      % (filename,step,timesteps_in_h5_file))

    if timesteps_in_h5_file == []:# first call, need to read time steps:

        if not os.path.exists(filename):
            log.debug("average_data_has_step(filename=%s,step=%d): Couldn't open %s -> False" \
                      % (filename,step,filename))

            return False

        fh = open_pytables_file(filename,'r')

        checktag(fh,'nsimdata','0.1')

        if not hasattr(fh.root,'data'):
            log.debug("average_data_has_step(filename=%s,step=%d): Couldn't find /root/data in %s -> False" \
                      % (filename,step,filename))

            close_pytables_file(fh)
            return False
        elif not hasattr(fh.root.data,'averages'):
            log.debug("average_data_has_step(filename=%s,step=%d): Couldn't find /root/data/averages in %s -> False" \
                      % (filename,step,filename))
            close_pytables_file(fh)
            return False

        timesteps_in_h5_file = list(fh.root.data.averages.cols.step[:])
        close_pytables_file(fh)

        log.debug("Have read time steps from file: %s" % str(timesteps_in_h5_file))

    else:
        log.debug("Use time steps from cached list. Lists starts %s, has length %d" %
		  (timesteps_in_h5_file[0:5],len(timesteps_in_h5_file)))

    if step in timesteps_in_h5_file:
        log.debug("Have found step %d in average table in %s" % (step,filename))
        result = True
    else:
        log.debug("Have not found step %d in average table in %s" % (step,filename))
        result = False
    return result

def hdf5_get_subfield(filename, subfieldname, id=None, row=None, unit=None):
    """
    Retrieve data from h5 file.
    Return two numpy.array-s (sites, vals): vals[i] is the value of the field
    on the site sites[i] in the mesh. The entries of vals are returned as
    :ref:`SI-value <SI object>`\ s.

    Analog to get_subfield_ (which returns subfield data for a
    subfield of a simulation object), but will retrieve data from
    saved ``_dat.h5`` file.

    :Parameters:
      `filename` : string
         The full name of the ``_dat.h5`` data file.

      `subfieldname` : string
         The name of the subfield to be retrieved.

      `id` : integer
         The ``id`` of the configuration to return (defaults to 0)

      `row` : integer
         If the ``id`` is not specified, the ``row`` can be used to address the
         data row with index ``row``.

         For example, the magnetisation may have been saved at some point
         during the simulation into a file (for example using the :ref:`Restart
         example <Restart example>` functionality, or using the save_data_
         method for the first time to save the m-field
         (i.e. ``sim.save_data(fields=['m']``) into a new file).

         We can use ``row=0`` to read the first magnetisation configuration
         that has been written into this file (and ``row=1`` to access the
         second etc).

    :Returns:
      numpy array
    """

    if unit:
        raise NotImplementedError("This feature is not implemented yet "
                                  "(unit=*).")

    fh = open_pytables_file(filename,'r')
    field = fieldname_by_subfieldname(fh, subfieldname)
    if id == None:
        if row == None:
            row = 0
    else:
        row, _, _, _ = \
          get_row_stage_step_time_for_field_and_id(fh, field, id)

    _, sidata, site = get_dof_row_data(fh, field, subfieldname, row)
    close_pytables_file(fh)
    return (site, sidata)

def get_subfield_from_h5file(*args, **nargs):
    return numpy.array(hdf5_get_subfield(*args, **nargs)[1])

def report_append_field_timings():
    for key in append_fields_timings:
        print "%30s : %7.3f sec" % (key, append_fields_timings[key])

def report_append_averages_timings():
    for key in append_averages_timings:
        print "%30s : %7.3f sec" % (key, append_averages_timings[key])

def report_timings():
    report_append_averages_timings()
    report_append_field_timings()

def get_field_data_from_file(mesh, field, filename, **kwargs):
    """Return a dictionary of numpy arrays containing the values of 'field' as
    stored in 'filename' for each of its subfields. These numpy arrays are
    reordered coherently with the currently used 'mesh'.  This is important, as
    the 'mesh' may be reordered in a different way with respect to the mesh
    used when saving the data. This may happen when the two simulations are
    ran (via MPI) on a different number of compute nodes (Parmetis is called
    just when the mesh is loaded, to permute the nodes and optimise the mesh
    connectivity before distributing the data). Not reordering the data, could
    lead to setting the magnetisation at the wrong nodes.
    """

    file_to_my = None
    my_perm = mesh.permutation
    file_perm = nmesh.hdf5_mesh_get_permutation(filename)
    if my_perm != None or file_perm != None:
        perms_are_compatible = (my_perm != None and file_perm != None 
                                 and numpy.array_equal(my_perm, file_perm))

        if not perms_are_compatible:
            # Compute the mapping from current indexing to file indexing
            if my_perm == None:
                my_perm = numpy.arange(len(file_perm))
            elif file_perm == None:
                file_perm = numpy.arange(len(my_perm))

            num_mesh_sites = len(my_perm)
            if len(file_perm) != num_mesh_sites:
                raise ValueError("You are trying to set a field from an "
                                 "incompatible data file")

            # Compute the mapping current indexing -> file indexing. This
            # is a permutation given by composing file_perm with the
            # inverse of my_perm
            inv_file_perm = numpy.arange(num_mesh_sites)
            inv_file_perm[file_perm] = numpy.arange(len(file_perm))
            file_to_my = inv_file_perm[my_perm]
            del inv_file_perm

    values = {}
    names_and_shapes = nfem.data_doftypes(field)
    for subfield_name, shape in names_and_shapes:                
        file_site_ids, file_values = \
          hdf5_get_subfield(filename, subfield_name, **kwargs)

        if file_to_my != None:
            # my_site_ids expresses how we want the numpy array to be
            # ordered (before passing it for setting the field). The
            # problem here is that the order of the numpy array as red from
            # the file is not this one (it is not my_site_ids, but rather
            # file_site_ids).  Moreover the two orderings my_site_ids and
            # file_site_ids refer to possibly different orderings of the
            # mesh (assuming the mesh has been partitioned differently in
            # the two cases). This complicates things a bit...
            my_site_ids, _, _, _ = \
              ocaml.mwe_subfield_metadata(field, subfield_name)

            # Flatten the arrays: as Nsim was built to deal with FE at any
            # order, the fields can be defined outside the mesh sites.
            # The sites where the fields are defined are - in general -
            # identified by a list of mesh sites ids (which are averaged
            # to get the actual coordinates). Here we restrict ourselves
            # to order 1 and we simply flatten the array (this will fail
            # when FE orders > 1 are used).
            file_site_ids = \
              numpy.array(my_site_ids).reshape(len(file_site_ids),)
            my_site_ids = \
              numpy.array(my_site_ids).reshape((len(my_site_ids),))

            # Compute inverse map of my_site_ids
            num_mesh_sites = len(file_to_my)
            my_site_ids_inv = numpy.arange(num_mesh_sites)
            my_site_ids_inv[my_site_ids] = numpy.arange(len(file_site_ids))

            permutation = my_site_ids_inv[file_to_my[file_site_ids]]
            my_values = file_values[permutation]

        else:
            my_values = file_values

        values[subfield_name] = my_values

    return values


if __name__ == "__main__":

    if os.path.exists('test.h5'):
        os.remove('test.h5')

    savefield('test.h5','test.h5')


    f=open_pytables_file('test.h5')
    node = f.root.sim.source
    fnode = FileNode.openNode(node)
    for line in fnode.readlines():
        print repr(line)
    fnode.close()

    f=open('test.h5','r')
    #print str(f)
    f.close()

    os.system('h5ls -r test.h5')
    #os.system('ptdump -av test.h5')
    #os.system('ptdump -d test.h5:/sim/source')
