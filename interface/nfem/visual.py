import os,types
from nfem_exceptions import *
import nsim
log = logging.getLogger('nfem')


"""Provides ability to save fields on meshes in VTK data format
(VTK www.vtk.org/pdf/file-formats.pdf)
using pyvtk (www.cens.ioc.ee/projects/pyvtk).

For now, only vector and scalar fields are supported.

This needs lots of tidying up and stream lining.
For now, the main aim is to be able to plot field data quickly.

Hans August 2006

$Header: /home/fangohr/local/pytables2/nmag-0.1/nsim/interface/nfem/visual.py,v f5bc0ae27ba2 2008/07/02 15:27:33 fangohr $
"""

try:
    import pyvtk
except ImportError,msg:
    print "We need pyvtk (www.cens.ioc.ee/projects/pyvtk) to run %s" % __name__
    print "Can't import pyvtk. Stopping here"
    raise ImportError,msg

try:
    import numpy as numerix
except ImportError,msg:
    try:
        import Numeric as numerix
    except ImportError,msg:
        print "Can't import neither numpy nor Numeric. Please install."
        raise ImportError,msg

import nfem #to get access to default values
             #strictly, nfem2 should export function to retrieve these default values.


def get_default_mesh():
    mesh = nfem.r_default_mesh[0]
    assert not mesh == None, "No default mesh defined yet."
    return mesh


def _vtk_createVtkData(meshpoints,meshsimplices,data,name,header="Data header (unused)"):
    #If data is list of float, convert into list of lists, each list containing one float
    if type(data[0]) in [types.FloatType,types.IntType]:
        #raw data in scalar data set, convert float a into [a]:
        data = map (lambda a : [a], data)


    #Due to inflexibility in pyvtk, we need to distinguish 4 different cases for
    #2d-meshes with scalar data
    #2d-meshes with vector data
    #3d-meshes with scalar data
    #3d-meshes with vector data

    #Here we go:
    if len(meshpoints[0])==2: 
        log.debug("Mesh seems 2d")
        #make 3d for pyvtk
        meshpoints = map( lambda pos : pos+[0.], meshpoints )
        if len(data[0]) ==1:
            log.debug("Data seems 1d (scalar)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, triangle=meshsimplices),pyvtk.PointData(pyvtk.Scalars( data, name = name, lookup_table='default')),header)
        elif len(data[0]) in [2,3]:
            if len(data[0]) ==2:
                log.debug("Data seems 2d (vector)")
                #make 3d for pyvtk
                data = map( lambda a : a + [0.], data) 
            else:
                log.debug("Data seems 3d (vector)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, triangle=meshsimplices),pyvtk.PointData(pyvtk.Vectors( data, name = name)),header)
        else:
            raise NfemValueError, "Can only deal with scalar or vector data"

    elif len(meshpoints[0])==3:
        log.debug("Mesh seems 3d")
        if len(data[0]) ==1:
            log.debug("Data seems 1d (scalar)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, tetra=meshsimplices),pyvtk.PointData(pyvtk.Scalars( data, name = name, lookup_table='default')),header)
        elif len(data[0]) in [2,3]:
            if len(data[0]) ==2:
                log.debug("Data seems 2d (vector)")
                #make 3d for pyvtk
                data = map( lambda a : a + [0.], data)
            else:
                log.debug("Data seems 3d (vector)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, tetra=meshsimplices),pyvtk.PointData(pyvtk.Vectors( data, name = name)),header)
        else:
            raise NfemValueError, "Can only deal with scalar or vector data"

    elif len(meshpoints[0])==1: 
        log.debug("Mesh seems 1d")
        #make 3d for pyvtk
        meshpoints = map( lambda pos : pos+[0.,0.], meshpoints )
        if len(data[0]) ==1:
            log.debug("Data seems 1d (scalar)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, line=meshsimplices),pyvtk.PointData(pyvtk.Scalars( data, name = name,lookup_table='default')),header)
        elif len(data[0]) in [2,3]:
            if len(data[0]) ==2:
                log.debug("Data seems 2d (vector)")
                #make 3d for pyvtk
                data = map( lambda a : a + [0.], data) 
            else:
                log.debug("Data seems 3d (vector)")
            log.debug("Creating vtk data structure with dof '%s'" % name )
            vtk = pyvtk.VtkData(pyvtk.UnstructuredGrid(meshpoints, line=meshsimplices),pyvtk.PointData(pyvtk.Vectors( data, name = name)),header)
        else:
            raise NfemValueError, "Can only deal with scalar or vector data"

    else:
        NfemValueError,"Mesh seems to be %d dimensional. Can only do 1, 2 or 3." % len(meshpoints[0])

    return vtk
        
    
    

def _vtk_addPointdata(vtk,data,name):
    """vtk is a vtk object (from pyvtk) to which the data will be appended.
    data is a list of lists. The inner lists contain the data, the outer lists the sites.
    Example: data = [[0.1],[0.2],[0.3],....] is scalar data
    data = [[0.1,1],[0.2,0.5],[0.3,0.4],....] is 2d vector data

    name is the name of the data (used in vtk file to label the field)"""
    
    log.debug("Adding dof %s to vtk object" % name)

    if type(data[0]) in [types.FloatType,types.IntType]:
        #raw data in scalar data set, convert float a into [a]:
        data = map (lambda a : [a], data)

    #print "In vtk_addPointdata. What is the structure of data?"
    #from IPython.Shell import IPShellEmbed
    #ipshell = IPShellEmbed()
    #ipshell()
    
    if len(data[0]) ==1:
        vtk.point_data.append(pyvtk.Scalars(data, name = name, lookup_table='default'))
    elif len(data[0]) in [2,3]:
        if len(data[0]) ==2:
            #make 3d
            data = map( lambda a : a + [0.], data)
        vtk.point_data.append(pyvtk.Vectors(data, name = name))
    else:
        raise NfemValueError, "Can only deal with scalar or vector data"
    return vtk
        

def _field2meshdata(nr_points_in_mesh,field,name,dim):
    log.debug("Converting %d-dimensional field %s into mesh-data list" % (dim,name))
    log.debug("Creating Matrix with size=(%d,%d)" % (nr_points_in_mesh,dim))
    
    data = numerix.zeros((nr_points_in_mesh,dim),'f') 
    
    def cb(i,dofname_index,site,pos,value):

        if len(site) > 1:
            log.critical("Problem -- Have only tested this for 1st order basis functions. Gut feeling is that we only want to export first order data to visualisation but this needs further thought. Order appears to be %d" % (len(site)))
            raise NotImplementedError,"This has only been implemented for first order meshes"
        
        dofname,index = dofname_index
        if dofname == name:
            if dim==1:
                data[site[0]][0] = value
            elif dim in [2,3]:
                data[site[0],index[0]] = value
            else:
                log.critical("Problem -- data seems not 1, not 2d, not3d. What's up? dim=%d" % dim)
                raise NfemUserError,"Shouldn't happen (4d-data?5d-data?...)"
        else:
            pass
            
    nfem.field_entry_wise(field,cb)

    return data.tolist()

def fields2vtkfile( fields, filename, mesh = None, only_dofname=None, format="binary",path=None,extrafields=None,header='Header (unused)'):
    """ def fields2vtkfile( fields, filename, mesh = None,
                            only_dofname=None, format="ascii",path=None):
        Given a list of FIELDS (or just one field), a FILENAME and, optionally
        a MESH and some other values, this function goes through all degrees of
        freedoms in the field list and writes their data to the vtk file with name
        FILENAME.

        Currently, scalar and vector data on 2d and 3d meshes is supported.

        Optional extra arguments are:
            format      : 'ascii' or 'binary' (how the data is stored in the vtk file)
            only_dofname: if a name for a degree of freedom (dof) is given here, then only
                          the data for this dof is written to the vtk file.
            mesh        : if not provided, the default mesh in nfem2 is chosen. If there is
                          no default mesh defined, then a mesh needs to be provided here,
                          or the function will report an error.
            path        : if given, the file will be written no PATH/FILENAME.
                          if path is not specified, the file will be written to the
                          nfem default directory (nbase.run_dir)

            extrafields : if given, this needs to be a tuple of a fieldname and the data
                          in Python lists. Example usage: providing the maxangle data
    """

    filepath = nsim.snippets.output_file_location(filename,path)

    log.debug("Entering fields2vtkfile to write %s" % filepath)
    #check that fields are of the right type

    if type(fields) != type([]):
        fieldlist = [fields]
    else:
        fieldlist = fields
    if len(fieldlist) == 0:
        raise NfemValueError, "No field received"
    import ocaml
    if ocaml.sys_ocamlpill_type(fieldlist[0]) != 'FEM Field':
        raise NfemValueError, "Expect FEM Fields as input"

    #get the mesh (should check the type here, really.)
    if mesh == None:
        mesh=get_default_mesh()
    meshinfo = mesh.tolists()
    points = meshinfo[0][2]
    log.debug("Number of points in mesh=%d" % len(points))
    tmp = meshinfo[2][2]
    simplices = map( lambda a: a[0], tmp)
    log.debug("Number of simplices in mesh=%d" % len(simplices))

    #check dimensionality of the mesh
    if len(points[0])==2:
        log.debug("mesh is 2d")
        mesh_dim = 2
    elif len(points[0])==3:
        log.debug("mesh is 3d")
        mesh_dim = 3
    else:
        log.debug("mesh is 1d")
        mesh_dim = 1
        log.warn("1d data is new -- may be buggy (fangohr 26/10/2006)")

    #assume that 'fieldlist' is a list of FEM fields.
    #First, create the following data structure.
    #dof_to_process = [field, dof, dim]
    dof_to_process = []

    for field in fieldlist:
        #learn about dof in this field:
        content = nfem.data_doftypes(field)
        for dof in content:
            name, maxind = dof
            if only_dofname:
                if name != only_dofname:
                    continue
                
            if len(maxind)>1:
                raise NfemValueError,"Can only deal with scalar and vector data at the moment (no tensor data)."
            if len(maxind)==0:
                dim = 1 #this is a scalar
            elif len(maxind)==1:
                if maxind[0] ==2: #this is a 2d vector
                    dim = 2
                elif maxind[0] ==3: #this is a 3d vector
                    dim = 3
                else:
                    raise NfemValueError,"Can only process vectors in 2 and 3 dimensions"
            dof_to_process.append( (field, name, dim) )

            log.debug("adding %s" % str(dof_to_process[-1]) )

    #pyvtk requires us to create a vtk object and to give it the first
    #dof at that time. For all subsequent dofs, we can add them one by one.
    
    #check that we have some dofs to process:
    if len(dof_to_process)==0:
        raise NfemUserError,"Have not found any degrees of freedom (Maybe you have used only_dofname with a wrong name?)"

    names = str(map(lambda a : a[1], dof_to_process))
    log.info("About to write field(s) '%s' to %s (%s)" % (names,filename,filepath))

    #create vtk object with first data set
    field, name,dim = dof_to_process[0]
    data = _field2meshdata(len(points),field,name,dim)
    vtk = _vtk_createVtkData(points,simplices,data,name,header)
    log.log(15,"Adding %d-dim field %s to %s" % (dim,name,filename))

    #and then process all others
    for dof in dof_to_process[1:]:
        field, name,dim = dof
        if only_dofname:
            if name!=only_dofname:
                continue
        log.debug("Adding %d-dim dof %s to %s" % (dim,name,filename))
        data = _field2meshdata(len(points),field,name,dim)
        vtk = _vtk_addPointdata(vtk,data,name)


    #and process all extra fields (not nfem fields)

    if extrafields:
        for name, data in extrafields:
            if only_dofname:
                if name!=only_dofname:
                    continue
            log.log(15,"Adding data %s to %s" % (name,filename))

            #from IPython.Shell import IPShellEmbed
            #ipshell = IPShellEmbed()
            #ipshell()



            vtk = _vtk_addPointdata(vtk,data,name)

    vtk.tofile(filepath,format=format)


