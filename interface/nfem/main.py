from __future__ import division

import logging
log = logging.getLogger('nfem')

from nfem_exceptions import *

try:
    import ocaml
except ImportError,msg:
    error = "We can't import the ocaml package. This usually means that you\n"
    error +="are running this code in a normal python interpreter, not within\n"
    error +="the nmesh or nsim executable.\n\n"
    raise NfemImportError,error+str(msg)

import types
import math

# T.F.: I just discovered that python does quite braindead things when
# it comes to lambda binding global variables and optional arguments:
# Many things are evaluated just at the wrong time (from the LISP perspective),
# so we have to bend over backwards...

r_default_dimension=[2]
r_default_order=[1]
r_default_mesh=[None]

def set_default_dimension(dim):
    log.log(15,"Setting default dimension to %s",str(dim))
    r_default_dimension[0]=dim

def set_default_order(ord):
    log.log(15,"Setting default order to %s",str(ord))
    r_default_order[0]=ord

def set_default_mesh(mesh):
    log.log(15,"Setting default mesh")
    r_default_mesh[0]=mesh

def make_element(name,indices,dim=None,ord=None):
    """
    Create an abstract element.
    """

    if dim==None:
        dim=r_default_dimension[0]
    if ord==None:
        ord=r_default_order[0]
    log.log(15,"Creating element '%s', indices=%s, ord=%s, dim=%s" %(name,str(indices),str(dim),str(ord)))
    elem = ocaml.make_element(name,indices,dim,ord)
    # elem.__str__=lambda (obj): "#<some element>"
    # Note: we cannot do this, as elem is a PyCObject.
    # If we wanted this functionality, we would have to add
    # yet another level of (python-side) wrapping!
    return elem

def fuse_elements(elem1,elem2):
    """
    Create an abstract element by fusing the contents of two abstract elements.
    """
    log.log(15,"Fusing elements XXX and XXX")
    return ocaml.fuse_elements(elem1,elem2)

def make_mwe(name,region_elements,mesh=None,outer_region=[]):
    """
    Associate elements to a mesh.
    """
    log.log(15,"Making mesh with elements '%s'" %(name))
    if mesh==None:
        mesh=r_default_mesh[0]
    return ocaml.make_mwe(name,mesh.raw_mesh,region_elements,outer_region)

def mwe_sibling(mwe,new_name,renaming_prefix,dof_renamings):
    log.log(15,"Mesh with elements, sibling XXX")
    return ocaml.mwe_sibling(mwe,new_name,renaming_prefix,dof_renamings)

def field_alias(field,mwe):
    log.log(15,"Field_alias XXX")
    return ocaml.field_alias(field,mwe)

def cofield_alias(cofield,mwe):
    log.log(15,"Co-Field_alias XXX")
    return ocaml.cofield_alias(cofield,mwe)

def field_zero(f):
    return ocaml.field_zero(f)

def cofield_zero(f):
    return ocaml.cofield_zero(f)

def field_scale(f,s):
    return ocaml.field_scale(f,s+0.0)

def cofield_scale(f,s):
    return ocaml.cofield_zero(f,s+0.0)

def field_copy_into(s,d):
    return ocaml.field_copy_into(s,d)

def cofield_copy_into(s,d):
    return ocaml.cofield_copy_into(s,d)

def field_axpby(a,b,s,d):
    return ocaml.field_axpby(a+0.0,b+0.0,s,d)

def cofield_axpby(a,b,s,d):
    return ocaml.cofield_axpby(a+0.0,b+0.0,s,d)


def data_length(x):
    length = ocaml.data_length(x)
    log.log(15,"data length of XXX is %d" % length)
    return length

def data_doftypes(x):
    """Given a raw ocaml field, this returns a list of pairs. Each pair contains first
    the name of the subfield and second the shape information for this subfield."""
    result = ocaml.data_doftypes(x)
    #log.debug("doftypes of XXX is %s" % str(result))
    return result

def subfieldnames(field):
    """Given a field name, this returns a list of names of all subfields of this field."""
    doftypes = data_doftypes(field)
    sfnames = [name for name,shape in doftypes]
    log.debug("subfieldnames: subfields are %s" % str(sfnames))
    return sfnames

def subfieldshape(field,subfield):
    """Given a field name, this returns the shape of this subfield."""
    doftypes = dict(data_doftypes(field))
    shape = doftypes[subfield]
    log.debug("subfieldshape: shape of subfield %s is %s" % (str(subfield),str(shape)))
    return shape

def field_entry_wise(field_or_cofield,callback):
    """

    def field_entry_wise(f,cb):

    where:
        f is a nfem-field structure
        cb() is a Python call back function (see below).
        

    field_entry_wise() executes function cb() for a
    given field or cofield f for every (scalar) degree of freedom in that field.

    The signature of the callback cb() is:

      cb(i, dof_name_stem, site, pos, value)

    where:

      i              : index of the degree of freedom (dof) within the field data structure
      dof_name_stem  : tuple contain (name of the dof, list of indices)
      site           : list of index of site (in the mesh data structure).\
                       For first order basis functions, this is just one number that
                       corresponds to the site in the mesh data structure.
                       For higher order basis functions, this could be more than one number
                       (see 'type site' in Ocaml code.)
      pos            : position of that site in space
      value          : the actual value of that dof

EXAMPLE:

    For example, if a (first order) field contains H_x, H_y and H_z at
    every site of the mesh, then this function will be called once for
    each component of H for every site.

    The corresponding data would read:
      i              : 46
      dof_name_stem  : ('H',[1]) #here [1] means y-component
      site           : [15]
      pos            : [-2.8493571587790001, 0.75288651951899999, -0.560647553809]
      value          : 5.0491522056363981e-05
    """
    log.debug("About to call function '%s' for every entry in field XXX" % (callback.__name__))

    return ocaml.field_entry_wise(field_or_cofield,callback)

def field_print_contents(field_or_cofield):
    def cb(nr_dof,dof_name_indices,dof_site,dof_pos,dof_val):
        print "DOF NR ",nr_dof,": ",dof_name_indices," site=",dof_site," pos=",dof_pos," value=",dof_val
    field_entry_wise(field_or_cofield,cb)


def make_field(mwe,initial_values=None,petsc_name="",restriction=""):
    log.log(15,"About to create field from mwe XXX")
    iv=[]
    if initial_values:
        iv=[initial_values]
    return ocaml.raw_make_field(mwe,iv,restriction,petsc_name)

# Right now, I am somewhat unsure what it may be good for
# being able to sample a co-field...
def make_cofield(mwe,initial_values=None,petsc_name="",restriction=""):
    log.log(15,"About to create cofield from mwe XXX")
    iv=[]
    if initial_values:
        iv=[initial_values]
    return ocaml.raw_make_cofield(mwe,iv,restriction,petsc_name)


def diffop(code):
    log.log(15,"Computing differential operator for %s " % str(code))
    return ocaml.make_diffop(code)

def _the_diffop(x):
    """ Internal: if x is a diffop, return x. If x is a string, promote it to a diffop."""
    if type(x) == str:
        return diffop(x)
    else:
        return x

def prematrix(diffop_or_string,
              mwe_left,mwe_right,
              mwe_mid=None,
              ignore_jumps=True):
    diffop=_the_diffop(diffop_or_string)
    log.log(15,"Creating prematrix for XXX")
    opt_mid=[]
    if mwe_mid:
        opt_mid=[mwe_mid]
    return ocaml.raw_make_prematrix(diffop,mwe_left,mwe_right,
                                    opt_mid,ignore_jumps)

def prematrix_applicator(prematrix,
                         field_mid=None,
                         interface_coeffs=[],
                         petsc_name="",
                         result="cofield"):
    log.log(15,"Creating prematrix applicator ")
    # XXX Note: interface coeffs have special conventions on "jokers":
    # (-2,-2,1.0) would mean: include all interfaces with coefficient 1.0
    # (-> see __parse_if_coeffs in pyfem2.ml) - Document this!
    opt_mid=[]
    if field_mid:
        opt_mid=[field_mid]
    app=ocaml.prematrix_to_applicator(interface_coeffs,
                                      opt_mid,petsc_name,prematrix)

    if result=="cofield":
        def py_app(field,target=None):
            opt_target=[]
            if target:
                opt_target=[target]
            return app(opt_target,field)
        return py_app
    elif result=="field":
        buffer=make_cofield(ocaml.prematrix_mwe(prematrix,"mwe_left"))
        print "DDD Buffer: ",buffer
        def py_app(field,target=None):
            cofield=app([buffer],field)
            return cofield_to_field(cofield,target=target)
        return py_app

def prematrix_applicator_ff(prematrix,
                         field_mid=None,
                         interface_coeffs=[],
                         petsc_name="",
                         result="field"):
    log.log(15,"Creating prematrix applicator field->field ")
    # XXX Note: interface coeffs have special conventions on "jokers":
    # (-2,-2,1.0) would mean: include all interfaces with coefficient 1.0
    # (-> see __parse_if_coeffs in pyfem2.ml) - Document this!
    opt_mid=[]
    if field_mid:
        opt_mid=[field_mid]
    app=ocaml.prematrix_to_applicator_ff(interface_coeffs,
                                      opt_mid,petsc_name,prematrix)

    if result=="field":
        def py_app(field,target=None):
            opt_target=[]
            if target:
                opt_target=[target]
            return app(opt_target,field)
        return py_app
    elif result=="cofield":
        def py_app(field,target=None):
            cofield=app([buffer],field)
            return field_to_cofield(app([],field),target=target)
        return py_app

def diffop_applicator(diffop_or_string,mwe_left,mwe_right,
                      field_mid=None,
                      interface_coeffs=[],
                      petsc_name="",
                      result="cofield"
                      ):
    """In many situations (when we do not use a changing middle field),
    we can map a diffop directly to an applicator by combining prematrix
    and prematrix_applicator"""
    log.debug("In diffop_applicator for '%s'" % diffop_or_string)
    mwe_mid=None
    ignore_jumps=True
    if field_mid:
        mwe_mid=ocaml.get_mwe(field_mid)
    if len(interface_coeffs)>0:
        log.debug("Setting ignore_jumps=False")
        ignore_jumps=False
    else:
        log.debug("Setting ignore_jumps=True")

    pmx=prematrix(diffop_or_string,mwe_left,mwe_right,mwe_mid=mwe_mid,
                  ignore_jumps=ignore_jumps)
    app=prematrix_applicator(pmx,field_mid=field_mid,
                             interface_coeffs=interface_coeffs,
                             petsc_name=petsc_name,result=result)
    return app

    

def dirichlet_bc_zero(dof_name_indices,coords):
    return 0.0

def laplace_solver(prematrix,
                   dirichlet_bcs=[],
                   mwe_mid=None,
                   interface_coeffs=[],
                   petsc_name=""):
    log.log(15,"Creating laplace_solver")
    opt_mid=[]
    if mwe_mid:
        opt_mid=[mwe_mid]
    solver=ocaml.laplace_solver(interface_coeffs,dirichlet_bcs,
                                opt_mid,petsc_name,prematrix)
    # We also wrap up this ml-generated solver in a more palatable way:
    def py_solver(source,dbc_values=dirichlet_bc_zero,target=None):
        opt_target=[]
        if target:
            opt_target=[target]
        return solver(opt_target,dbc_values,source)
    return py_solver

def laplace_solver_bem(prematrix,
                       inside_regions=[],
                       petsc_name=""):
    log.log(15,"Creating laplace_solver for BEM")
    solver=ocaml.laplace_solver_bem(inside_regions,petsc_name,prematrix)
    # We also wrap up this ml-generated solver in a more palatable way:
    def py_solver(source,target=None):
        opt_target=[]
        if target:
            opt_target=[target]
        return solver(opt_target,source)
    return py_solver


def cofield_to_field(cofield,target=None, box_method=False):
    opt_target=[]
    if target:
        opt_target=[target]
    return ocaml.cofield_to_field(opt_target, cofield, box_method)

def field_to_cofield(field,target=None, box_method=False):
    opt_target=[]
    if target:
        opt_target=[target]
    return ocaml.field_to_cofield(opt_target, field, box_method)


def site_wise_applicator(parameter_names,code,field_mwes=[],cofield_mwes=[],strict_mwe_check=True):
    app=ocaml.site_wise_applicator(field_mwes,cofield_mwes,parameter_names,
                                   code,strict_mwe_check)
    def py_app(params,fields=[],cofields=[]):
        return app(params,fields,cofields)
    return py_app

def integrate_field(field,name_stem=""):
    """Integrates the degrees of freedom with a given name stem
    contained in field over all space
    """
    log.debug("Integrating field %s" % name_stem)
    return ocaml.integrate_field(field,name_stem)

def probe_field(field,position,subfieldname=""):
    """Returns the interpolated value of a collection of degrees
    of freedom with given name in given field at a specific position.

    Trivial interface to probe_field (all it does it to convert the position
    entries to float in case they are ints).

    This is not used by nmag.
    """
    
    #convert position list into list of floats (in case they are ints, etc)
    position = map( lambda a : float(a), position)

    return ocaml.probe_field(field,subfieldname,position)

def plot_scalar_field(field,
                      dof_name,filename,
                      plot_order=1,plot_edges=True,
                      scaling=[-6.0,7.0,40.0,40.0],
                      color_scheme=[(-2.0,[0.2,0.2,1.0]),
                                    (0.0,[0.8,0.8,0.8]),
                                    (2.0,[1.0,0.2,0.2])]):
    ocaml.plot_scalar_field(field,dof_name,filename,color_scheme,plot_order,plot_edges,scaling)
    return None




def field2numpy( field, only_dofname=None):
    """ def field2numpy( field, only_dofname=None)

    Returns a list of tuples. One tuple is returned for every degree of freedom of the field.

    The first entry is the name of the degree of freedom.

    The second entry is a numerix array containing the data (in site order).
    """

    import numpy as numerix

    def _field2numerix(field,name,dim):

        nr_points = data_length(field)/dim
        log.debug("Creating Matrix with size=(%d,%d)" % (nr_points,dim))
        data = numerix.zeros((nr_points,dim),'f') 

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

        field_entry_wise(field,cb)

        return data


    if ocaml.sys_ocamlpill_type(field) != 'FEM Field':
        raise NfemValueError, "Expect FEM Fields as input"

    #First, create the following data structure.
    #dof_to_process = [field, dof, dim]
    dof_to_process = []

    #learn about dof in this field:
    content = data_doftypes(field)

    if len(content) >1:
        raise NotImplementedError,"This has never been tested with more than one degree of freedom per field"
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
    log.log(15,"About to sample field(s) '%s' " % (names))


    results = []
    for dof in dof_to_process:
        field, name,dim = dof
        if only_dofname==1:
            name=only_dofname
        log.log(15,"Adding %d-dim dof %s to return data list" % (dim,name))
        results.append((name,_field2numerix(field,name,dim)))
    
    return results


def tensor2componentstrings(name,data,unit=None,underscore='_'):
    """The behaviour of this function is best explained with short
    examples:

    >>> print tensor2componentstrings('E',4.0)
    ('E', 4.0)

    >>> print tensor2componentstrings('M_Py',[1,2,3])
    (['M_Py_0', 'M_Py_1', 'M_Py_2'], [1, 2, 3])

    >>> print tensor2componentstrings('eps_Py',[[11,12,13],[21,22,23],[31,32,33]])
    (['eps_Py_00', 'eps_Py_01', 'eps_Py_02', 'eps_Py_10', 'eps_Py_11', 'eps_Py_12', 'eps_Py_20', 'eps_Py_21', 'eps_Py_22'], [11, 12, 13, 21, 22, 23, 31, 32, 33])

    :Parameters:
      `name` : string
         name (dofname)

      `data` : float or list (of lists (of lists)), or numpy array
         the data

      `underscore` : string
         the string used to separate dofname from component indices (default is '_')

      `unit` : SI object or ``None`` (default is ``None``)
         can be used to specify unit-objects. If so, units will be
         returned for each component.

    :Returns:
       `(componentnames,componentdata) (if ``unit``==None [default])
         Both tuple entries are lists 

       `(componentnames,componentdata,componentunits) (if ``unit`` provided)
         All triple entries are lists 
    """

    import numpy

    log.debug("Entering tensor2componentstrings(name,data,unit=None,underscore")
    log.debug("With name=%s, data=%s, unit=%s" % (str(name),str(data),str(unit)))

    #convert into numpy.array to exploit 'shape' attribute (=dofmaxindices)
    try:
        nd = numpy.array(data)
    except ValueError,msg:
        log.error("something went wrong with name=%s and data=%s" % (name,str(data)))
        log.error("Error mgs is " + str(msg))
        raise ValueError,msg
    
    #must be vector or tensor
    componentnames = []
    componentdata = []
    componentunit = []
    
    if nd.shape == (): #data is a float
        componentnames=[name]
        componentdata=[data]
        componentunit=[unit]
    elif len(nd.shape) == 1: #data is vector
        for i in range(nd.shape[0]):
            componentnames.append(name + underscore+"%d"%i)
            componentdata.append(data[i])
            componentunit.append(unit)
    elif len(nd.shape) ==2: #data is 'matrix'
        rows, cols = nd.shape
        for j in range(rows):
            rownames,rowdata = tensor2componentstrings(name+underscore+"%d"%j,data[j],underscore='')
            componentnames += rownames
            componentdata += rowdata
    else:
        msg= "Rank for data '%s' seems to be %d (shape is %s)" % (name,len(nd.shape),str(nd.shape))
        msg+="\nCan't deal with rank-3 or larger tensor data yet (not hard to do; just missing)"

        raise NotImplementedError,msg

    if unit:
        return componentnames,componentdata,componentunit
    else:
        return componentnames,componentdata

