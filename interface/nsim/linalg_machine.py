# (C) 2007 Dr. Thomas Fischbacher, SES, Univ. Southampton

import ocaml

# NOTE: we need way better internal representation wrt type error handling!
# lam_vector should produce some very basic vector_spec object (that only
# contains an array with all the data, but provides guarantees that this is
# a vector_spec!), which is then used by make_lam.

def lam_vector(name,mwe_name,restriction="",is_field=True,initial_value=0.0):
    return [name,mwe_name,restriction,is_field,initial_value]

def lam_operator(name,mwe_name_le,mwe_name_ri,op,matoptions=[],mwe_name_mid=None):
    opt_mwe_name_mid=[]
    if mwe_name_mid:
        opt_mwe_name_mid.append(mwe_name_mid)
    return [name,mwe_name_le,mwe_name_ri,op,matoptions,opt_mwe_name_mid]

def lam_bem(name, is_hlib=False, mwe_name="", dof_name=["phi"],
            boundary_spec="outer and material", lattice_info=[], matoptions=[],
            hlib_params=None):
    hlp = {'algorithm':4, 'nfdeg':2, 'nmin':50, 'eta':2.0, 'eps_aca':0.00001,
           'eps':0.00001, 'p':3, 'kmax':50}
    if hlib_params != None:
        for param_name in hlib_params:
            if hlp.has_key(param_name):
                hlp[param_name] = hlib_params[param_name]

            else:
                raise "Unknow HLib parameter '%s'" % param_name
    hlp_list = [hlp['algorithm'], hlp['nfdeg'], hlp['nmin'], hlp['eta'],
                hlp['eps_aca'], hlp['eps'], hlp['p'], hlp['kmax']]
    return [name, is_hlib, mwe_name, dof_name, boundary_spec,
            lattice_info, matoptions, hlp_list]

def lam_ksp(name,matrix_name,precond_name=None,
            ksp_type=None,pc_type=None,initial_guess_nonzero=None,
            rtol=None, atol=None, dtol=None, maxits=None,nullspace_subfields=None,
            nullspace_has_constant=False
            ):
    if(not(precond_name)):
        precond_name=matrix_name

    if(ksp_type==None):
        ksp_type=[]
    else:
        ksp_type=[ksp_type]

    if(pc_type==None):
        pc_type=[]
    else:
        pc_type=[pc_type]

    if(initial_guess_nonzero==None):
        initial_guess_nonzero=[]
    else:
        initial_guess_nonzero=[initial_guess_nonzero]

    if(rtol==None):
        rtol=[]
    else:
        rtol=[rtol]

    if(atol==None):
        atol=[]
    else:
        atol=[atol]

    if(dtol==None):
        dtol=[]
    else:
        dtol=[dtol]

    if(maxits==None):
        maxits=[]
    else:
        maxits=[maxits]

    if(nullspace_subfields==None):
        nullspace_subfields=[]
    else:
        nullspace_subfields=[[nullspace_has_constant,nullspace_subfields]]

    return [name,matrix_name,precond_name,ksp_type,pc_type,initial_guess_nonzero,
            rtol,atol,dtol,maxits,nullspace_subfields]

def lam_local(name,field_mwes=[],cofield_mwes=[],equation=None,c_code=None,aux_args=[]):
    code=None
    must_parse=False

    if(c_code<>None):
        code=c_code

    if(equation<>None):
        if(code<>None):
            error("Cannot both provide equation and C code specification!")
        else:
            code=equation
            must_parse=True

    return [name,code,aux_args,field_mwes,cofield_mwes,must_parse]

def lam_program(name,args_fields=[],args_cofields=[],commands=[]):
    return [name,args_fields,args_cofields,commands]

def lam_jplan(name,mwe_lhs,mwes,derive_me,eom,debugprint=False):
    return [name,mwe_lhs,mwes,derive_me,eom,debugprint]

def lam_timestepper(name,
                    names_phys_mwes,                      #for example ['m','T','dmdt','dTdt','H_total']
                                                          #contains all fields used in the time integrator
                                                          #(including its Jacobian). Primary fields first,
                                                          #then derivatives of primary fields,
                                                          #then auxiliary fields.
                    names_phys_field_buffers,             #corresponding names of distributed vector resources, eg
                                                          #'v_m'.
                    name_script_velocities,               #name of linalg script that updates all configuration
                                                          #velocities (i.e. the time derivatives of primary fields)
                                                          #Note: for nmag, this is NOT 'rhs', but 'update_dmdt'.
                                                          #      The former would involve copying data from and to
                                                          #      the master buffers. We need the version that operates
                                                          #      on the distributed vectors.
                                                          #      (In fact, the 'rhs' script is a wrapper around 'compute_dmdt'
                                                          #       that additionally distributes the data from the master
                                                          #       buffer to slaves, does the calculation, and gathers the data
                                                          #       back to the master.)
                    nr_primary_fields=1,                  # For primary fields ['m','T'] this would be 2.
                    name_jacobian=None,                   
                    jacobi_prealloc_diagonal=60,          # number of the expected matrix entries where this node is
                                                          # responsible for the corresponding part of the input vector.
                                                          # the node is responsible.
                    jacobi_prealloc_off_diagonal=20,      # number of the expected matrix entries where the column index
                                                          # is associated with a different computational node
                    pc_same_nonzero_pattern=True,         # This should hold in most cases.
                    pc_rtol=None,                           # Tolerance parameters for ksp solver used by sundials.
                    pc_atol=None,
                    pc_dtol=None,
                    pc_maxit=None,
                    max_order=2,
                    krylov_max=300,
                    jacobi_eom="",
                    phys_field_derivs=[]
                    ):
    use_jacobian=True
    name_jacobian="no_jacobian"
    #if name_jacobian==None:
    #    name_jacobian="no_jacobian"
    #    use_jacobian=False

    if pc_rtol == None:
        pc_rtol=[]
    else:
        pc_rtol=[pc_rtol]

    if pc_atol == None:
        pc_atol=[]
    else:
        pc_atol=[pc_atol]

    if pc_dtol == None:
        pc_dtol=[]
    else:
        pc_dtol=[pc_dtol]

    if pc_maxit == None:
        pc_maxit=[]
    else:
        pc_maxit=[pc_maxit]

    return [name,
            name_jacobian,
            nr_primary_fields,
            jacobi_prealloc_diagonal,
            jacobi_prealloc_off_diagonal,
            use_jacobian,
            pc_same_nonzero_pattern,
            pc_rtol,
            pc_atol,
            pc_dtol,
            pc_maxit,
            names_phys_field_buffers,
            max_order,
            krylov_max,
            name_script_velocities,
            names_phys_mwes,
            jacobi_eom,
            phys_field_derivs
            ]



def make_lam_body(name,
                  intensive_params=[],
                  mwes=[],
                  vectors=[], # internal_buffers
                  operators=[], # operator_matrices
                  bem_matrices=[],
                  ksps=[],
                  local_operations=[],
                  jacobi_plans=[],
                  programs=[],
                  timesteppers=[],
                  lam_debugfile=None):
    lam = ocaml.raw_make_lam(mwes,
                             vectors,
                             operators,
                             bem_matrices,
                             ksps,
                             local_operations,
                             jacobi_plans,
                             programs,
                             intensive_params,
                             timesteppers,
                             name) # prefix
    return lam

def make_lam(*args, **kwargs):
    if kwargs["lam_debugfile"]:
        import pprint
        pp = pprint.PrettyPrinter(indent=4,width=72)
        f=open(kwargs["lam_debugfile"],"w")
        f.write("=== LAM Specification ===\nargs:\n-----\n%s\n\nkwargs:\n-------\n%s\n"%(pp.pformat(args),pp.pformat(kwargs)))
        f.close()
    return make_lam_body(*args,**kwargs)

# DEPRECATED: provided for compatibility
make_linalg_machine = make_lam

def make_lam_cvode(linalg_machine,
                   initial_field,
                   name_jacobian,
                   name_jacobi_script, # the script to re-build the Jacobi matrix
                   name_rhs_script,
                   same_nonzero_pattern=True,
                   max_order= 2,
                   krylov_max=300):
    cvode_and_timings = ocaml.raw_make_lam_cvode(linalg_machine,
                                                 initial_field,
                                                 name_jacobian,
                                                 name_jacobi_script,
                                                 name_rhs_script,
                                                 same_nonzero_pattern,
                                                 max_order,
                                                 krylov_max)
    return cvode_and_timings

def cvode_advance(cvode,result_vector,target_time,max_steps=-1):
    return ocaml.raw_cvode_advance(cvode,result_vector,target_time,max_steps)

