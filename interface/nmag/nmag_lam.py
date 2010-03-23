import math
import ocaml
from nsim import linalg_machine as nlam
import logging
from nmag_exceptions import *
import nsim.preprocess as preprocess


# Set up a logger object which will collect and dispatch log messages
# according to loglevel:
logger = logging.getLogger('nmag')

# given a list 'li', this returns a list where None entries have been removed:
without_nones = lambda li: [x for x in li if x != None]

def _local_couplings_ccode(local_couplings):
    """[internal helper function]
       For simulations involving two magnetic atomic species in the
       same crystal (such as the DyFe2 Laves phase structure),
       we have to introduce an exchange-type local coupling between
       magnetic momenta associated to the different atomic species.
    
       Given the dictionary of material-material coupling strengths,
       generate the C-code which adds the couplings to the total
       magnetic field.
       The dictionary of couplings is something like:
         {'Dy':{'Fe': SI(-9876, 'A/m')}, 'Fe':{'Dy': SI(-1234, 'A/m')}}
    """
    # NOTE: the structure of this diary is part of the official nmag_lam
    # calling interface. We should at some point deprecate dictionaries
    # of this structure and instead switch over to dictionaries of the form
    # {'mat1:mat2':<coupling>}
    h_lc_ccode = "" # C-code for the H_mat field strength
    e_lc_ccode = "" # C-code for the E_mat energy density
    for mat1 in local_couplings:
        m1_field_stem = "m_%s" % mat1
        h_field_stem = "H_lc_%s" % mat1
        e_field_stem = "E_lc_%s" % mat1

        h_ccode = "int i; for(i=0; i<3; i++) %s(i) = 0.0;\n" % h_field_stem
        e_ccode = "%s = 0.0;\n" % e_field_stem
        for mat2 in local_couplings[mat1]:
            m2_field_stem = "m_%s" % mat2
            c1, c2 = local_couplings[mat1][mat2]
            h_ccode += (
                "if (have_%s) {\n" % m2_field_stem +
                "  int i;\n" +
                "  for(i=0; i<3; i++)\n" +
                "    %s(i) += %s*%s(i);\n" % (h_field_stem, c1, m2_field_stem) +
                "}\n")
            e_ccode += (
                "if (have_%s) {" % m2_field_stem +
                "  int i;\n" +
                "   for(i=0; i<3; i++)\n" +
                "     %s += %s*%s(i)*%s(i);\n"
                % (e_field_stem, c2, m1_field_stem, m2_field_stem) +
                "}\n")

        # We assume here that have_H_lc_X == have_m_X
        h_lc_ccode += "if (have_%s) {\n%s}\n" % (m1_field_stem, h_ccode)
        e_lc_ccode += "if (have_%s) {\n%s}\n" % (m1_field_stem, e_ccode)

    return (h_lc_ccode, e_lc_ccode)

def _extended_properties_by_region(region_materials,min_region=-1,extra_pbr=[]):
    """Internal: Generate properties_by_region"""
    pbr={}

    def add_prop(region,prop):
        if pbr.has_key(region):
            pbr[region][prop]=True
        else:
            pbr[region]={prop: True}

    # Ensure all the outer regions have the property "outer":
    for vac in range(min_region,0):
        add_prop(vac,"outer")

    for nr_region in range(len(region_materials)):
        add_prop(nr_region,str(nr_region))
        materials = region_materials[nr_region]
        for m in materials:
            add_prop(nr_region,m.name)
            for p in m.properties:
                add_prop(nr_region,p)
                
    # Now that we initialized these hashes, map them back to lists:

    def sorted_keys(h):
        k=h.keys()
        k.sort()
        return k
    
    srk=sorted_keys(pbr)

    result=[(k,sorted_keys(pbr[k])) for k in srk]
    logger.info("properties_by_region: %s" % repr(result))
    return result
    
def nmag_lam(timestepper,
             name="nmag",
             mesh=None,
             region_materials=[],
             min_region=-1,
             # [ [mat1 of region1, mat2 of region1, ...],
             #   [mat1 of region2, mat2 of region2, ...],
             #   ... ]
             properties_by_region=[],
             intensive_params=["TIME"],
             # this is the list of global parameters known to the simulation.
             # nsim normally groups time with intensive thermodynamic quantities
             # like pressure, temperature, etc. -- in nmag, this normally is
             # just time.
             energy_factor=1.0,
             local_couplings={},
             periodic_bc=None,
             use_hlib=False,
             su_temperature=None,
             user_seed_T=0,
             do_demag=True,
             # ^ NOTE: we can disable H_demag with this parameter, but that is - strictly speaking -
             # not yet implemented properly, as we rather should adjust all the field dependencies
             # so that e.g. querying E_total will not auto-trigger recomputation of H_demag!
             # (Minor issue, as this only happens when doing field probes, not in the main
             # simulation loop.)
             #
             # Also, we presumably need not build all these H_demag related operators if
             # do_demag is false.
             #
             # Suggestion: should change this later on into demag_method="FEM/BEM" (default),
             # while offering alternative options "FEM/FEM/BEM", "SC-FEM/BEM", "FEM/HLIB"
             # and None.
             do_anisotropy_jacobian=False,
             ksp_tolerances={},
             hlib_params=None, # Parameters for HLib, if used
             lam_debugfile=None, # file to log LAM spec to
             pre_rhs_funs=[],
             post_rhs_funs=[]
             ):

    simulation_prefix=name

    def get_tol(name):
        if ksp_tolerances.has_key(name):
            return ksp_tolerances[name]
        else:
            return None

    ext_pbr=_extended_properties_by_region(region_materials,min_region,properties_by_region)

    # Get the meshes' dimension (simulations need not necessarily be 3d --
    # we can e.g. use "1d meshes" for multilayers)
    dim = ocaml.mesh_dim(mesh)
    thermal = False

    if su_temperature <> None:
        thermal=True
        intensive_params += ["THERMAL_DELTA_T"]
    else:
        su_temperature=0.0

    # properly translate periodic_bc internally to our 
    # conventions for passing optional args to ocaml:
    is_periodic=False
    if periodic_bc == None:
        periodic_bc = []
    else:
        periodic_bc = [periodic_bc]
        is_periodic = True

    if(use_hlib and is_periodic):
        raise NmagUserError, "Hlib cannot yet deal with periodic boundary conditions"

    if(mesh==None):
        raise NmagUserError, "No mesh provided!"

    if(region_materials==[]):
        raise NmagUserError, "No region_materials provided!"

    elem_H_demag = ocaml.make_element("H_demag", [3], dim, 1)
    elem_scalar = ocaml.make_element("scalar", [], dim, 1)

    all_materials={}

    grad_m_mats = []
    for region in region_materials:
        for mat in region:
            logger.info("Processing material '%s'" % (mat.name))
            logger.log(15,"Processing material '%s' (%s)" % (mat.name,repr(mat)))
            logger.debug("Processing material '%s'\n%s" % (mat.name,str(mat)))
            if(not all_materials.has_key(mat.name)):
                elem_E = ocaml.make_element("E_" + mat.name, [], dim, 1)
                elem_m = ocaml.make_element("m_" + mat.name, [3], dim, 1)
                if mat.su_llg_stt_prefactor != 0:
                    grad_m_mats.append(mat.name)
                    elem_grad_m = ocaml.make_element("grad_m_" + mat.name, [3, 3], dim, 1)
                else:
                    elem_grad_m = ocaml.empty_element
                all_materials[mat.name] = (mat, elem_E, elem_m, elem_grad_m)

    need_grad_m = (len(grad_m_mats) > 0)

    # In nmag, multi-material simulations presently are implemented by
    # dynamically generating the strings describing the equation of
    # motion and related operators from the materials
    # specification. An alternative design would be to define one
    # micromagnetic physical sector for each material and couple these
    # using multiphysics capabilities. Most likely, this strategy
    # makes more sense in the long run and nmag v1.0 will hence
    # utilize it. Building up abstract symbolic equations by code is a bit
    # against the point of using abstract formalism, but for the purpose of
    # demonstrating that an efficient simulation compiler core with symbolic
    # capabilities can be built, while also providing a useful application,
    # that is okay. Eventually, all this code will vanish and be replaced by
    # a more expressive formalism that is directly available to the user.
    # What is missing at present is mostly the parser.
    
    # Suppose we have many materials in the system and hence have to write
    # down the equations of motion for all the materials. Equations
    # may be written as: lhs_m = c1_m * term1_m + c2_m * term2_m + ...,
    # where m is the material index.
    # Each of {lhs_m, c1_m, term1_m, c2_m, ...} may contain numbers which
    # may change from material to material.
    #
    # This (internal) function takes template strings for each of these terms
    # and produces all the equations as its output.
    # The equations are concatenated with the provided string ``concat``,
    # and are sandwitched between the strings ``prefix`` and ``postfix``.
    # The terms in equations are concatenated using the provided string
    # ``concat_terms`` (which is " + " by default).
    # ``template_lhs`` is the template string for the left hand side (lhs_m)
    # while ``template_rhs_terms`` is a list of tuples
    # ``(coeff_template, term_template)`` made of the template
    # for the coefficient and the term. If, after the substitution,
    # ``float(coeff_template) == 0.0``, then the term is omitted.
    def vivify_from_terms(template_lhs, template_rhs_terms,
                          prefix="", postfix="", concat="",
                          concat_terms=" + ",
                          materials=all_materials):
        equations = []
        param = {}
        # First we do the material-independent parameters
        param["dmdt"] = "dmdt_$MAT$"
        param["m"] = "m_$MAT$"
        param["H_total"] = "H_total_$MAT$"
        param["grad_m"] = "grad_m_$MAT$"
        param["dm_dcurrent"] = "dm_dcurrent_$MAT$"
        # Then we run over all the materials
        for mat_name in materials:
            mat = all_materials[mat_name][0]
            param["MAT"] = mat.name
            param["ABS_MAG"] = mat.su_Ms
            param["VOL_CHARGE_FACTOR"] = mat.su_Ms*mat.scale_volume_charges
            param["SURF_CHARGE_FACTOR"] = mat.su_Ms
            param["negJ"] = -mat.su_exch_prefactor
            param["LLG_C1"] = mat.su_llg_coeff1
            param["LLG_C2"] = mat.su_llg_coeff2
            param["LLG_C3"] = mat.su_llg_normalisationfactor
            param["LLG_C4"] = mat.su_llg_stt_adiab
            param["LLG_C5"] = mat.su_llg_stt_nadiab
            param["THERMAL_FACTOR"] = mat.su_thermal_factor
            param["STT_PREFACTOR"] = mat.su_llg_stt_prefactor
            # Run over all the RHS terms
            terms = []
            for template_coeff, template_term in template_rhs_terms:
                coeff = preprocess.preprocess(template_coeff, param)
                try:
                    coeff_value = float(coeff)
                except:
                    coeff_value = 1.0
                if coeff_value != 0.0:
                    coeff_str = ""
                    if coeff_value != 1.0:
                        coeff_str = "(%s) * " % coeff
                    term = preprocess.preprocess(template_term, param)
                    terms.append("%s%s" % (coeff_str, term))

            lhs = preprocess.preprocess(template_lhs, param)
            equation = lhs + concat_terms.join(terms)
            if len(equation.strip()) > 0:
                equations.append(equation)
        return prefix + concat.join(equations) + postfix

    def vivify_template(tpl, prefix="", postfix="", concat="",
                        materials=all_materials):
        return vivify_from_terms("", [("1.0", tpl)], prefix, postfix, concat,
                                 materials=materials)

    # We have to define a number of elements which we associate to the
    # cells of the mesh. Again, the issue is complicated by the fact
    # that a region can carry more than a single magnetic atomic species.

    elems_m = map(lambda region:reduce(ocaml.fuse_elements,
                                       map(lambda mat:all_materials[mat.name][2],
                                           region),
                                       ocaml.empty_element),
                  region_materials)




    elems_E = map(lambda region:reduce(ocaml.fuse_elements,
                                       map(lambda mat:all_materials[mat.name][1],region),
                                       ocaml.empty_element),
                  region_materials)

    elems_H_demag = map(lambda region:elem_H_demag,region_materials)

    elems_scalar = map(lambda region:elem_scalar,region_materials)

    elems_grad_m = map(lambda region: reduce(ocaml.fuse_elements,
                                             map(lambda mat: all_materials[mat.name][3], region),
                                             ocaml.empty_element),
                       region_materials)

    material_names = [all_materials[mn][0].name for mn in all_materials]

    def enumerated_list(z):
        return zip(range(len(z)),z)

    def m_relabeled(new_label):
        return map(lambda mn: ("m_" + mn, new_label + mn), material_names)

    def E_relabeled(new_label):
        return map(lambda mn: ("E_" + mn, new_label + mn), material_names)

    logger.debug("ELEMS M: %s" % enumerated_list(elems_m))

    mwe_m=ocaml.make_mwe("m",mesh,enumerated_list(elems_m),[],ext_pbr)
    mwe_E=ocaml.make_mwe("E",mesh,enumerated_list(elems_E),[],ext_pbr)
    mwe_H_demag=ocaml.make_mwe("H_demag",mesh,enumerated_list(elems_H_demag),[],ext_pbr)
    mwe_scalar=ocaml.make_mwe("scalar",mesh,enumerated_list(elems_scalar),[],ext_pbr)
    mwe_grad_m = None
    mwe_current_density = None
    mwe_dm_dcurrent = None
    if need_grad_m:
        mwe_grad_m=ocaml.make_mwe("grad_m",mesh,enumerated_list(elems_grad_m),[],ext_pbr)
        mwe_current_density=ocaml.mwe_sibling(mwe_H_demag,
                                              "current_density",
                                              "current_density/H_demag",
                                              [("H_demag","current_density")])
        mwe_dm_dcurrent=ocaml.mwe_sibling(mwe_m,
                                          "dm_dcurrent",
                                          "dm_dcurrent/m",
                                          m_relabeled("dm_dcurrent_"))

    # A number of meshes-with-elements (mwes, to be renamed to "field layouts")
    # are structurally equal, but just need their degrees of freedom renamed.
    # These are generated below:

    mwe_dmdt=ocaml.mwe_sibling(mwe_m,"dmdt","dmdt/m",m_relabeled("dmdt_"))
    mwe_M=ocaml.mwe_sibling(mwe_m,"M","M/m",m_relabeled("M_"))
    mwe_H_exch=ocaml.mwe_sibling(mwe_m,"H_exch","H_exch/m",m_relabeled("H_exch_"))
    mwe_H_anis=ocaml.mwe_sibling(mwe_m,"H_anis","H_anis/m",m_relabeled("H_anis_"))
    mwe_H_total=ocaml.mwe_sibling(mwe_m,"H_total","H_total/m",m_relabeled("H_total_"))

    mwe_H_ext=ocaml.mwe_sibling(mwe_H_demag,"H_ext","H_ext/H_demag",[("H_demag","H_ext")])
    mwe_E_exch=ocaml.mwe_sibling(mwe_E,"E_exch","E_exch/E",E_relabeled("E_exch_"))
    mwe_E_total=ocaml.mwe_sibling(mwe_E,"E_total","E_total/E",E_relabeled("E_total_"))
    mwe_E_demag=ocaml.mwe_sibling(mwe_E,"E_demag","E_demag/E",E_relabeled("E_demag_"))
    mwe_E_ext=ocaml.mwe_sibling(mwe_E,"E_ext","E_ext/E",E_relabeled("E_ext_"))
    mwe_E_anis=ocaml.mwe_sibling(mwe_E,"E_anis","E_anis/E",E_relabeled("E_anis_"))

    # The "pinning field" named "pin" is a bookkeeping trick, a field
    # whose value is either 0 or 1 at every site, and multiplies
    # dm/dt. This allows us to nail down magnetisation at some sites
    # while at the same time keeping our time integrator happy:

    mwes_for_dmdt = ["m", "dmdt", "H_total", "pin"]
    if need_grad_m: mwes_for_dmdt += ["dm_dcurrent"]
    fields_for_dmdt = ["v_"+x for x in mwes_for_dmdt]

    mwes_for_H_total = ["H_total", "H_exch", "H_ext", "H_anis"]
    mwes_for_E_total = ["m", "E_anis", "E_ext", "E_demag", "E_exch",
                        "E_total", "H_ext", "H_demag", "H_exch", "H_total"]
    template_eq_H_total = "H_total_$MAT$(j) <- H_ext(j) + H_exch_$MAT$(j) + H_anis_$MAT$(j)"


    if do_demag or True: # disabled check - always build this
        template_eq_H_total +=  " + H_demag(j)"
        mwes_for_H_total.append("H_demag")
        mwe_rho=ocaml.mwe_sibling(mwe_scalar,"rho","rho/scalar",[("scalar","rho")])
        mwe_phi=ocaml.mwe_sibling(mwe_scalar,"phi","phi/scalar",[("scalar","phi")])
        mwe_pin=ocaml.mwe_sibling(mwe_scalar,"pin","pin/scalar",[("scalar","pin")])

    norm_dist_fun_m = ocaml.mwe_norm_dist_fun(mwe_m)

    if thermal:
        template_eq_H_total += " + H_therm_$MAT$(j)"
        mwes_for_H_total += ["H_therm"]

    do_lc = (len(local_couplings) > 0)
    mwe_H_lc = None
    mwe_E_lc = None
    extra_E_contribs = ""
    if do_lc:
        mwes_for_H_total.append("H_lc")
        mwes_for_E_total.append("E_lc")
        mwe_H_lc = ocaml.mwe_sibling(mwe_m, "H_lc", "H_lc/m",
                                     m_relabeled("H_lc_"))
        mwe_E_lc = ocaml.mwe_sibling(mwe_E, "E_lc", "E_lc/E",
                                     E_relabeled("E_lc_"))
        template_eq_H_total += " + H_lc_$MAT$(j)"
        extra_E_contribs += " + E_lc_$MAT$"

    fields_for_E_total = ["v_" + name for name in mwes_for_E_total]

    fields_for_H_total = ["v_" + x for x in mwes_for_H_total]

    template_eq_H_total += ";"

    # Build the equations of motion from a template parametrized by
    # material (so that multi-magnetic-species simulations use the
    # right equations):

    lhs = "$dmdt$(i) <-"
    rhs_coeffterms = \
      [("$LLG_C1$", "eps(i,j,k) * $m$(j) * $H_total$(k) * pin"),
       ("$LLG_C2$", "eps(i,j,k) * $m$(j) * eps(k,p,q) * $m$(p) * $H_total$(q) * pin"),
       ("$LLG_C3$", "(1.0 + (-1.0)*$m$(j)*$m$(j)) * $m$(i) * pin"),
       ("$LLG_C4$", "eps(i,j,k) * $m$(j) * eps(k,p,q) * $m$(p) * $dm_dcurrent$(q) * pin"),
       ("$LLG_C5$", "eps(i,j,k) * $m$(j) * $dm_dcurrent$(k) * pin")]

    eq_rhs_full = \
      vivify_from_terms(lhs,
                        rhs_coeffterms,
                        prefix="%range i:3, j:3, k:3, p:3, q:3;\n",
                        concat=";\n\n",
                        postfix=";\n\n")

 
    template_eq_rhs_jacobi_ts1 = """
dmdt_$MAT$(i) <-
    $LLG_C1$ * eps(i,j,k) * m_$MAT$(j) * H_total_$MAT$(k) * pin
  + $LLG_C2$ * eps(i,j,k) * m_$MAT$(j) * eps(k,p,q) * m_$MAT$(p) * H_total_$MAT$(q) * pin;
"""

    template_eq_rhs_jacobi_ts2 = """
dmdt_$MAT$(i) <-
    $LLG_C1$ * eps(i,j,k) * m_$MAT$(j) * H_exch_$MAT$(k) * pin
  + $LLG_C2$ * eps(i,j,k) * m_$MAT$(j) * eps(k,p,q) * m_$MAT$(p) * H_exch_$MAT$(q) * pin;
"""

    template_eq_M = "M_$MAT$(i) <- ($ABS_MAG$)*m_$MAT$(i);"
    template_op_H_exch = "($negJ$)*<d/dxj H_exch_$MAT$(k)||d/dxj m_$MAT$(k)>"
    template_op_div_m = \
      (   "($VOL_CHARGE_FACTOR$)*<rho||d/dxj m_$MAT$(j)> "  # Volume charges
       "+ ($SURF_CHARGE_FACTOR$)*<rho||D/Dxj m_$MAT$(j)>" ) # Surface charges
    template_op_grad_m = "<$grad_m$(i, j)||d/dxj $m$(i)>"
    postfix_grad_m = ", i:3, j:%d" % dim

    # For energies, we follow an all-or-nothing philosophy: we update them all at once:

    template_E = """
E_demag_$MAT$ <- -0.5*($ABS_MAG$)*(%f)*m_$MAT$(i)*H_demag(i);
E_exch_$MAT$ <- -0.5*($ABS_MAG$)*(%f)*m_$MAT$(i)*H_exch_$MAT$(i);
E_ext_$MAT$ <- -1.0*($ABS_MAG$)*(%f)*m_$MAT$(i)*H_ext(i);
E_total_$MAT$ <- E_demag_$MAT$ + E_exch_$MAT$ + E_ext_$MAT$ + E_anis_$MAT$%s;
"""%(energy_factor, energy_factor, energy_factor, extra_E_contribs)


    template_correct_m_lengths = """
 if(have_m_$MAT$)
 {
   double z;
   z = 1.0 / sqrt(m_$MAT$(0)*m_$MAT$(0)+m_$MAT$(1)*m_$MAT$(1)+m_$MAT$(2)*m_$MAT$(2));
   m_$MAT$(0) = z * m_$MAT$(0);
   m_$MAT$(1) = z * m_$MAT$(1);
   m_$MAT$(2) = z * m_$MAT$(2);
 }
"""
    eq_H_total=vivify_template(template_eq_H_total,prefix="%range j:3;\n")

    eq_rhs_jacobi_ts1=vivify_template(template_eq_rhs_jacobi_ts1,prefix="%range i:3, j:3, k:3, p:3, q:3;\n")
    eq_rhs_jacobi_ts2=vivify_template(template_eq_rhs_jacobi_ts2,prefix="%range i:3, j:3, k:3, p:3, q:3;\n")

    eq_M=vivify_template(template_eq_M,prefix="%range i:3;\n")
    eq_E=vivify_template(template_E,prefix="%range i:3;\n")

    ccode_correct_m_lengths=vivify_template(template_correct_m_lengths)

    eq_H_anis=""
    eq_E_anis=""

    for mname in all_materials.keys():
        mat = all_materials[mname][0]
        if mat.su_anisotropy == None:
            pass
        else:
            (eq_E_mat,eq_H_mat) = ocaml.nsim_anisotropy_equations(
                "E_anis_"+mat.name, "m_"+mat.name, "H_anis_"+mat.name,
                3, # number of directions of a magnetic vector - may be different
                   # from the dimension of the mesh!
                mat.anisotropy_order,
                mat.su_anisotropy,
                1.0/(mat.su_mu0*mat.su_Ms)
                )

            eq_E_anis = eq_E_anis + eq_E_mat
            eq_H_anis = eq_H_anis + eq_H_mat


    logger.log(15,"LAM Anisotropy\n\n  E_anis: %s\n  H_anis: %s"\
               %(eq_E_anis,eq_H_anis))

    postfix_H_exch=",j:%d, k:3" % dim

    if is_periodic:
        postfix_H_exch=vivify_template(";periodic:H_exch_$MAT$(k)") + postfix_H_exch

    str_H_exch = vivify_template(template_op_H_exch,concat=" + ",postfix=postfix_H_exch)
    str_div_m  = vivify_template(template_op_div_m ,concat=" + ",postfix=",j:3")
    str_grad_m = ""
    if need_grad_m:
        str_grad_m = vivify_from_terms("",
                                       [("$STT_PREFACTOR$", template_op_grad_m)],
                                       concat=" + ",
                                       postfix=postfix_grad_m)

    master_fields_and_mwes_by_name={}

    for (name,mwe) in [("m",mwe_m),("dmdt",mwe_dmdt),("M",mwe_M),
                       ("grad_m",mwe_grad_m),
                       ("current_density",mwe_current_density),
                       ("dm_dcurrent", mwe_dm_dcurrent),
                       ("rho",mwe_rho),("phi",mwe_phi),
                       ("H_total",mwe_H_total),("H_exch",mwe_H_exch),
                       ("H_anis",mwe_H_anis),("H_lc",mwe_H_lc),
                       ("H_demag",mwe_H_demag),("H_ext",mwe_H_ext),
                       ("E_total",mwe_E_total),("E_exch",mwe_E_exch),
                       ("E_demag",mwe_E_demag),("E_ext",mwe_E_ext),
                       ("E_anis",mwe_E_anis),("E_lc",mwe_E_lc)]:
        if mwe != None:
            m_f = (mwe,ocaml.raw_make_field(mwe,[],"",""))
            master_fields_and_mwes_by_name[name]=m_f

    master_fields_and_mwes_by_name["pin"]=(mwe_pin,
                                           ocaml.raw_make_field(mwe_pin,[lambda x, y: 1.0],
                                                                "", ""))

    # Below, we see the complete specification of the micromagnetic
    # model as it is passed on to the linear algebra machine
    # abstraction in the nsim prototype. What is desperately missing
    # here is a middle layer that analyzes all the abstract formula
    # definitions for dependencies, fields occuring in the equations,
    # etc., and then autogenerates these data structures from the more
    # abstract definition. Essentially, most of what we did by hand
    # above can be mechanized to the largest degree.

    lam_mwes={"m":mwe_m,
              "E":mwe_E,
              "scalar":mwe_scalar,
              # Note: while we do not use mwe_E and mwe_scalar as mwes corresponding to some field,
              # we nevertheless have to list it here, as reconstruction of sibling mwes on
              # parallel nodes will have to construct the parent mwe first!
              "dmdt":mwe_dmdt, "M":mwe_M,
              "H_demag":mwe_H_demag, "H_exch":mwe_H_exch,
              "H_anis":mwe_H_anis, "H_total":mwe_H_total, "H_ext":mwe_H_ext,
              "E_demag":mwe_E_demag, "E_exch":mwe_E_exch, "E_total":mwe_E_total,
              "E_ext":mwe_E_ext, "E_anis":mwe_E_anis,
              "rho":mwe_rho, "phi":mwe_phi, "pin":mwe_pin}

    lam_vectors={"v_m":nlam.lam_vector(name="v_m",mwe_name="m"),
                 "v_dmdt":nlam.lam_vector(name="v_dmdt",mwe_name="dmdt"),
                 "v_M":nlam.lam_vector(name="v_M",mwe_name="M"),
                 "v_H_ext":nlam.lam_vector(name="v_H_ext",mwe_name="H_ext"),
                 "v_H_demag":nlam.lam_vector(name="v_H_demag",mwe_name="H_demag"),
                 "v_H_exch":nlam.lam_vector(name="v_H_exch",mwe_name="H_exch"),
                 "v_H_anis":nlam.lam_vector(name="v_H_anis",mwe_name="H_anis"),
                 "v_H_total":nlam.lam_vector(name="v_H_total",mwe_name="H_total"),
                 "v_H_ea":nlam.lam_vector(name="v_H_ea",mwe_name="H_exch"),  # temporary for v_H_exch + v_H_anis used to create the Jacobian
                 "v_E_ext":nlam.lam_vector(name="v_E_ext",mwe_name="E_ext"),
                 "v_E_demag":nlam.lam_vector(name="v_E_demag",mwe_name="E_demag"),
                 "v_E_exch":nlam.lam_vector(name="v_E_exch",mwe_name="E_exch"),
                 "v_E_anis":nlam.lam_vector(name="v_E_anis",mwe_name="E_anis"),
                 "v_E_total":nlam.lam_vector(name="v_E_total",mwe_name="E_total"),
                 "v_rho":nlam.lam_vector(name="v_rho",mwe_name="rho"),
                 "v_rho_s":nlam.lam_vector(name="v_rho_s",mwe_name="rho"),
                 "v_phi":nlam.lam_vector(name="v_phi",mwe_name="phi"),
                 "v_phi1":nlam.lam_vector(name="v_phi1",mwe_name="phi"),
                 "v_phi2":nlam.lam_vector(name="v_phi2",mwe_name="phi"),
                 "v_phi1b":nlam.lam_vector(name="v_phi1b",mwe_name="phi",restriction="phi[outer]"),
                 "v_phi2b":nlam.lam_vector(name="v_phi2b",mwe_name="phi",restriction="phi[outer]"),
                 "v_pin":nlam.lam_vector(name="v_pin",mwe_name="pin")
                 }

    lam_operators={"op_H_exch":nlam.lam_operator("op_H_exch","H_exch","m",
                                                 str_H_exch),
                   "op_div_m":nlam.lam_operator("op_div_m","rho","m",
                                                str_div_m),
                   "op_neg_laplace_phi":nlam.lam_operator("op_neg_laplace_phi","rho","phi",
                                                          # "<d/dxj rho || d/dxj phi>;gauge_fix:phi, j:3",
                                                          "<d/dxj rho || d/dxj phi>, j:3",
                                                          matoptions=["MAT_SYMMETRIC",
                                                                      "MAT_SYMMETRY_ETERNAL"]
                                                          ),
                   "op_grad_phi":nlam.lam_operator("op_grad_phi","H_demag","phi",
                                                   "-<H_demag(j) || d/dxj phi>, j:3"),
                   "op_laplace_DBC":nlam.lam_operator("op_laplace_DBC","phi","phi",
                                                      "-<d/dxj phi[not outer] || d/dxj phi[not outer]>;phi[outer]=phi[outer], j:3",
                                                      matoptions=["MAT_SYMMETRIC","MAT_SYMMETRY_ETERNAL"]
                                                      ),
                   "op_load_DBC":nlam.lam_operator("op_load_DBC","phi","phi",
                                                   "<d/dxj phi[not outer] || d/dxj phi[outer]>;(L||R)=(*||phi[outer]), j:3"),
                   }

    lam_bem={"BEM":nlam.lam_bem(name="BEM",
                                is_hlib=use_hlib,
                                hlib_params=hlib_params,
                                mwe_name="phi",
                                lattice_info=periodic_bc
                                )}

    lam_ksps={"solve_neg_laplace_phi":nlam.lam_ksp(name="solve_neg_laplace_phi",
                                                   matrix_name="op_neg_laplace_phi",
                                                   ksp_type="gmres", pc_type="ilu",
                                                   initial_guess_nonzero=True,
                                                   rtol=get_tol("NBC.rtol"),
                                                   atol=get_tol("NBC.atol"),
                                                   dtol=get_tol("NBC.dtol"),
                                                   maxits=get_tol("NBC.maxits"),
                                                   nullspace_has_constant=False,
                                                   nullspace_subfields=["phi"],
                                                   ),
              "solve_laplace_DBC":nlam.lam_ksp(name="solve_laplace_DBC",
                                               matrix_name="op_laplace_DBC",
                                               ksp_type="gmres", pc_type="ilu",
                                               initial_guess_nonzero=True,
                                               rtol=get_tol("DBC.rtol"),
                                               atol=get_tol("DBC.atol"),
                                               dtol=get_tol("DBC.dtol"),
                                               maxits=get_tol("DBC.maxits"),
                                               )
              }
    lam_local={"local_H_total":nlam.lam_local("local_H_total",
                                              aux_args=intensive_params,
                                              field_mwes=mwes_for_H_total,
                                              equation=eq_H_total),
               "local_H_anis":nlam.lam_local("local_H_anis",
                                             aux_args=intensive_params,
                                             field_mwes=["H_anis","m"],
                                             equation=eq_H_anis),
               "local_E_anis":nlam.lam_local("local_E_anis",
                                             aux_args=intensive_params,
                                             field_mwes=["E_anis","m"],
                                             equation=eq_E_anis),
               "local_dmdt":nlam.lam_local("local_dmdt",
                                           aux_args=intensive_params,
                                           field_mwes=mwes_for_dmdt,
                                           equation=eq_rhs_full),
               "local_M_from_m":nlam.lam_local("local_M_from_m", # set M from m
                                               aux_args=intensive_params,
                                               field_mwes=["m","M"],
                                               equation=eq_M),
               "local_correct_m_lengths":nlam.lam_local("local_correct_m_lengths", 
                                               aux_args=intensive_params,
                                               field_mwes=["m"],
                                               c_code=ccode_correct_m_lengths),
               "local_E_total":nlam.lam_local("local_E_total",
                                              aux_args=intensive_params,
                                              field_mwes=mwes_for_E_total,
                                              equation=eq_E),
               }

    jacobi_contribs_dH_dm=[("operator","op_H_exch")]

    if do_anisotropy_jacobian:
        jacobi_contribs_dH_dm.append(("equation",eq_H_anis))

    #Sequential jacobian (i.e. not MPI)
    lam_jacobi={"jacobian":nlam.lam_jplan("jacobian", # name
                                          "dmdt",
                                          ["m","H_total","H_anis","pin"], # mwe_names
                                          [[], # opt_derive_me
                                           jacobi_contribs_dH_dm,
                                           [],
                                           []], 
                                          eq_rhs_jacobi_ts1, # eom_str
                                          debugprint=True,
                                          ),
                }

    #MPI Jacobian with H_total instead of H_exch (current default)
    ts1=nlam.lam_timestepper(timestepper.name,
                             ['m','dmdt','H_total','H_anis','pin'], #check auxiliary fields for Jacobian are right
                             #also: need names prefix?
                             ['v_m','v_dmdt','v_H_total','v_H_anis','v_pin'],
                             'update_dmdt',
                             nr_primary_fields=1,
                             name_jacobian=None, # XXX TO BE OBSOLETED!
                             pc_rtol=timestepper.pc_rtol,
                             pc_atol=timestepper.pc_atol,
                             max_order=timestepper.max_order,
                             krylov_max=timestepper.krylov_max,
                             jacobi_eom=eq_rhs_jacobi_ts1,
                             phys_field_derivs=[("PRIMARY",""),
                                                ("PRIMARY",""),
                                                ("OPERATOR","op_H_exch"),
                                                ("IGNORE",""),
                                                ("IGNORE","")],
                             jacobi_prealloc_diagonal=75,
                             jacobi_prealloc_off_diagonal=45)

    ts2=None

    if False:
        #MPI Jacobian with H_exch for H_exch (alternative to ts1).
        # Seems to perform very similarly for bigbar problem.
        ts2=nlam.lam_timestepper(timestepper.name,
                                 ['m','dmdt','H_exch','H_anis','pin'], #check auxiliary fields for Jacobian are right
                                 #also: need names prefix?
                                 ['v_m','v_dmdt','v_H_exch','v_H_anis','v_pin'],
                                 'update_dmdt',
                                 nr_primary_fields=1,
                                 name_jacobian=None, # XXX TO BE OBSOLETED!
                                 pc_rtol=1e-2,pc_atol=1e-5,
                                 jacobi_eom=eq_rhs_jacobi_ts2,
                                 phys_field_derivs=[("PRIMARY",""),
                                                    ("PRIMARY",""),
                                                    ("OPERATOR","op_H_exch"),
                                                    ("IGNORE",""),
                                                    ("IGNORE","")],
                                 jacobi_prealloc_diagonal=75,
                                 jacobi_prealloc_off_diagonal=45)
        
    lam_timesteppers={"timestepper":ts1}
    
    # These are "execution sequence scripts" much akin to OpenGL
    # "display lists": their parallel execution can be triggered
    # easily with a single MPI packet, and they ensure all machines do
    # all collective steps that make up a complex calculation in
    # unison.

    lam_programs={"set_H_exch":nlam.lam_program("set_H_exch",
                                            commands=[["TSTART","H_exch"],
                                                      ["SM*V","op_H_exch","v_m","v_H_exch"],
                                                      ["CFBOX","H_exch","v_H_exch"],
                                                      ["TSTOP","H_exch"],
                                                      ]),
                  "set_H_demag":nlam.lam_program("set_H_demag",
                                                 commands=[["TSTART","H_demag"],
                                                           ["SM*V","op_div_m","v_m","v_rho"],
                                                           ["SCALE","v_rho",-1.0],
                                                           ["TSTART","H_demag_neg_laplace_phi"],
                                                           ["SOLVE","solve_neg_laplace_phi","v_rho","v_phi1"],
                                                           ["TSTOP","H_demag_neg_laplace_phi"],
                                                           ["PULL-FEM","phi","phi[outer]","v_phi1","v_phi1b"],
                                                           # ["DEBUG","BEM v-in","v_phi1b",0], # DDD
                                                           ["DM*V","BEM","v_phi1b","v_phi2b"],
                                                           # ["DEBUG","BEM v-out","v_phi2b",0], # DDD
                                                           ["SM*V","op_load_DBC","v_phi2b","v_rho_s"],
                                                           ["TSTART","H_demag_laplace_dbc"],
                                                           ["SOLVE","solve_laplace_DBC","v_rho_s","v_phi2"],
                                                           ["TSTOP","H_demag_laplace_dbc"],
                                                           ["PUSH-FEM","phi","phi[outer]","v_phi2b","v_phi2"],
                                                           ["AXPBY",1.0,"v_phi1",0.0,"v_phi"],
                                                           ["AXPBY",1.0,"v_phi2",1.0,"v_phi"],
                                                           ["SM*V","op_grad_phi","v_phi","v_H_demag"],
                                                           ["CFBOX","H_demag","v_H_demag"],
                                                           ["TSTOP","H_demag"],
                                                           ]),

                  "set_H_anis":nlam.lam_program("set_H_anis",
                                                commands=[["SITE-WISE-IPARAMS","local_H_anis",["v_H_anis","v_m"],[]],
                                                      ]),
                  "set_E_anis":nlam.lam_program("set_E_anis",
                                                commands=[["SITE-WISE-IPARAMS","local_E_anis",["v_E_anis","v_m"],[]],
                                                          ]),
                  "set_H_total":nlam.lam_program("set_H_total",
                                                 commands=[["SITE-WISE-IPARAMS","local_H_total",fields_for_H_total,[]]
                                                            ]),
                  "set_m":nlam.lam_program("set_m",args_fields=[["arg_m","m"]],
                                           commands=[["DISTRIB","arg_m","v_m"],
                                                     ]),
                  "set_M":nlam.lam_program("set_M",
                                           commands=[["SITE-WISE-IPARAMS","local_M_from_m",["v_m","v_M"],[]]
                                                     ]),
                  "update_E":nlam.lam_program("update_E",
                                              commands=[["GOSUB", "set_E_anis"],
                                                        ["GOSUB", "set_E_lc"],
                                                        ["SITE-WISE-IPARAMS", "local_E_total",
                                                        fields_for_E_total, []]
                                                        ]),
                  "update_dmdt":nlam.lam_program("update_dmdt",
                                                 commands=[["TSTART","update_dmdt"],
                                                           ["GOSUB", "set_dm_dcurrent"],
                                                           ["CALLPY", pre_rhs_funs],
                                                           ["GOSUB", "update_H_total"],
                                                           ["SITE-WISE-IPARAMS","local_dmdt",fields_for_dmdt,[]],
                                                           # ["DEBUG","dm/dt","v_dmdt",6],
                                                           # ["DEBUG","dm","v_m",6],
                                                           ["CALLPY", post_rhs_funs],
                                                           ["TSTOP","update_dmdt"]]),
                  "debug_m":nlam.lam_program("debug_m",
                                                 commands=[["DEBUG","dm/dt","v_dmdt",6],
                                                           ["DEBUG","dm","v_m",6],
                                                           ]),
                  "rhs":nlam.lam_program("rhs",
                                         args_fields=[["arg_dm","dmdt"],["arg_m","m"]], # [arg_name,mwe_name]
                                         # args_fields=[["arg_dm","dmdt"],["arg_m","m"],["m_periodically_eq","m"]] <- TODO
                                         # ^ [arg_name,mwe_name]
                                         commands=[["TSTART","rhs"],
                                                   ["DISTRIB","arg_m","v_m"], # XXX REMOVE - see above!
                                                   ["GOSUB", "update_dmdt"],
                                                   ["COLLECT","arg_dm","v_dmdt"],
                                                   ["TSTOP","rhs"]
                                                   ]),
                  "execute_jplan":nlam.lam_program("execute_jplan",
                                                   commands=[["TSTART","execute_jplan"],
                                                             ["GOSUB", "set_H_exch"],
                                                             ["GOSUB", "set_H_anis"],
                                                             ["AXPBY",1.0,"v_H_exch",0.0,"v_H_ea"],
                                                             ["AXPBY",1.0,"v_H_anis",1.0,"v_H_ea"],
                                                             ["JPLAN","jacobian",["v_m","v_H_ea","v_H_anis","v_pin"]],
                                                             ["TSTOP","execute_jplan"],
                                                             ]),
                  "clear_timers":nlam.lam_program("clear_timers",
                                                  commands=[["TCLEAR","rhs","H_demag","H_exch","execute_jplan","grad_m"]]),
                  "report_timers":nlam.lam_program("report_timers",
                                                   commands=[["TREPORT","rhs","H_demag","H_exch","execute_jplan","grad_m"]]),
                  }

    # XXX NOTE: setting H_total is not as modular/flexible as we would want to.
    # We have to re-structure this in such a way that we have commands to clear_H_total()
    # and add_to_H_total() some other H_xyz field.


    update_H_total_commands=[["GOSUB", "set_H_demag"],
                             ["GOSUB", "set_H_exch"],
                             ["GOSUB", "set_H_anis"],
                             ["GOSUB", "set_H_lc"],
                             ["GOSUB", "set_H_total"]]

    if do_lc:
        lam_mwes["E_lc"] = mwe_E_lc
        lam_mwes["H_lc"] = mwe_H_lc
        lam_vectors["E_lc"] = nlam.lam_vector(name="v_E_lc", mwe_name="E_lc")
        lam_vectors["H_lc"] = nlam.lam_vector(name="v_H_lc", mwe_name="H_lc")

        H_lc_ccode, E_lc_ccode = _local_couplings_ccode(local_couplings)
        #print H_lc_ccode
        #raw_input()
        #print E_lc_ccode
        #raw_input()

        mwes_for_H_lc = ["m", "H_lc"]
        fields_for_H_lc = ["v_" + name for name in mwes_for_H_lc]
        lam_local["local_H_lc"] = \
          nlam.lam_local("local_H_lc",
                         aux_args=intensive_params,
                         field_mwes=mwes_for_H_lc,
                         c_code=H_lc_ccode)

        mwes_for_E_lc = ["m", "E_lc"]
        fields_for_E_lc = ["v_" + name for name in mwes_for_E_lc]
        lam_local["local_E_lc"] = \
          nlam.lam_local("local_E_lc",
                         aux_args=intensive_params,
                         field_mwes=mwes_for_E_lc,
                         c_code=E_lc_ccode)

        cmds = [["SITE-WISE-IPARAMS", "local_H_lc", fields_for_H_lc, []]]
        lam_programs["set_H_lc"] = nlam.lam_program("set_H_lc", commands=cmds)

        cmds = [["SITE-WISE-IPARAMS", "local_E_lc", fields_for_E_lc, []]]
        lam_programs["set_E_lc"] = nlam.lam_program("set_E_lc", commands=cmds)

    else:
        lam_programs["set_H_lc"] = nlam.lam_program("set_H_lc", commands=[])
        lam_programs["set_E_lc"] = nlam.lam_program("set_E_lc", commands=[])

    if need_grad_m:
        lam_mwes["grad_m"] = mwe_grad_m
        lam_mwes["current_density"] = mwe_current_density
        lam_mwes["dm_dcurrent"] = mwe_dm_dcurrent
        lam_vectors["grad_m"] = nlam.lam_vector(name="v_grad_m", mwe_name="grad_m")
        lam_vectors["current_density"] = \
          nlam.lam_vector(name="v_current_density", mwe_name="current_density")
        lam_vectors["dm_dcurrent"] = \
          nlam.lam_vector(name="v_dm_dcurrent", mwe_name="dm_dcurrent")
        lam_operators["op_grad_m"] = \
          nlam.lam_operator("op_grad_m", "grad_m", "m", str_grad_m)
        template_eq_grad_m_x_current = \
          "dm_dcurrent_$MAT$(i) <- grad_m_$MAT$(i, j) * current_density(j);"
        eq_grad_m_x_current = vivify_template(template_eq_grad_m_x_current,
                                              prefix="%range i:3, j:3;\n",
                                              materials=grad_m_mats)

        lam_local["local_grad_m_x_current"] = \
          nlam.lam_local("local_grad_m_x_current",
                         aux_args=intensive_params,
                         field_mwes=["dm_dcurrent", "grad_m", "current_density"],
                         equation=eq_grad_m_x_current)

        commands = [
          ["TSTART","grad_m"],
          ["SM*V","op_grad_m","v_m","v_grad_m"],
          ["SITE-WISE-IPARAMS", "local_grad_m_x_current",
              ["v_dm_dcurrent", "v_grad_m", "v_current_density"], []],
          ["CFBOX", "dm_dcurrent", "v_dm_dcurrent"],
          ["TSTOP","grad_m"]]
        lam_programs["set_dm_dcurrent"] = \
          nlam.lam_program("set_dm_dcurrent", commands=commands)

    else:
        lam_programs["set_dm_dcurrent"] = \
          nlam.lam_program("set_dm_dcurrent", commands=[])

    # Thermal code presently is a relic, and a somewhat awkward hack
    # that uses highly specialized C code to implement a fluctuating
    # field with the right statistical properties.

    if thermal:
        ccode_thermal=vivify_template("""

if(have_Temperature && have_H_therm_$MAT$)
{
  double seed[2];
  double rnd[8];
  double sigma;

  /* generate four pairs of random doubles */
  seed[0]=TIME;
  seed[1]=SITE_PMID+"""+repr(user_seed_T)+""";

  four_random_01_doubles_from_seeds(2, seed, &rnd[0]);
  four_random_01_doubles_from_seeds(4, &rnd[0], &rnd[4]);

  if(THERMAL_DELTA_T != 0)
  {
    /* calculate standard deviance: note that the factor 1/sqrt(volume) is missing, but is multiplied onto later in update_H_total */
    sigma = sqrt($THERMAL_FACTOR$ * Temperature / THERMAL_DELTA_T);

    /* calculate thermal field */
    H_therm_$MAT$(0) = gauss_random(rnd[0], rnd[1], 0.0, sigma);
    H_therm_$MAT$(1) = gauss_random(rnd[2], rnd[3], 0.0, sigma);
    H_therm_$MAT$(2) = gauss_random(rnd[4], rnd[5], 0.0, sigma);
  }
  else
  {
    /* THERMAL_DELTA_T hasn't been set yet -- this happens when fields are initialised before a time step has been performed */
    H_therm_$MAT$(0) = 0.0;
    H_therm_$MAT$(1) = 0.0;
    H_therm_$MAT$(2) = 0.0;
  }

  /* fprintf(stderr,\"DDD T=%f rnd=%f %f %f %f\\n\",Temperature,rnd[0],rnd[1],rnd[2],rnd[3]);fflush(stderr); */

  /* fprintf(stderr,\"DDD THERMAL site=%6.1f time=%8.4f H_therm=[%6.3f %6.3f %6.3f]\\n\",SITE_PMID,TIME,H_therm_$MAT$(0),H_therm_$MAT$(1),H_therm_$MAT$(2));fflush(stderr); */
}
""")
        
        mwe_temperature=ocaml.mwe_sibling(mwe_scalar,"Temperature","Temperature/scalar",[("scalar","Temperature")])
        master_fields_and_mwes_by_name["Temperature"]=(mwe_temperature,ocaml.raw_make_field(mwe_temperature,[lambda x, y: su_temperature],"",""))

        mwe_H_therm = ocaml.mwe_sibling(mwe_m,"H_therm","H_therm/m",m_relabeled("H_therm_"))
        master_fields_and_mwes_by_name["H_therm"]=(mwe_H_therm,ocaml.raw_make_field(mwe_H_therm,[],"",""))
        
        # Note that we do not introduce a thermal energy for now,
        # as this will just be noise anyway.
        lam_mwes["H_therm"]=mwe_H_therm
        lam_mwes["Temperature"]=mwe_temperature

        # We will not be able to use CVODE for our time-stepper in the chaotic thermal case,
        # so we will also need additional buffers for the Heun method:



        lam_vectors["v_dmdt2"]=nlam.lam_vector(name="v_dmdt2",mwe_name="dmdt")
        lam_vectors["v_m2"]=nlam.lam_vector(name="v_m2",mwe_name="m")

        # T.F.: The trick I use to properly initialize the internal
        # vector containing the square roots of the m-dof volumes
        # (needed for proper fluctuation statistics of the thermal
        # field) feels like a very ugly hack, but for now, we will
        # leave it at that...

        lam_vectors["v_m_inv_sqrtvol"]=nlam.lam_vector(name="v_m_inv_sqrtvol",mwe_name="m",initial_value=1.0)
        lam_programs["init_m_inv_sqrtvol"]=\
            nlam.lam_program("init_m_inv_sqrtvol",\
                 commands=[["CFBOX","m","v_m_inv_sqrtvol"],
                           ["PW-SQRT","v_m_inv_sqrtvol"]])
        
        lam_programs["update_dmdt_Heun"]=\
            nlam.lam_program("update_dmdt_Heun",\
             commands=[["TSTART","update_dmdt_Heun"],
                       # Take a copy of the original m in m2:
                       ["AXPBY",1.0,"v_m",0.0,"v_m2"],
                       ["GOSUB", "set_dm_dcurrent"],
                       ["GOSUB", "update_H_total"],
                       ["SITE-WISE-IPARAMS","local_dmdt",fields_for_dmdt,[]],
                       # Take a copy of the original dmdt in dmdt_2:
                       ["AXPBY",1.0,"v_dmdt",0.0,"v_dmdt2"],
                       # update m to m + delta_t * dm/dt:
                       ["AXPBY","THERMAL_DELTA_T","v_dmdt",1.0,"v_m"],
                       # compute once again H_total and then dmdt from this new m.
                       # NOTE: this is somewhat tricky: we use the same thermal noise
                       # field as before - this is not updated to the new time step!
                       ["GOSUB", "set_dm_dcurrent"],
                       ["GOSUB", "update_H_total"],
                       ["SITE-WISE-IPARAMS","local_dmdt",fields_for_dmdt,[]],
                       # Now that we have this second estimate for dmdt, build the proper dmdt:
                       ["AXPBY",0.5,"v_dmdt2",0.5,"v_dmdt"],
                       ["TSTOP","update_dmdt_Heun"]])

        lam_programs["advance_time_thermal"]=\
            nlam.lam_program("advance_time_thermal",
                             args_fields=[], # [arg_name,mwe_name]
                             commands=[["TSTART","advance_time_thermal"],
                                       ["GOSUB", "update_dmdt_Heun"],
                                       ["AXPBY", "THERMAL_DELTA_T","v_dmdt",1.0,"v_m"],
                                       ["SITE-WISE-IPARAMS","local_correct_m_lengths",["v_m"],[]],
                                       ["AXPBY-IPARAMS","THERMAL_DELTA_T","",1.0,"TIME"],
                                       ["TSTOP","advance_time_thermal"],
                                       ])
        
        lam_vectors["v_H_therm"]=nlam.lam_vector(name="v_H_therm",mwe_name="H_therm")
        lam_vectors["v_Temperature"]=nlam.lam_vector(name="v_Temperature",mwe_name="Temperature")
        
        lam_local["local_H_therm"]=nlam.lam_local("local_H_therm",
                                                  aux_args=intensive_params,
                                                  field_mwes=["H_therm","Temperature"],
                                                  c_code=ccode_thermal)

        update_H_total_commands += [["SITE-WISE-IPARAMS","local_H_therm",["v_H_therm","v_Temperature"],[]],
                                    ["V*V","v_m_inv_sqrtvol","v_H_therm","v_H_therm"]
                                    ]

    lam_programs["update_H_total"]=nlam.lam_program("update_H_total",
                                                    commands=update_H_total_commands)

    # Hack to allow nmag4 to deal with 1-D, 2-D simulations
    # mf, 2 Oct 2007
    if not do_demag:
        lam_operators["op_div_m"] = None
        lam_operators["op_neg_laplace_phi"] = None
        lam_operators["op_grad_phi"] = None
        lam_operators["op_laplace_DBC"] = None
        lam_operators["op_load_DBC"] = None
        lam_bem["BEM"] = None
        lam_ksps["solve_neg_laplace_phi"] = None
        lam_ksps["solve_laplace_DBC"] = None
        lam_programs["set_H_demag"] = nlam.lam_program("set_H_demag", commands=[])

    lam=nlam.make_lam(
            simulation_prefix,
            intensive_params=intensive_params,
            mwes=lam_mwes.values(),
            vectors=lam_vectors.values(),
            operators=without_nones(lam_operators.values()),
            bem_matrices=without_nones(lam_bem.values()),
            ksps=without_nones(lam_ksps.values()),
            local_operations=lam_local.values(),
            jacobi_plans=lam_jacobi.values(),
            programs=lam_programs.values(),
            timesteppers=lam_timesteppers.values(),
            lam_debugfile=lam_debugfile
            )

    ocaml.lam_set_field(lam, master_fields_and_mwes_by_name["pin"][1], "v_pin")
    if thermal:
        ocaml.lam_set_field(lam, master_fields_and_mwes_by_name["Temperature"][1], "v_Temperature")
        ocaml.lam_execute(lam,"init_m_inv_sqrtvol",[],[])
        # XXX NOTE: shouldn't I provide the proper intensive parameters here?!??
    
    # The LAM is set up now.
    # The question is: what else apart from that do we return?
    # Most certainly, access to the master buffers.
    # But do we do more? I do not think so. Building and taking care of the timestepper
    # that drives this LAM is the duty of the simulation object!

    return [lam,master_fields_and_mwes_by_name]


__all__ = ['nmag_lam']
