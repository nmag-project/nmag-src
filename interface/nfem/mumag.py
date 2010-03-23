# (C) 2006 Matteo Franchin
#
# This file defines the class mumag, which represent the basic framework
# needed by algorithms for micromagnetics

try:
  import ocaml
except ImportError, msg:
  error = str(msg) + " --- This usually means that you "
  error +="are running this code in a normal python interpreter, "
  error +="not within the nmesh or nsim executable."
  raise ImportError, error

try:
  import nmesh
  import nfem as nfem
except ImportError, msg:
  error = str(msg) + " --- " + \
  "This means the PYTHONPATH is not set to the location of these files. " + \
  "Where they are, may depend on your installation."
  raise ImportError,error

import sys

# This string is concatenated with the other strings containing C-code:
# it contains some common declarations
ccode_header = """
#line 29 "mumag.py"

/* Mathematical constants */
#define pi 3.14159265358979323844

/* (10^(-6) N/A^2) magnetic permeability */
#define mu0 1.25663706143591729537

/* This macro defines the vectorial product */
#define VEC_PRODUCT(v, v1, v2) \\
  (v)[0] = (v1)[1]*(v2)[2] - (v1)[2]*(v2)[1]; \\
  (v)[1] = (v1)[2]*(v2)[0] - (v1)[0]*(v2)[2]; \\
  (v)[2] = (v1)[0]*(v2)[1] - (v1)[1]*(v2)[0];

/* Scalar product */
#define SCAL_PRODUCT(v1, v2) \\
  ((v1)[0] * (v2)[0] + (v1)[1] * (v2)[1] + (v1)[2] * (v2)[2])

/* Linear combination of two vectors */
#define S(v, v1, s1, v2, s2) \\
  (v)[0] = (v1)[0] * (s1) + (v2)[0] * (s2); \\
  (v)[1] = (v1)[1] * (s1) + (v2)[1] * (s2); \\
  (v)[2] = (v1)[2] * (s1) + (v2)[2] * (s2);

typedef double Real;
typedef Real Vector[3];

/*#define PRINT_FIELD*/
"""

# This is the code used to implement the uniaxial anisotropy
ccode_uniaxial = ccode_header + """
#line 61 "mumag.py"

/* These lines calculate the uniaxial anisotropy (defined by the parameters
 * k1, k2 and the axis 'axis_?') and adds it to the field h.
 * m is the normalized magnetization and mag_sat its norm.
 */
if ( have_h_total ) {
  Real t = m(0)*axis_x + m(1)*axis_y + m(2)*axis_z;
  t = (2.0*k1*t + 4.0*k2*t*t*t)/m_sat_mu0;
  h_total(0) += t*axis_x; h_total(1) += t*axis_y; h_total(2) += t*axis_z;
}
"""

# This is the code used to implement the cubic anisotropy
ccode_cubic = ccode_header + """
#line 76 "mumag.py"

/* Cubic anisotropy */
if (have_h_total ) {
  /* Compute axis 3 at every time-step is a waste of time, but for now.. */
  Real m1, m2, m3, m1sq, m2sq, m3sq, m1sqsq, m2sqsq, m3sqsq,
        c1, c2, c3, overall_factor;
  Vector mag = {m(0), m(1), m(2)}; /* Not efficient, but for now... */
  Vector axis1 = {axis1_x, axis1_y, axis1_z};
  Vector axis2 = {axis2_x, axis2_y, axis2_z};
  Vector axis3, t;

  VEC_PRODUCT(axis3, axis1, axis2);
  m1 = SCAL_PRODUCT(axis1, mag); m1sq = m1*m1; m1sqsq = m1sq*m1sq;
  m2 = SCAL_PRODUCT(axis2, mag); m2sq = m2*m2; m2sqsq = m2sq*m2sq;
  m3 = SCAL_PRODUCT(axis3, mag); m3sq = m3*m3; m3sqsq = m3sq*m3sq;
  c1 = m1 * (k1*(m2sq + m3sq) + k2*m2sq*m3sq + k3*(m2sqsq + m3sqsq)*m1sq);
  c2 = m2 * (k1*(m3sq + m1sq) + k2*m3sq*m1sq + k3*(m3sqsq + m1sqsq)*m2sq);
  c3 = m3 * (k1*(m1sq + m2sq) + k2*m1sq*m2sq + k3*(m1sqsq + m2sqsq)*m3sq);
  t[0] = c1*axis1[0] + c2*axis2[0] + c3*axis3[0];
  t[1] = c1*axis1[1] + c2*axis2[1] + c3*axis3[1];
  t[2] = c1*axis1[2] + c2*axis2[2] + c3*axis3[2];
  overall_factor = -2.0/m_sat_mu0;
  h_total(0) += overall_factor*t[0];
  h_total(1) += overall_factor*t[1];
  h_total(2) += overall_factor*t[2];
}
"""

ccode_add_fields = """
if ( have_h_total ) {
  double hx = 0.0, hy = 0.0, hz = 0.0;

# ifdef have_h_ext
    if ( have_h_ext ) {
      hx += h_ext(0);
      hy += h_ext(1);
      hz += h_ext(2);
    }
# endif
# ifdef have_h_demag
    if ( have_h_demag ) {
      hx += h_demag(0);
      hy += h_demag(1);
      hz += h_demag(2);
    }
# endif
# ifdef have_h_exch
    if ( have_h_exch ) {
      hx += h_exch(0);
      hy += h_exch(1);
      hz += h_exch(2);
    }
# endif
  h_total(0) = hx;
  h_total(1) = hy;
  h_total(2) = hz;
}
"""

ccode_calculate_energy = """
if ( have_h_total && have_m )
  energy += m(0)*h_total(0) + m(1)*h_total(1) + m(2)*h_total(2);
"""

mu0 = 1.25663706143591729537 # (10^(-6) N/A^2) magnetic permeability

# To obtain a field which share the same data, but has a different file
def renamed_field(field, old_name, new_name):
  mwe = ocaml.get_mwe(field)
  new_mwe = nfem.mwe_sibling(mwe, new_name, "renamed_"+old_name, [(old_name, new_name)])
  return nfem.field_alias(field, new_mwe)

# Function used to initialize a field to zero
def set_to_zero(dof, x): return 0.0

def field_function(values):
  if type(values) == list:
    return lambda dof, r: values[ dof[1][0] ]
  else:
    return values

class MumagCore:
  '''Class which allocates a magnetisation over a mesh, defines a total field
    (which can include the exchange, the demag, the external field,
    anistropies) and gives a method to calculate it.
    Therefore this class could be seen as the common denominator of every
    algorithm which deals with micromagnetic systems (including
    all the algorithms for time integration and all the algorithms
    for energy minimization)
  '''
  def __init__(self, mesh, where, m_sat, initial_mag=set_to_zero, order=1):
    # Status
    self.is_ready = False

    # The lists of added uniaxial and cubic anisotropies
    self.uniaxial_anis = []
    self.cubic_anis = []
    self.calculate_uniaxial_anis = None
    self.calculate_cubic_anis = None
    self.calculate_h_demag = None
    self.calculate_h_exch = None

    self.mesh = mesh
    self.where = where
    self.initial_mag = initial_mag
    self.m_sat = m_sat
    self.m_sat_mu0 = m_sat*mu0

    self.order = order

    # Default settings
    self.features = {
      'include_demag' : False,
      'include_exchange' : False,
      'exchange_coupling' : None,
      'external_field' : None,
      'calculate_energy' : False
    }

    # The dictionaries which will be used to collect all the mwes and fields
    self.mwes = {}   # Dictionary containing all the mwe-s
    self.fields = {} # Dictionary containing all the field-s

  def set(self, **features):
    '''This function sets the parameters for the simulation.
       You can set the following parameters:
         damping, alpha, absolute_step_error, relative_step_error,
         step_headroom, stopping_time, stopping_dm_dt, max_iterations,
         max_rejections, initial_dt, initial_time, include_exchange,
         include_demag
       EXAMPLE: s.set(damping = 0.02, max_iterations = 10000)
    '''
    if self.is_ready:
      raise "You are trying to call 'set', but the setup has already been done!"

    for feature in features:
      if self.features.has_key(feature):
         self.features[feature] = features[feature]
      else:
        raise "%s <-- the method 'set' knows nothing about it!" % feature

  def add_uniaxial_anis(self, axis, k1, k2=0.0):
    '''Add uniaxial anisotropy'''
    self.uniaxial_anis.append( (axis, k1, k2) )

  def add_cubic_anis(self, axis1, axis2, k1, k2=0.0, k3=0.0):
    '''Add cubic anisotropy'''
    self.cubic_anis.append( (axis1, axis2, k1, k2, k3) )

  def get_field(self, name):
    '''Return a field used in the simulation by name.
       EXAMPLE: s.get_field("m")
    '''
    return self.fields[name]

  def set_field(self, name, values=set_to_zero):
    '''Set the value of a field using a function or a vector
    '''
    ocaml.set_field(self.fields[name], field_function(values))

  def new_mwe(self, name, indices=[], where=None):
    if where == None: where = self.where
    element = nfem.make_element(name, indices, self.mesh.dim, self.order)
    associations = list((body_nr, element) for body_nr in where)
    mwe = nfem.make_mwe(name, associations, mesh=self.mesh)
    return mwe

  def new_mwe_and_field(self, name, indices=[], where=None, initial_values=set_to_zero):
    if where == None: where = self.where
    element = nfem.make_element(name, indices, self.mesh.dim, self.order)
    associations = list((body_nr, element) for body_nr in where)
    mwe = nfem.make_mwe(name, associations, mesh=self.mesh)
    field = nfem.make_field(mwe, initial_values=field_function(initial_values))
    self.mwes[name] = mwe
    self.fields[name] = field
    return (mwe, field)

  def new_field(self, name, indices=[], where=None, initial_values=set_to_zero):
    _, field = self.new_mwe_and_field(name, indices, where, initial_values)
    return field

  def mwe_list(self, mwe_names):
    '''This function creates a list of mwes from the list of their names,
       ignoring the mwe names which do not appear inside self.mwes
    '''
    mwe_list = []
    for mwe_name in mwe_names:
      if self.mwes.has_key(mwe_name):
        mwe_list.append( self.mwes[mwe_name] )
    return mwe_list

  def field_list(self, field_names):
    '''This function creates a list of fields from the list of their names,
       ignoring the field names which do not appear inside self.fields
    '''
    field_list = []
    for field_name in field_names:
      if self.fields.has_key(field_name):
        field_list.append( self.fields[field_name] )
    return field_list

  def setup(self):
    '''This function should be called after the method 'set' to setup
       the simulation (create the fields, the operators and so on)'''

    # Should not do initializizations more than once
    if self.is_ready: return

    mwe_m, field_m = self.new_mwe_and_field("m", [3], initial_values=self.initial_mag)
    mwe_h_total, field_h_total = self.new_mwe_and_field("h_total", [3])

    if self.features["include_demag"]:
      self.new_field("h_demag", indices=[3])
    if self.features["include_exchange"]:
      self.new_field("h_exch", indices=[3])
    if self.features["external_field"]:
      h0 = self.features["external_field"]
      self.new_field("h_ext", indices=[3], initial_values=h0)

    # The demag field
    if self.features["include_demag"]:
      if self.mesh.dim != 3:
        raise "Sorry, the demag-calculation is implemented only for 3-D space."

      mwe_h_demag = self.mwes["h_demag"]
      field_h_demag = self.fields["h_demag"]

      mwe_scalar = self.new_mwe("scalar")
      mwe_rho_m = nfem.mwe_sibling(mwe_scalar, "mwe_rho_m", "renamed_scalar", [("scalar", "rho_m")])
      mwe_phi_m = nfem.mwe_sibling(mwe_scalar, "mwe_phi_m", "renamed_scalar", [("scalar", "phi_m")])

      field_div_m = nfem.make_field(mwe_rho_m)
      field_phi_m = nfem.make_field(mwe_phi_m)

      diffop_div_m_str = "%f <rho_m||d/dxj m(j)>, j:3" % self.m_sat
      print diffop_div_m_str
      compute_div_m = \
       nfem.diffop_applicator(diffop_div_m_str,
                              mwe_rho_m, mwe_m,
                              interface_coeffs=[(-2,-2,1.0)],
                              petsc_name="mumag_div_m")

      prematrix_laplace = \
       nfem.prematrix("-<d/dxj rho_m||d/dxj phi_m>, j:3", mwe_rho_m, mwe_phi_m)

      solve_bem = \
       nfem.laplace_solver_bem(prematrix_laplace, inside_regions=self.where)

      compute_grad_phi = \
       nfem.diffop_applicator("<h_demag(j)||d/dxj phi_m>, j:3",
                              mwe_h_demag, mwe_phi_m, result="field")

      cofield_div_m = compute_div_m(self.fields["m"])
      solve_bem(cofield_div_m, target=field_phi_m)
      compute_grad_phi(field_phi_m, target=field_h_demag)

      def calculate_h_demag():
        compute_div_m(self.fields["m"], target=cofield_div_m)
        solve_bem(cofield_div_m, target=field_phi_m)
        compute_grad_phi(field_phi_m, target=field_h_demag)

      self.calculate_h_demag = calculate_h_demag

    # Now we add the exchange and demag fields if needed
    if self.features["include_exchange"]:
      if not self.features["exchange_coupling"]:
        raise "You want to include exchange interaction, " + \
         "but you did not specify the exchange coupling constant!"
      ec = self.features["exchange_coupling"]
      if ec < 0.0:
        raise "Error: you specified a negative exchange coupling constant."
      mwe_h_exch = self.mwes["h_exch"]
      field_h_exch = self.fields["h_exch"]
      exch_factor  = -2.0*ec/self.m_sat_mu0
      op_str = "%f <d/dxi h_exch(j) || d/dxi m(j)>, i:%d, j:3" % (exch_factor, self.mesh.dim)
      op_h_exch = nfem.diffop(op_str)
      p = nfem.prematrix(op_h_exch, mwe_h_exch, mwe_m, ignore_jumps=True)
      compute_h_exch = nfem.prematrix_applicator(p)
      h_exch_cofield = compute_h_exch(field_m)
      nfem.cofield_to_field(h_exch_cofield, target=field_h_exch)
      def calculate_h_exch():
        compute_h_exch(field_m, target=h_exch_cofield)
        nfem.cofield_to_field(h_exch_cofield, target=field_h_exch)

      self.calculate_h_exch = calculate_h_exch

    # Create the C-functions which performs the different parts
    # of the computation

    some_names = ["m", "h_total"]
    some_mwes = self.mwe_list(some_names)
    some_fields = self.field_list(some_names)

    if self.uniaxial_anis:
      args = ["m_sat_mu0", "axis_x", "axis_y", "axis_z", "k1", "k2"]
      c_uniaxial = nfem.site_wise_applicator(args, ccode_uniaxial, field_mwes=some_mwes)
      def calculate_uniaxial_anis():
        for ua in self.uniaxial_anis:
          axis, k1, k2 = ua
          axis_x, axis_y, axis_z = axis
          args_values = [self.m_sat_mu0, axis_x, axis_y, axis_z, k1, k2]
          c_uniaxial(args_values, fields=some_fields)

      self.calculate_uniaxial_anis = calculate_uniaxial_anis

    if self.cubic_anis:
      args = ["m_sat_mu0", "axis1_x", "axis1_y", "axis1_z",
       "axis2_x", "axis2_y", "axis2_z", "k1", "k2", "k3"]
      c_cubic = nfem.site_wise_applicator(args, ccode_cubic, field_mwes=some_mwes)
      def calculate_cubic_anis():
        for ca in self.cubic_anis:
          axis1, axis2, k1, k2, k3 = ca
          axis1_x, axis1_y, axis1_z = axis1
          axis2_x, axis2_y, axis2_z = axis2
          args_values = [self.m_sat_mu0, axis1_x, axis1_y, axis1_z,
          axis2_x, axis2_y, axis2_z, k1, k2, k3]
          c_cubic(args_values, fields=some_fields)
      self.calculate_cubic_anis = calculate_cubic_anis

    more_names = ["h_total", "h_ext", "h_demag", "h_exch"]
    more_mwes = self.mwe_list(more_names)
    more_fields = self.field_list(more_names)
    add_fields = nfem.site_wise_applicator([], ccode_add_fields, field_mwes=more_mwes)
    def add_ext_demag_exch():
      add_fields([], fields=more_fields)
    self.add_ext_demag_exch = add_ext_demag_exch

    if self.features["calculate_energy"]:
      swa_calculate_energy = \
       nfem.site_wise_applicator(["energy"], ccode_calculate_energy,
                                 field_mwes=[mwe_m],cofield_mwes=[mwe_h_total])
      cofield_h_total = nfem.field_to_cofield(field_h_total)
      def calculate_energy():
        nfem.field_to_cofield(field_h_total, target=cofield_h_total)
        energy = swa_calculate_energy([0.0], fields=[field_m], cofields=[cofield_h_total])
        return -self.m_sat_mu0*energy[0]
      self.__calculate_energy = calculate_energy

    self.is_ready = True

  def calculate_h_total(self):
    '''Calculates the total effective field associated with
       the given magnetisation
    '''
    if self.calculate_h_exch: self.calculate_h_exch()
    if self.calculate_h_demag: self.calculate_h_demag()
    self.add_ext_demag_exch()
    if self.calculate_uniaxial_anis: self.calculate_uniaxial_anis()
    if self.calculate_cubic_anis: self.calculate_cubic_anis()

  def total_energy(self):
    '''Calculate the total energy'''
    return self.__calculate_energy()

  def get_field(self, name):
    '''Return a field used in the simulation by name.
       EXAMPLE: s.get_field("m")
    '''
    return self.fields[name]
