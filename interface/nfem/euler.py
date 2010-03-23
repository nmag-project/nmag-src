# (C) 2006 Matteo Franchin
#
# NEED TO ADD CHECKS ON THE ORDER OF THE CALL TO THE METHODS
# (cannot call .set() after .setup(), etc.)

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
#import copy
from mumag import MumagCore

# This string is concatenated with the other strings containing C-code:
# it contains some common declarations
ccode_header = """
#line 31 "euler.py"

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

# String containing the code which executes the Euler step with correction
# and computes the maximum norm of dm/dt for the old step (this number
# is needed by ccode_step_check to decide whether accept or reject the move.
ccode_step_make = ccode_header + """
#line 65 "euler.py"
/*****************************************************************************
   We can write the LLG equation in the following way:
     dM/dt = - alpha [ M x H + (g/m_s) M x (M x H) ]
   where m_s is the module of the magnetization M
   (is the saturation magnetization).
   We can rewrite all, if we let M = m_s m, where m is a versor:
     dm/dt = - alpha [ m x H + g m x (m x H) ]
   This is what we have in mind in what follows.
 *****************************************************************************/

  if ( have_m ) {
/*****************************************************************************
 * EULER STEP WITH CORRECTION AND NORMALIZATION                              *
 *****************************************************************************/
    Vector dm, mag;
    Real a, b, len_mag;
    dm[0] = old_dm_dt(0) * dt;
    dm[1] = old_dm_dt(1) * dt;
    dm[2] = old_dm_dt(2) * dt;
    a = 0.5 * sqrt(dm[0]*dm[0] + dm[1]*dm[1] + dm[2]*dm[2]);
    b = 1.0 / (1.0 + a);
    mag[0] = old_m(0) + b*(dm[0] - a*old_m(0));
    mag[1] = old_m(1) + b*(dm[1] - a*old_m(1));
    mag[2] = old_m(2) + b*(dm[2] - a*old_m(2));

    len_mag = sqrt(mag[0]*mag[0] + mag[1]*mag[1] + mag[2]*mag[2]);
    m(0) = mag[0]/len_mag;
    m(1) = mag[1]/len_mag;
    m(2) = mag[2]/len_mag;

    /* Here we calculate the maximum value of the norm of old_dm_dt */
    {
      Real x, y, z, old_norm_dm_dt;
      x = old_dm_dt(0);
      y = old_dm_dt(1);
      z = old_dm_dt(2);
      /* Maybe I should take the sqrt of the norm outside the loop,
       * for now let's do it in this way!
       */
      old_norm_dm_dt = sqrt(x*x + y*y + z*z);
      if ( old_norm_dm_dt > old_max_norm_dm_dt )
        old_max_norm_dm_dt = old_norm_dm_dt;
    }
  }
"""

ccode_step_check = ccode_header + """
#line 160 "euler.py"
/*****************************************************************************
 * STEP LENGTH CALCULATION USED BY OOMMF                                     *
 *****************************************************************************/

/* derived constants */
static Vector h_tot, mag, mxh, mxmxh;

  if ( have_m ) {
    Real A, B;
    Vector my_dm_dt;

    /* Ok, now old_m and old_dm_dt contain the magnetization and its derivative
     * at time step n. m contains the magnetization at time step n + 1
     * and h_exch and h_demag contain the fields calculated for m (not old_m!).
     * Now we can calculate the derivative dm/dt at time step n + 1
     * and store it in dm_dt
     */
    mag[0] = m(0); mag[1] = m(1); mag[2] = m(2);
    h_tot[0] = h_total(0); h_tot[1] = h_total(1); h_tot[2] = h_total(2);

    VEC_PRODUCT(mxh, mag, h_tot); /* Compute M x H */
    VEC_PRODUCT(mxmxh, mag, mxh); /* Compute M x (M x H) */

    /* Compute (RHS of LL equation) * delta_t */
    A = -llg_gamma_LL;
    B = A * llg_damping;
    S(my_dm_dt, mxh, A, mxmxh, B);

    /* Store the derivative of magnetization with respect to time */
    dm_dt(0) = my_dm_dt[0];
    dm_dt(1) = my_dm_dt[1];
    dm_dt(2) = my_dm_dt[2];

    /* Error in the calculation of m for the current step */
    if ( dt > 0.0 ) { /* dt == 0.0 only for the first step */
      Real ex, ey, ez, next_dt, error_rate;
      ex = my_dm_dt[0] - old_dm_dt(0);
      ey = my_dm_dt[1] - old_dm_dt(1);
      ez = my_dm_dt[2] - old_dm_dt(2);
      error_rate = 0.5*sqrt(ex*ex + ey*ey + ez*ez);
      /* The error depends linearly from the step length:
       *   error = error_rate * dt
       * So, if we want that error < allowed_error, we should use:
       *   dt < allowed_error/error_rate
       * In our case allowed_error will be the minimum of the absolute error
       * and the relative error, which is given by:
       *   relative_step_error*norm_old_dm_dt*dt
       * which is a fraction of the last step length.
       */

      {
        Real allowed_error, error_from_relative, error_from_absolute;

        error_from_absolute = absolute_step_error;
        error_from_relative = relative_step_error*old_max_norm_dm_dt*dt;
        allowed_error = error_from_relative < error_from_absolute ?
          error_from_relative : error_from_absolute;

        if ( error_rate > 0.0 ) {
          next_dt = allowed_error/error_rate;
          if ( next_dt > dt ) {
            delta_t = next_dt*step_headroom;
          } else {
            delta_t = -next_dt*step_headroom;
          }

        } else {
          delta_t = 0.0; /* No suggested dt */
        }
      }
    }
  }
"""

# This string contains the code which scans the field containing the suggested
# values for the next time step length for each site and finds the one which
# is smaller.
ccode_calculate_next_dt = ccode_header + """
#line 259 "euler.py(ccode_calculate_next_dt)"
/* The following function scans the values of the given scalar field
   containing the time step lengths, which are interpreted as follows:
   dt < 0  --> step not accepted, suggested dt for next attempt is |dt|
   dt > 0  --> step accepted, suggested dt for next step is dt
   dt == 0 --> ignored
 */
  if ( have_delta_t ) {
    if ( delta_t != 0.0 ) {
      Real dt = fabs(delta_t);
      if ( previous_dt < 0.0 )
        previous_dt = dt;
      else
        if (dt < previous_dt) previous_dt = dt;

      if ( delta_t < 0.0 ) accepted_step = -1.0;
    }
  }
"""

# Function used to initialize a field to zero
def set_to_zero(dof, x): return 0.0

###############################################################################
# Default settings for euler integrators
euler_defaults = {
  "damping" : 0.5,
  "gamma_G" : 0.2211,
  "gamma_LL" : None,
  "absolute_step_error" : 0.349e-2,  # radians --> 0.2 degrees
  "relative_step_error" : 0.2,
  "step_headroom" : 0.85,
  "stopping_time" : -1.0,
  "stopping_dm_dt" : 1.7e-5,   # in radians/ps, = 1 degrees/ns
  "max_iterations" : -1,
  "max_rejections" : 2,
  "initial_dt" : 1e-7,
  "max_dt_ratio" : 1.25,
  "initial_time" : 0.0,
}

###############################################################################
def from_to(vec_a, vec_b, num_steps):
  if num_steps < 1:
    return [ vec_a ]
  else:
    ax, ay, az = vec_a
    bx, by, bz = vec_b
    dx = float(bx - ax)/num_steps
    dy = float(by - ay)/num_steps
    dz = float(bz - az)/num_steps
    return [ [ax + dx*i, ay + dy*i, az + dz*i] for i in range(num_steps+1) ]

###############################################################################
class Euler(MumagCore):
  """Class for simulation with only one magnetization"""
  def __init__(self, mesh, initial_mag, m_sat, where=[1], order=1):
    MumagCore.__init__(self, mesh, where, m_sat, initial_mag, order)
    self.step_calculate_h_total = None

    # Default settings
    for p in euler_defaults: self.features[p] = euler_defaults[p]

    self.ID = None # Integer assigned by the parent 'Eulers' class
    self.parent_ID = None # The 'Eulers' instance which generated this object

  def setup(self):
    '''This function should be called after the method 'set' to setup
       the simulation (create the fields, the operators and so on)'''

    # Should not do initializizations more than once
    if self.is_ready: return

    self.damping = self.features["damping"]
    self.gamma_G = self.features["gamma_G"]
    self.gamma_LL = self.features["gamma_LL"]
    self.absolute_step_error = self.features["absolute_step_error"]
    self.relative_step_error = self.features["relative_step_error"]
    self.step_headroom = self.features["step_headroom"]
    self.stopping_time = self.features["stopping_time"]
    self.stopping_dm_dt = self.features["stopping_dm_dt"]
    self.max_iterations = self.features["max_iterations"]
    self.max_rejections = self.features["max_rejections"]
    self.initial_dt = self.features["initial_dt"]
    self.max_dt_ratio = self.features["max_dt_ratio"]
    self.initial_time = self.features["initial_time"]
    self.time = 0.0


    MumagCore.setup(self)

    # self.llg_gamma_LL == None if llg_gamma_G is given by the user
    if self.gamma_LL == None:
      self.gamma_LL = self.gamma_G/(1.0+self.damping**2)

    self.new_field("old_m", indices=[3], initial_values=self.initial_mag)
    self.new_field("dm_dt", indices=[3])
    self.new_field("old_dm_dt", indices=[3])
    self.new_field("delta_t", indices=[])

    # Just some shorthands for what follows
    mwe_m = self.mwes["m"]
    field_m = self.fields["m"]

    # The auxiliary arguments passed from python to C
    args_names = ["time", "dt", "old_max_norm_dm_dt",
      "llg_damping", "llg_gamma_LL",
      "absolute_step_error", "relative_step_error", "step_headroom",
      "m_sat"]

    # Create the C-functions which perform the different parts
    # of the computation

    # The fields which will be passed to 'step_make' and 'step_check'
    some_names = ["m", "old_m", "dm_dt", "old_dm_dt", "delta_t", "h_ext",
     "h_total", "h_demag", "h_exch"]
    some_mwes = self.mwe_list(some_names)
    some_fields = self.field_list(some_names)

    # Called to execute the euler step (calculate the new m and the
    # value of old_norm_dm_dt needed for the computation of the error)
    c_step_make = nfem.site_wise_applicator(args_names,
      ccode_step_make, field_mwes=some_mwes)
    def step_make(time, dt):
      self.old_max_norm_dm_dt = 0.0
      aux_arg_list = [time, dt, self.old_max_norm_dm_dt, self.damping,
        self.gamma_LL, self.absolute_step_error, self.relative_step_error,
        self.step_headroom, self.m_sat]
      modified_args = c_step_make(aux_arg_list, fields=some_fields)
      self.old_max_norm_dm_dt = modified_args[2]
    #----------------------End of definition of the function step_make
    self.step_make = step_make

    c_step_check = nfem.site_wise_applicator(args_names,
      ccode_step_check, field_mwes=some_mwes)

    def step_check(time, dt):
      aux_arg_list = [time, dt, self.old_max_norm_dm_dt, self.damping,
       self.gamma_LL, self.absolute_step_error, self.relative_step_error,
       self.step_headroom, self.m_sat]
      c_step_check(aux_arg_list, fields=some_fields)
    #----------------------End of definition of the function step_check
    self.step_check = step_check

    self.c_calculate_next_dt = nfem.site_wise_applicator(
      ["accepted_step", "previous_dt"], ccode_calculate_next_dt,
      field_mwes=[self.mwes["delta_t"]])

    self.step_calculate_h_total = self.calculate_h_total
    self.is_ready = True
    self.next_stage(time=self.initial_time)

  def next_stage(self, h_ext=None, time=None):
    if h_ext: self.set_field("h_ext", h_ext)
    self.nr_iterations = 0
    self.nr_rejections = 0
    if time: self.time = time
    self.old_dt = self.initial_dt
    self.whole_step(0.0, 0.0)
    self.is_converged = False
    self.whole_step(0.0, 0.0) # Just to calculate dm_dt
    self.step_accept()        # This sets old_dm_dt = dm_dt

  def whole_step(self, time, dt):
    self.step_make(time, dt)
    self.step_calculate_h_total()
    self.step_check(time, dt)
    return (self.old_max_norm_dm_dt <= self.stopping_dm_dt)

  # This function accepts a step by copying the new fields into the old ones
  def step_accept(self):
    nfem.field_copy_into(self.fields["m"], self.fields["old_m"])
    nfem.field_copy_into(self.fields["dm_dt"], self.fields["old_dm_dt"])

  def calculate_next_dt(self, default_dt, max_dt):
    '''
      Each site in the mesh is integrated separately and has its own suggested
      value for the next time step length 'dt'. 'dt' is interpreted in the
      following way:
      dt < 0  --> step not accepted, suggested dt for next attempt is |dt|
      dt > 0  --> step accepted, suggested dt for next step is dt
      dt == 0 --> step accepted, no suggestion
      These values are stored into the scalar field 'delta_t'.
      This function scans the field 'delta_t' and returns the smaller
      of the suggested values for 'dt'. If no value is suggested it
      returns 'default_dt'. If the suggested value is greater than 'max_dt',
      it returns 'max_dt'.
      Actually this function returns a pair (is_accepted, suggested_dt),
      where 'is_accepted' is a boolean which is true if the step has been
      accepted.
    '''
    [accepted_step, suggested_dt] = self.c_calculate_next_dt([1.0, -1.0],
      fields=[self.fields["delta_t"]])

    if suggested_dt > 0.0:
      next_dt = suggested_dt
    else:
      next_dt = default_dt
    if next_dt > max_dt:
      next_dt = max_dt
    return ((accepted_step > 0.0), next_dt)

  def advance(self):
    '''
      Carry on the simulation one step further. This is an "end-user" function,
      which takes care of evolving the magnetization, calculating the exchange,
      demag and all the other needed fields, checking the errors, accepting or
      rejecting the move, calculating the next step length.
    '''
    if not self.is_ready: self.setup()

    check_time = (self.stopping_time > 0.0)
    # max_iterations < 0 ==> infinite loop
    if ( self.nr_iterations == self.max_iterations
       | check_time & (self.time > self.stopping_time) ):
      self.is_converged = False
      return False

    self.is_converged = self.whole_step(self.time, self.old_dt)

    # Calculate the next dt
    is_accepted, suggested_dt = self.calculate_next_dt(
      self.initial_dt, # default dt used if there's no suggested dt(all=0)
      self.max_dt_ratio*self.old_dt) # maximum limit for next dt

    if is_accepted:
      self.step_accept()
      self.nr_rejections = 0
      self.nr_iterations += 1
      self.time += self.old_dt
      self.old_dt = suggested_dt
      return True

    else:
      print "REJECTED!!!"
      self.nr_rejections += 1
      self.is_converged = False
      # Too many consecutive rejections? It should not happen!
      if ( self.nr_rejections > self.max_rejections ):
        raise "Too many rejections!"
      self.old_dt = suggested_dt
      return True

  def every(self, num_steps):
    if int(num_steps) < 1: return False
    return (self.nr_iterations % int(num_steps) == 0)

###############################################################################

ccode_surface_interaction = ccode_header + """
#line 494 "euler.py(ccode_surface_interaction)"
#define SURF_INTERACTION(M1, Ms1, H1, M2, Ms2, H2, ec) \\
if ( have_##M1 && have_##M2 ) { \\
  Real c = ec/mu0; \\
  H1(0) += c*Ms2*M2(0); H1(1) += c*Ms2*M2(1); H1(2) += c*Ms2*M2(2); \\
  H2(0) += c*Ms1*M1(0); H2(1) += c*Ms1*M1(1); H2(2) += c*Ms1*M1(2); \\
}

"""

def surface_interaction(m_h_list, couplings):
  """This function is used to calculate the exchange interactions between
  magnetisations defined at the same sites in the mesh."""

  mwes = []
  fields = []
  i = 1
  for m_h_couple in m_h_list:
    m, h = m_h_couple

    m_new_name = "m%d" % i
    h_new_name = "h%d" % i
    i += 1

    mwe_m = ocaml.get_mwe(m)
    mwe_mn = nfem.mwe_sibling(mwe_m, m_new_name, "renamed_m", [("m", m_new_name)])
    mn = nfem.field_alias(m, mwe_mn)

    mwe_h = ocaml.get_mwe(h)
    mwe_hn = nfem.mwe_sibling(mwe_h, h_new_name, "renamed_h", [("h_total", h_new_name)])
    hn = nfem.field_alias(h, mwe_hn)

    fields.append( mn )
    fields.append( hn )

    mwes.append( mwe_mn )
    mwes.append( mwe_hn )

  ccode = ccode_surface_interaction
  for ac in couplings:
    i1, i2, m_sat1, m_sat2, ec = ac # ec is the exchange coupling constant
    ccode += "SURF_INTERACTION(m%d, %f, h%d, m%d, %f, h%d, %f)\n" % \
      (i1, m_sat1, i1, i2, m_sat2, i2, ec)

  original_fn = nfem.site_wise_applicator([], ccode, field_mwes=mwes)
  def fn():
    original_fn([], fields=fields)

  return fn

###############################################################################
class Eulers:
  """Class for simulation of several magnetisations"""
  instances = 0

  def __init__(self, mesh, order=1):
    # Status
    self.is_ready = False

    # Copy default settings
    self.features = euler_defaults.copy()
    self.mesh = mesh
    self.order = order
    self.external_field = set_to_zero

    self.num_llg = 0
    self.llgs = []
    self.couplings = []

    self.ID = Eulers.instances
    Eulers.instances += 1

  def Euler(self, initial_mag, m_sat, where=[1]):
    # Create the child Euler instance
    llg = Euler(self.mesh, initial_mag, m_sat, where, order=self.order)
    # Marks the Euler instance as a child of this Eulers instance
    llg.parent_ID = self.ID
    # Assigns an identifier to distinguish between the child
    llg.ID = self.num_llg
    self.num_llg += 1
    self.llgs.append( llg )
    return llg

  def next_stage(self, h_ext=None, time=None):
    if h_ext: self.set_field("h_ext", h_ext)
    self.nr_iterations = 0
    self.nr_rejections = 0
    if time: self.time = time
    self.old_dt = self.initial_dt
    self.whole_step(0.0, 0.0)
    for llg in self.llgs: llg.step_accept()
    self.whole_step(0.0, 0.0)
    for llg in self.llgs: llg.step_accept()

  def setup(self):
    if self.is_ready: return

    self.damping = self.features["damping"]
    self.gamma_G = self.features["gamma_G"]
    self.gamma_LL = self.features["gamma_LL"]
    self.absolute_step_error = self.features["absolute_step_error"]
    self.relative_step_error = self.features["relative_step_error"]
    self.step_headroom = self.features["step_headroom"]
    self.stopping_time = self.features["stopping_time"]
    self.stopping_dm_dt = self.features["stopping_dm_dt"]
    self.max_iterations = self.features["max_iterations"]
    self.max_rejections = self.features["max_rejections"]
    self.initial_dt = self.features["initial_dt"]
    self.max_dt_ratio = self.features["max_dt_ratio"]
    self.initial_time = self.features["initial_time"]
    self.time = 0.0

    # self.llg_gamma_LL == None if llg_gamma_G is given by the user
    if self.gamma_LL == None:
      self.gamma_LL = self.gamma_G/(1.0+self.damping**2)

    for llg in self.llgs: llg.setup()

    if len(self.couplings) > 0:
      i = 0
      m_h_list = []
      for llg in self.llgs:
        if ( llg.ID != i ): raise "Eulers.setup: Internal error!"
        m = llg.get_field("m")
        h = llg.get_field("h_total")
        m_h_list.append( (m, h) )
        i += 1

      self.calculate_surf_int = surface_interaction(m_h_list, self.couplings)
    else:
      self.calculate_surf_int = None

    self.next_stage(time=self.initial_time)
    self.is_converged = False
    self.is_ready = True

    # Should also check the coherence of the parameters between the different
    # 'Euler' instances (damping, alpha should be the same: we should
    # give a warning at least!)

  def set(self, **features):
    '''This function sets the parameters for the simulation.
       You can set the following parameters:
         damping, gamma_G, gamma_LL absolute_step_error, relative_step_error,
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

  def set_field(self, name, values=set_to_zero):
    '''Set all the fields of the child Euler instances using
       the provided function'''
    for llg in self.llgs: llg.set_field(name, values)

  def exchange_coupling(self, couple, exchange_coupling):
    llg1, llg2 = couple
    if llg1.parent_ID != self.ID or llg2.parent_ID != self.ID:
      raise "'Euler' instance is not child of this 'Eulers' instance"

    if llg1.ID == llg2.ID:
      if llg1 != llg2: raise "Broken 'Euler' instances!"
      llg1.set(exchange_coupling=exchange_coupling)

    else:
      self.couplings.append( (llg1.ID+1, llg2.ID+1,
       llg1.m_sat, llg2.m_sat, exchange_coupling) )

  def whole_step(self, time, dt):
    self.old_max_norm_dm_dt = -1.0
    for llg in self.llgs:
      llg.step_make(time, dt)
      self.old_max_norm_dm_dt = \
        max(self.old_max_norm_dm_dt, llg.old_max_norm_dm_dt)
    for llg in self.llgs: llg.old_max_norm_dm_dt = self.old_max_norm_dm_dt

    for llg in self.llgs: llg.step_calculate_h_total()
    if self.calculate_surf_int: self.calculate_surf_int()

    self.is_converged = True
    accepted = True
    dt_tot = -1.0
    for llg in self.llgs:
      llg.step_check(time, dt)
      llg.is_converged = (llg.old_max_norm_dm_dt <= llg.stopping_dm_dt)
      self.is_converged &= llg.is_converged

      accepted_i, dt_i = llg.calculate_next_dt(self.initial_dt,
        self.max_dt_ratio*self.old_dt)
      accepted &= accepted_i
      if dt_tot < 0.0:
        dt_tot = dt_i
      else:
        dt_tot = min(dt_tot, dt_i)

    return (accepted, dt_tot)

  def advance(self):
    if not self.is_ready:
      raise "You should call the method 'setup' first!"

    check_time = (self.stopping_time > 0.0)
    # max_iterations < 0 ==> infinite loop
    if ( self.nr_iterations == self.max_iterations
       | check_time & (self.time > self.stopping_time) ):
      self.is_converged = False
      return False

    accepted, dt = self.whole_step(self.time, self.old_dt)

    if accepted:
      for llg in self.llgs: llg.step_accept()
      self.nr_rejections = 0
      self.nr_iterations += 1
      self.time += self.old_dt
      self.old_dt = dt
      return True

    else:
      self.nr_rejections += 1
      # Too many consecutive rejections? It should not happen!
      if ( self.nr_rejections > self.max_rejections ):
        raise "Too many rejections!"
      self.old_dt = dt
      return True

  def every(self, num_steps):
    if int(num_steps) < 1: return False
    return (self.nr_iterations % int(num_steps) == 0)

  def total_energy(self):
    ''' Returns the total energy of the system calculated as:
          U = mu0 * integral_of M*H_total
    '''
    energy = 0.0
    for llg in self.llgs: energy += llg.total_energy()
    return energy
