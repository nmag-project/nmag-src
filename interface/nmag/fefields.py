import types, numpy

import ocaml, nfem
from nmag_exceptions import *
from nsim.vectools import normalised_vector
from nsim.si_units import SI
import nsim.features

features = nsim.features.Features()
experimental = (features.get("etc", "experimental").strip().lower() == "true"
                if features.has_sectionkey("etc", "experimental")
                else False)

import logging
log = logging.getLogger('nmag')

def _set_what_msg(subfieldname, site=None):
    '''Internal function, used by _prepare_set_field when reporting error
    messages'''
    if site != None:
        return "subfield %s at site %s" % (subfieldname, site)
    else:
        return "subfield %s" % subfieldname

def _data_msg(data):
  return (" By the way, your data is '%s'." % data) if data != None else ""

def _check_field(field, subfieldname, data=None, site=None):
    """
    Internal function used to make some sanity checks before setting a field.
    """

    # We do a number of sanity checks for which we need some metadata
    list_of_subfield_and_shape_tuples = nfem.data_doftypes(field)

    # Gets all the shapes mapped by the subfield names
    shape_by_subfieldname = {}
    for sfname, shape in list_of_subfield_and_shape_tuples:
        shape_by_subfieldname[sfname] = shape

    # 1 Does the field have this subfield:
    if subfieldname not in shape_by_subfieldname:
        subfields = shape_by_subfieldname.keys()
        msg = ("You want to set %s but I have only found subfield(s) '%s' "
               "in the field.%s" % (_set_what_msg(subfieldname, site),
                                    subfields, _data_msg(data)))
        raise NmagUserError(msg)

    # 2 Is the shape of the data right?
    shape = list(numpy.array(data).shape) # quite a silly way to get the
                                          # shape, but it works!
    if shape != shape_by_subfieldname[subfieldname]:
        msg = ("When trying to set %s we have a shape mismatch. The shape of "
               "the subfield data is '%s' but the shape of your data is '%s'."
               "%s"
               % (_set_what_msg(subfieldname, site),
                  shape_by_subfieldname[subfieldname], shape,
                  _data_msg(data)))
        raise NmagUserError(msg)

def _prepare_set_field(field, subfieldname, data, site=None,
                       scale_factor=1.0, normalise=False, check_site=False):
    """
    Internal function used to make some sanity checks before setting a field.
    """

    # 1-2 check size and shape of field
    _check_field(field, subfieldname, data, site)

    # 3 is the data of the right type --> done by ocaml

    # 4 check that the site type is a list
    if check_site and type(site) != types.ListType:
        raise NmagUserError("'site' has to be a list of integers. For "
                            "first order basis functions, there is only "
                            "one integer in the list, the node id.")

    # Now we can actually set the data.

    # Do we need to normalise?
    if normalise:
        data = normalised_vector(data)

    # Do we need to apply the scale factor
    if type(data) in [types.IntType, types.FloatType, types.BooleanType]:
      data *= float(scale_factor)

    else:
      # FIXME: this doesn't work with tensors!
      data = map(lambda x : x*float(scale_factor), data)

    return data

def set_field_at_site(field, subfieldname, site, data,
                      scale_factor=1.0, normalise=False):
    """Interface to ocaml.set_field_at_site (includes error checking).

    Arguments:

     field: is an OCamlPill

     <snip> to be completed
    """

    data = _prepare_set_field(field, subfieldname, data, site,
                              scale_factor, normalise, check_site=True)
    status = ocaml.set_field_at_site(field, site, subfieldname, [data])

    if status == "set":
        return

    elif status == "not found" or status == "unset":
        raise NmagUserError("Subfield '%s' is not defined at site '%s'."
			    % (subfieldname, site))

def set_fielddata_from_function(field, subfieldname, function,
                                pos_unit_length, normalise=False,
                                scale_factor=1.0):
    if type(function) != types.FunctionType:
        raise NmagUserError("Expect function-object but got type '%s'"
			    % type(function))

    log.log(15, "set_fielddata_from_function: Entering, populate "
            "subfield '%s' from function '%s'"
            % (subfieldname, function.__name__))

    site_ids, site_pos, shape, site_vols = \
      ocaml.mwe_subfield_metadata(field, subfieldname)

    for pos, site in zip(site_pos, site_ids):
        # convert position from mesh coordinates to meter
        pos = [(x*pos_unit_length).in_units_of(SI("m")) for x in pos]

        # sample function and set field at current site
        value = function(pos)
        set_field_at_site(field, subfieldname, site, value,
                          normalise=normalise, scale_factor=scale_factor)

    log.debug("set_fielddata_from_function: Done.")

# The following functions are for setting subfields.
def set_fielddata_from_vector(field, subfieldname, data,
                              scale_factor=1.0, normalise=False):
    # First check that shape of data and shape of this subfield agree:
    log.debug("set_fielddata_from_vector: setting %s to '%s'."
              % (_set_what_msg(subfieldname), data))

    if True:
        data = _prepare_set_field(field, subfieldname, data, None, # <- site
                                  scale_factor, normalise)
        exit_status = ocaml.set_field_uniformly(field, subfieldname, data)

        if exit_status != 0:
            raise NmagUserError("set_fielddata_from_vector: couldn't set "
                                "subfield '%s'." % subfieldname)

    else:
        site_ids, site_pos, shape, site_vols = \
	      ocaml.mwe_subfield_metadata(field, subfieldname)

        for site in site_ids:
            set_field_at_site(field, subfieldname, site, data,
                              normalise=normalise,
                              scale_factor=scale_factor)

    log.debug("set_fielddata_from_vector: Done.")

# To be removed soon
def set_fielddata_from_numpyarray_old(field, subfieldname, data,
                                      normalise=False, scale_factor=1.0):
    if type(data) != numpy.ndarray:
        raise NmagUserError,"Expect numpy array but got '%s'" % str(type(data))

    # check that length of data agrees with subfield
    site_ids, site_pos, shape, site_vols = \
      ocaml.mwe_subfield_metadata(field,subfieldname)

    # check whether we have the same number of sites
    if len(site_ids) != len(data):
        raise NmagUserError("Subfield '%s' has %d sites but the data you "
                            "provided as a numpy array has %d site entries."
                            % (subfieldname, len(site_ids), len(data)))

    # check whether the 'tensor' for each site has the correct shape
    for i in range(len(data)):
        site = site_ids[i]
        tensor_value = data[i].tolist()
        set_field_at_site(field, subfieldname, site, tensor_value,
                          scale_factor=scale_factor,
                          normalise=normalise)

def normalise_array(x):
  norms = numpy.sqrt((x*x).reshape(len(x),-1).sum(1))
  new_norm_shape = [len(x)] + [1]*(len(x.shape) - 1)
  return x/norms.reshape(*new_norm_shape)

def set_fielddata_from_numpyarray(field, subfieldname, data,
                                  normalise=False, scale_factor=1.0):
    if type(data) != numpy.ndarray:
        raise NmagUserError("Expect numpy array but got '%s'" % type(data))

    _check_field(field, subfieldname, data[0] if len(data) > 0 else None)

    if normalise:
      data = normalise_array(data)

    sf = float(scale_factor)
    if sf != 1.0:
      data *= sf

    ocaml.set_field_from_array(field, subfieldname, data)

def flexible_set_fielddata(field, subfieldname, data, pos_unit_length,
                           scale_factor=1.0, normalise=False):
    """A function that can set lam fields, delegating to various
    subfunctions depending on the type of the data provided. This
    functions is not meant to be called directly from the nmag user
    because it needs to know the ``field`` and the ``subfieldname``
    which can be figured out by nmag automatically.

    :Parameters:

      `field` : OCaml Pill for field
        The field that should be modified.

      `data` : list of floats, function or numpy array
        The input data. See Simulation.set_m for further explanation.

      `subfieldname` : string
        The name of the subfield that is meant to be modified

      `pos_unit_length` : SI object
        The scale factor that converts coordinates in simulation units
        into SI positions.  (This is required when the user provides a
        function to sample as ``data``.)

      `scale_factor=1.0` : float
        The nmag user is setting fields using SI objects. The
        associated numbers need to be converted to simulation
        units. This is done at a higher level (where we know more
        about what kind of field this is, and in particular what units
        it has). The relevant scaling_factor is then passed to the
        setting routine here.

      `normalise` : True or False
        If ``True``, then the ``data`` provided for the vector field
        will be normalised (i.e.  have magnitude 1.0). Used to
        normalise the magnetisation ``m``.

    """

    log.debug("flexible_set_fielddata: Entering. subfieldname='%s', "
              "scale_factor=%f" % (str(subfieldname),scale_factor))

    if type(data) == types.ListType:
        # - constant vector (length must match dimension)
        set_fielddata_from_vector(field, subfieldname, data,
                                  scale_factor=scale_factor,
                                  normalise=normalise)

    elif type(data) in [types.IntType, types.FloatType, types.BooleanType]:
        set_fielddata_from_vector(field, subfieldname, float(data),
                                  scale_factor=scale_factor,
                                  normalise=normalise)

    elif type(data) == types.FunctionType:
        function = data
        set_fielddata_from_function(field, subfieldname, function,
                                    pos_unit_length,
                                    scale_factor=scale_factor,
                                    normalise=normalise)

    elif type(data) == numpy.ndarray:
        fn = (set_fielddata_from_numpyarray if experimental
              else set_fielddata_from_numpyarray_old)
        fn(field, subfieldname, data, scale_factor=scale_factor,
           normalise=normalise)

    else:
        print flexible_set_fielddata.__doc__
        raise NmagUserError("Don't know how to set fielddata from type '%s'"
                            % type(data))

