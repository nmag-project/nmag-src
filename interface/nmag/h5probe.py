'''
This module provides the implementation of the nmagprobe command line tool.

Copyright (C) 2010 University of Southampton
Author: Matteo Franchin
'''

import sys, math, cmath, numpy, tables, logging
import numpy.fft as fft

log = logging.getLogger('nmagprobe')
logmsg = log.info

import ocaml
from nmeshlib import load_hdf5
import nmag.fefields as ff
from nmag.nmag_exceptions import *
import nfem.hdf5_v01 as hdf5
from nsim.timings import Timer, show_timers

timer1 = Timer("readh5")

def first_difference(la, lb):
    """Given two lists 'la' and 'lb', returns the index at which the two lists
    differ or len(la) if the first entries in 'lb' match with all the entries
    of 'la'."""
    for i, a in enumerate(la):
        if a != lb[i]:
            return i
    return len(a)

def complex_filter(filter_name):
    """Returns a function which is supposed to act as a filter on complex
    numbers. 'complex_filter' takes a string with the name of the filter
    and returns the corresponding function. Available filters are:
    
      - identity: just return the identity function (lambda x: x);
      - norm: return the norm of the complex number;
      - real: return the real part of the complex number;
      - imag: return the imaginary part of the complex number

    Example:
        filter = complex_filter('imag')
        print filter(1.23 + 4.56j) # this prints just 4.56
    """
    if filter_name in ['complex', 'identity']:
        return lambda x: x

    elif filter_name in ['abs', 'norm', 'mod', 'modulus']:
        return lambda x: abs(x)

    elif filter_name == 'real':
        return lambda x: x.real

    elif filter_name in ['imag', 'imaginary']:
        return lambda x: x.imag

    raise NmagUserError("Cannot find the filter you specified for converting "
                        "complex numbers to real numbers.")

def parse_lattice_spec(s):
    """The lattice specification should be a string such as "-5,10,5/0.5,2,2",
    which defines a two dimensional lattice where the x coordinate goes
    from -5 to 10 in 5 steps, while the y coordinate goes from 0.5 to 2 in 2
    steps. Another example is "-5,10,5/1.23,1.23,1", which defines a one
    dimensional lattice (since the y component is fixed to 1.23).
    The same can be expressed also as "-5,10,5/1.23".
    """
    def parse_dim_spec(dim_spec):
        try:
            nums = dim_spec.split(',')
            if len(nums) == 1:
                x_min = x_max = nums[0]
                num_steps = 1
            else:
                x_min, x_max, num_steps = nums
        except:
            raise NmagUserError('Error in lattice specification: '
                                + parse_lattice_specs.__doc__)
        return [float(x_min), float(x_max), int(num_steps)]
    return [parse_dim_spec(spec) for spec in s.split('/')]

class Lattice:
    """This class allows to define a n-dimensional square lattice an perform
    various operations on it. In particular it allows to iterate over the
    points of the lattice. No storage is needed for this (a Lattice of one
    millions of points doesn't require more memory that a Lattice of 1 point).
    The points of the lattice can be referred univocally by index."""

    def __init__(self, min_max_num_list, reduction=0.0):
        """Creates a lattice given a list containing, for each dimension,
        the corresponding minimum and maximum coordinate and the number of
        points in which it is discretised. Something like:
            
          [(x_min, x_max, x_num), (y_min, y_max, y_num), ...]
          
        Alternatively, a string is accepted following the same specification
        accepted by the function 'parse_lattice_spec'.
        """
        if type(min_max_num_list) == str:
            min_max_num_list = parse_lattice_spec(min_max_num_list)
        self.min_max_num_list = list(min_max_num_list)
        self.dim = len(min_max_num_list)
        self.reduction = reduction

    def __repr__(self):
        return "Lattice(%s)" % self.min_max_num_list

    def __add__(self, right):
        reduction = max(self.reduction, right.reduction)
        return Lattice(self.min_max_num_list + right.min_max_num_list,
                       reduction=reduction)

    def get_shape(self):
        """Returns the shape of the lattice, i.e. a list containing the number
        of points for each dimension of the lattice. Example: [10, 5, 20] for
        a 3D lattice made of 10 by 5 by 20 points."""
        return [i for _, _, i in self.min_max_num_list]

    def get_num_points(self):
        """Returns the total number of points in the lattice."""
        return reduce(lambda x, y: x*y, self.get_shape())

    def get_closest(self, position):
        """Given a position in space, returns the point in the lattice which
        is closer to it. What is returned is actually the index of the point
        in the lattice."""
        def get_closest_i(x, i, min_max_num_list):
            x_min, x_max, x_num = min_max_num_list
            if x_min < x_max:
                return int(round((x_num - 1) * (x - x_min)/(x_max - x_min)))
            else:
                return 0
        return [get_closest_i(position[i], i, min_max_num_list_i)
                for i, min_max_num_list_i in enumerate(self.min_max_num_list)]

    def get_pos_from_idx(self, idx):
        """Return the position of the point in the lattice which has the given
        index."""
        pos = []
        for nr, i in enumerate(idx):
            x_min, x_max, x_num = self.min_max_num_list[nr]
            if x_num > 1:
                delta_x = (x_max - x_min)/(x_num - 1)
                pos.append(x_min + delta_x*i)
            else:
                pos.append(x_min)
        return pos

    def _foreach(self, nr_idx, idx, pos, fn):
        if nr_idx == self.dim:
            fn(idx, pos)

        else:
            x_min, x_max, num_steps = self.min_max_num_list[nr_idx]
            x_min += self.reduction
            x_max -= self.reduction
            xi = x_min
            assert num_steps > 0, ("Number of steps is less than 1 for "
                                   "dimension %d of the lattice!" % nr_idx)
            if num_steps == 1:
                delta_xi = 0.0
            else:
                delta_xi = (x_max - x_min)/(num_steps - 1)

            for i in range(num_steps):
                pos[nr_idx] = xi
                idx[nr_idx] = i
                xi += delta_xi
                self._foreach(nr_idx+1, idx, pos, fn)

    def foreach(self, fn):
        """Iterates over all the points in the lattice and, for each of those,
        call 'fn(idx, pos)' where 'idx' is the index of the current point,
        while 'pos' is its position as given by the method 'get_pos_from_idx'.
        """
        idx = [0]*self.dim
        pos = [0.0]*self.dim
        self._foreach(0, idx, pos, fn)

def vector_norm(v):
    """Return the Euclidean norm of a vector."""
    return math.sqrt(reduce(lambda s, x: s + x*x, v, 0.0))

def vector_component(nr_component):
    """Return a function that returns the nr_component component of the input
    vector."""
    return lambda v: v[nr_component]

def vector_projection(nr_component):
    def fn(v):
        s = 0.0
        for i, vi in enumerate(v):
            if i != nr_component:
                s += vi*vi
        return math.sqrt(s)
    return fn

def vector_filter(filter_spec):
    """Returns a function which maps a vector into a scalar.
    filter_spec specifies what kind of filter to return:
        
      - 'component,1' returns the second component of the vector
      
      - 'norm' returns the norm of the vector;
      
      - 'projection,1' project the vector into the plane orthogonal to the
        second axis and return its norm.
    """
    filter_parts = filter_spec.split(',', 1)
    if len(filter_parts) == 1:
        filter_parts.append(None)
    filter_name, filter_args = filter_parts
    if filter_name == 'identity':
        fn = lambda x: x
    elif filter_name == 'component':
        fn = vector_component(int(filter_args))
    elif filter_name == 'norm':
        fn = vector_norm
    elif filter_name == 'projection' or filter_name == 'proj':
        fn = vector_projection(int(filter_args))
    else:
        raise NmagUserError("vector_component: unrecognised filter '%s'"
                            % filter_name)
    def fn_with_opt_args(v, idx=None, t=None):
        return fn(v)

    return fn_with_opt_args

default_vector_filter = vector_filter('identity')

class ProbeStore:
    def __init__(self, times, lattice, dtype=numpy.float64):
        self.times = times
        self.lattice = lattice
        self.ts_lattice = times + lattice
        self.dtype = dtype
        self.data_shape = times.get_shape() + lattice.get_shape()
        self.item_shape = None
        self.data = None
        self.data0 = None
        logmsg("Creating ProbeStore object: shape=%s" % (self.data_shape,))

    def __str__(self):
        out = [""]
        def out_writer(s): out[0] += s
        self.write(out=out_writer)
        return out[0]

    def get_data(self):
        if self.data == None:
            shape = tuple(self.data_shape + self.item_shape)
            logmsg("Allocating ProbeStore data array: shape = %s"
                   % str(shape))
            self.data = numpy.ndarray(shape, dtype=self.dtype)
            self.data.fill(0.0)

        return self.data

    def define_item_shape(self, item_shape):
        if self.item_shape == None or self.data == None:
            self.item_shape = list(item_shape)

        else:
            l = list(item_shape)
            if l != self.item_shape:
                logmsg("Incoherent item shape: %s, should be %s"
                       % (l, self.item_shape))

    def define_item_shape_from_example(self, item_example):
        ie = item_example
        if item_example != numpy.ndarray:
            ie = numpy.array(item_example)
        self.define_item_shape(ie.shape)

    def generate_filler(self, filter=None):
        if filter != None:
            my_filter = filter
        else:
            my_filter = default_vector_filter

        def filler(t, pos, value):
            idx_t = self.times.get_closest([t])
            idx_space = self.lattice.get_closest(pos)
            idx = tuple(idx_t + idx_space)
            v = my_filter(value, idx=idx, t=t)
            self.define_item_shape_from_example(v)
            data = self.get_data()
            data.__setitem__(idx, v)
        return filler

    def probe_field(self, file_name, field_name, subfield_name,
                    filter=None):
        filler = self.generate_filler(filter=filter)
        probe_field_from_file(file_name, self.times, self.lattice, field_name,
                              subfield_name, out=filler)

    def define_reference_field(self, file_name, field_name, subfield_name,
                               time, filter=None):
        one_time = Lattice([(time, time, 1)])
        ref = ProbeStore(one_time, self.lattice, dtype=self.dtype)
        ref.probe_field(file_name, field_name, subfield_name, filter=filter)
        self.data0 = ref.get_data()

    def probe_field_variation(self, file_name, field_name, subfield_name,
                              filter=None):
        if self.data0 == None:
            raise NmagUserError("Cannot use ProbeStore.probe_field_variation "
                                "if a reference field has not been already "
                                "set with ProbeStore.define_reference_field.")
        if filter == None:
            filter = default_vector_filter

        def my_filter(v, idx=None, t=None):
            my_idx = list(idx)
            my_idx[0] = 0
            my_idx = tuple(my_idx)
            v0 = self.data0.__getitem__(my_idx)
            return filter(v - v0, idx=idx, t=t)

        self.probe_field(file_name, field_name, subfield_name,
                         filter=my_filter)

    def compute_fft(self, axes):
        """Return a new ProbeStore object containing the Fourier trasform
        of the starting ProbeStore object. axes is the list of axes which
        should be fourier transformed."""
        if self.item_shape != []:
            raise NmagUserError("Cannot compute the FFT of a ProbeStore "
                                "object having non scalar items!")
        data = self.get_data()
        ft_data = fft.fftn(data, axes=axes)
        ft_ps = ProbeStore(self.times, self.lattice, dtype=ft_data.dtype)
        ft_ps.data = fft.fftshift(ft_data, axes=axes)
        l = ft_ps.ts_lattice
        factor = complex(1.0)
        for axis in axes:
            x_min, x_max, N = l.min_max_num_list[axis]
            delta_x = x_max - x_min
            delta_k = 2*math.pi/delta_x
            factor *= delta_x * cmath.exp(complex(0, -x_min))
            if N % 2 == 0:
                x_min = -(N/2)*delta_k
                x_max = (N/2 - 1)*delta_k

            else:
                x_max = ((N - 1)/2)*delta_k
                x_min = -x_max
            l.min_max_num_list[axis] = (x_min, x_max, N)
        ft_ps.data *= factor
        return ft_ps

        #ft_data = fft.fftshift(ft_data)

    def write(self, out=sys.stdout.write, fmt="%s", filter=None, 
              sep_blocks="\n"):
        if filter == None:
            filter = lambda x: x
        last_idx = [None]
        fastest_digit = [None]
        def my_out(idx, pos):
            lidx = last_idx[0]
            if lidx != None:
                fd = first_difference(idx, lidx)
                fastest_digit[0] = mfd = max(fastest_digit[0], fd)
                if sep_blocks != None and fd < mfd:
                    out(sep_blocks)
            last_idx[0] = list(idx)
            value = filter(self.data.__getitem__(tuple(idx)))
            s = " ".join([fmt % xi for xi in pos]) + " " + fmt % value + "\n"
            out(s)

        self.ts_lattice.foreach(my_out)

    def write_to_file(self, file_name, fmt="%s", filter=None,
                      sep_blocks="\n"):
        f = open(file_name, "w")
        self.write(out=f.write, fmt=fmt, filter=filter, sep_blocks=sep_blocks)
        f.close()

def probe_field_on_lattice(lattice, field, subfield, out):
    def do(idx, position):
        probed = ocaml.probe_field(field, subfield, position)
        if probed != None:
            assert(len(probed) == 1)
            dofname, value = probed[0]
            out(position, value)
    lattice.foreach(do)

def build_full_field_name(field_name, subfield_name):
    return '%s_%s' % (field_name, subfield_name)

class Fields:
    """Class which can be used to load the fields stored inside an Nmag h5
    file. The fields are reconstructed so that they can be manipulated."""

    def __init__(self, dat_file_name):
        self.field_order = 1 # we support only order 1
        self.file_name = dat_file_name
        self.mesh = load_hdf5(dat_file_name)
        self.fields = {}
        self.root_data_fields = None
        self.handler = None
        self.read_cache = {}
        self._counters_for_field = {}

    def __del__(self):
        self.close_handler()

    def open_handler(self):
        if self.handler == None:
            logmsg("Opening tables handler for file '%s'" % self.file_name)
            self.handler = tables.openFile(self.file_name, 'r')
        return self.handler

    def close_handler(self):
        if self.handler != None:
            logmsg("Closing handler for file '%s'" % self.file_name)
            self.fields = {}
            self.root_data_fields = None
            self.handler.close()
        self.handler = None

    def get_root_data_fields(self):
        if self.root_data_fields!= None:
            return self.root_data_fields

        f = self.open_handler()
        try:
            self.root_data_fields = f.root.data.fields
            return self.root_data_fields

        except:
            raise NmagUserError("The h5 file doesn't have the section "
                                "data.fields: are you sure "
                                "this is an Nmag data file?")

    def get_dof_row_data(self, field_name, subfield_name, row, cache_size=1000):
        """Wrapping for hdf5.get_dof_row_data which may be optimised to
        include buffering (for speedup)."""
        full_field_name = build_full_field_name(field_name, subfield_name)
        f = self.open_handler()
        tuple = hdf5.get_dof_row_data(f, field_name, full_field_name, row)
        return tuple

        row_data_id = (full_field_name, row)
        if self.read_cache.has_key(row_data_id):
            print "%s found in cache" % str(row_data_id)
            return self.read_cache[row_data_id]

        else:
            # we read more rows, so that we can fill the cache
            # NOTE: this is much more efficient that doing separate reads
            tuple = hdf5.get_dof_data_rows(f, field_name, full_field_name,
                                           row, row+cache_size)
            ps, vss, sites = tuple
            for i, cached_vs in enumerate(vss):
                cached_row_data_id = (full_field_name, row + i)
                print "caching %s" % str(cached_row_data_id)
                self.read_cache[cached_row_data_id] = (ps, cached_vs, sites)

            return (ps, vss[0], sites)
 
    def get_field_array(self, field_name, subfield_name, row):
        """This method can be used to extract the data for the specified field
        and the specified row. Example:

          positions, values, site_ids = fields.get_field_array('m', 'Py', 0)

        The method accepts also a more flexible and powerful specification
        of the row, such as row = [(coeff1, row1), (coeff2, row2), ...],
        which returns a linear combination of the field evaluated at different
        values of 'row' multiplied by the corresponding coefficient. Example:

          row = [(0.5, 0), (0.5, 1)]
          positions, values, site_ids = fields.get_field_array('m', 'Py', row)

        will return the average of the values of the field at row=0 and row=1.
        """
        f = self.open_handler()
        if type(row) == list:
            if len(row) < 1:
                raise NmagUserError("The row should be an integer or a list "
                                    "of tuples (coefficient, row)!")

            coefficient, idx = row[0]
            ps, vs_tot, sites = self.get_dof_row_data(field_name,
                                                      subfield_name, idx)
            vs_tot *= coefficient

            for coefficient, idx in row[1:]:
                _, vs, _ = self.get_dof_row_data(field_name, 
                                                 subfield_name, idx)
                vs *= coefficient
                vs_tot += vs

            return (ps, vs_tot, sites)

        else:
            ps_vs_sites = self.get_dof_row_data(field_name, 
                                                subfield_name, row)
            return ps_vs_sites

    def _create_field(self, field_name, subfield_name=None, row=0):
        # This is the real subfield name
        full_field_name = build_full_field_name(field_name, subfield_name)

        # Now we get the data
        f = self.open_handler()
        root_data_fields = self.get_root_data_fields()
       
        dim = self.mesh.dim
        field_table = f.getNode(root_data_fields, field_name)
        field_stuff = getattr(root_data_fields, field_name) # need to improve this
        field_shape = list(field_stuff[0][1].shape)[1:]

        # Get the sites where the subfield is defined (sites) and the
        # corresponding coordinates (ps) and values (vs)
        ps, vs, sites = self.get_field_array(field_name, subfield_name, row)

        # sites now contains the dof allocation for the subfield, i.e. in
        # which sites the subfield is defined.

        # We now want to build a numarray of one boolean value per each site
        # which says whether for that site the field is defined or not
        pointsregions = self.mesh.pointsregions
        num_sites = len(pointsregions)
        defined = numpy.ndarray(num_sites, dtype=numpy.bool)
        defined.fill(False)
        for site_where_defined in sites:
            defined[site_where_defined] = True

        # Run over all the sites and build a list of regions where the field
        # is undefined
        all_regions = range(1, self.mesh.numregions)
        regions_where_defined = range(1, self.mesh.numregions)
        for site in range(num_sites):
            if not defined[site]:
                regions_owning_this_site = pointsregions[site]
                for region in regions_owning_this_site:
                    if region in regions_where_defined:
                        regions_where_defined.remove(region)

        # Now we know in which regions of the mesh the field is defined
        logmsg("'%s' has been found in regions %s"
               % (full_field_name, regions_where_defined))

        # Consistency check: there musn't be a region where the field
        # is partially defined! 
        logmsg("Now checking that there the field is always present...")
        for site in range(num_sites):
            field_should_be_defined_here = \
              (True in [rwd in pointsregions[site]
                        for rwd in regions_where_defined])
            # ^^^ True when this site belongs to one region which is listed
            # in 'regions_where_defined'
            if field_should_be_defined_here != defined[site]:
                logmsg("Inconsistency while checking the field definition "
                       "regions: site %d belongs to region %s, but the field "
                       "is defined in regions %s."
                       % (site, pointsregions[site], regions_where_defined))
                raise NmagUserError("The given file seems to be corrupt!"
                                    "Cannot proceed!")

        logmsg("Check successful: definition regions are %s"
               % regions_where_defined)

        logmsg("Creating a new element '%s' for the field" % full_field_name)
        element = ocaml.make_element(full_field_name, field_shape, dim,
                                     self.field_order)

        logmsg("Creating MWE")
        element_assoc = zip(regions_where_defined,
                            [element]*len(regions_where_defined))
        properties = zip(regions_where_defined,
                         [[full_field_name]]*len(regions_where_defined))
        mwe = ocaml.make_mwe(field_name,
                             self.mesh.raw_mesh, 
                             element_assoc,
                             [],
                             properties)

        logmsg("Creating the field")
        field = ocaml.raw_make_field(mwe, [], "", "")

        # We now try to understand how to map the data from file into the
        # newly created field
        metadata = ocaml.mwe_subfield_metadata(field, full_field_name)
        new_site_ids, new_pos, new_shape, new_site_vols = metadata
        num_new_sites = len(new_site_ids)
        if num_new_sites != len(sites):
            raise NmagUserError("The re-created field seems inconsistent "
                                "with the one saved to file. Number of "
                                "sites is %d for saved and %d for new field. "
                                "Cannot proceed!" % (num_sites,num_new_sites))

        # Check that the new field is binary compatible with the old one
        # this is really what we expect and allows us to set the field
        # by just passing a numarray as retrieved from the file
        need_map = False
        for i in range(num_new_sites):
            if new_site_ids[i] != sites[i]:
                need_map = True

        if need_map:
            raise NmagUserError("need_map=True, not implemented because this"
                                "was not expected to happen, anyway! Contact"
                                "Matteo (franchin@soton.ac.uk)")

        self.fields[full_field_name] = field
        timer1.start("x")
        ff.set_fielddata_from_numpyarray(field, full_field_name, vs)
        timer1.stop("x")
        return field

    def set_field_data(self, field_name, subfield_name=None, row=0):
        full_field_name = build_full_field_name(field_name, subfield_name)
        if self.fields.has_key(full_field_name):
            logmsg("found already generated field '%s': setting this"
                   % full_field_name)
            field = self.fields[full_field_name]
            f = self.open_handler()
            ps, vs, sites = \
              self.get_field_array(field_name, subfield_name, row)
            timer1.start("x")
            ff.set_fielddata_from_numpyarray(field, full_field_name, vs)
            timer1.stop("x")
            return field

        else:
            return self._create_field(field_name, subfield_name=subfield_name,
                                      row=row)

    def get_counters_for_field(self, field_name, subfield_name):
        """Necessary in order to alleviate the very bad performance
        of hdf5.get_counters_for_field (use caching)."""
        full_field_name = build_full_field_name(field_name, subfield_name)
        if self._counters_for_field.has_key(full_field_name):
            return self._counters_for_field[full_field_name]

        else:
            f = self.open_handler()
            counters = hdf5.get_counters_for_field(f, field_name)
            self._counters_for_field[full_field_name] = counters
            return counters

    def set_field_data_at_time(self, field_name, subfield_name, time,
                               equality_tolerance=1e-19):
        f = self.open_handler()
        counters = self.get_counters_for_field(field_name, subfield_name)
        ids = counters['id']
        times = counters['time']
        rows = counters['row']
        num_ids = len(ids)
        assert len(times) == num_ids, ("Inconsistency found in "
                                       "Fields.set_field_data_at_time")
        lower_maximum = None
        upper_minimum = None
        for i in range(num_ids):
            save_time = times[i]
            save_id = ids[i]
            save_row = rows[i]
            if abs(save_time - time) < equality_tolerance:
                logmsg("The time you specified (t=%g) matches sufficiently "
                       "well (tolerance=%g) with one for which the data was "
                       "saved (t=%g, row=%d): using this!"
                       % (time, equality_tolerance, save_time, save_row))
                return self.set_field_data(field_name, subfield_name, save_id)

            elif save_time < time:
                if lower_maximum == None or save_time > lower_maximum[0]:
                    lower_maximum = (save_time, save_id, save_row)

            else: # save_time > time
                if upper_minimum == None or save_time < upper_minimum[0]:
                    upper_minimum = (save_time, save_id, save_row)

        if lower_maximum == None or upper_minimum == None:
            logmsg("The time you specified (t=%g) lies outside the "
                   "interval for which the field has been saved ([%g, %g])!"
                   % (time, min(times), max(times)))
            if lower_maximum == None:
                ret = upper_minimum
            else:
                ret = lower_maximum
            logmsg("Returning field for time=%g (id=%d, row=%d)" % tuple(ret))
            return self.set_field_data(field_name, subfield_name, ret[2])

        t_min, _, row_min = lower_maximum
        t_max, _, row_max = upper_minimum
        logmsg("Returning linear interpolation between t=%g (row=%d) "
               "and t=%g (row=%d)" % (t_min, row_min, t_max, row_max))

        delta_t = t_max - t_min
        linear_comb = [((t_max - time)/delta_t, row_min),
                       ((time - t_min)/delta_t, row_max)]
        return self.set_field_data(field_name, subfield_name, linear_comb)

def probe_field_from_file(file_name, times, lattice, field_name, subfield,
                          out=None):
    def myout(t, pos, val):
        print " ".join([str(s) for s in [t] + pos + val])

    if out == None:
        out = myout

    f = Fields(file_name)

    def do_for_time(idx, t):
        partial_out = lambda pos, val: out(t[0], pos, val)

        field = f.set_field_data_at_time(field_name, subfield, t[0])

        if lattice.dim != f.mesh.dim:
            raise NmagUserError("The lattice of points you specified has a "
                                "number of dimension (%d) which does not "
                                "match the one of the mesh (%d)."
                                % (lattice.dim, f.mesh.dim))

        full_field_name = build_full_field_name(field_name, subfield) 
        probe_field_on_lattice(lattice, field, full_field_name, partial_out)

    times.foreach(do_for_time)

    f.close_handler()

def probe_and_fft_field_from_file(file_name, times, lattice, field_name,
                                  subfield_name, axes=None,
                                  vector_to_scalar=None,
                                  real_out=None,
                                  fft_out=None,
                                  complex_to_real=None,
                                  ref_time=None):

    if vector_to_scalar == None:
        vector_to_scalar = default_vector_filter

    if complex_to_real == None:
        complex_to_real = lambda x: abs(x)

    ps = ProbeStore(times, lattice)
    if ref_time != None:
        ps.define_reference_field(file_name, field_name, subfield_name,
                                  time=ref_time)
        ps.probe_field_variation(file_name, field_name, subfield_name,
                                 filter=vector_to_scalar)

    else:
        ps.probe_field(file_name, field_name, subfield_name,
                       filter=vector_to_scalar)


    if real_out != None:
        ps.write_to_file(real_out)

    ft_ps = None
    if fft_out != None:
        ft_ps = ps.compute_fft(axes=axes)
        ft_ps.write_to_file(fft_out, filter=complex_to_real)

    return (ps, ft_ps)

def parse_cmdline(prog, args):
    import optparse
    parser = optparse.OptionParser(usage=usage, prog=prog)

    # LETTERS USED FOR OPTIONS: ftmrao
    help = ("Print the control parameters on the screen.")
    parser.add_option("-v", "--verbose",
                      help=help,
                      action="store_true", dest="verbose")

    # PROBE CONTROL OPTIONS
    desc = "Probe control options (lattice specification, etc):"
    help = ("The '--time' and '--space' options define when and where the "
            "field should be probed. This is a four dimensional lattice of "
            "points in time and space. Linear interpolation is used in order "
            "to extrapolate the values at the required points in time and "
            "space. "
            "EXAMPLE 1: '--space=2.0/3/4.5 --time=0,10e-9,11' to probe point"
            "(2.0, 3, 4.5) at time t=0, 1, 2, ..., 10 nanoseconds. Note that "
            "the spatial coordinates are expressed in the units of the mesh, "
            "while the time is expressed in SI units. "
            "EXAMPLE 2: '--space=0,100,11/2/2 --time=0,10e-9,11' to probe "
            "the points (0, 2, 2), (10, 2, 2), ..., (100, 2, 2) at the same "
            "moments in time which were used in the previous example. "
            "EXAMPLE 3: '--space=0,100,21/0,10,3/1,10,3 --time=0' to probe a "
            "three dimensional grid of points at time t=0.0.")
    lattice_group = optparse.OptionGroup(parser, desc, help)

    help = ("the name of the field which you want to probe. "
            "EXAMPLE: '--field=m_Py'")
    lattice_group.add_option("-f", "--field",
                             help=help,
                             action="store", dest="fieldname")

    help = ("Where to probe the field. This is a 3D lattice which is "
            "specified through a string such as 'X/Y/Z', where X, Y and Z "
            "are relative to each coordinate and follow the same syntax "
            "described for the '--time' option.")
    lattice_group.add_option("-s", "--space", metavar="LATTICE",
                             help=help, action="store",
                             dest="space_lattice")

    help = ("When to probe the field. This can be either a precise moment in "
            "time (i.e. a real number expressing the time in seconds) or "
            "a specification 'START,END,STEPS', in case the field needs to "
            "be probed for multiple time values. In particular, START and "
            "END are respectively the initial and final time (in seconds) "
            "while STEPS is the number of values equally spaced in the "
            "given time interval.")
    lattice_group.add_option("-t", "--time", metavar="LATTICE",
                             help=help, action="store",
                             dest="time_lattice")

    help = ("When the probed field is a vector field, this option can be "
            "used to specify how it should be reduced to a scalar field "
            "(this may be necessary for applying the Fourier transform, "
            "for example). Available choices are: "
            "'--scalar-mode=identity' (default) to keep the full vector. "
            "Fourier transform won't be computed, then. '--scalar-mode=norm' "
            "to compute the norm of the vector field. "
            "'--scalar-mode=component,n' to extract the n-th component of "
            "the vector field (EXAMPLE: '--scalar-mode=component,1'). "
            "'--scalar-mode=projection,n' to compute the norm of the "
            "projection of the vector in the plane orthogonal to the n-th "
            "axis, i.e. if v = (x0, x1, x2) is the vector, then returns "
            "sqrt(x0^2 + x1^2 + x2^2 - xn^2). (EXAMPLE: "
            "'--scalar-mode=projection,1' returns sqrt(x0*x0 + x2*x2)).")
    lattice_group.add_option("-m", "--scalar-mode",
                             help=help, action="store",
                             dest="scalar_mode", metavar="MODE")

    help = ("If a reference time is provided, then the data of the field "
            "is subtracted with the value it had at that time. This means "
            "that the data which will be analysed (saved or Fourier "
            "transformed) is not the data itself, but the variation with "
            "respect to the given time, i.e. field(t) - field(t0), where t0 "
            "is the reference time provided with this option.")
    lattice_group.add_option("-r", "--ref-time", metavar="TIME",
                             help=help, action="store",
                             dest="ref_time", type='float')

    parser.add_option_group(lattice_group)

    # FOURIER TRANSFORM CONTROL OPTIONS
    desc = "Fourier transform options:"
    help = ("Options to control how the Fourier transform is made.")
    ft_group = optparse.OptionGroup(parser, desc, help)

    help = ("Specifies which axes should be Fourier-transormed. "
            "EXAMPLE: '--ft-axes=0,1'")
    ft_group.add_option("-a", "--ft-axes",
                        help=help, action="store",
                        dest="ft_axes", metavar="AXES")

    help = ("Specifies if and how to convert the complex numbers coming "
            "from the Fourier transform of the probed field to real numbers."
            "Available choices for mode are:  '--ft-out-mode=complex' "
            "(default) to save the full complex number. '--ft-out-mode=real' "
            "to save only the real part. '--ft-out-mode=imag' to save only "
            "the imaginary part. '--ft-out-mode=norm' save the norm "
            "(modulus) of the complex number.")
    ft_group.add_option("--ft-out-mode",
                        help=help, action="store",
                        dest="out_mode", metavar="MODE")

    parser.add_option_group(ft_group)

    # OUTPUT CONTROL OPTIONS
    desc = "Specification of output files and their format"
    help = ("Options which specify the name of the files where to save "
            "the output.")
    out_group = optparse.OptionGroup(parser, desc, help)

    help = "The file where to put the probed values."
    out_group.add_option("-o", "--out",
                         help=help, action="store",
                         dest="out", metavar="FILENAME")

    help = "The file where to put the Fourier transform of the probed values."
    out_group.add_option("--ft-out",
                         help=help, action="store",
                         dest="ft_out", metavar="FILENAME")

    help = ("The format to use when saving the data. "
            "Available choices are: text, ovf. '--out-format=text' to save "
            "the data as a text file (which is the default); "
            "'--out-format=ovf' to save the data in the OOMMF vector field "
            "data format, the format used by OOMMF when saving its data. "
            "The latter choice may be used to probe the magnetisation of a "
            "finite element simulation, so that it can be used by OOMMF in a "
            "finite difference simulation.")
    out_group.add_option("--out-format", metavar="FORMAT",
                         help=help, action="store",
                         dest="fileformat")

    parser.add_option_group(out_group)

    options, p_args = parser.parse_args(args)
    return (options, p_args)

usage = '''
%prog [probe control options] [Fourier transf. control options]
      [output options] filename_dat.h5

(C) 2010, University of Southampton, United Kingdom

The Nmag data probing tool (nmagprobe).

Examples:

- (shortest possible) probe the field m_Py at time=0 and position (0, 0, 0),
  save the result into out.txt

  %prog -f m_Py dyn_dat.h5

- probing field m_Py over a 1x2x3 grid from time t=0 to t=10 ns, saving the
  output into a text file with name 'evolution.txt'

  %prog dyn_dat.h5 -f m_Py -s 0/0,10,2/0,20,3 -t 0,10e-9,11 -o evolution.txt

- probing the component 1 of the magnetisation and doing the Fourier transform
  in time and space

  %prog --verbose dyn_dat.h5 --field=m_Py \\
    --time=0,100e-12,101 --space=0,400,201/2/2 --ref-time=0.0 \\
    --scalar-mode=component,1 --ft-axes=0,1 \\
    --out=real-space.dat --ft-out=rec-space.dat

Overview:
The %prog command can be used in order to probe the data stored inside
the provided Nmag data file 'filename_dat.h5' (which usually has 'h5'
extension). %prog uses linear interpolation to extract the value of a
field at the specified points in space and time. The extracted data can then
be further processed (Fourier transformed, for example) and finally saved to
disk in various formats. You can use %prog for the following purposes:

  - analysing the dynamics of the magnetisation at a single point in space
    or for a 3D lattice of points by saving it to a text file, which is then
    easy to inspect;

  - probe the magnetisation over an arbitrary 3D lattice for visualisation
    purposes;

  - probe the magnetisation over an arbitrary 3D lattice and convert it into
    an omf file which can be loaded by OOMMF (you could transfer your
    magnetisation configuration from Nmag to OOMMF);

  - Fourier transform the magnetisation along one or more of the four
    coordinates of time-space in order to produce dispersion plots
    useful for studies of the magnonic properties of your sample;

Here we focused on the magnetisation field, but you can obviously choose any
other field which is stored inside the provided input file.
'''

def main(prog, args):
    # Parse the command line
    options, p_args = parse_cmdline(prog, args)

    # Collect the parsed data and check that it is enough to proceed
    if len(p_args) < 2:
        raise ValueError("Need filename of Nmag data file to process "
                         "(use '-h' for help)")

    elif len(p_args) > 2:
        raise ValueError("You should only provide one Nmag data file, "
                         "but you provided %d files." % len(p_args))

    input_filename = p_args[1]
    fieldname = options.fieldname
    ref_time = options.ref_time
    time_lattice = options.time_lattice
    space_lattice = options.space_lattice
    out_filename = options.out
    ft_out_filename = options.ft_out
    scalar_mode = options.scalar_mode
    ft_out_mode = options.out_mode

    d_time_lattice = 0
    d_space_lattice = (0, 0, 0)
    d_out_filename = "out.txt"

    if fieldname == None:
        raise ValueError("You should use the option '-f' to specify "
                "what field to process. Example: '-f m_Py'.")

    if time_lattice == None:
        logmsg("Missing specification of time lattice. "
               "Probing at time=%s..." % d_time_lattice)
        time_lattice = str(d_time_lattice)

    if space_lattice == None:
        logmsg("Missing specification of spatial lattice. "
               "Probing at point (%s, %s, %s)..." % d_space_lattice)
        space_lattice = "%s/%s/%s" % d_space_lattice

    if out_filename == None:
        logmsg("Missing specification of output file. Using '%s'"
               % d_out_filename)
        out_filename = d_out_filename

    ft_axes = None
    if options.ft_axes != None:
        ft_axes = [int(axis) for axis in options.ft_axes.split(',')]

    if scalar_mode == None:
        scalar_mode = 'identity'

    vec2scal = vector_filter(scalar_mode)

    if ft_out_mode == None:
        ft_out_mode = "norm"

    cmplx2real = complex_filter(ft_out_mode)

    fieldname = "m"
    subfieldname = "Py"

    l = Lattice(space_lattice, reduction=1e-15)
    ts = Lattice(time_lattice, reduction=1e-20)

    if options.verbose:
        heading = "EXECUTING PROBE WITH FOLLOWING SETTINGS:"
        print
        print heading
        print "-"*len(heading)
        settings = [('input file', input_filename),
                    ('output file', out_filename),
                    ('FT output file', ft_out_filename),
                    ('field name', fieldname),
                    ('subfield name', subfieldname),
                    ('reference time', ref_time),
                    ('lattice in space', l),
                    ('lattice in time', ts),
                    ('vector to scalar reduction', scalar_mode),
                    ('FT axes', ft_axes),
                    ('FT output mode', ft_out_mode)]
        for name, setting in settings:
            print "%30s: %s" % (name, setting)
        print

    out_fd = open(out_filename, 'w')

    #previous_time = [None]
    #def out(t, pos, val):
    #    if t != previous_time[0]:
    #        out_fd.write("\n")
    #    previous_time[0] = t
    #    out_fd.write(" ".join([str(s) for s in [t] + pos + val]) + "\n")

    #probe_field_from_file(args['filename'], ts, l, args['field'],
    #                      args['subfield'], out=out)

    probe_and_fft_field_from_file(input_filename, ts, l, fieldname,
                                  subfieldname, axes=ft_axes,
                                  ref_time=ref_time,
                                  vector_to_scalar=vec2scal,
                                  real_out=out_filename,
                                  fft_out=ft_out_filename)

#import cProfile
#cProfile.run("profile()")

#import pstats
#pstats.
#show_timers()



