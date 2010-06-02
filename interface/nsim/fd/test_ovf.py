import sys

from ovf import OVFFile, OVF10, OVF20
from lattice import FieldLattice

small_value = {"binary8": 1e-10,
               "binary4": 1e-6,
               "text": 1e-10}

# The following function is included for debugging
def ipython(globals=None, locals=None):
    calling_frame = sys._getframe(1)
    if globals == None:
        globals = calling_frame.f_globals
    if locals == None:
        locals = calling_frame.f_locals
    from IPython.Shell import IPShellEmbed
    IPShellEmbed([])(local_ns=locals, global_ns=globals)

def test_one(fl, ver, data_type, mesh_type, filename):
    quasi_zero = small_value[data_type]

    print "Testing OVF%s,%s,%s" % (ver[0], data_type, mesh_type)

    print "Writing file to disk"
    f = OVFFile()
    f.new(fl, version=ver, data_type=data_type,
          mesh_type=mesh_type)
    f.write(filename)

    print "Reading file back"
    g = OVFFile(filename)

    print "Checking consistency"
    g_data = g.content.a_segment.a_data.field
    f_data = f.content.a_segment.a_data.field
    diff = f_data - g_data
    diff_norm = ((diff**2).sum())**0.5

    assert diff_norm <= quasi_zero, \
      ("Saved and loaded data differ. Difference is %g > %g."
       % (diff_norm, quasi_zero))

    print "norm(loaded - saved) = %g <= %g" % (diff_norm, quasi_zero)
    print

    import os
    try:
        os.unlink(filename)
    except:
        pass

def norm(r):
    x, y, z = r
    return (x*x + y*y + z*z)**0.5

def data1():
    fl = FieldLattice([(2.5e-9, 97.5e-9, 20),
                       (2.5e-9, 47.5e-9, 10),
                       (2.5e-9,  7.5e-9, 1)],
                      order="F")

    ctr = [50.0e-9, 25.0e-9, 2.5e-9]
    def fn(pos):
        d = [(ci - pi) for ci, pi in zip(ctr, pos)]
        dn = norm(d)
        return [0, 0, 0] if dn < 1e-15 else [di/dn for di in d]

    fl.set(fn)
    return fl

def data2():
    fl = FieldLattice([(2.5e-9, 97.5e-9, 20),
                       (2.5e-9, 47.5e-9, 10),
                       (2.5e-9, 22.5e-9, 5)],
                      order="F")

    ctr = [50.0e-9, 25.0e-9, 12.5e-9]
    def fn(pos):
        d = [(ci - pi) for ci, pi in zip(ctr, pos)]
        dn = norm(d)
        return [0, 0, 0] if dn < 1e-15 else [di/dn for di in d]

    fl.set(fn)
    return fl

def test_ovf_read_and_write(filename="ovftest.ovf"):
    fls = [data1(), data2()]

    for i, fl in enumerate(fls):
        print "TESTING DATA %d" % i
        for ver in [OVF10, OVF20]:
            for data_type in ["binary4", "binary8", "text"]:
                for mesh_type in ["rectangular"]:
                    test_one(fl, ver, data_type, mesh_type, filename)

if __name__ == "__main__":
    test_ovf_read_and_write()

