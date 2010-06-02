from ovf import OVFFile, OVF10, OVF20
from lattice import FieldLattice

def test_ovf_read_and_write(filename="ovftest.ovf"):
    fl = FieldLattice([(2.5e-9, 97.5e-9, 20),
                       (2.5e-9, 47.5e-9, 10),
                       (2.5e-9,  7.5e-9, 1)],
                      order="F")

    def fn(pos):
        return [1, 0, 0]

    fl.set(fn)

    for ver in [OVF10, OVF20]:
        for data_type in ["binary4", "binary8", "text"]:
            for mesh_type in ["rectangular"]:
                print "Testing OVF%s,%s,%s" % (ver[0], data_type, mesh_type)

                print "Writing file to disk"
                f = OVFFile()
                f.new(fl, version=ver, data_type=data_type,
                      mesh_type=mesh_type)
                f.write(filename)

                print "Reading file back"
                g = OVFFile(filename)
                print

            import os
            try:
                os.unlink(filename)
            except:
                pass

if __name__ == "__main__":
    test_ovf_read_and_write()

