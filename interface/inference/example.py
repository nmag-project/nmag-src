
from inference import *



def from_mbuf_copier(name):
    def f():
        print "copy mbuf '%s' -> LAM" % name
    return f

def to_mbuf_copier(name):
    def f():
        print "copy LAM -> mbuf '%s'" % name
    return f

def lam_executor(name):
    def f():
        print "lam_execute '%s'" % name
    return f

def llg():
    print "Executing LLG!"


mag_maker=InferenceEngine(\
    [{"name":"mbuf_m"},
     {"name":"mbuf_H_ext"},
     # --
     {"name":"mbuf_dmdt","depends_on":["lam_dmdt"],"how_to_make":[to_mbuf_copier("dmdt")]},
     {"name":"mbuf_rho","depends_on":["lam_rho"],"how_to_make":[to_mbuf_copier("rho")]},
     {"name":"mbuf_phi","depends_on":["lam_phi"],"how_to_make":[to_mbuf_copier("phi")]},
     {"name":"mbuf_M","depends_on":["lam_M"],"how_to_make":[to_mbuf_copier("M")]},
     {"name":"mbuf_H_exch","depends_on":["lam_H_exch"],"how_to_make":[to_mbuf_copier("H_exch")]},
     {"name":"mbuf_H_demag","depends_on":["lam_H_demag"],"how_to_make":[to_mbuf_copier("H_demag")]},
     {"name":"mbuf_H_total","depends_on":["lam_H_total"],"how_to_make":[to_mbuf_copier("H_total")]},

     {"name":"mbuf_E_ext","depends_on":["lam_E_ext"],"how_to_make":[to_mbuf_copier("E_ext")]},
     {"name":"mbuf_E_exch","depends_on":["lam_E_exch"],"how_to_make":[to_mbuf_copier("E_exch")]},
     {"name":"mbuf_E_demag","depends_on":["lam_E_demag"],"how_to_make":[to_mbuf_copier("E_demag")]},
     {"name":"mbuf_E_total","depends_on":["lam_E_total"],"how_to_make":[to_mbuf_copier("E_total")]},
     # ---
     {"name":"lam_m","depends_on":["mbuf_m"],"how_to_make":[from_mbuf_copier("m")]},
     {"name":"lam_H_ext","depends_on":["mbuf_H_ext"],"how_to_make":[from_mbuf_copier("H_ext")]},
     # ---
     {"name":"lam_rho",
      "depends_on":["lam_m"],
      "how_to_make":[lam_executor("set_H_demag")],
      "also_updates":["lam_H_demag","lam_rho","lam_phi"]},

     {"name":"lam_phi",
      "depends_on":["lam_m"],
      "how_to_make":[lam_executor("set_H_demag")],
      "also_updates":["lam_H_demag","lam_rho","lam_phi"]},
     
     {"name":"lam_M","depends_on":["lam_m"],"how_to_make":[lam_executor("set_M")]},

     {"name":"lam_H_exch","depends_on":["lam_m"],"how_to_make":[lam_executor("set_H_exch")]},

     {"name":"lam_H_demag",
      "depends_on":["lam_m"],
      "how_to_make":[lam_executor("set_H_demag")],
      "also_updates":["lam_H_demag","lam_rho","lam_phi"]},

     {"name":"lam_H_total",
      "depends_on":["lam_H_exch","lam_H_demag","lam_H_ext"],
      "how_to_make":[lam_executor("set_H_total")]},

     {"name":"lam_energies",
      "depends_on":["lam_m","lam_H_total"],
      "how_to_make":[lam_executor("update_E")],
      "also_updates":["lam_E_ext","lam_E_exch","lam_E_demag","lam_E_total"]},
     
     {"name":"lam_E_ext","depends_on":["lam_energies"]},
     {"name":"lam_E_exch","depends_on":["lam_energies"]},
     {"name":"lam_E_demag","depends_on":["lam_energies"]},
     {"name":"lam_E_total","depends_on":["lam_energies"]},
     # ---
     {"name":"lam_dmdt","depends_on":["lam_H_total"],"how_to_make":[llg]}
     ])



def change_m():
    mag_maker.invalidate("lam_m")

def advance_time():
    print "Ensure we have a timestepper"
    print "Use cvode_advance_time()"
    print "copy result to mbuf_m"
    mag_maker.invalidate("lam_m")

def read_field(name):
    mag_maker.make("mbuf_"+name)

