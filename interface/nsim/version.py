# Nmag micromagnetic simulator
# Copyright (C) 2011 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

unknown_version = (None, None, None)

version      = unknown_version
release_date = None
vcinfo       = None
dist_date    = None
dist_mode    = None
dist_vcinfo  = None

all_infos = {     "version": unknown_version,
             "release_date": None,
                   "vcinfo": None,
                "dist_date": None,
                "dist_mode": None,
              "dist_vcinfo": None}

default_infos = all_infos.copy()

try:
    import info
    for n in all_infos:
        if hasattr(info, n):
            all_infos[n] = getattr(info, n)

except:
    print "WARNING: cannot find version information"

# Add variables to this module
this_module = vars()
for n in all_infos:
    this_module[n] = all_infos[n]

version_str = ("(unknown)" if version == unknown_version
               else "%d.%d.%d" % tuple(version))

if release_date == None:
    version_str += "-dev"

def generate_info():
    this_file = this_module.get("__file__", "?")
    ls  = (["# File generated automatically by %s" % this_file,
            "# DO NOT EDIT MANUALLY!"] +
           ["%s = %s" % (key, repr(value))
            for key, value in all_infos.iteritems()
            if value != default_infos[key]])
    return "\n".join(ls) + "\n"

if __name__ == "__main__":
    print version_str
