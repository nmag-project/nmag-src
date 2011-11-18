import sys
import os

try:
    import py.test

except:
    raise SystemError("py.test must be installed in order to run the "
                      "Nmag tests")

import py.test.cmdline

my_path = os.path.split(sys.argv[0])[0]
root_path = os.path.realpath(os.path.join(my_path, ".."))

os.chdir(root_path)

args = sys.argv[1:]
sys.argv = [sys.argv[0]]
py.test.cmdline.main(args)
