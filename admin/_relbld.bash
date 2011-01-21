# Automatically generated using distmake-builder 0.1
# Script generated from the following answers:
# Q: Do you want to include documentation?
#    Yes, include documentation.
# Q: Do you want to include the test suite?
#    Yes, include the big test suite.
# Q: Do you want to use the repositories?
#    No, I don't care about version control.
# Q: Do you want a tarball or the directory?
#    Give me just a tarball.
# Q: I collected enough information. What should I do:
#    Generate a bash script to create the distribution file
. disttools.sh
PKGNAME=nmag-`get_version ..`
allsrc_dev_compose "$PKGNAME" 'trunk'
add_doc "$PKGNAME"
add_test "$PKGNAME"
remove_hg "$PKGNAME"
remove_hg "$PKGNAME/nsim"
remove_dir "$PKGNAME" nsim/admin nsim/devel nsim/obsolete nsim/prototypes nsim/usersupport nsim/utils
remove_hg "$PKGNAME/nsim/tests"
remove_hg "$PKGNAME/nsim/interface/nmag/manual"
gen_tarball "$PKGNAME"
remove_directory "$PKGNAME"
