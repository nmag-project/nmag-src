# Automatically generated using distmake-builder 0.1
# Script generated from the following answers:
# Q: Which username should I use to access the repositories?
#    Use 'mf'.
# Q: Which repositories do you want to use as source for building the tarball?
#    Use the current repository for src and central repositories for doc and test.
# Q: Do you want to create the tarball for a previous version of the software?
#    Yes, I want to give the tag name and create a tarball for that.
# Q: What tag do you want to use? (try "hg tag" from the src repository)
#    nsim-0.1.1
# Q: Do you want to use the same tag for dist, test and doc?
#    No, use the latest versions for dist, test and doc (may create inconsistent tarballs).
# Q: Do you want to include documentation?
#    Yes, include documentation.
# Q: Do you want to include the test suite?
#    Yes, include the big test suite.
# Q: Do you want to keep the repositories in the tarball?
#    No, I don't care about version control.
# Q: Do you want to remove nmagprobe and its documentation?
#    Keep it, as I'm distributing the tarball to members of DYNAMAG!
# Q: Do you want a tarball or the directory?
#    Give me just a tarball.
# Q: I collected enough information. What should I do:
#    Generate a bash script to create the distribution file
. disttools.sh

USERAT=${USERNAME:+$USERNAME@}

# Configuration for the host and the repositories
REPOS_NSIM_MAIN=`cd .. && pwd`
REPOS_NSIM_TEST="ssh://${USERAT}gamma.kk.soton.ac.uk//var/local/hg/nsim/master/test"
REPOS_NSIM_DOC="ssh://${USERAT}gamma.kk.soton.ac.uk//var/local/hg/nsim/master/doc"
REPOS_NSIM_DIST="ssh://${USERAT}gamma.kk.soton.ac.uk//var/local/hg/nsim/dist"
PKGS_FILE=/var/local/data/nsim/pkgs.tar
REMOTE_MACHINE="${USERAT}gamma.kk.soton.ac.uk"
MAIN_TAG=nsim-0.1.1
PKGNAME=nmag-`get_version ..`
allsrc_dev_compose "$PKGNAME" 'nsim-0.1.1' 'tip'
add_doc "$PKGNAME" 'tip'
add_test "$PKGNAME" 'tip'
remove_hg "$PKGNAME"
remove_hg "$PKGNAME/nsim"
remove_dir "$PKGNAME" nsim/admin nsim/devel nsim/obsolete nsim/prototypes nsim/usersupport nsim/utils
remove_hg "$PKGNAME/nsim/tests"
remove_dir "$PKGNAME" nsim/tests/devtests
remove_hg "$PKGNAME/doc/manual"
gen_tarball "$PKGNAME"
remove_directory "$PKGNAME"
