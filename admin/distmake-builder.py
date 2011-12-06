#!/usr/bin/python
# Nmag micromagnetic simulator
# Copyright (C) 2010 University of Southampton
# Hans Fangohr, Thomas Fischbacher, Matteo Franchin and others
#
# WEB:     http://nmag.soton.ac.uk
# CONTACT: nmag@soton.ac.uk
#
# AUTHOR(S) OF THIS FILE: Matteo Franchin
# LICENSE: GNU General Public License 2.0
#          (see <http://www.gnu.org/licenses/>)

import os
import getpass

from choice import ChoiceList, Choice, Alternative, OpenQuestion, Script

import repositories

version = "distmake-builder 0.1"

# This may not be the best way to retrieve the username, but we are asking
# the user anyway, therefore it shouldn't matter too much...
username = getpass.getuser()

def comment(text):
    return "\n".join(["# %s" % line for line in text.splitlines()])

#=============================================================================
# Create the Plan object: we need to add all the possible choices a user may
# want to make and the code to use for each choice.
plan = ChoiceList("I'm now going to ask a few questions in order to build a "
                  "plan for making the distribution (a plan is just a shell "
                  "script that you can run to re-create the distribution so "
                  "that you don't need to answer the same questions again "
                  "and again).")

use_def_un = plan.add_choice(Choice("Which username should I use to access "
                                    "the repositories?"))
use_def_un.add_alternative(Alternative("Use '%s'." % username))
use_def_un.add_alternative(Alternative("Use a different username."))

what_un = plan.add_choice(OpenQuestion("What username shoud I use?"))
what_un.when_should_ask(lambda: use_def_un.chosen == 1)

want_ctr = plan.add_choice(Choice("Which repositories do you want to use as "
                                  "source for building the tarball?"))
want_ctr.add_alternative(Alternative("Use the central repositories."))
want_ctr.add_alternative(Alternative("Use the current repository for src and "
                                     "central repositories for doc and test."))

want_old = plan.add_choice(Choice("Do you want to create the tarball for a "
                                  "previous version of the software?"))
want_old.add_alternative(Alternative("No, I want the latest dev version."))
want_old.add_alternative(Alternative("Yes, I want to give the tag name and "
                                     "create a tarball for that."))

which_tag = plan.add_choice(OpenQuestion("What tag do you want to use? "
                                         "(try \"hg tag\" from the src "
                                         "repository)"))
which_tag.when_should_ask(lambda: want_old.chosen == 1)

same_tag = plan.add_choice(Choice("Do you want to use the same tag for "
                                  "dist, test and doc?"))
same_tag.when_should_ask(lambda: want_old.chosen == 1)
same_tag.add_alternative(Alternative("No, use the latest versions for dist, "
                                     "test and doc (may create inconsistent "
                                     "tarballs)."))
same_tag.add_alternative(Alternative("Yes, use the same tags (may fail if "
                                     "dev, test and doc do not all have such "
                                     " tag)."))

want_doc = plan.add_choice(Choice("Do you want to include documentation?"))
want_doc.add_alternative(Alternative("No, I'm concerned with size."))
want_doc.add_alternative(Alternative("Yes, include documentation."))
want_doc.add_alternative(Alternative("Yes, include and ALSO build it (this "
                                     "requires a number of packages to be"
                                     "installed in your system)."))

want_test = plan.add_choice(Choice("Do you want to include the test suite?"))
want_test.add_alternative(Alternative("No, I'm concerned with size."))
want_test.add_alternative(Alternative("Yes, include the big test suite."))

want_repo = plan.add_choice(Choice("Do you want to keep the repositories "
                                   "in the tarball?"))
want_repo.add_alternative(Alternative("No, I don't care about version "
                                      "control."))
want_repo.add_alternative(Alternative("Yes, I'm a developer, I want the "
                                      "repositories."))
repos = want_repo.add_alternative(ChoiceList("For each package (src, doc, test) "
                                             "I want do decide separately."))

distr = repos.add_choice(Choice("All-from-source repository: dist (Makefile "
                                "to build everything, including PETSc, etc)"))
distr.add_alternative(Alternative("Do not include version control for dist."))
distr.add_alternative(Alternative("Include version control for dist."))

srcr = repos.add_choice(Choice("Source repository: src"))
srcr.add_alternative(Alternative("Do not include version control for src."))
srcr.add_alternative(Alternative("Include version control for src."))

docr = repos.add_choice(Choice("Documentation repository: doc"))
docr.add_alternative(Alternative("Do not include version control for doc."))
docr.add_alternative(Alternative("Include version control for doc."))
docr.when_should_ask(lambda: want_doc.chosen == 1)

testr = repos.add_choice(Choice("Full test suite repository: test"))
testr.add_alternative(Alternative("Do not include version control for test."))
testr.add_alternative(Alternative("Include version control for test."))
testr.when_should_ask(lambda: want_test.chosen == 1)

msg = "Do you want to remove nmagprobe and its documentation?"
rm_nmagprobe = plan.add_choice(Choice(msg))
rm_nmagprobe.add_alternative(Alternative("Keep it, as I'm distributing the "
                                         "tarball to members of DYNAMAG!"))
rm_nmagprobe.add_alternative(Alternative("Remove it, as this tarball will be "
                                         "given to people outside the "
                                         "consortium,"))

want_tarb = plan.add_choice(Choice("Do you want a tarball or the directory?"))
want_tarb.add_alternative(Alternative("Give me just a tarball."))
want_tarb.add_alternative(Alternative("Give me just the corresponding directory."))
want_tarb.add_alternative(Alternative("Give me both."))

final = plan.add_choice(Choice("I collected enough information. What should I do:"))
final.add_alternative(Alternative("Create the distribution file(s)"))
final.add_alternative(Alternative("Generate a bash script to create the "
                                  "distribution file"))
final.add_alternative(Alternative("Do both things"))

#=============================================================================
# Ask the user to take decision and take note of the answers
script = Script()
plan.ask_user(script)


#=============================================================================
# Consider the answer and build the script
script.writeln("# Automatically generated using %s" % version)
script.writeln("# Script generated from the following answers:")
script.writeln(comment(str(plan)))
script.writeln(". disttools.sh")

# Use the user-provided username, if necessary
if use_def_un.chosen == 1:
    username = what_un.answer

want_local_main = (want_ctr.chosen == 1)
local_main = "`cd .. && pwd`" if want_local_main else None

varname_repository_haslocal_local = \
  (("REPOS_NSIM_MAIN", repositories.repos_main, want_local_main, local_main),
   ("REPOS_NSIM_TEST", repositories.repos_test, 0, None),
   ("REPOS_NSIM_DOC", repositories.repos_doc, 0, None),
   ("REPOS_NSIM_DIST", repositories.repos_dist, 0, None))

userat = username + "@"

script.writeln(comment("Configuration for the host and the repositories"))
for varname, repository, haslocal, local in varname_repository_haslocal_local:
    if haslocal:
        line = "%s=%s" % (varname, local)
    else:
        line = "%s='ssh://%s%s'" % (varname, userat, repository)
    script.writeln(line)

main_tag = ("tip" if want_old.chosen == 0 else which_tag.answer)
dev_tag = doc_tag = test_tag = "tip"

if same_tag.chosen == 1:
    dev_tag = doc_tag = test_tag = main_tag

lines = \
    ("PKGS_FILE=%s" % repositories.pkgs_file,
     "REMOTE_MACHINE=%s%s" % (userat, repositories.pkgs_host),
     "MAIN_TAG=%s" % main_tag)
for line in lines:
    script.writeln(line)

# # Import interface/nsim/versions.py to get version information
# import os
# import sys
# sys.path.insert(0, os.path.join("..", "interface", "nsim"))
# import version
# sys.path.pop(0)

# # Here is the package name
# pkg_name = "nmag-%s" % version.version_str

script.writeln("allsrc_dev_compose nmag-X.Y.Z '%s' '%s'"
               % (main_tag, dev_tag))

script.writeln("PKGNAME=nmag-`get_version nmag-X.Y.Z/nsim`")
script.writeln("mv nmag-X.Y.Z \"$PKGNAME\"")

has_doc = (want_doc.chosen != 0)
if has_doc:
    script.writeln("add_doc \"$PKGNAME\" '%s'" % doc_tag)
if want_doc.chosen == 2:
    script.writeln("build_doc \"$PKGNAME\"")

has_test = (want_test.chosen != 0)
if has_test:
    script.writeln("add_test \"$PKGNAME\" '%s'" % test_tag)

if want_repo.chosen != 1:
    if want_repo.chosen == 0:
        remove_dist_hg = True
        remove_src_hg = True
        remove_doc_hg = has_doc
        remove_test_hg = has_test
    else:
        remove_dist_hg = (distr.chosen == 0)
        remove_src_hg = (srcr.chosen == 0)
        remove_doc_hg = (docr.chosen == 0)
        remove_test_hg = (testr.chosen == 0)

    if remove_dist_hg:
        script.writeln('remove_hg "$PKGNAME"')

    if remove_src_hg:
        script.writeln('remove_hg "$PKGNAME/nsim"')
        script.writeln('remove_dir "$PKGNAME" nsim/admin nsim/devel nsim/obsolete '
                       'nsim/prototypes nsim/usersupport nsim/utils')

    if remove_test_hg:
        script.writeln('remove_hg "$PKGNAME/nsim/tests"')
        script.writeln('remove_dir "$PKGNAME" nsim/tests/devtests')

    if remove_doc_hg:
        script.writeln('remove_hg "$PKGNAME/doc/manual"')

if rm_nmagprobe.chosen:
    script.writeln('remove_file "$PKGNAME" nsim/bin/nmagprobe')
    if has_doc:
        script.writeln('remove_dir "$PKGNAME" doc/manual/nmag/example_nmagprobe')
    if has_test:
        script.writeln('remove_dir "$PKGNAME" nsim/tests/regression/nmag/nmagprobe')

if want_tarb.chosen in [0, 2]:
    script.writeln('gen_tarball "$PKGNAME"')

if want_tarb.chosen == 0:
    script.writeln('remove_directory "$PKGNAME"')

#=============================================================================
# Save the script, execute it and show final messages
fn = script.save("distmake.bash")

if final.chosen in [0, 2]:
    os.system("/bin/bash %s" % fn)

if final.chosen == 0:
    try:
        os.remove(fn)
    except:
        print "Cannot remove '%s'" % fn

else:
    print "="*40
    print """I CREATED THE FILE '%s'"
    This is a bash file which generates the distribution file(s) according to your specifications.
    Use it as follows:

    /bin/bash %s
    """ % (fn, fn)
