import os
import sys
import time
from commands import getoutput

from choice import ChoiceList, Choice, OpenQuestion, Alternative, Script

# Retrieve version information
sys.path.insert(0, os.path.join("..", "interface", "nsim"))
import version
sys.path.pop(0)


transition_repos = "./transition"
if os.path.exists(transition_repos):
    print("ERROR: directory '%s' already exists. This typically means that a "
          "release was attempted but not completed. Please, complete the "
          "release or remove the directory manually and retry."
          % transition_repos)
    sys.exit(1)

class Version(object):
    def __init__(self, major=None, minor=None, patch=None):
        self.major = major
        self.minor = minor
        self.patch = patch

    def __str__(self):
        return "%d.%d.%d" % (self.major, self.minor, self.patch)

    def from_str(self, s):
        try:
            self.major, self.minor, self.patch = map(int, s.split("."))
            return self
        except:
            raise ValueError("Invalid string version in method "
                             "Version.from_str")

    def next(self, kind="patch"):
        kind = kind.lower().strip()
        if kind == "patch":
            return Version(self.major, self.minor, self.patch + 1)
        elif kind == "minor":
            return Version(self.major, self.minor + 1, 0)
        elif kind == "major":
            return Version(self.major + 1, 0, 0)
        else:
            raise ValueError("Unknown kind in method Version.next")

current = version.version
v = Version(current[0], current[1], current[2])

#=============================================================================
plan = ChoiceList("I'm now going to ask a few questions in order to collect "
                  "enough information for the release. You can quit any time "
                  "with CTRL+C. I will give you an opportunity to review "
                  "the operation after I collected all the information.")

remind1 = plan.add_choice(Choice("Have you updated the changelog?"))
remind1.add_alternative(Alternative("Yes"))
remind1.add_alternative(Alternative("No"))

rel_kind = plan.add_choice(Choice("What kind of release do you want to make?"))
rel_kind.add_alternative(Alternative("Patch:  %s --> %s (just bugfixes)"
                                     % (v, v.next("patch"))))
rel_kind.add_alternative(Alternative("Minor:  %s --> %s (improvements without "
                                     "radical interface changes)"
                                     % (v, v.next("minor"))))
rel_kind.add_alternative(Alternative("Major:  %s --> %s (improvements with "
                                     "radical interface changes)"
                                     % (v, v.next("major"))))
rel_kind.add_alternative(Alternative("Custom: %s --> X.Y.Z (want to specify "
                                     "version manually)" % v))

what_ver = plan.add_choice(OpenQuestion("What version do you want to use?"))
what_ver.when_should_ask(lambda: rel_kind.chosen == 3)

def problem_in_answer(ans):
    try:
        Version().from_str(ans)
        return None
    except:
        return ("Bad version. Should be something like X.Y.Z, "
                "where X, Y and Z are three integers")
what_ver.answer_checker(problem_in_answer)

#=============================================================================
# Ask the user to take decision and take note of the answers
script = Script()
plan.ask_user(script)

def confirm(s="", ask_anyway=False):
    if ask_anyway:
        print s
        print "PRESS [RETURN] TO CONTINUE...",
        raw_input()

if rel_kind.chosen == 3:
    new_version = Version().from_str(what_ver.answer)

else:
    new_version = v.next(["patch", "minor", "major"][rel_kind.chosen])

pkgname = "nsim-%s" % new_version
maintag = str(new_version)

#=============================================================================
print "\n"
print "-"*50
print """
I collected enough information. I can now proceed building the release.

SUMMARY:
Old version: %s
New version: %s""" % (v, new_version)

confirm(ask_anyway=True)

msg = \
"""
HOW VERSIONING WORKS:
- the script uses the repository to which it belongs (which we call below
  the "starting repository") and creates a new clone repository, which we call
  below "transition repository"
- the transition repository is then marked with the new version of Nmag and is
  tagged
- the transition repository is then used to generate a tarball which can be
  tested until you are satisfied
- if you are not satisfied, then you can remove it together with transition
  repository, thus removing all traces of your versioning intent...
- if you are satisfied then you go on and update the Nmag webpages so that the
  tarball file is online and anyone can download it
- in that case you should use the script again to confirm that the new version
  was released. The script pushes the transition repository into the starting
  repository. It then updates the version of the starting repository to -dev
  and commits.
- The scripts finally asks you whether it should remove the transition
  repository (which is the one which should be used to generate the tarball
  for the officially released version of Nmag).
"""
confirm(msg)

print("I will now create a clone of the repository in the current "
      "directory. I will call it %s" % transition_repos)
confirm()
print getoutput("hg clone .. %s" % transition_repos)


# We take the old version information and overwrite the version number
# and release date
info_filename = os.path.join(".", "transition", "interface", "nsim", "info.py")
print "Updating version information in %s" % info_filename

version.all_infos["version"] = \
  (new_version.major, new_version.minor, new_version.patch)
version.all_infos["release_date"] = time.asctime()

f = open(info_filename, "w")
f.write(version.generate_info())
f.close()

print("\nCommitting version change...")
confirm()
print getoutput("cd %s && hg commit -m \"Committed version %s\""
                % (transition_repos, new_version))

print("\nTagging version...")
print getoutput('cd %s && hg tag "%s"'
                % (transition_repos, maintag))

print("\nCreating tarball...")
confirm()
#print getoutput("cd transition && cd admin && bash rbld.bash")
print getoutput('cd %s && cd admin &&' 
                'NMAGNAME="%s" bash _relbld.bash'
                % (transition_repos, pkgname))

print("\nMoving tarball to ./")
print getoutput("mv %s/admin/nmag-*.tar.gz ." % transition_repos)

# Getting back to dev mode
info_filename = os.path.join(".", "transition", "interface", "nsim", "info.py")
print "Updating version information in %s" % info_filename

version.all_infos["version"] = \
  (new_version.major, new_version.minor, new_version.patch)
version.all_infos["release_date"] = version.default_infos["release_date"]

f = open(info_filename, "w")
f.write(version.generate_info())
f.close()

print("\nCommitting...")
confirm()
print getoutput("cd %s && hg commit -m \"Back to %s-dev\""
                % (transition_repos, new_version))

new_release_script = sys.argv[0]
confirm_script = "confirm-release.py"

repls = [("$NEW-RELEASE$", new_release_script),
         ("$TRANSITION$", transition_repos),
         ("$CONFIRM-SCRIPT$", confirm_script),
         ("$TAG$", maintag)]

def subst(msg):
  for inp, out in repls:
    msg = msg.replace(inp, out)
  return msg

print "Creating confirmation script..."
with open(confirm_script + ".in", "r") as f:
  content = f.read()
with open(confirm_script, "w") as f:
  f.write(subst(content))

msg = \
  ("The tarball was created together with a confirmation script. You should "
   "now test the tarball and make sure it works. If you find out that the "
   "tarball needs further modifications, then you can just remove the "
   "directory $TRANSITION$ and try again. If you are happy with the tarball "
   "you can confirm the release by launching the $CONFIRM-SCRIPT$ script as "
   "follows:\n\n  python $CONFIRM-SCRIPT$\n\nJust follow the instructions.")

print subst(msg)
sys.exit(0)



help_msg = \
"""This script will help you to release a new version of Nmag.

TYPICAL USAGE:
  The script creates the transition repository in a directory named
  "transition" under the current working directory. The transition repository
  will correspond to the new version. A tarball is created inside the same
  directory. You should take the tarball and test it. Once you have published
  the tarball online you can use:

    python new-version.py confirm

  This will pull the changes from the transition repository into the starting
  repository, it will remove the transition repository and push the changes
  into the central Nmag server.

For more info try "python new-version.py help"

"""
