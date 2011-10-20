import os
import re

module_re = re.compile(r"[A-Z][a-z_]+[.]")
open_module_re = re.compile(r"open[ \t]+[A-Z][a-z_]+")

def find(text, string, start):
  n = len(text)
  i = text.find(string, start)
  return i if i >= 0 else n

def discard_comments(text, start=0):
  OPEN_NEXT, CLOSE_NEXT, EOF_NEXT = range(3)
  OUTSIDE_COMMENT, INSIDE_COMMENT = range(2)

  out = ""
  cur_pos = 0
  state = OUTSIDE_COMMENT
  comment_level = 0

  while True:
    copen = find(text, "(*", start)
    cclose = find(text, "*)", start)
    if copen < cclose:
      inp = OPEN_NEXT
    elif copen > cclose:
      inp = CLOSE_NEXT
    else:
      inp = EOF_NEXT

    if state == OUTSIDE_COMMENT:
      if inp == OPEN_NEXT:
        out += text[cur_pos:copen]
        start = copen + 2
        state = INSIDE_COMMENT

      elif inp == EOF_NEXT:
        out += text[cur_pos:]
        return out

      else:
        print "Warning: found closing-comment-mark '*)' outside a comment."
        start = cclose + 2

    else:
      assert state == INSIDE_COMMENT
      if inp == OPEN_NEXT:
        start = copen + 2
        comment_level += 1

      elif inp == CLOSE_NEXT:
        start = cclose + 2
        if comment_level == 0:
          cur_pos = start
          state = OUTSIDE_COMMENT

        else:
          comment_level -= 1

      else:
        print "Warning: EOF inside a comment."
        return out

def find_modules(text):
  text = discard_comments(text)
  modules1 = map(lambda s: s[:-1], module_re.findall(text))
  modules2 = map(lambda s: s.split()[1].strip(),
                 open_module_re.findall(text))

  return list(set(modules1 + modules2))

def module_filename(module):
  return module.lower() + ".ml"

def modules_sys_split(modules):
  my_modules = []
  sys_modules = []
  for module in modules:
    module_filename = module.lower() + ".ml"
    if os.path.exists(module_filename):
      my_modules.append(module)
    else:
      sys_modules.append(module)

  return (my_modules, sys_modules)

class Module(object):
  def __init__(self, name, filename=None, order=None):
    self.name = name
    self.filename = filename or module_filename(name)
    self.deps = {}
    self.order = order

  def get_filename(self, ext):
    return self.name.lower() + "." + ext

  def get_cmx(self):
    return self.get_filename("cmx")

  def add_dep(self, module):
    assert isinstance(module, Module)
    return self.deps.setdefault(module.name, module)

  def write(self, out):
    my_deps = [self.get_filename("ml")]
    if os.path.exists(self.get_filename("mli")):
      my_deps.append(self.get_filename("cmi"))
    deps = [module.get_cmx()
            for module in self.deps.itervalues()]
    out.write("%s: %s\n" % (self.get_cmx(), " ".join(my_deps + deps)))

class DependencyBuilder(object):
  def __init__(self):
    self.filenames = set()
    self.visited = set()
    self.modules = {}

  def add_module(self, modulename, *kwargs):
    if modulename not in self.modules:
      order = len(self.modules)
      self.modules[modulename] = Module(modulename, order=order, *kwargs)
    return self.modules[modulename]

  def visit(self, modulename, filename=None):
    if filename == None:
      filename = module_filename(modulename)

    if filename not in self.visited:
      print "Reading '%s'..." % filename
      self.visited.add(filename)
      this_module = self.add_module(modulename)
      with open(filename, "r") as f:
        modulenames = find_modules(f.read())

      mymodulenames, _ = modules_sys_split(modulenames)
      for modulename in mymodulenames:
        module = self.add_module(modulename)
        this_module.add_dep(module)

      for modulename in mymodulenames:
        self.visit(modulename)

  def write(self, out):
    def mycmp(module_a, module_b):
      return cmp(len(module_a.deps), len(module_b.deps))

      a = module_a.order
      b = module_b.order
      if a == b:
        return 0
      elif a == None:
        return 1
      elif b == None:
        return -1
      else:
        return cmp(b, a)

    modules = list(self.modules.itervalues())
    modules.sort(mycmp)

    for module in modules:
      module.write(out)

import sys
db = DependencyBuilder()
db.visit("Nsimexec", "nsimexec.ml")
db.write(sys.stdout)
