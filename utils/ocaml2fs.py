import re, sys, commands

text_editor = "xterm -e vim"

tmp_file = "/tmp/ocaml2fs_removeme.ml"

red   = '\033[00;31m'
green = '\033[00;32m'
norm  = '\033[0m'

def redprint(s): print red + s + norm,
def greenprint(s): print green + s + norm,

def printsep():
  greenprint("########################################################\n")

def delim_str(tag, fnname=None):
  if fnname == None:
    return "(*ocaml2fs*%s*)" % tag
  else:
    return "(*ocaml2fs*%s*%s*)" % (tag, fnname)

class Replace:
  def __init__(self, source):
    self.src = source
    self.replacements = []
    self.cmp_fn = lambda a, b: a[0].__cmp__(b[0])

  def replace(self, begin, end, replacement, do_optimize=True):
    if not do_optimize:
      self.replacements.append((begin, end, replacement))
      return

    if replacement == self.src[begin:end]: return
    # We now optimise the replacement process, by finding the couple
    # (begin1, end1) which produces the same result as (begin, end)
    # and where begin1 is as big as possible, while end1 is as small
    # as possible

    #print "REPLACING: '%s' with '%s'" % (self.src[begin:end], replacement)

    i = 0
    while replacement[i] == self.src[begin + i]: i += 1
    begin1 = begin + i
    r = replacement[i:]
    n = len(r)
    for i in range(n - 1, -1, -1):
      if r[i] != self.src[end + i - n]:
        i += 1
        break
    r = r[:i]
    end1 = end + i - n
    #print "REPLACING: '%s' with '%s'" % (self.src[begin1:end1], r)
    self.replacements.append((begin1, end1, r))

  def do(self):
    self.replacements.sort(self.cmp_fn)
    offset = 0

    i = 0
    new_src = ""
    for r in self.replacements:
      begin, end, replacement = r
      if i < begin and begin < end:
        new_src += self.src[i:begin] + replacement
        i = end
      else:
        redprint("WARNING: Skipping overlapping replacement\n")

    new_src += self.src[i:]
    return new_src

def mfind(src, subs, start=0, end=None, reverse=False):
  if end == None: end = len(src)
  if reverse:
    find_method = src.rfind
  else:
    find_method = src.find
  next = -1
  found = None
  idx = -1
  for sub in subs:
    idx += 1
    i = find_method(sub, start, end)
    if i < 0: continue
    this_is_best = False
    if next < 0:
      this_is_best = True
    elif i == next:
      this_is_best = (len(sub) > len(subs[found]))
    elif i > next:
      this_is_best = reverse
    else: # i < next
      this_is_best = not reverse
    if this_is_best:
      next = i
      found = idx
  return (next, found)

# Return a list of the OCaml comments found inside src
# the list contain, for each comment, a pair [begin, end]
def detect_comments(src):
  comments = []
  i = 0
  while True:
    begin = src.find("(*", i)
    i = begin + 1
    if begin < 0: return comments
    count = 1
    while count > 0:
      i = src.find("*", i+1)
      if i < 0:
        raise "File ended between a comment."
        return comments
      if src[i-1] == "(":
        count += 1
      elif i+1 < len(src) and src[i+1] == ")":
        count -= 1
    comments.append([begin, i+1])

def detect_endlines(src):
  ls = src.splitlines(True)
  i = 0
  ends = []
  for l in ls:
    i += len(l)
    ends.append(i)
  return ends

def line_from_pos(src, endlines, pos):
  l = 0
  for el in endlines:
    if pos < el:
      return l
    l += 1
  return l

# Return True if i is a position inside a comment, where
# comments is obtained using detect_comments
def inside_comment(comments, i):
  for comment in comments:
    begin, end = comment
    if i >= begin and i <= end: return True
  return False

def go_through_parens(s, i, paren_left="(", paren_right=")"):
  if s[i] != paren_left:
    raise "go_through_parens expects a left paren as first character"
  count = 1
  while count > 0:
    i += 1
    c = s[i]
    if c == paren_left:
      count += 1
    elif c == paren_right:
      count -= 1
  return i

def go_through_args(s, i):
  begin = i
  while True:
    c = s[i]
    i += 1
    if c.isalnum() or c.isspace() or c == "_":
      pass
    elif c == "(":
      i = go_through_parens(s, i-1)+1
    elif c == "-" and s[i] == ">":
      return True, (i + 1)
    elif c == "=":
      return False, i

def go_through_optarg(s, pos):
  i = pos+1
  if s[i] == "(":
    return go_through_parens(s, i)

  else:
    while s[i].isalnum() or s[i] == "_": i += 1
    return i

def get_fn_name(s, i, begin_str="let"):
  i0 = s.rfind(begin_str, 0, i)
  i = i0 + 3
  begin = i + 1
  if i < 0: return None
  while True:
    i += 1
    c = s[i]
    if not (c.isalnum() or c == "_"):
      name = (begin, i)
      name_str = s[begin:i]
      if name_str != "rec":
        return (i0, name)
      else:
        while s[i].isspace(): i += 1
        begin = i

def skip(s, i, skip_this_char):
  while skip_this_char(s[i]): i += 1
  return i

def skip_spaces(s, i):
  return skip(s, i, skip_this_char=lambda c: c.isspace())

def skip_word(s, i):
  return skip(s, i, skip_this_char=lambda c: c.isalnum() or c == "_")

def skip_parens(s, i, paren_left="(", paren_right=")"):
  if s[i] != paren_left:
    raise "go_through_parens expects a left paren as first character"
  count = 1
  while count > 0:
    i += 1
    c = s[i]
    if c == paren_left:
      count += 1
    elif c == paren_right:
      count -= 1
  return i + 1

def skip_named_arg(s, i):
  """Skip something like 'arg:name' or arg:(...)"""
  i = skip_word(s, i)
  if s[i] != ":":
    return (i, i)
  i = skip_spaces(s, i+1)
  if s[i] == "(":
    return (i, skip_parens(s, i))
  elif s[i] == '"':
    return (i, 1 + skip(s, i + 1, lambda c: c != '"'))
  else:
    return (i, skip_word(s, i))

def parse_arg(s, i):
  c = s[i]
  if c == "?":
    if s[i+1] == "(":
      i = skip_parens(s, i+1)
    else:
      i = skip_word(s, i+1)
  elif c == "~":
    i = skip_word(s, i+1)
  else:
    i = skip_word(s, i)
  return i

def parse_next_val(s, comments, i, val_str="val"):
  while True:
    while True:
      ni = s.find(val_str, i)
      if ni == -1: return None
      if not inside_comment(comments, ni): break
      i = ni + 1

    i = skip_spaces(s, ni + len(val_str))
    ni = skip_word(s, i)
    fn_name = s[i:ni]
    i = skip_spaces(s, ni)
    if s[i] == ":":
      i = skip_spaces(s, i + 1)
      break

  ni = parse_arg(s, i)
  fn_arg = s[i:ni+5]
  print "Found function '%s'" % fn_name
  print "  with arg '%s'" % fn_arg
  return fn_name

# Find all the function to which each optional argument belongs
class OCamlFunction:
  def __init__(self, src=None, name=None, name_str=None,
               head=None, head_str=None, new_head_str=None):
    self.src = src
    self.name = name
    self.name_str = name_str # can be inconsistent with self.name
                             # (this happens for anonymous functions)
    self.head = head
    if head_str != None:
      self.head_str = head_str
    elif head != None:
      self.head_str = src[head[0]:head[1]]
    self.new_head_str = new_head_str
    self.args = []

  def add_optarg(self, name_pos, name_str):
    self.args.append((name_pos, name_str))

# Represent a call to a function
class OCamlCall:
  def __init__(self, location, fn, fn_name):
    self.location = location
    self.name = fn_name
    self.fn = fn
    self.args_to_adj = []

  def add_arg(self, arg_to_adj, arg_prototype):
    self.args_to_adj.append((arg_to_adj, arg_prototype))

  def approx_max_location(self):
    max_location = self.location
    for (arg, arg_str), _ in self.args_to_adj:
      if arg[1] > max_location:
        max_location = arg[1]
    return max_location

class OCamlArg:
  def __init__(self, fn, name_str, local=True):
    self.name_str = name_str
    self.fn = fn
    self.local = local
    self.arg_type = None

  def get_type(self):
    if self.arg_type != None:
      return self.arg_type
    head_str = self.fn.head_str
    i = head_str.find(self.name_str)
    if head_str[i-1] == "~":
      self.arg_type = "~"
    else:
      self.arg_type = "?"
    return self.arg_type

class Parser:
  def __init__(self, src, pos=0):
    self.comments = comments = detect_comments(src)
    self.endlines = detect_endlines(src)
    self.src = src
    self.functions = functions = {}
    self.function_list = []
    self.few_before = 2
    self.few_after = 2
    self.call_args_to_adj = []
    self.log = ""
    self.extrn_functions = {}
    self.dest = None

    # Find all optional argument in the form ?(...) or ?literal
    start = pos
    while True:
      where = src.find("?", start)
      if where == -1: break

      if inside_comment(comments, where):
        start = where + 1
        continue

      end = go_through_optarg(src, where)
      optarg = (where, end) # Found optarg

      is_decl, fnpos = go_through_args(src, where) # Now find function head
      i0, fnname = get_fn_name(src, where)
      fnhead = (i0, fnpos) # Entire function head

      fnname_str = src[fnname[0]:fnname[1]] # function name (literal)
      if not functions.has_key(fnname_str):
        functions[fnname_str] = fn = \
          OCamlFunction(src, fnname, fnname_str, fnhead)
        self.function_list.append(fn)

      fn = functions[fnname_str]
      if fnhead != fn.head: raise "Inconsistency in function head"

      fn.add_optarg(optarg, src[optarg[0]:optarg[1]])

      start = end + 1
      #print "Found at %d -> '%s'" % (where, src[where:start])

    # Find all named arguments in the form ~(...) or ~literal
    start = pos
    while True:
      where = src.find("~", start)
      if where == -1: break

      if inside_comment(comments, where):
        start = where + 1
        continue

      end = go_through_optarg(src, where)
      arg = (where, end)
      arg_str = src[arg[0]:arg[1]]

      # The found occurrence of ~ may belong to a function declaration, but
      # also to a function call, we must understand which is the case here!
      # We proceed as follows: if the ~something belongs to a declaration,
      # then it should appear in the following form:
      #   let func_name arg1 arg2 ... ~argI ... =
      # or
      #   fun arg1 arg2 ... argI ... ->

      i = where
      while True:
        i, found = mfind(src, ["let", "fun"], end=i, reverse=True)
        if found == None or (not inside_comment(self.comments, i)):
          break

      fnname = None
      is_decl = False
      if i == -1:
        # No let/fun found: then this should not be a declaration!
        pass

      elif found == 0: # found "let"
        word_begin = skip_spaces(src, i + 3)
        word_end = skip_word(src, word_begin)
        word_str = src[word_begin:word_end]
        if word_str == "rec":
          word_begin = skip_spaces(src, word_end)
          word_end = skip_word(src, word_begin)
          word_str = src[word_begin:word_end]
        word = (word_begin, word_end)
        word_len = word_end - word_begin
        if word_len > 0:
          fn_end = src.find("=", word_end)
          if where < fn_end: # works also for fn_end == -1
            fnhead = (i, fn_end)
            if not functions.has_key(word_str):
              functions[word_str] = fn = \
                OCamlFunction(src, word, word_str, fnhead)
              self.function_list.append(fn)

            fn = functions[word_str]
            if fnhead != fn.head: raise "Inconsistency in function head"
            fn.add_optarg(arg, arg_str)
            is_decl = True

      else: # found "fun"
        fnname = "anon-%d" % i
        fn_end = src.find("->", i + 3)
        if where < fn_end: # works also for fn_end == -1
          fn_end += 1
          fnhead = (i, fn_end)
          if not functions.has_key(fnname):
            functions[fnname] = fn = \
              OCamlFunction(src, (i, i), fnname, fnhead)
            self.function_list.append(fn)

          print "Anonymous function with named arguments at %s" % i
          fn = functions[fnname]
          fn.add_optarg(arg, arg_str)
          is_decl = True

      if not is_decl:
        self.call_args_to_adj.append((arg, arg_str))

      start = where + 1
      #print "Found at %d -> '%s'" % (where, src[where:start])

  def get_adjusted(self):
    return self.dest.do()

  def let_user_edit(self, begin, end, suggestion=None, help=""):
    # Let the user edit some few lines before and some few lines after
    src = self.src
    begin_line = (line_from_pos(src, self.endlines, begin) - self.few_before)
    end_line = (line_from_pos(src, self.endlines, end) + self.few_after)
    begin_pos = self.endlines[begin_line]
    end_pos = self.endlines[end_line]

    original_text = src[begin_pos:end_pos]
    if suggestion != None:
      suggested_text = src[begin_pos:begin] + suggestion + src[end:end_pos]
    else:
      suggested_text = original_text
    delim = "\n" + delim_str("", "") + "\n"
    editable_text =  help + original_text + delim + suggested_text
    f = open(tmp_file, "w")
    f.write(editable_text)
    f.close()
    commands.getoutput("%s %s" % (text_editor, tmp_file))
    f = open(tmp_file, "r")
    edited_text = f.read()
    f.close()
    # If the user removes the delimiter, then we skip the replacement
    if delim in edited_text:
       _,  user_text = edited_text.split(delim, 2)
       self.dest.replace(begin_pos, end_pos, user_text)

  def replace_optarg(self):
    src = self.src
    self.dest = dest = Replace(src)
    default_ans = None
    for fn in self.function_list:
      offset = fn.head[0] # To express all positions relative
                          # to the substring 'original'
      original = src[fn.head[0]:fn.head[1]]
      r = Replace(original)

      addition = ""
      for arg in fn.args:
        name_pos = (arg[0][0] - offset, arg[0][1] - offset)
        name_str = arg[1]
        if name_str[0:2] == "?(":
          inside = original[name_pos[0]+2:name_pos[1]]
          word_begin = i = skip_spaces(inside, 0)
          word_end = i = skip_word(inside, i)
          i = skip_spaces(inside, i)
          if inside[i] == "=":
            i = skip_spaces(inside, i+1)
            name = inside[word_begin:word_end]
            r.replace(name_pos[0], name_pos[1]+1, "opt_%s" % name)
            default_val = inside[i:]
            addition += ("\n  let %s = optional_arg opt_%s %s in"
                         % (name, name, default_val))
          elif inside[i] == ":":
            r.replace(name_pos[0], name_pos[1]+1, "(%s)" % inside)
        elif name_str[0] == "?":
          r.replace(name_pos[0], name_pos[1], name_str[1:])
        elif name_str[0] == "~":
          r.replace(name_pos[0], name_pos[1], name_str[1:])

      modified = r.do()
      if len(addition) > 0:
        modified += addition
        i = skip_spaces(src, fn.head[1])
        if src[i] == "\n": # aesthetics
          modified += "\n"

      printsep()
      redprint("Proposed action: replace {{{\n")
      print original
      redprint("}}} with {{{\n")
      print modified
      redprint("}}}\n")
      needs_manual_adj = False
      while True:
        if default_ans == None:
          greenprint("What should I do? [P]roceed, [S]kip "
                     "or [L]et you do it manually? ")
          ans = raw_input().lower()
          if len(ans) == 2 and ans[0] == "a" and ans[1] in "psl":
            default_ans = ans[1]
        else:
          ans = default_ans

        if ans == "p":
          dest.replace(fn.head[0], fn.head[1], modified)
          break
        elif ans == "s":
          modified = original
          break
        elif ans == "l":
          # Let the user edit some few lines before and some few lines after
          self.let_user_edit(fn.head[0], fn.head[1], modified)
          redprint("*** WARNING: you'll have to adjust the log file "
                   "manually ***")
          redprint("Press return to continue...")
          needs_manual_adj = True
          raw_input()
          break

      if modified != original:
        str_org = delim_str("original", fn.name_str)
        str_adj = delim_str("adjusted")
        str_end = delim_str("end")
        note_open = note_end = ""
        if needs_manual_adj:
          note_open = "(**** ocaml2fs: ADJUST THE CODE BELOW! ****)\n"
          note_end = "(**** ocaml2fs: END ****)\n"
        self.log += (str_org + note_open + original + note_end + str_adj
                     + note_open + modified + note_end + str_end)

  def add_extra_log(self, log):
    extra_fns = log.split(delim_str("end"))[:-1]
    for extra_fn in extra_fns:
      original, adjusted = extra_fn.split(delim_str("adjusted"))
      i = original.find(")")
      fnname = original[:i+1].split("*")[-2]
      original = original[i+1:]
      self.extrn_functions[fnname] = \
        OCamlFunction(name_str=fnname, head_str=original,
                      new_head_str=adjusted)

  def find_function_from_arg(self, arg_name):
    possible_fns = []
    for fn_name in self.functions:
      fn = self.functions[fn_name]
      if arg_name[1:] in fn.head_str:
        possible_fns.append(OCamlArg(fn, arg_name[1:], local=True))
    for fn_name in self.extrn_functions:
      fn = self.extrn_functions[fn_name]
      if arg_name[1:] in fn.head_str:
        possible_fns.append(OCamlArg(fn, arg_name[1:], local=False))
    return possible_fns

  def adjust_calls(self, saver=None):
    calls_to_adj = {}
    for arg_to_adj in self.call_args_to_adj:
      arg, arg_str = arg_to_adj
      possible_fns = self.find_function_from_arg(arg_str)
      possible_fn_names = [a.fn.name_str for a in possible_fns]
      i, found = mfind(self.src, possible_fn_names, reverse=True, end=arg[0])
      if found != None:
        candidate = possible_fns[found]
        #print "Argument name '%s' has type '%s'" % (candidate.name_str, candidate.get_type())
        if not calls_to_adj.has_key(i):
          calls_to_adj[i] = OCamlCall(i, candidate.fn, candidate.fn.name_str)
        call_to_adj = calls_to_adj[i]
        call_to_adj.add_arg(arg_to_adj, candidate)
        #print "%s belongs to %s, probably to %s" % (arg_str, possible_fn_names, candidate_fn)
      else:
        redprint("Didn't find '%s' as a member of a known function\n" % arg_str)

    # Now we group calls by function name
    calls_by_fn_name = {}
    for call_pos in calls_to_adj:
      call_to_adj = calls_to_adj[call_pos]
      if not calls_by_fn_name.has_key(call_to_adj.name):
        calls_by_fn_name[call_to_adj.name] = []
      calls_by_fn_name[call_to_adj.name].append(call_to_adj)

    self.calls_to_adj = calls_to_adj
    self.calls_by_fn_name = calls_by_fn_name
    num_calls = len(calls_to_adj)

    printsep()
    greenprint("%d calls to adjust\n" % num_calls)
    greenprint("Now we go through some function calls and adjust them, "
               "and make sure they follow the new prototypes.\n")
    redprint("Press return when you are ready")
    raw_input()

    call_idx = 0
    for fn_name in calls_by_fn_name:
      greenprint("Dealing with function '%s'\n" % fn_name)

      calls = calls_by_fn_name[fn_name]
      for call in calls:
        call_idx += 1
        progress = "%s/%s" % (call_idx, num_calls)
        redprint("%s - Press enter to edit the next call of '%s': "
                 % (progress, fn_name))
        raw_input()

        replacements = []
        for arg_to_adj, arg_prototype in call.args_to_adj:
          (begin, _), arg_str = arg_to_adj
          colon, end = skip_named_arg(self.src, begin+1)
          arg = (begin, end)
          arg_str = self.src[begin:colon-1]
          val_str = self.src[colon:end]
          replacement = "(* %s *) (arg %s)" % (arg_str, val_str)
          replacements.append((begin, end, replacement))

        help = (("\n(*** FUNCTION DECLARATION ***)\n%s\n\n"
                 "(*** OLD CODE ***)\n")
                % call.fn.head_str)
        begin = call.location
        end = max([i for _, i, _ in replacements]) + 2 # + 2 just for safety
        s = Replace(self.src[begin:end])
        for begin1, end1, replacement in replacements:
          s.replace(begin1 - begin, end1 - begin, replacement)
        suggestion = s.do()
        self.let_user_edit(begin, end, suggestion, help=help)
        if saver != None: saver(self)

#-----------------------------------------------------------------------------

if len(sys.argv) < 3:
  print ("USAGE: python ocaml2fs.py original.ml adjusted.fs "
         "[file1.ml.2fs [file2.ml.2fs [...]]]")
  print ("DESCRIPTION: command to help porting OCaml code to F# (removal"
         " of optional arguments)")
  sys.exit(0)

filename = sys.argv[1]
dest_filename = sys.argv[2]

f = open(filename, "r")
src = f.read()
f.close()

parser = Parser(src)
parser.replace_optarg()

if len(sys.argv) > 3:
  for log_filename in sys.argv[3:]:
    f = open(log_filename, "r")
    parser.add_extra_log(f.read())
    f.close()

def save_adjusted(parser):
  f = open(dest_filename, "w")
  f.write(parser.get_adjusted())
  f.close()

parser.adjust_calls(saver=save_adjusted)

f = open(filename+".2fs", "w")
f.write(parser.log)
f.close()
