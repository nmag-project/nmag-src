import sys

# Trickery to assist in the transition from the magic ocaml module to
# a sane (from Python perspective) situation where the required
# functionality is imported as a module. The new version of the magic
# ocaml module is called 'nsimcore' (for now). We try to use it if it
# is available and use it to replace the magic ocaml module, otherwise
# we fall back on the magic ocaml module. For now, we copy any stuff
# that is still missing in nsimcore from the magic module, but we aim
# to put it all into nsimcore itself, so that the magic module may be
# removed completely.

def find_magic_ocaml_module():
    try:
        magic_ocaml = sys.modules['ocaml']
    except KeyError:
        magic_ocaml = None
    return magic_ocaml

def find_nsimcore_module():
    try:
        import nsimcore
    except ImportError:
        nsimcore = None
    return nsimcore

def replace_magic_ocaml_with_nsimcore():
    print "\n" * 10
    print "Replacing magic with nsimcore"
    dir_nsimcore = dir(nsimcore)
    for name in dir(magic_ocaml):
        if name not in dir_nsimcore and not name.startswith('__'):
            print "Copying %s from magic ocaml module to nsimcore." % (name,)
            setattr(nsimcore, name, getattr(magic_ocaml, name))
    sys.modules['ocaml'] = nsimcore
    return nsimcore


# Need to import nsimcore first, as this will trigger make pycaml
# create the magic ocaml module.

nsimcore = None
if 'ocaml' not in sys.modules: # If it's there, then we ran a magic Python interpreter
    nsimcore = find_nsimcore_module()

magic_ocaml = find_magic_ocaml_module()
if nsimcore and magic_ocaml:
    ocaml = replace_magic_ocaml_with_nsimcore()
# When we have finished, we hope to replace ALL of the above code with
# a simple 'import nsimcore'

# End of magic-ocaml -> nsimcore transition trickery

def err_msg(our_msg, original_exception):
    return "%s\n\nOriginal python error is '%s'" % (our_msg, original_exception)

def err_msg_import(missing_module_name, original_exception):
    our_msg = "We can't import the %s module. This means it needs " \
              "to be installed or you are using the wrong " \
              "PYTHONPATH/interpreter. \n\nYour python path is: " \
              "\n%s." % (missing_module_name, sys.path)

    if missing_module_name in special_messages:
        our_msg = special_messages[missing_module_name]

    if missing_module_name in module_web_locations:
        web_location = module_web_locations[missing_module_name]
        our_msg += "\n\n%s's home page is %s." % (missing_module_name, web_location)

    return err_msg(our_msg, original_exception)


special_messages = \
{'ocaml': "We can't import the ocaml package. This usually means that " \
          "you are running this code in a normal python interpreter, " \
          "not within the nsim executable."}

module_web_locations = \
{'numpy'   :'http://numpy.scipy.org/'}

def no_module_named_p(exception):
    message = str(exception)
    flag = 'No module named '
    if message.startswith(flag):
        return message[len(flag):]
    else:
        return None
try:
    import ocaml
except:
    import nsim.ocamlcompat
    sys.modules['ocaml'] = nsim.ocamlcompat
    ocaml = nsim.ocamlcompat

if True: #try:
    import nmeshlib as nmesh
    import nfem
    import nsim
    from inference import inference as infer
    import numpy
    #from IPython.Shell import IPShellEmbed

#except ImportError, exception:
    # Extract the name of the module whose import failed, from the
    # exception. Also check that the exception message matches our
    # expectations. Not all ImportErrors mean that the module is
    # missing, for example "from inference import not_there".
    #    missing_module_name = no_module_named_p(exception)
    #    if missing_module_name:
    #        raise ImportError, err_msg_import(missing_module_name, exception)
    #    else:
    #        raise exception

