import math
import re
import os
import os.path
import types

from nsim.setup import get_exec_path

nsim_raw = get_exec_path("nsim-raw")
netgen_bin = "netgen"
nmeshpp_bin = "%s %s" % (nsim_raw, get_exec_path("nmeshpp"))
nmeshimport_bin = "%s %s" % (nsim_raw, get_exec_path("nmeshimport"))

def netgen_mesh_from_file(geo_filename, mesh_filename, mesh_type=None,
                          keep_neu=False, keep_logs=False):
    """Run Netgen on the given GEO file (first arg) creating a mesh
    with the desired name (second arg). The function examines the extension
    of the mesh file name to understand what is the desired output.
    If the extension is 'neu', then a Neutral file is produced.
    If the extension is 'h5' or 'nmesh', then the neutral file created
    by Netgen is converted to the given format and it is removed.
    If 'keep_neu' is true, the intermediate neutral file is kept.
    If 'keep_logs' is true, the Netgen logs are kept, otherwise they are
    removed. If 'mesh_type' is provided, it specifies the type of the
    output format (and the extension of the mesh file is ingored)."""
    if mesh_type == None:
        mesh_type = os.path.splitext(mesh_filename)[1][1:]
        if mesh_type not in ["neu", "nmesh", "h5", "nmesh.h5"]:
            raise ValueError("Cannot determine mesh type from extension. "
                             "Please specify the mesh type explicitly using "
                             "the optional argument \"mesh_type\".")

    if mesh_type == "neu":
        tmp_filename = mesh_filename

    else:
        tmp_filename = os.path.splitext(mesh_filename)[0] + ".neu"

    os.system('%s -geofile=%s -moderate -meshfiletype="Neutral Format" '
              '-meshfile=%s -batchmode' % (netgen_bin, geo_filename,
                                           tmp_filename))

    if not keep_logs:
        for log_filename in ["ng.ini", "test.out"]:
            try:
                os.remove(log_filename)
            except:
                pass

    if mesh_type == "neu":
        return

    exts = ["nmesh", "h5", "nmesh.h5"]
    if mesh_type in exts:
        if not mesh_filename.endswith("." + mesh_type):
            raise ValueError("The file name must have the extension '.%s' "
                             "for files of type '%s'."
                             % (mesh_type, mesh_type))
        os.system("%s --netgen %s %s" % (nmeshimport_bin, tmp_filename,
                                         mesh_filename))
        if not keep_neu:
            os.remove(tmp_filename)

def netgen_mesh_from_string(s, mesh_filename, mesh_type=None,
                            geo_filename=None,
                            keep_geo=False, keep_neu=False):
    """Similar to netgen_mesh_from_file, but accepts the GEO file as a string
    (first argument), writes the GEO file to disk, generate the mesh and
    finally delete the intermediate files. Example:

    from nsim.netgen import netgen_mesh_from_string
    netgen_mesh_from_string("algebraic3d\\n"
                            "solid main = sphere(0, 0, 0; 10) -maxh=3.0;"
                            "tlo main;", "a.nmesh.h5")
    """
    if geo_filename == None:
        geo_filename = os.path.splitext(mesh_filename)[0] + ".geo"

    f = open(geo_filename, "w")
    f.write(s)
    f.close()

    netgen_mesh_from_file(geo_filename, mesh_filename,
                          mesh_type=mesh_type,
                          keep_neu=keep_neu)
    if not keep_geo:
        os.remove(geo_filename)

def file_extension(file_name):
    """Returns the extension in the given file name."""
    return file_name.split(os.path.extsep)[-1]

def replace_extension(file_name, new_extension):
    """Replace the extension in a file name."""
    pieces = file_name.split(os.path.extsep)[0:-1]
    pieces.append(new_extension)
    return os.path.extsep.join(pieces)

def netgen_caller(src_file, dest_file):
    os.system('netgen -geofile=%s -moderate '
              '-meshfiletype="Neutral Format" '
              '-meshfile=%s -batchmode' % (src_file, dest_file))

def neu_to_nmesh(neu_file_name, nmesh_file_name):
    '''Calls nmeshimport to convert a mesh from "neutral" file format,
       to nmesh file format.
    '''
    os.system('%s --netgen %s %s'
              % (nmeshimport_bin, neu_file_name, nmesh_file_name))

class NetgenMesh:
    '''This class simplifies the creation of meshes using Netgen.
       The user can give the netgen geo file, using some variables
       inside it. Variables are expressed in the form $var_name$
       and are substituted with the values specified from python
       before calling netgen.
       Netgen is called automatically. An example of usage is:

         nm = NetgenMesh()
         nm.set_vars({'l': 20.0, 'maxh':2.0})
         nm.geo("""
           algebraic3d
           solid prism = orthobrick (0.0, 0.0, 0.0; $l$, $l$, $l$)
                         -maxh = $maxh$;
           tlo prism;
           """)
         nm.save('mesh_esdevice.neu')
    '''
    def __init__(self,
                 file_name=None,
                 geo="",
                 variables={},
                 netgen_caller=netgen_caller):
        self._length_unit = 1.0
        self.netgen_caller = netgen_caller
        self._var_re = re.compile("[$][^$]*[$]")
        self._file_name = file_name
        self._vars_dict = variables
        self._geo_content = self._replace(geo) + "\n"

    def _replace(self, src):
        def substitutor(var):
            try:
                var_name = var.group(0)[1:-1]
            except:
                raise ValueError("NetgenMesh: Error when substituting "
                                 "variable.")
            if self._vars_dict.has_key(var_name):
                return str(self._vars_dict[var_name])
            raise ValueError("NetgenMesh: Variable '%s' not found!"
                             % var_name)

        return re.sub(self._var_re, substitutor, src)

    def geo(self, geo_content):
        self._geo_content += self._replace(geo_content) + "\n"

    def set_vars(self, dict, unit=None):
        if type(dict) == types.DictType:
            for key in dict: self.set_var(key, dict[key], unit=unit)
        else:
            for key, value in dict: self.set_var(key, value, unit=unit)

    def set_var(self, name, value, unit=None):
        try:
            if unit != None:
                v = float(value/unit)
            else:
                v = value
        except:
            v = value
        self._vars_dict[name] = v

    def save(self, file_name, force_update=False, delete_neu=True):
        def save_error(msg):
            raise ValueError("Error in method `save` of class `NetgenMesh`: "
                             "%s." % msg)

        if not ("." in file_name):
            save_error("file name (%s) does not have an extension"
                       % file_name)

        ext = file_extension(file_name).lower()
        if not (ext in ["neu", "h5", "nmesh"]):
            save_error("'%s' extension not recognized" % ext)

        geo_file_name = replace_extension(file_name, "geo")
        geo_file_content = self._replace(self._geo_content)
        geo_file_exists = os.path.isfile(geo_file_name)
        want_neu = (ext == "neu")
        if want_neu:
            neu_file_name = file_name
        else:
            neu_file_name = replace_extension(file_name, "neu")

        file_exists = os.path.isfile(file_name)
        if file_exists and not force_update:
            if geo_file_exists:
                f = open(geo_file_name, 'rt')
                geo_need_update = (f.read() != geo_file_content)
                f.close()
                # The output file exists and also a geo file exists
                # whose content is the same of what we would write anyway:
                # we assume in this case that the output already contains
                # a mesh coherent to the geo specification and exit without
                # invoking netgen!
                if not geo_need_update and file_exists: return

            else:
                save_error("destination file already exists, "
                           "but geo file doesn't! Use force_update=True "
                           "to force the file to be overwritten")

        # Write geo file
        f = open(geo_file_name, 'w')
        f.write(self._replace(self._geo_content))
        f.close()

        # Invoke netgen
        self.netgen_caller(geo_file_name, neu_file_name)

        if want_neu: return

        # Now we have to convert the mesh from Neutral file format to nmesh(.h5)
        neu_to_nmesh(neu_file_name, file_name)
        if delete_neu: os.remove(neu_file_name)


if __name__ == "__main__":
    L =  5.0 # Length of the hard layers
    l = 40.0 # Length of the soft layer
    r = 5.0 # Radius of the nanopillar

    nm = NetgenMesh()
    nm.set_vars({'hL': 0.5*l + L, 'hl': 0.5*l, 'r': r, 'maxh':2.0})
    nm.geo("""
    algebraic3d

    solid softlayer     =  cylinder (-$hl$, 0, 0; $hl$, 0, 0; $r$)
                          and plane (-$hl$, 0, 0;   -1, 0, 0)
                          and plane ( $hl$, 0, 0;    1, 0, 0)
                          -maxh=$maxh$;

    solid hardlayerleft =  cylinder (-$hL$, 0, 0; $hl$, 0, 0; $r$)
                          and plane (-$hL$, 0, 0; -1, 0, 0)
                          and plane (-$hl$, 0, 0;  1, 0, 0)
                          -maxh=$maxh$;

    solid hardlayeright =  cylinder ( $hl$, 0, 0; $hL$, 0, 0; $r$)
                          and plane ( $hL$, 0, 0;  1, 0, 0)
                          and plane ( $hl$, 0, 0; -1, 0, 0)
                          -maxh=$maxh$;

    tlo hardlayerleft;
    tlo softlayer;
    tlo hardlayeright;
    """)
    nm.save('mesh_esdevice.neu')

