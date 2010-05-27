import struct
from numpy import array

def name_normalise(name):
    for c in [" ", "\t", "\n"]:
        name = name.replace(c, "")
    return name.lower()

class OVFReadError(Exception):
    pass

class OVFNode(object):
    def __init__(self, subnodes=[], data=None):
        #print "Creating node %s:%s" % (type(self), data)
        self.subnodes = list(subnodes)
        self.data = data

    def _to_str(self, indent=0):
        s = " "*indent + "Node %s: data=%s\n" % (type(self), self.data)
        for subnode in self.subnodes:
            s += subnode._to_str(indent=indent+2)
        return s

    def __str__(self):
        return self._to_str()

    def _get_name(self):
        return self.data[0]

    def _set_name(self, n):
        self.data = (n, self.data[1])

    def _get_value(self):
        return self.data[1]

    def _set_value(self, v):
        self.data = (self.data[0], v)

    name = property(_get_name, _set_name)
    value = property(_get_value, _set_value)

    def read(self, stream, root=None):
        raise NotImplementedError("OVFNode.read not implemented!")

    def write(self, stream, root=None):
        raise NotImplementedError("OVFNode.write not implemented!")

class OVFSectionNode(OVFNode):
    required = None

    def __init__(self, value=[], data=None):
        OVFNode.__init__(self, value, data)

        name, value = data
        self.section_action = lvalue = value.lower()
        assert lvalue in ["begin", "end"], "lvalue is %s" % lvalue
        self.received = {}
 
    def read(self, stream, root=None):
        while True:
            node = read_node(stream)
            if node == None:
                return

            node_name = node.data[0]
            self.subnodes.append(node)

            self.received[node_name] = node
            attr_name = "a_" + name_normalise(node_name)
            setattr(self, attr_name, node)
            assert self != node
 
            if isinstance(node, OVFSectionNode):
                sa = node.section_action
                if sa == "begin":
                    node.read(stream, root=root)
                elif sa == "end":
                    self._end_section(node_name)
                    return
                else:
                    raise OVFReadError("Expected section end, but got '%s'."
                                       % name)

    def _end_section(self, name):
        expected = self.name
        if name != expected:
            raise OVFReadError("Expected end of section %s, but got end "
                               "of section %s." % (expected, name))

        # We check wether we got all we needed
        missing_value = []
        if self.required != None:
            for required_value in self.required:
                if not self.received.has_key(required_value):
                    missing_value.append(required_value)

        if missing_value:
            raise OVFReadError("Missing entries from %s section: %s."
                               % (name, ", ".join(missing_value)))

    def write(self, stream, root=None):
        if self.section_action == "begin":
            stream.write_line("# Begin: %s" % self.name)
        else:
            stream.write_line("# End: %s" % self.name)

        for n in self.subnodes:
            n.write(stream, root=root)

class OVFValueNode(OVFNode):
    def write(self, stream, root=None):
        stream.write_line("# %s: %s" % (self.name, self.value))

class OVFType:
    def __init__(self, s):
        mesh_type = None

        pieces = s.lower().split()
        if pieces[0] == "OVF":
            version_str = pieces[1]

        elif pieces[1] == "mesh":
            mesh_type = pieces[0]
            version_str = pieces[2]
            if version_str.startswith("v"):
                version_str = version_str[1:]

        if version_str in ["0.0a0", "0.99", "1.0"]:
            version = (1, 0)

        elif version_str in ["2.0"]:
            version = (2, 0)

        else:
            print ("Unknown OVF version '%s'. Trying to read assuming "
                   "version 2.0.")
            version = (2, 0)
            version_str = "2.0"

        self.version_str = version_str
        self.version = version
        self.mesh_type = mesh_type

    def __str__(self):
        if self.version == (1, 0):
            return "%s mesh v%s" % (self.mesh_type, self.version_str)
        else:
            return "OVF %s" % self.version_str

# List of known values in OOMMF file
known_values_list = [
  ("OOMMF", OVFType, "First line of OVF file"),
  ("Segment count", int, "Number of segments in data file", 1),
  ("Title", str, "Title/long filename of the file"),
  ("Desc", str, "Extra lines used by postprocessing programs"),
  ("meshtype", str, "Mesh type"),
  ("meshunit", str, "Fundamental mesh measurement unit"),
  ("xbase", float, "x coordinate of first point in data section"),
  ("ybase", float, "y coordinate of first point in data section"),
  ("zbase", float, "z coordinate of first point in data section"),
  ("xstepsize", float, "Distance between adjacent grid points."),
  ("ystepsize", float, "Distance between adjacent grid points."),
  ("zstepsize", float, "Distance between adjacent grid points."),
  ("xmin", float, "Minimum x coordinate of the mesh"),
  ("ymin", float, "Minimum y coordinate of the mesh"),
  ("zmin", float, "Minimum z coordinate of the mesh"),
  ("xmax", float, "Maximum x coordinate of the mesh"),
  ("ymax", float, "Maximum y coordinate of the mesh"),
  ("zmax", float, "Maximum z coordinate of the mesh"),
  ("xnodes", int, "Number of cells along x dimension in the mesh"),
  ("ynodes", int, "Number of cells along y dimension in the mesh"),
  ("znodes", int, "Number of cells along z dimension in the mesh"),
  ("valueunit", str, "Units for data values"),
  ("valuemultiplier", float, "Multiply data values by this to get true "
                             "value in valueunit-s"),
  ("ValueRangeMaxMag", float, "Maximum value of data (used as hint)"),
  ("ValueRangeMinMag", float, "Minimum value of data (used as hint)")
]

# Build a dictionary corresponding to known_values_list
known_values = {}
for known_value_tuple in known_values_list:
    value_name = name_normalise(known_value_tuple[0])
    known_values[value_name] = known_value_tuple

class OVFSegmentSectionNode(OVFSectionNode):
    required = ["Header"]

class OVFHeaderSectionNode(OVFSectionNode):
    pass

def _info_binary(oommf_version, data_size):
    endianness = '!' if oommf_version == (1, 0) else '<'
    if data_size == 8:
        float_type = 'd'
        expected_tag = 123456789012345.0

    else:
        assert data_size == 4
        float_type = 'f'
        expected_tag = 1234567.0 
    return endianness, float_type, expected_tag

class OVFDataSectionNode(OVFSectionNode):
    def __init__(self, value=[], data=None):
        OVFSectionNode.__init__(self, value, data)
        self.mesh_type = None
        self.data_type = None
        self.nodes = None
        self.num_nodes = None
        self.num_stored_node = None
        self.floats_per_node = None
        self.field = None

    def _retrieve_info_from_root(self, root):
        h = root.a_segment.a_header
        xn, yn, zn = self.nodes = \
          (h.a_xnodes.value, h.a_ynodes.value, h.a_znodes.value)
        self.num_nodes = xn*yn*zn

        field_size = 3
        self.mesh_type = root.get_mesh_type()
        if self.mesh_type == "rectangular":
            self.floats_per_node = field_size
            self.num_stored_nodes = self.num_nodes

        else:
            assert self.mesh_type == "irregular"
            self.floats_per_node = 3 + field_size
            self.num_stored_nodes = h.a_pointcount

        self.data_type = name_normalise(self.name)

    def read(self, stream, root=None):
        self._retrieve_info_from_root(root)

        if self.data_type == 'databinary8':
            self._read_binary(stream, root=root, data_size=8)
        elif self.data_type == 'databinary4':
            self._read_binary(stream, root=root, data_size=4)
        elif self.data_type == 'datatext':
            self._read_ascii(stream, root=root)
        else:
            raise OVFReadError("Unknown data type '%s' in OVF file."
                               % self.name)

    def _read_binary(self, stream, root=None, data_size=8):
        endianness, float_type, expected_tag = \
          _info_binary(root.a_oommf.value.version, data_size)

        fmt = endianness + float_type
        verification_tag, = struct.unpack(fmt, stream.read_bytes(data_size))
        if verification_tag != expected_tag:
            raise OVFReadError("Data carries wrong signature: got '%s' but "
                               "'%s' was expected. This usually means that "
                               "the file is corrupted or is not being read "
                               "correctly."
                               % (verification_tag, expected_tag))

        num_floats = self.num_stored_nodes*self.floats_per_node
        fmt = endianness + float_type*num_floats
        data = stream.read_bytes(num_floats*data_size)
        big_float_tuple = struct.unpack(fmt, data)
        self.field = array(big_float_tuple).reshape((-1, 3))

        while True:
            l = stream.next_line()
            if l.startswith("# End:"):
                return

    def _read_ascii(self, stream, root=None):
        raise NotImplementedError("Cannot read OVF in ASCII format.")

    def write(self, stream, root=None):
        self._retrieve_info_from_root(root)

        stream.write_line("# Begin: %s" % self.name)
        if self.data_type == "databinary8":
            self._write_binary(stream, root=root, data_size=8)
        elif self.data_type == "databinary4":
            self._write_binary(stream, root=root, data_size=4)
        elif self.data_type == "datatext":
            self._write_ascii(stream, root=root)
        stream.write_line("# End: %s" % self.name)

    def _write_binary(self, stream, root=None, data_size=8):
        endianness, float_type, expected_tag = \
          _info_binary(root.a_oommf.value.version, data_size)

        fmt = endianness + float_type
        out_data = struct.pack(fmt, expected_tag)
 
        num_floats = self.num_stored_nodes*self.floats_per_node
        fmt = endianness + float_type*num_floats
        out_data += struct.pack(fmt, *self.field.flat) + "\n"
        stream.write(out_data)

    def _write_ascii(self, stream, root=None):
        pass


def remove_comment(line, marker="##"):
    """Return the given line, without the part which follows the comment
    marker ## (and without the marker itself)."""
    i = line.find(marker)
    if i < 0:
        return line
    else:
        return line[:i]

def known_value_node(name, value):
    lname = name_normalise(name)
    if known_values.has_key(lname):
        val_type = known_values[lname][1]
        value = val_type(value)

    else:
        print "Unknown value '%s' while reading OVF file." % name

    return OVFValueNode(data=(name, value))

def known_section_node(action, name):
    lname = name_normalise(name)

    cls = None
    if lname == "segment":
        cls = OVFSegmentSectionNode
    elif lname == "header":
        cls = OVFHeaderSectionNode
    elif lname.startswith("data"):
        cls = OVFDataSectionNode
    else:
        print "Unknown section '%s' while reading OVF file." % name
        cls = OVFSectionNode

    return cls(data=(name, action))

def read_node(stream):
    l = None
    while l in ["", "#", None]:
        l = stream.next_line()
        if l == None:
            return None
        else:
            l = remove_comment(l).lstrip()

    if not l.startswith("#"):
        raise OVFReadError("Error reading OVF header. "
                           "Expected #, but got '%s'" % l)

    piece = l[1:].split(":", 1)
    name = piece[0].strip()
    lname = name_normalise(name)
    value = None
    if len(piece) > 1:
        value = piece[1].strip()

    if lname in ["begin", "end"]:
        return known_section_node(name, value)
    else:
        return known_value_node(name, value)

class OVFRootNode(OVFSectionNode):
    required = ["OOMMF", "Segment count"]

    def __init__(self):
        OVFSectionNode.__init__(self, data=("main", "begin"))

    def get_mesh_type(self):
        ovf_version = self.a_oommf.value
        v = ovf_version.version
        if v == (1, 0):
            return ovf_version.mesh_type
        else:
            return self.a_section.a_header.a_meshtype

    def write(self, stream, root=None):
        for n in self.subnodes:
            n.write(stream, root=root)

class OVFStream(object):
    def __init__(self, filename, mode="r"):
        if type(filename) == str:
            self.filename = filename
            self.f = open(filename, mode)
        else:
            self.filename = None
            self.f = filename
        self.no_line = 0
        self.lines = []

    def __del__(self):
        self.f.close()

    def next_line(self):
        if self.no_line < len(self.lines):
            l = self.lines[self.no_line]

        else:
            n = self.no_line - len(self.lines)
            for _ in range(n + 1):
                l = self.f.readline()
                if len(l) == 0:
                    return None
                l = l[:-1]
                self.lines.append(l)

        self.no_line += 1
        return l

    def read_bytes(self, num_bytes):
        l = self.f.read(num_bytes)
        self.lines.append(l)
        self.no_line = len(self.lines)
        return l

    def read_lines_ahead(self):
        self.lines += self.f.readlines()

    def write(self, data):
        self.f.write(data)

    def write_line(self, line):
        self.f.write(line + "\n")

if __name__ == "__main__":
    import sys
    s = OVFStream(sys.argv[1])
    ovf = OVFRootNode()
    ovf.read(s, root=ovf)
    ovf._end_section("main")

    s2 = OVFStream(sys.argv[2], mode="w")
    ovf.write(s2, root=ovf)

