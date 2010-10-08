import sys

def _set_dict_from_list(d, l):
    for item_name, item_value in l:
        d[item_name] = item_value
    return d

def _value_in_units(value, units):
    if units != None:
        return float(value/units)
    else:
        return value

def _build_header(col_data_list, columns):
    header = []
    for col_name, col_data in col_data_list:
        if columns.has_key(col_name):
            column = columns[col_name]
            col_fmt = column.format
            col_units = column.units
            col_text = col_fmt % _value_in_units(col_data, col_units)
            col_width = max(len(col_text), len(column.title))
            col_title = column.title.rjust(col_width)

            header.append((col_name, col_fmt, col_title,
                           col_units, column.units_str))

    return header

def _print_header(out, header):
    line1 = "# "
    line2 = "# "
    sep = " "
    for _, _, col_title, _, col_units_str in header:
        line1 += col_title + sep
        line2 += col_units_str.rjust(len(col_title)) + sep
    out("%s\n%s\n" % (line1, line2))

def _print_row(out, header, data_dict):
    row = " "
    for col_name, col_fmt, col_title, col_units, _ in header:
        print col_name
        col_data = _value_in_units(data_dict[col_name], col_units)
        col_text = (col_fmt % col_data).rjust(len(col_title))
        row += col_text + " "
    out(" " + row + "\n")

class ColDescriptor:
    def __init__(self, name, type=None, title=None, format=None,
                 units=None, units_str=None):
        self.name = name
        self.format = format
        self.type = type
        self.units = units
        if units_str != None:
            self.units_str = units_str
        else:
            self.units_str = str(units)
        if units == None:
            self.units_str = "<>"
        if title != None:
            self.title = title
        else:
            self.title = name

class ColWriter:
    def __init__(self, out=None, append=True):
        self.header = []
        self.type_fmts = {'int':" %9d", 'float':" %15g"}
        self.col_types = {}
        self.col_titles = {}

        # descriptors for each column
        self.columns = {}
        self.column_list = []

        # self.out is used to write the output. It can be a file name,
        # a function or None. In the latter case data is written to stdout.
        self._stream_out = None
        self.out = lambda s: sys.stdout.write(s)
        if out == None:
            pass

        elif type(out) == str:
            mode = "w"
            if append: mode = "a"
            def out_fn(s):
                if self._stream_out == None:
                    self._stream_out = open(out, mode)
                self._stream_out.write(s)
                self._stream_out.flush()
            self.out = out_fn

        else:
            self.out = out

    def __del__(self):
        self.close()

    def close(self):
        '''Close the out stream, if any was opened by this class.'''
        if self._stream_out != None:
            self._stream_out.close()

    def set_format(self, type, format):
        '''Set the default format used to display the given type of
        quantities.'''
        self.type_fmts[type] = format

    def has_columns(self):
        '''Returns True if at least one column has been defined.'''
        return len(self.column_list) > 0

    def set_formats(self, type_format_list):
        '''Simimlar to the method 'set_format', but allows to set many
        formats (it takes a list (type, format) as input).'''
        for type, format in type_format_list:
            self.set_format(type, format)

    def define_column(self, column_def):
        '''Define a new column. column_def must be an instance of
        ColDescriptor.'''
        if column_def.format == None:
            if column_def.type != None:
                column_def.format = self.type_fmts[column_def.type]
            else:
                column_def.format = "%s"

        self.column_list.append(column_def)
        self.columns[column_def.name] = column_def

    def define_columns(self, column_def_list):
        '''Similar to the method 'define_column', but takes a list of columns
        to define (it is hence useful to define many columns).'''
        for column_def in column_def_list:
            self.define_column(column_def)

    def write_row(self, col_list):
        if len(self.header) < 1:
            self.header = _build_header(col_list, self.columns)
            _print_header(self.out, self.header)

        values = _set_dict_from_list({}, col_list)
        _print_row(self.out, self.header, values)

if __name__ == "__main__":
    l = [('nsteps', 871.0), ('nfevals', 1256.0), ('nlinsetups', 57.0),
         ('netfails', 1.0), ('qlast', 2.0), ('qcur', 2.0),
         ('hinused', 8.3658308464789344e-08), ('hlast', 0.8923316562175474),
         ('hcur', 0.8923316562175474), ('tcur', 300.52394476564865),
         ('npevals', 57.0), ('npsolves', 2494.0), ('nliters', 1247.0),
         ('nlcfails', 0.0), ('njvevals', 1247.0), ('nfevalsLS', 0.0)]
    cols = [ColDescriptor(name=name, type=float)
            for name in ['hinused', 'hlast', 'hcur', 'tcur']]
    cdw = ColWriter()
    cdw.define_columns(cols)
    print cdw.columns
    cdw.write_row(l)

