'''
Created on 21.07.2009

@author: Matteo Franchin, Massoud Najafi
'''

import types, os
from os.path import abspath
from numpy import array, asarray, append, swapaxes

default_omf_header = ( 
'''# OOMMF: rectangular mesh v1.0
# Segment count: 1
# Begin: Segment
# Begin: Header
# Title: Oxs_TimeDriver::Magnetization
# Desc: Oxs vector field output
# Desc:  MIF source file: no\\mif\\generated\\by\\nmag
# Desc:  Iteration: 0, State id: 0
# Desc:  Stage: 0, Stage iteration: 0
# Desc:  Stage simulation time: 0.0 s
# Desc:  Total simulation time: 0.0 s
# meshunit: m
# valueunit: A/m
# valuemultiplier: 1.0
# xmin: 0
# ymin: 0
# zmin: 0
# xmax: 0.0
# ymax: 0.0
# zmax: 0.0
# ValueRangeMaxMag: 0.0
# ValueRangeMinMag: 0.0
# meshtype: rectangular
# xbase: 0.0
# ybase: 0.0
# zbase: 0.0
# xstepsize: 0.0
# ystepsize: 0.0
# zstepsize: 0.0
# xnodes: 0
# ynodes: 0
# znodes: 0
# End: Header
''')

default_omf_header_dict = None

endline = os.linesep

def next_container(out, name):
    basename = name.strip()
    i = 0
    full_name = (basename, i)
    while out.has_key(full_name):
        i += 1
        full_name = (basename, i)
    return (full_name, {})

def _parse_top(state, current, id, val, line_num):
    my_val = [val, line_num] # so that it's easy to reassemble the header
    out = current[-1]
    lid = id.lower() # case insensitive
    if lid == 'begin':
        container_name, container_dict = next_container(out, val)
        out[container_name] = container_dict
        current.append(container_dict)
        container_dict[':self:'] = [container_name, line_num]

    elif lid == 'end':
        prev_container_name = current.pop()
        begin_data = prev_container_name[':self:']
        begin_data.append(line_num)
        begin_name = begin_data[0][0]
        if val.strip() != begin_name:
            print ("Warning: End (%s) does not match Begin (%s)!" 
                   % (val, begin_name))

    else:
        if out.has_key(id):
            prev = out[id]
            if type(prev[0]) == list:
                prev.append(my_val)
            else:
                out[id] = [prev, my_val]
        else:
            out[id] = my_val

def parse_header(header_text):
    '''Parse an OOMMF OMF header and return a dictionary made of the parsed
    values.'''
    state = [_parse_top]
    current = [{}]

    lines = header_text.splitlines()
    for line_num, line in enumerate(lines):
        if line.startswith('#'):
            if ':' in line:
                line = line[1:]
                id, rest = line.split(':', 1)
                parse_fn = state[-1]
                parse_fn(state, current, id.strip(), rest, line_num)

    return current[0]

def fill_line(l, line_num, val):
    if line_num >= len(l):
        l += [False]*(line_num - len(l) + 1)
    l[line_num] = val

def assemble_header(header_dict):
    '''Does the inverse operation of ``parse_header``: takes a dictionary as
    returned by ``parse_header`` and returns the corresponding header text.'''
    def assemble(lines, this_dict):
        for key in this_dict:
            if key == ':self:':
                continue
            item = this_dict[key]
            if type(item) == list:
                name = item[0]
                if type(name) == str:
                    value, line_num = item
                    fill_line(lines, line_num, '# %s:%s' % (key, value))
                elif type(name) == list:
                    for value, line_num in item:
                        fill_line(lines, line_num, '# %s:%s' % (key, value))
                else:
                    print "Error when processing '%s', type=%s" % (name,
                                                                   type(name))

            elif type(item) == types.DictType:
                details = item[':self:']
                sub_dict_name, line_start = details[0:2]
                name = sub_dict_name[0]
                fill_line(lines, line_start, '# Begin: %s' % name)
                if len(details) >= 3:
                    line_end = details[2]
                    fill_line(lines, line_end, '# End: %s' % name)
                assemble(lines, item)

            else:
                print key, type(item)

    lines = [False]*40
    assemble(lines, header_dict)
    return endline.join(filter(None, lines)) + endline

def parse_desc(header_dict):
    '''Extract the Desc field as a dictionary from the header_dict returned
    by ``parse_header``.'''
    desc_dict = {}
    descs = header_dict[('Segment', 0)][('Header', 0)]['Desc']
    for desc, line_num in descs:
        subdescs = desc.split(',')
        for subdesc in subdescs:
            id_value = subdesc.split(':', 1)
            if len(id_value) > 1:
                id, value = id_value
                desc_dict[id.strip()] = [value, line_num]
            else:
                desc_dict[id_value[0]] = [None, line_num]
    return desc_dict

def assemble_desc(header_dict, desc_dict):
    '''Inverse of ``parse_desc``: takes a dictionary as returned by
    ``parse_desc`` and write its content into the header dictionary.'''
    pass

def load_mat(file,dim):
    file = abspath(file)
    f = open(file,'r')        
    lines = f.readlines()
    data = []
    for line in lines:
        values = line.split()
        converted_values = [float(val) for val in values]
        data.append(converted_values)

    f.close()
    data = asarray(data)
    
    data.shape = (-1,dim)
    data = swapaxes(data,0,1)
    return data
#    except:
#        raise Exception('could not read file: "' +  file + '"')


def load_omf(file):
    '''
    if including_position: 
        result is [data, X]  
    else:
        result is [data, None]
    '''
    f = open(file, 'r')

    X = []
    data = []

    cellsX = -1
    cellsY = 1
    cellsZ = 1

    building_header = True
    omf_header = ''
    for line in f.readlines():
        if line.startswith['#']:
            if building_header:
                omf_header += line + endline
            else:
                # lines starting with # are ignored after parsing the header
                pass
        else:
            if building_header:
                header_dict = parse_header(omf_header)
                building_header = False

                data = header_dict[('Segment', 0)][('Header', 0)]
                cellsX = int(data['xnodes'][0])
                cellsY = int(data['ynodes'][0])
                cellsZ = int(data['znodes'][0]) 

            else:
                values = line.split()
                converted_values = [float(val) for val in values]
                if len(values) == 6:
                    X.append(converted_values[0:2])
                    data.append(converted_values[3:5])

                elif len(values) == 3:
                    data.append(converted_values)

                else:
                    raise Exception('file contains a line wrong number of elements')

    f.close()

    # convert to numpy.array and make the components first dimension
    data = asarray(data)
    data = swapaxes(data, 0, 1)
    data.shape = (3, cellsX, cellsY, cellsZ)
    
    # change (x, y) to (y, x), because OOMMF and Python use different
    # coordinate systems
    data = swapaxes(data, 1, 2)

    arr_X = None
    if len(X) > 0:
        arr_X = array(X)
        arr_X.shape = (3, cellsX, cellsY, cellsZ)

    return [data, arr_X]

def new_header():
    return parse_header(default_omf_header)

def add_header_data(header_dict, section, data_to_set):
    section_dict = header_dict
    for subsection_name in section:
        section_dict = section_dict[subsection_name]
    for name, val in data_to_set:
        section_dict[name][0] = " %s" % val

def write_omf(filename, data, header_dict=None, positions=None,
              fmt='%.11f'):
    global default_omf_header_dict
    if header_dict == None:
        if default_omf_header_dict == None:
            default_omf_header_dict = parse_header(default_omf_header)
        header_dict = default_omf_header_dict

    header_data = header_dict[('Segment', 0)][('Header', 0)]
    shape = [int(header_data[dim][0])
             for dim in ['xnodes', 'ynodes', 'znodes']]

    my_data = array(data)
    my_data.shape = tuple([3] + shape)
    my_data = swapaxes(my_data, 1, 2)

    num_cells = my_data.size / 3
    my_data = my_data.reshape([3, -1])

    header_text = assemble_header(header_dict)
    f = open(filename, "wt")

    # write the header
    f.write(header_text)

    # write the data
    f.write("# Begin: Data Text" + endline)
    for num_col in range(num_cells):
        line = ' '.join([fmt % my_data[i, num_col] for i in range(3)])
        f.write(line + endline)

    f.write("# End: Data Text" + endline)
    f.write("# End: Segment" + endline)
    f.close()

