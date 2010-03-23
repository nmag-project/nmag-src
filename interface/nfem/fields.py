import new,copy
import nfem
import ocaml

class Dof:
    def __init__(self,name,data):
        self.name = name
        self.data = data

    def __repr__(self):
        return "<dof '%s': %s>" % (self.name, self.data)

class Site(object):
    def __init__(self,site,pos):
        self.site = site
        self.pos = pos
        self.dofs = []

    def add_dof(self,name,data):
        self.dofs.append(Dof(name,data))
        self.__setattr__(name,data)
        
                
    def __repr__(self):
        return "<site %s with dofs: %s>" % (self.site, " ".join([x.name for x in self.dofs]))


def make_dof_list_structure(maxind,default=None):
    #Given the maxindex data structure as returned by
    #ocaml.data_doftypes(self._rawfield), this function returns a
    #skeleton of such a data structure. This is useful in different
    #contexts, such as gathering data via the field_entrywise function
    #or to save data to a file (nmag2.save_data_table())
    if len(maxind) == 0:
        data = []
    elif len(maxind) == 1:
        data = [None]*maxind[0]
    else:
        raise NotImplementedError,"Can only deal with scalars and vectors for now"
    return data

def make_dof_list_structure_name(maxind):
    if len(maxind) == 0:
        name = 'scalar'
    elif len(maxind) == 1:
        name = '%d-vector' % maxind[0]
    else:
        raise NotImplementedError,"Can only deal with scalars and vectors for now"
    return name



class VectorDOF(object):
    def __init__(self,dofname,data,pos,site,dof_id,maxind):
        self.name = dofname
        self._maxind = maxind
        #pos,data = ocaml.data_dofvector_from_field(rawfield,dofname)
        self.__setattr__("pos",pos)
        self.__setattr__("data",data)
        self.__setattr__("site",site)
        self.__setattr__("dof_id",dof_id)

    def __repr__(self):
        msg = "degree of freedom '%s' " % self.name
        msg += "(%d points, %s)" % (len(self.pos),make_dof_list_structure_name(self._maxind))
        return msg


class Field(object):
    def __init__(self,rawfield,name):
        self._name = name
        self._rawfield = rawfield

        #learn about degrees of freedom:
        tmp_dof_maxind_by_name = {}

        self.dof_names = []
        self.dofs = []
        
        dof_maxnames = ocaml.data_doftypes(self._rawfield)
        for dofname,maxind in dof_maxnames:
            tmp_dof_maxind_by_name[dofname] = maxind

        #populate degrees of freedom
        vecdofs = {}

        def cb(i, dof_name_stem, site, pos, value):
            site = tuple(site)
            dofname, ind = dof_name_stem

            #have we seen this dof before?
            if not vecdofs.has_key(dofname):
                vecdofs[dofname]={}

            #have we seen this site before?
            if not vecdofs[dofname].has_key(site):
                maxind = tmp_dof_maxind_by_name[dofname]
                data = make_dof_list_structure(maxind,None)
                dof_id = copy.copy(data)
                vecdofs[dofname][site] = [data,pos,site,dof_id]

            if ind==[]: #scalar
                vecdofs[dofname][site][0] = value
                vecdofs[dofname][site][3] = i
            else:
                vecdofs[dofname][site][0][ind[0]] = value
                vecdofs[dofname][site][3][ind[0]] = i

        nfem.field_entry_wise( self._rawfield, cb )

        #now sort into neat vectors
        for dofname,maxind in dof_maxnames:
            tmp = vecdofs[dofname].values()
            data = map( lambda a : a[0], tmp)
            pos = map( lambda a : a[1], tmp)
            site = map( lambda a : a[2], tmp)
            dof_id = map( lambda a : a[3], tmp)

            tmpdof = VectorDOF(dofname,data,pos,site,dof_id,maxind)
            self.__setattr__(dofname,tmpdof)
            self.dof_names.append(dofname)
            self.dofs.append(tmpdof)

    def __repr__(self):
        msg = "Field '%s' " % self._name
        msg += "(dofs:%s)" % "".join([" '%s'" % x for x in self.dof_names])
        return msg
    

class NfemFields(object):

    def __init__(self, dic_of_fields):
        self._fields = dic_of_fields
        self.field_names = []
        self.fields = []
        self.tmp = {}
        self._bysite = None

        #gather initial data
        for field_name in self._fields.keys():
            field=self._fields[field_name]
            #
            #dof_maxnames=ocaml.data_doftypes(field)
            #print dof_maxnames
            #for mn in dof_maxnames:
            #    self.tmp[mn[0]]=(field_name,mn[1])

            tmpfield = Field(field,field_name)
            self.__setattr__(field_name,tmpfield)
            self.field_names.append(field_name)
            self.fields.append(tmpfield)


    def __repr__(self):
        msg = "<NfemFields:\n"
        for field in self.fields:
            msg += " -> "+field.__repr__() + "\n"
            for dof in field.dofs:
                msg += "     -> "+dof.__repr__()+"\n"

        return msg+">\n"
            

    def gather_site_data(self):
        if not self._bysite == None:
            return self._bysite

        sitesdic = {}
        for field in self.fields:
            for dof in field.dofs:
                for i in range(len(dof.site)):
                    if not sitesdic.has_key(dof.site[i]):
                        sitesdic[dof.site[i]] = Site(dof.site[i],dof.pos[i])
                    sitesdic[dof.site[i]].add_dof(dof.name,dof.data[i])

        self._bysite = sitesdic
        return self._bysite


    bysite = property(gather_site_data,doc="Data order by site")
