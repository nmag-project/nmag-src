"""Globally accessible dictionaries (one for Python, one for OCaml)"""

__docformat__="restructuredtext"

import ConfigParser, types, logging

log = logging.getLogger('nsim') 
"""Defining logger"""

try:
    import ocaml

except:
    import ocamlcompat as ocaml

class OcamlFeatures(object):
    """Provide convenient interface to OCaml 'features' as accessed
    via ocaml.snippets_*.
    
    Allow to set these features by reading configuration file
    with Python's ConfigParser.

    """
    def __init__(self):
	"""OcamlFeatures Constructor

	Does not do anything.
	"""
        pass

    def _must_be_strings(self, function, name_vals):
        if type(name_vals) != types.ListType:
            name_vals = [name_vals]

        for (name, val) in name_vals:
            if type(val) != types.StringType:
                raise TypeError("OcamlFeatures.%s: '%s' has to be of type"
                                " string but is of type '%s' (value: %s)"
                                % (function, name, type(val), val))

    def set(self, section, key, value):
	"""Set key,value - pair in given section"""
        self._must_be_strings('set', [('section', section), ('key', key),
                                      ('value', value)])
        ocaml.snippets_register_feature(section, key, value)

    def get(self,section,key):
	"""Get value for given section and key"""
        self._must_be_strings('get', [('section', section), ('key', key)])

    def __repr__(self):
	"""Representation, same as __str__()"""
        return self.__str__()

    def __str__(self):
	"""String representation of configuration"""
        tmpf = Features(local=True)
        tmpf.from_ocaml_features()
        return str(tmpf)

    def set_from_file(self,filename):
	"""Read config data from file.

	Read Config file with name filename, and add/override
	OCaml's feature settings with settings provided in that file
	"""
        log.debug("Setting Ocaml Features from file %s" % filename)
        tmpf = Features(local=True)
        tmpf.from_file(filename)
        tmpf.to_ocaml_features()
    
class Features(object):
    """High-level interface to Python's ConfigParser.ConfigParser module.
    
    Allows to read config files from files, filepointers, strings in a
    convenient way.  Used for example for nmesh's mesh generation
    parameters.

    By default, there is only one such configuration object
    (technically a class object). This means the same configuration data
    can be modified and accessed from different modules, each of which
    imports this file and creates a features object.

    Typical entries are::

      [etc]
      runid = '...'
      savedir = '...'
    
    """

    config = None
    """``ConfigParser.ConfigParser`` object (Standard Python)

    This keeps the global Features data (but is an implementation detail)."""

    def __init__(self,defaults=None,local=False):
	"""
	``Features`` constructor

	:Parameters:

  	  `defaults` : dictionary or boolean
	     can be dictionary with default values that are passed to Python's ConfigParser.ConfigParser, or None.

          `local` : boolean
	     if true, then this is a new instance of the Feature object. If false (default), then this is the global feature object.
	"""
        
        if local:
            self.__config = ConfigParser.ConfigParser(defaults=defaults)
	    """``ConfigParser.ConfigParser`` object (Standard Python)

	    This either points to ``Features.config`` or is a new instance
	    of ``ConfigParser.ConfigParser`` if ``local==True``.
	    """

        else:
            if Features.config == None:
                Features.config = ConfigParser.ConfigParser(defaults=defaults)

            elif defaults != None:
                raise ValueError("Can't impose defaults on global Features "
                                 "class object")
 
            self.__config = Features.config

    def get( self, section, key, raw=False):
        """Retrieve value of ``key`` in section ``section``.

	If raw=True, then no expansion of %(defaults)s is done. """
        return self.__config.get(section,key,raw=raw)

    def add_section( self, section):
	"""Add section"""
	self.__config.add_section( section )

    def has_section( self, section):
	"""Check whether object has section ``section``.

	:returns: bool
	"""
        return self.__config.has_section(section)

    def has_sectionkey( self, section, key):
	"""Check whether object has section ``section`` and key ``key``.
	
	Return True if both exist, otherwise False.
	"""
        return self.__config.has_option(section,key)
    
    def set( self, section, key, value, check_exists_already=False):
	"""Set value for key ``key`` in section ``section``
	
	if ``check_already_exists==True``, then complain if that 
	value exists already.

	Otherwise an existing value will be overriden.
	"""
        if check_exists_already:
            if not self.__config.has_section(section):
                log.warn("Current config is: \n%s" % self)
                raise NsimValueError,"Section/Domain: %s is not known" % section
            if not self.__config.has_option(section,key):
                log.warn("Current config is: \n%s" % self)
                raise NsimValueError,"Option/key/feature: %s is not known." % key
        else:
            if not self.__config.has_section(section):
                self.add_section(section)
	self.__config.set(section,key,value)

    def sections(self):
	"""Return lits of sections."""
        return self.__config.sections()

    def items(self,section):
	"""Return list of (key,value) pairs for section ``section``.

	Example: 

	Assume we have Features object ``x`` such that

          >>> print x
          [test]
          two = 2
          one = 1
	  >>> x.items('test')
          [('two', 2), ('one', 1)]

        """
        return self.__config.items(section,raw=True)

    def __str__(self):
        return self.to_string()

    def to_string(self):
        """Represents current config in string. This strings looks like
	a ConfigParser-config file."""
        import StringIO
        msg = StringIO.StringIO()
        self.__config.write(msg)
        me = msg.getvalue()
        msg.close() #frees memory
        return me

    def from_string(self,string):
        """Add configuration in string to current config. String format
	must follow ConfigParser conventions."""
	import StringIO
	string_stream = StringIO.StringIO(string)
	self.__config.readfp(string_stream)

    def from_file(self,file):
	"""Read configuration from file.

	:Parameters:
	  `file` : string or filehandler
            If type is string then this is the filename, otherwise
	    file is the file handle to an file.  

    """

        if type(file)==types.StringType:
            self.__config.read(file)
        elif type(file)==types.FileType:
            self.__config.readfp(file)
        else:
            raise NsimTypeError,"Can only understand filenames [string] or file pointers [file]"

    def from_ocaml_features(self):
        """Populate features object with features as in ocaml's features (defined in snippets).
        This is useful to use the high-level Python abilities to write these features to a
        file/string etc."""
        import ocaml
        for section, key_value_list in ocaml.snippets_all_features():
            for key,value in key_value_list:
                self.set(section,key,value)

    def to_ocaml_features(self):
        """Populate ocaml features object (defined in snippets) with
        content of this feature object.  This is useful to use the
        high-level Python abilities to read these features from a
        file/string before setting the ocaml features with this data."""

        import ocaml
        for section in self.sections():
            for key,value in self.items(section):
                log.debug("to_ocaml_features: section=%s, key=%s, value=%s" % (section,key,str(value)))
                ocaml.snippets_register_feature(section,str(key),str(value))


    def to_file(self,file):
        """
	Write configuration to file. 

	:Parameters:
	  `file` : string or file handler
             File can be file handler, or filename

        """
        if type(file)==types.StringType:
            f=open(file,'w')
            self.__config.write(f)
            f.close()
        elif type(file)==types.FileType:
            self.__config.writefp(file)
        else:
            raise NsimTypeError,"Can only understand filenames [string] or file pointers [file]"

    def section_to_dictionary(self,section):
	"""Return all key-value pairs in this section as key-value pairs in a dictionary."""
        d={}
        if not section in self.__config.sections():
            raise NsimValueError,"No section '%s' in Features. Sections available are %s" \
                  % (section,self.__config.sections())
        for key,value in self.__config.items(section,raw=True):
            d[key]=value
        return d

    def remove_all(self):
        """Remove all entries in all sections"""
        for section in self.__config.sections()+['DEFAULT']:
            self.__config.remove_section(section)









def test():
    """Demonstration"""
    c = Features()

    c.add_section('general')
    c.set('general','test',1,check_exists_already=False)
    c.set('general','banana',10,check_exists_already=False)

    #features_to_ocaml_features(c)

    print c

    test="""[general2]
test=1
test2=42
"""

    c.from_string(test)
    print "===="
    print c

    c.remove_all()
    print "===="
    print c

    c.from_string(test)
    print "===="
    print c

    d=c.section_to_dictionary('general2')
    print d


    ocamlf = OcamlFeatures()
    print ocamlf

    ocamlf.set('newsection','a','dog')
    print ocamlf


if __name__=="__main__":
    test()
