raise StandardError,"This should not be used anymore. (fangohr 26/01/2007)"
from nsim_exceptions import *

log = logging.getLogger('nmesh') 

import types, os
import ConfigParser

class Features(object):
    """High-level interface to Python's ConfigParser.ConfigParser module.
    Allows to read config files from files, filepointers, strings in a convenient way.
    Used for example for nmesh's mesh generation parameters.
    """

    def __init__(self,defaults=None):
	self.__config = ConfigParser.ConfigParser(defaults=defaults)

    def get( self, section, key, raw=False):
        """If raw=True, then no expansion of %(defaults)s is done. """
        return self.__config.get(section,key,raw=raw)

    def add_section( self, section):
	self.__config.add_section( section )
        
    def set( self, section, key, value, check_exists_already=False):
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
        return self.__config.sections()

    def items(self,section):
        return self.__config.items(section,raw=True)

    def __str__(self):
        return self.to_string()

    def to_string(self):
        """Represents current config in string"""
        import StringIO
        msg = StringIO.StringIO()
        self.__config.write(msg)
        me = msg.getvalue()
        msg.close() #frees memory
        return me

    def from_string(self,string):
        """adds configuration in string to current config"""
	import StringIO
	string_stream = StringIO.StringIO(string)
	self.__config.readfp(string_stream)

    def from_file(self,file):
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
                ocaml.snippets_register_feature(section,key,value)


    def to_file(self,file):
        """Write configuration to file. File can be file handler, or filename"""
        if type(file)==types.StringType:
            f=open(file,'w')
            self.__config.write(f)
            f.close()
        elif type(file)==types.FileType:
            self.__config.writefp(file)
        else:
            raise NsimTypeError,"Can only understand filenames [string] or file pointers [file]"

    def section_to_dictionary(self,section):
        d={}
        if not section in self.__config.sections():
            raise NsimValueError,"No section '%s' in Features. Sections available are %s" \
                  % (section,self.__config.sections())
        for key,value in self.__config.items(section,raw=True):
            d[key]=value
        return d

    def remove_all(self):
        """empties all entries"""
        for section in self.__config.sections()+['DEFAULT']:
            self.__config.remove_section(section)


def get_absolute_librarypath(library):
    """Given a packgase (such as nmesh, nsim, ...) this returns
    the name of the __init__.py file that defines the package
    and the location of that file.

    Useful to get the path to configuration files that are stored in
    the same place.
    """
    if type(library) == types.ModuleType: 
        this_file_path = os.path.realpath(library.__file__)
    elif type(library) == types.StringType:
        #assume this is just a filename
        this_file_path = os.path.realpath(library)
        
    library_directory, filename = os.path.split( this_file_path )
    return library_directory,filename



#def features_to_ocaml_features(python_features):
#    """Takes a Features object, and adds all the registered section(=domains)
#    to the ocaml-features object defined in snippets."""
#    import ocaml
#    p = python_features
#    for section in p.sections():
#        for key,value in p.items(section):
#            ocaml.snippets_register_feature(str(section),str(key),str(value))
#    log.error("Python features are %s" % str(python_features))
#    log.error("Ocaml  features are %s" % ocaml.snippets_all_features())
#


def test():
    c = Features()

    c.add_section('general')
    c.set('general','test',1,check_exists_already=False)
    c.set('general','banana',10,check_exists_already=False)

    features_to_ocaml_features(c)

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

if __name__=="__main__":
    test()






















## A first draft where 'are' a Configparser (rather than 'having' one as above)
##
##import ConfigParser
##class MeshConfig(ConfigParser.ConfigParser):
##    def __init__(self):
##        ConfigParser.ConfigParser.__init__(self)
##        self.default_context = 'nmesh0'
##    def set_default_context(self, context):
##        self.default_context = context

##    def get( self, key, section=None):
##        if section == None:
##            section = self.default_context
##        return ConfigParser.ConfigParser.get(self,section,key)

##    def set( self, key, value, section=None):
##        if section == None:
##            section = self.default_context
##        #check this is a valid key (=option in ConfigParser, feature in Snippets)

##        if key == "xxx":
##            pass


##        if ConfigParser.ConfigParser.has_option(self,section,key):
##            ConfigParser.ConfigParser.set(self,section,key,value)
##        else:
##            raise ValueError,"Option/key/feature: %s is not known" % key

##    def __str__(self):
##        import StringIO
##        msg = StringIO.StringIO()
##        config.write(msg)
##        me = msg.getvalue()
##        msg.close() #frees memory
##        return me
