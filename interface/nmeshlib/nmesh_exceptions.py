
import logging
class NmeshBaseError(Exception):
    """NmeshBaseException"""
    def __init__(self,msg):
	self.msg = msg
        self.logger = logging.getLogger('root')
        self.logger.critical("Exception type: %s " % self.__doc__)
        self.logger.critical("Exception msg : %s " % msg)

    def __str__(self):
        return self.msg

class NmeshDimError(NmeshBaseError):
    """NmeshDimError"""
    def __init__(self,msg):
        NmeshBaseError.__init__(self,msg)

class NmeshImportError(NmeshBaseError):
    """NmeshImportError"""
    def __init__(self,msg):
        NmeshBaseError.__init__(self,msg)


class NmeshUserError(NmeshBaseError):
    """NmeshUserError"""
    def __init__(self,msg):
        NmeshBaseError.__init__(self,msg)

class NmeshIOError(NmeshBaseError):
    """NmeshIOError"""
    def __init__(self,msg):
        NmeshBaseError.__init__(self,msg)

