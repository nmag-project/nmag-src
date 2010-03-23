
import logging
class NfemBaseError(Exception):
    """NfemBaseException"""
    def __init__(self,msg):
	self.msg = msg
        self.logger = logging.getLogger('root')
        self.logger.critical("Exception type: %s " % self.__doc__)
        self.logger.critical("Exception msg : %s " % msg)

    def __str__(self):
        return self.msg

class NfemDimError(NfemBaseError):
    """NfemDimError"""
    def __init__(self,msg):
        NfemBaseError.__init__(self,msg)

class NfemImportError(NfemBaseError):
    """NfemImportError"""
    def __init__(self,msg):
        NfemBaseError.__init__(self,msg)

class NfemUserError(NfemBaseError):
    """NfemUserError"""
    def __init__(self,msg):
        NfemBaseError.__init__(self,msg)

class NfemValueError(NfemBaseError):
    """NfemValueError"""
    def __init__(self,msg):
        NfemBaseError.__init__(self,msg)

