"""Nsim Exceptions"""

__docformat__="restructuredtext"

import logging
class NsimBaseError(Exception):
    """NsimBaseException"""
    def __init__(self,msg):
	self.msg = msg
        self.logger = logging.getLogger('')
        self.logger.critical("Exception type: %s " % self.__doc__)
        self.logger.critical("Exception msg : %s " % msg)

    def __str__(self):
        return self.msg

class NsimDimError(NsimBaseError):
    """NsimDimError"""
    def __init__(self,msg):
        NsimBaseError.__init__(self,msg)

class NsimImportError(NsimBaseError):
    """NsimImportError"""
    def __init__(self,msg):
        NsimBaseError.__init__(self,msg)

class NsimValueError(NsimBaseError):
    """NsimValueError"""
    def __init__(self,msg):
        NsimBaseError.__init__(self,msg)


class NsimUserError(NsimBaseError):
    """NsimUserError"""
    def __init__(self,msg):
        NsimBaseError.__init__(self,msg)

