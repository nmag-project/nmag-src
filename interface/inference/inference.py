# (C) 2007 Dr. Thomas Fischbacher
# a simple "make-like" inference engine in python

import sys


#for use in nmag, connect to logger:
import logging
log = logging.getLogger('nmagmake')

class InferenceError(Exception):
    """InferenceError"""
    def __init__(self,msg):
        self.msg=msg

    def __str__(self):
        return self.msg

class InferenceEntity:
    """ XXX document me! """
    def __init__(self,name,depends_on=[],how_to_make=[],also_updates=[]):
        self.name=name
        self.depends_on=depends_on
        self.is_prerequisite=[]
        self.how_to_make=how_to_make
        self.also_updates=also_updates
        self.is_uptodate=False

class InferenceEngine:
    """ XXX document me! """
    def __init__(self,
                 entities=[]):
        self.inference_entity_by_name = {}
    
        for e_desc in entities:
            ie = apply(InferenceEntity,(),e_desc)
            self.inference_entity_by_name[ie.name]=ie

        # Introduce prerequisite back-links:
            
        for name in self.inference_entity_by_name.keys():
            ie=self.inference_entity_by_name[name]
            for dep in ie.depends_on:
                if not self.inference_entity_by_name.has_key(dep):
                    raise InferenceError,"'%s' depends on '%s', but the latter is unknown!" % (name,dep)
                ie_dep=self.inference_entity_by_name[dep]
                ie_dep.is_prerequisite.append(name)

    # When we invalidate a resource, we actively percolate the "no
    # longer valid" information to all its children. This may be a bit
    # simple-minded, but is just what we need here.
                
    def invalidate(self,name):
        # We need tree recursion here.
        # How do we ensure we do not perform any unnecessary work?
        # Answer: we make use of the guarantee that if invalidate()
        # returns, all the (recursive) dependents on the entity will
        # have been set to invalid as well.

        if not self.inference_entity_by_name.has_key(name):
            raise InferenceError,"Inference engine does not know how about '%s'" % name

        ie = self.inference_entity_by_name[name]

        if ie.is_uptodate:
            # If it was up-to-date, we change it to not-up-to-date,
            # and percolate this to its children.
            ie.is_uptodate=False
            log.debug("DDD No longer up-to-date: '%s'" % name)
            for d_name in ie.is_prerequisite:
                self.invalidate(d_name)
        else:
            # If it already was invalid, its children will
            # have been invalidated as well.
            pass
            
    def _make(self,name,make_args):
        if not self.inference_entity_by_name.has_key(name):
            raise InferenceError,"Inference engine does not know about '%s'" % name


        
        ie = self.inference_entity_by_name[name]
        if ie.is_uptodate:
            log.debug("DDD '%s' is up-to-date" % name)
        else:
            for n in ie.depends_on:
                self._make(n,make_args)
            log.debug( "DDD making '%s'" % name)

            for m in ie.how_to_make:
                log.debug( "DDD apply - m='%s', make_args='%s'" % (repr(m),repr(make_args)))
                sys.stdout.flush()
                apply(m,(),make_args)

            for a in ie.also_updates:
                self.inference_entity_by_name[a].is_uptodate=True
            
            ie.is_uptodate=True

    def make(self,name,**make_args):
        self._make(name,make_args)

    
