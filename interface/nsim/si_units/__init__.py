"""
Support for Physical quantities carrying SI units

----------------------------------------------------------------------
Support for Physical quantities carrying SI units
----------------------------------------------------------------------

Motivation
----------

Often, we need to store numbers that are associated with physical
units such as meter, seconds, etc. This class"Physical" provides an
object that provides exactly this functionality.

Base units
----------

The basic units are SI units, i.e. m (metre), kg (kilo gram),
s (seconds), A (Ampere), K (Kelvin), mol (mol), cd (Candela).




Usage examples
--------------

(Taken from a session with nsim_i)

::

  In [2]:from nsim.si_units import SI
  
  In [3]:distance = SI(10,"m")
  
  In [4]:time = SI(5,"s")
  
  In [5]:velocity = distance / time
  
  In [6]:print velocity
  <SI: 2  m / s >
  
  In [7]:velocity.value
  Out[7]:2.0
  
  In [8]:velocity.units
  Out[8]:['m', 1, 's', -1]
  
  In [9]:print velocity**2
  Out[9]:<SI: 4  m^2 / s^2 >
  
  In [10]:m = SI(1,'m')
  
  In [11]:s = SI(1,'s')
  
  In [12]:km = 1000*m
  
  In [13]:h = 3600*s
  
  In [14]:velocity.in_units_of(km/h)
  Out[14]:7.1999999999999993
  

Implementation Notes
--------------------

* Internally, Physical numbers are automatically being converted to
  floatingpoint.

* When you want a numerical value, use the extract() method with the
  desired/expected dimensions. You will get an error message if dimensions
  do not match.



To do
-----

An extension which I would like to see is a parser which would allow us
to specify Physical units dorectly in the form "kg m / s^2" rather than
the more clumsy ["kg",1,"m",1,"s",-2] - and make this extensible so it
knows about aliases (e.g. "W" = "kg m^2 / s^3").



:authors: Dr. Thomas Fischbacher, Hans Fangohr, Matteo Franchin

:date: 2006, 2007

"""

__docformat__="restructuredtext"


# Define what gets imported with a 'from fem import *'
__all__ = ['si']

# Load __all__ in nmag namespace so that a simple 'import nmag' gives
# access to them via nmesh.<name>
glob,loc = globals(),locals()
for name in __all__:
    __import__(name,glob,loc,[])

# Namespace cleanup
del name,glob,loc

from lib import *
