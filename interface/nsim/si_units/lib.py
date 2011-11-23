"""
Support for Physical quantities carrying SI units (library function)

"""

__docformat__="restructuredtext"

import dim_parser

import types

def str_of_SI_vector(vector):
    # We don't want to fail just because we didn't manage to 'pretty-print'
    # a vector: this would be quite annoying.
    return _unsafe_str_of_SI_vector(vector)
    try:
        return _unsafe_str_of_SI_vector(vector)
    except:
        return str(vector)

def scalars_are_compatible(a, b):
    """Return True if two scalars are compatible."""
    # 0 is compatible with everything
    try:
        if a.is_compatible_with(b): return True
        return False
    except:
        pass
    try:
        if b.is_compatible_with(a): return True
        return False
    except:
        pass

    number_type = [int, float]
    if type(a) in number_type and type(b) in number_type: return True
    return False

def _unsafe_str_of_SI_vector(vector):
    try:
        # Check that components have the same units
        if len(vector) >= 2:
            for i in range(len(vector)-1):
                if not scalars_are_compatible(vector[i], vector[i+1]):
                    return str(vector)

    except TypeError:
        return str(vector)

    units = SI(1)
    units_str = ""
    for vi in vector:
        try:
            units = vi.get_units()
            units_str = units.dens_str()
            break
        except:
            pass
    numbers = map(lambda x : "%g" % float(x/units), vector)
    return "[" + " ".join(numbers) + ']' + units_str

# mf: don't know a better way to avoid round-off errors in __pow__ method
def float_is_integer(x):
    """
    Return true if float x is an integer.

    More precisely: return true if x deviates less than one significant
    bit from an integer.

    float_is_integer(x) should return true only when x differs from an integer
    by the less significant bit in the machine representation of the floating
    point number. For example:

    in base 10 you can write 1 as 0.99999999999...
    in base 2, 1 is (binary)0.11111111111...

    the machine however has to truncate the number, therefore the statement::

      print 1 == (binary)0.11111...1

    will print false. The function defined here solves this problem.

    You can see it in another way: try to execute the python program::

      for i in range(1, 1000): print (1.0/i)*i == 1.0

    one would expect that only true is printed on the screen. Actually this does
    not happen due to truncation errors. This function solves the problem::

      for i in range(1, 1000): print float_is_integer((1.0/i)*i)

    will print always true.

    This is done in a tricky way: we have to check that the fractional part of
    the number is all zero except for the less significant bit. This is done by
    dividing this deviation by 2 summing 1 to it and comparing the result to 1.
    """
    return (round(x) - x)*0.5 + 1.0 == 1.0

class Physical(object):
    """
    Physical quantity in the SI units system.

    This class allows to associate SI-dimensions (such as meter,
    kilogram, Ampere, seconds, candela and mol) with a floating point
    number.

    The resulting object supports addition, subtraction, (which fails if
    the dimensions of the objects in a sum or difference disagree),
    multiplication and division.

    There are different ways to create objects:

    1. The most fundamental approach is to
       provide a value and a list of pairs where each pair is
       a character identifying the SI base unit and an integer that
       provides its power.

       Examples:

       A. ``v = SI(10,['m',1,'s',-1])`` is the code to create an SI object v
          that represents 10 m/s.

       B. ``B = SI(0.6,['kg',1,'s',-2,'A',-1])`` is the code to create an SI
          object T that represents  0.6 kg/(s^2 A) (i.e. 0.6 Tesla)

    2. A more convenient way is to first define all the base units
       like this (these are already defined in the ``si`` submodule, so
       instead of the following lines below, we could also just write:
       ``from si import meter,second,Ampere``)::

         meter = SI(1,'m') # alternative spelling: metre
         second = SI(1,'s')
         Ampere = SI(1,'A')

       and then to use these SI objects to create more complex
       expressions::

         v = 10*meter/second
         B = 0.6*kilogram/second**2/Ampere

       Of course, short hand notations can be defined such as::

         T = kilogram/second**2/Ampere
         B = 0.6*Tesla

    3. Finally, there is another convenient way:

       Instead of a SI dimension vector as in (1), it is possible to pass
       a string specifying dimensions. Examples are:

       "A/m", "V/m", "J/m^3", "m^2 s^(-2)", "m^-3 s^-1" etc.

       The dimensions parser will understand (in addition to m, kg, s, A, K, mol, cd):
       J, N, W, T, V, C, Ohm, H

    A very basic demonstration of the SI object in use::

        >>> a = SI(1,'m')
        >>> b = SI(1e-3,'m')
        >>> print a+b
        <SI: 1.001  m >
        >>> print a*b
        <SI: 0.001  m^2 >
        >>> print a/b
        >>> <SI: 1000  >           #Note that this is dimensionless
                                   #because we divided meters by meters

    """

    _si_unit_name_to_posn={"m":0,"kg":1,"s":2,"A":3,"K":4,"mol":5,"cd":6}
    _si_posn_to_unit_name=["m","kg","s","A","K","mol","cd"]
    _zero_dims = [0 for _ in _si_posn_to_unit_name]
    _derived_si_units = ["J","N","W","T","V","C","Ohm","H"]
    _known_units_parser=_si_posn_to_unit_name + _derived_si_units

    _definitions_derived_si_units = {
        "J": [2,1,-2,0,0,0,0],
        "N": [1,1,-2,0,0,0,0],
        "W": [2,1,-3,0,0,0,0],
        "T": [0,1,-2,-1,0,0,0],
        "V": [2,1,-3,-1,0,0,0],
        "C": [0,0,1,1,0,0,0],
        "Ohm": [2,1,-3,-2,0,0,0],
        "H": [2,1,-2,-2,0,0,0],
        }

    def __init__(self,
                 value,
                 dimensions=[]
                 # a string (to be parsed) or a vector of alternating entries unit,power,
                 # e.g. ["m",2,"kg",-1,"s",-2]
                 ):

	"""
        Create a Physical object, see documentation of the SI class.

        """

        if dimensions == [] and type(value) == types.StringType:
            # We were given arguments as in SI("m/s")
            dimensions = value
            value = 1.0

        self._value = float(value) # force float

	if type(dimensions) == types.StringType:
            dimensions_raw = dim_parser.parse(dimensions, self._known_units_parser)

            self._dims=dimensions_raw[0:len(self._si_posn_to_unit_name)]

            for (power,name) in zip(dimensions_raw,self._known_units_parser):
                try:
                    defn = self._definitions_derived_si_units[name]
                    for i in range(len(defn)):
                        self._dims[i] += power*defn[i]
                except KeyError:
                    # name was m,s,kg, etc. We cannot encounter anything exotic,
                    # as this would have been recognized by the ML parser.
                    pass
        else:
            self._dims=[0 for x in range(0,len(self._si_posn_to_unit_name))]
            nr_dims=len(dimensions)
            if nr_dims % 2 <> 0:
                raise ValueError("Physicalquantity: Bad dimensions given! "
                                 "(%s)" % str(dimensions))
            nr_entries=nr_dims/2

            for i in range(0,nr_entries):
                key = dimensions[2*i]
                if not(Physical._si_unit_name_to_posn.has_key(key)):
                    raise \
                      ValueError("Physicalquantity: Bad dimensions given!"
                                 " You provided '%s' but known types are '%s'"
                                 % (key,
                                    Physical._si_unit_name_to_posn.keys()))
                self._dims[Physical._si_unit_name_to_posn[key]]=0+dimensions[2*i+1]
                # 0+... is a quick hack to ensure we throw an error if we did
                # not receive a number

    def __str__(self):
        """ 'Nice' presetation of units object. See also method dens_str() if you are short of space"""
        def name_power(index,value,want_positive):
            if value==0:
                return ""
            if (value>0 and not(want_positive)) or (value<0 and want_positive):
                return ""
            abs_val=abs(value)
            if abs_val==1:
                return " %s" % Physical._si_posn_to_unit_name[index]
            return " %s^%d"%(Physical._si_posn_to_unit_name[index],abs_val)

        pos_powers="".join([name_power(x,self._dims[x],True) for x in range(0,7)])
        neg_powers="".join([name_power(x,self._dims[x],False) for x in range(0,7)])
        powers=pos_powers
        if neg_powers <> "":
            powers="%s /%s"%(pos_powers,neg_powers)
        repr_str="<SI: %g %s >" %(self._value,powers)
        return repr_str

    def __repr__(self):
        """Returns a representation of the object that can be used to re-instatiate this
        object using eval. Example::

          M = SI(1e6,["A",1,"m",-1])
          asstring = repr(M)
          M2 = eval(asstring)

        Now M2 is an SI object with values as M.

	More information and other represenations:

	Example:

          >>> H=SI(1e6,["A",1,"m",-1])
          >>> print H
          <SI: 1e+06  A / m >
          >>>str(H)
          '<SI: 1e+06  A / m >'
          >>> repr(H)
          "SI(            1000000,['m',-1,'A',1])"
	  >>>
	  >>> #Note that we print the value with many digits to
	  >>> #ensure that we serialise all significant digits of
	  >>> #the float.
	  >>>
          >>> H.dens_str()
          '<1e+06A/m>'

        """
        def list2str(a):
            """override repr(list) as repr(list) contains too many spaces"""
            return  "["+",".join(map(repr,a))+"]"


        output = "SI("
        if self.value == 1.0:
            output += "1"
        else:
            output += "%.17g" % self.value
        output += ","+list2str(self.units)+")"
        return output

    def dens_str(self, angles=True):
	"""Provide a dense string describing the object

	Example:

  	  >>> H=SI(1e6,["A",1,"m",-1])
          >>> print H
          <SI: 1e+06  A / m >
          >>>str(H)
          '<SI: 1e+06  A / m >'
          >>> repr(H)
          "SI(            1000000,['m',-1,'A',1])"
          >>> H.dens_str()
          '<1e+06A/m>'
	"""

        def name_power(index,value,want_positive):
            if value==0:
                return ""
            if (value>0 and not(want_positive)) or (value<0 and want_positive):
                return ""
            abs_val=abs(value)
            if abs_val==1:
                return "%s" % Physical._si_posn_to_unit_name[index]
            return "%s^%d"%(Physical._si_posn_to_unit_name[index],abs_val)

        pos_powers = "".join([name_power(x, self._dims[x], True)
                              for x in range(0, 7)])
        neg_powers = "".join([name_power(x, self._dims[x], False)
                              for x in range(0, 7)])
        powers = pos_powers
        if neg_powers <> "":
            powers = "%s/%s" % (pos_powers, neg_powers)
        if self._value in [1.0, 1]:
            repr_str = powers if powers != "" else "1"
        else:
            repr_str = "%g%s" % (self._value, powers)
        return ("<%s>" % repr_str if angles else repr_str)

    def _check_type(self,x):
        """Internal checking of type"""
        if not isinstance(x,Physical):
            if type(x) in [types.FloatType,types.IntType]:
                x = Physical(float(x),[])
            else:
                raise ValueError("Expected Physical quantity or float or int "
                                 "but got: %s (type=%s)" % (x,type(x)))
        return x

    def __float__(self):
        """Return the value of Physical Object"""
        if self._value == 0.0: return 0.0
        if not self.is_compatible_with(Physical(1, "")):
            raise ValueError("Objects of this type can be converted to float "
                             "only when they are dimensionless! This object "
                             "however has dimension: " + str(self))
        return self.value

    def _comparison_operator(self, x, operator):
        """Generic implementation of a binary operation
        between Physical objects"""
        x = self._check_type(x)
        if self._dims == x._dims:
            return operator(self._value, x._value)
        else:
            if self._value == 0.0 or x._value == 0.0:
                return operator(self._value, x._value)
            else:
                raise ValueError("Expected Physical quantities of matching "
                                 "dimensions. Operands are %s and %s"
                                 % (repr(self), repr(x)))

    def _unary_operator(self, operator):
        """Generic implementation of unary operator on Physical objects"""
        result = self.copy()
        result._value = operator(result._value)
        return result

    def _binary_operator(self, x, operator):
        """Generic implementation of a binary operation
        between Physical objects"""
        x = self._check_type(x)
        if self._dims == x._dims:
            result = self.copy()
            result._value = operator(result._value, x._value)
            return result
        else:
            if self._value == 0.0:
                result = x.copy()
                result._value = operator(result._value, x._value)
                return result
            elif x._value == 0.0:
                result = self.copy()
                result._value = operator(result._value, x._value)
                return result
            else:
                raise ValueError("Expected Physical quantities of matching "
                                 "dimensions. Operands are %s and %s"
                                 % (repr(self), repr(x)))

    def __abs__(self):
        """Return the absolute value of the Physical object,
           with the same units."""
        return self._unary_operator(lambda x: abs(x))

    def __neg__(self):
        """Unary operator -"""
        return self._unary_operator(lambda x: -x)

    def __pos__(self):
        """Unary operator +"""
        return self.copy()

    def __lt__(self, x):
        """Comparison operator <"""
        return self._comparison_operator(x, lambda x, y: x<y)

    def __le__(self, x):
        """Comparison operator <="""
        return self._comparison_operator(x, lambda x, y: x<=y)

    def __gt__(self, x):
        """Comparison operator >"""
        return self._comparison_operator(x, lambda x, y: x>y)

    def __ge__(self, x):
        """Comparison operator >="""
        return self._comparison_operator(x, lambda x, y: x>=y)

    def __eq__(self, x):
        """Comparison operator =="""
        if not isinstance(x, Physical): return False
        return self._comparison_operator(x, lambda x, y: x==y)

    def __ne__(self, x):
        """Comparison operator !="""
        if not isinstance(x, Physical): return True
        return self._comparison_operator(x, lambda x, y: x!=y)

    def __add__(self, x):
        """Addition operator"""
        return self._binary_operator(x, lambda x, y: x+y)

    __radd__ = __add__

    def __sub__(self,x):
        """Subtraction operator"""
        return self._binary_operator(x, lambda x, y: x-y)

    __rsub__ = __sub__

    def __mul__(self,x):
	"""Multiplication operator"""
	x = self._check_type(x)
        result=self.copy()
        result._value=result._value*x._value
        result._dims=[self._dims[i]+x._dims[i] for i in range(0,7)]
        return result

    __rmul__ = __mul__

    def __div__(self,x):
	"""Division operator"""
	x = self._check_type(x)
        result=self.copy()
        result._value=result._value/x._value
        result._dims=[self._dims[i]-x._dims[i] for i in range(0,7)]
        return result

    def __rdiv__(self,x):
	"""Division operator"""
	x = self._check_type(x)
        result=self.copy()
        result._value=x._value/result._value
        result._dims=[x._dims[i]-self._dims[i] for i in range(0,7)]
        return result

    #def __pow__(self,exponent):
        #if not isinstance(exponent,int):
            #ValueError,"Can only take to the power of integers"
        #res = self.copy()
        #if exponent == 0:
            #res = Physical(1,"")
        #for i in range(1,abs(exponent)):
            #res = self*res
        #if exponent < 0 :
            #res = Physical(1.0,[])/res
        #return res

    def __pow__(self, exponent):
	"""Expontiation operator"""
        result = Physical(0, [])
        result._value = self._value**exponent
        result._dims = []
        for i in range(0, 7):
            new_dim = self._dims[i] * exponent
            result._dims.append(int(round(new_dim)))
            if not float_is_integer(new_dim):
                raise ValueError("Exponentiation with non-integer exponent "
                                 "would lead to fractional units in the "
                                 "result.")
        return result

    def copy(self):
	"""Create new instance of this object"""
        new=Physical(0,[])
        new._value=self._value
        new._dims=self._dims
        return new

    def extract(self,unit_quantity):
	"""(deprecated) --> use 'in_units_of' instead"""
        raise NotImplementedError("Physical.extract has been renamed "
                                  "to Physical.in_units_of")

    def get_units(self):
        """Returns the units of the SI object as an SI object."""
        result = self.copy()
        result._value = 1.0
        return result

    def is_compatible_with(self, physical_quantity):
        """
        Returns True when the given physical quantity is compatible with the object
        itself.

        Example::

          >>> from nsim.si_units import SI
          >>> m_per_sec = SI(1,'m')/SI(1,'s')
          >>> km_per_hour = SI(1000,'m')/SI(3600,'s')
          >>> Newton = SI(1,'kg')*SI(1,'m')/SI(1,'s')**2
          >>> m_per_sec.is_compatible_with(Newton)
          False
          >>> m_per_sec.is_compatible_with(km_per_hour)
          True

        """
        # NOTE: zero is compatible with everything.
        if type(physical_quantity) in [int, float]:
            return ((physical_quantity == 0)
                    or reduce(lambda x, y: x and (y == 0), self._dims, True))

        elif self._value == 0.0:
            return True

        elif isinstance(physical_quantity, SI):
            return (physical_quantity._dims == self._dims)

        else:
            # If we get here, it means that 'physical_quantity' is not an
            # int/float nor a Physical object. The last thing we can do is
            # to try to transform it to a float and - if that succeeds -
            # try again.
            try:
                x = float(physical_quantity)

            except:
                return False

            return self.is_compatible_with(x)


    def in_units_of(self,unit_quantity):
	"""
        The object will be expressed in multiplies of
        'unit_quantity'. This is useful to convert from one
        measurement convention (such as m/s) to another one (such as
        km/h). The return value is just a float.

        The units of 'unit_quantity' have to be compatible with the
        units of the object itself (otherwise an exception is raised).

        A simple example::

          >>> d = SI(10,'m')
          >>> inch = SI(2.54e-2,'m')
          >>> d.in_units_of(inch)
          393.70078740157478

        Another example::

          >>> m = SI(1,'m')
          >>> s = SI(1,'s')
          >>> velocity=2*m/s
          >>> print velocity
          <SI: 2  m / s >
          >>> km = 1000*m
          >>> h = 3600*s
          >>> print velocity.in_units_of(km/h)
          8.2

        :Parameters:
          `unit_quantity` : SI Object
            The SI object itself (i.e. ``self``) will be expressed in
            multiplies of this ``unit_quantity``.  `

        :Returns:
          float
            This is the number that, multiplied by the ``unit_quantitity`` will
            provide the SI quantity of the object itself.

        """
        if unit_quantity._dims <> self._dims:
            raise ValueError("Physical extraction - incompatible dimensions: "
                             "wanted: %s have: %s" %(unit_quantity, self))
        return (self._value/unit_quantity._value)


    def _get_value(self): return self._value

    _property_value_doc__ = """
        Read-only attribute to obtain (dimensionless) value of Physical Object

        :Returns:
          `value` : float
            The numerical value.

        Example:

          >>> from nmag import SI
          >>> H = SI(10, 'A/m')
          >>> print H.value
          10.0
          >>> print H
          <SI: 10  A / m >
        """

    value = property(_get_value,doc=_property_value_doc__)
    """Return value of the object (i.e. float without units)"""

    def _get_units(self):
        """Return internal representation of units (can be used to serialise the object)."""
        result = []
        for unit,power in zip(Physical._si_posn_to_unit_name,self._dims):
            if power == 0:
                continue
            result += [unit,power]
        return result

    units = property(_get_units,doc="Read-only attribute to obtain units of Physical Object (returned as list of pairs of dimension name and power)" )


SI = Physical

#examples:
#if __name__ == "__main__":
#
#    time=SI(0.5,["s",1])
#
#    kg = SI(1.0,["kg",1])
#    m = SI(1.0,["m",1])
#    A = SI(1.0,["A",1])
#    s = SI(1.0,["s",1])
#
#    force=SI(20,["m",1,"kg",1,"s",-2])
#    force = 20*m*kg/s**2
#    print force
#
#    print force
#    print time
#    print time.in_units_of(SI(1,["s",1]))
#
#    momentum=force*time
#
#    print momentum
#
#    print momentum.in_units_of(SI(1,["kg",1,"m",1,"s",-1]))
#
#    import math
#    mu0 = 4*math.pi*1e-7*kg*m/A**2/s**2;
#
#    if False:
#        print "The next line fails"
#        print momentum.in_units_of(["kg",1,"m",1,"s",-2])
#
#    print "Can also use this to define what simulation units we use:"
#    conv_L = SI(1e-9,["m",1])
#
#    my_x = SI(45e-9,["m",1])
#
#    print "This provides dimensionless simulation units"
#    my_x_insimunits = my_x.in_units_of(conv_L)
#    print my_x_insimunits
#
#    print "If the user requests it, we can return SI units by multiplying with the conversion factor"
#
#    print my_x_insimunits * conv_L
#    M = SI(10,"A / m")
#    print 100*M
#
#
#    print M*3
#    print 3*M
#    print M/3
#
#    #check eval(repr):
#    unitstring = repr(M)
#    M2 = eval(unitstring)
#    print unitstring
#    print M2
#    print M2-M
#
#    print M,"->",repr(M)
#
#
#    import si
#    print "Oersted:",si.Oe
#    print "Oersted:",si.Oe.dens_str()
