AC_INIT(nsim, 0.1, fangohr@soton.ac.uk)

AC_PREREQ(2.52)

#AC_CONFIG_SRCDIR([src/compiler.c])
#AC_CONFIG_HEADER([config.h])

# AM_INIT_AUTOMAKE

# Determines the
# AC_CANONICAL_HOST

# Libtool support
# AC_PROG_LIBTOOL

# Checks for programs.
AC_PROG_CC
#AC_CHECK_PROG([GAWK], [gawk], [found], [not found], [$PATH])

AC_CHECK_SIZEOF([int])

AX_COMPILER_VENDOR

# find options for optimized compilation
AX_CC_MAXOPT

if test "$GCC" = "yes"; then
  AX_CHECK_COMPILER_FLAGS(-Wall, [WARN_CFLAGS="-Wall"], [WARN_CFLAGS=""])
  AX_CHECK_COMPILER_FLAGS(-msse, [SSE_CFLAGS="-msse"], [SSE_CFLAGS=""])
  # Generate machine specific code?
  PORTABLE=yes
  save_CFLAGS=$CFLAGS
  AX_GCC_ARCHFLAG([PORTABLE], [ARCH_CFLAGS=$ax_cv_gcc_archflag], [])
  CFLAGS=$save_CFLAGS
fi


# Checks for header files.
#AC_HEADER_STDC
#AC_CHECK_HEADERS([stdlib.h string.h unistd.h dlfcn.h])

# Checks for libraries.
AC_CHECK_LIB([m], [floor])
AC_CHECK_LIB([dl], [dlopen])

# Checks for typedefs, structures, and compiler characteristics.
#AC_C_CONST

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_FUNC_REALLOC
#AC_CHECK_FUNCS([floor memmove memset pow rint sqrt strdup strncasecmp strndup])
#AC_CHECK_FUNCS([getenv stat])

AC_SUBST([host_cpu])
AC_SUBST([WARN_CFLAGS])
AC_SUBST([SSE_CFLAGS])
AC_SUBST([ARCH_CFLAGS])
AC_SUBST([SIZEOF_INT], $ac_cv_sizeof_int)

AC_CONFIG_FILES([../arch.py])
AC_OUTPUT
