#! /bin/bash
#
# NOT WELL TESTED!!!
#
# This script generates shared libraries from static ones for petsc 2.2.0
# Change the following variables and run this script as root.

# Sets the default values for library directories
LIBDIR_ALL[0]="/sw/lib/gcc4/lib"
ALL_NUM=1
LIBDIR_PETSC="/usr/local/petsc/2.2.0/lib/libg/darwin8.4.0"
LIBDIR_MPICH="/usr/local/mpich2/1.0.3/lib"
LIBDIR_VEC="/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A"
LIBDIR_X="/usr/X11R6/lib"
LIBDIR_FORTRAN=""
LIB_FORTRAN="gfortran" # <-- You may want to change this (gcc4 or gcc3?)

LIB_DEST=""
LIB_DIRS=""

SH_OPTIONS="-dynamiclib -fno-common"
LOGFILE="./make_petsc_dylibs.log"
TMPDIR="make_petsc_dylibs_dir29071979"

SHOW_HELP_AND_EXIT="no"
NO_ARGS="yes"
AUTO="no"

STR_YES="[ yes ]"
STR_NO="[ no ]"
IND="" # indentation

# USAGE: check_if_lib_is_here paths name
# DESCRIPTION: This function checks that 'paths' is a valid directory,
#  and try to locate the file 'name' inside it.
function check_if_lib_is_here {
  if [ "$1" == "" ]; then return 1; fi

  echo -n "$IND""Checking if '$1' is a valid directory... " >>$LOGFILE
  if [ ! -d $1 ]; then
    echo $STR_NO >>$LOGFILE
    return 1
  fi
  echo $STR_YES >>$LOGFILE

  echo -n "$IND""  Is there the library we are looking for? " >>$LOGFILE
  CURPATH=$(pwd)
  cd $1
  if [ ! -e $2 ]; then
    cd $CURPATH
    echo $STR_NO >>$LOGFILE
    return 1
  fi
  cd $CURPATH
  echo $STR_YES >>$LOGFILE
  return 0
}

# USAGE: check_if_lib_is_there paths name
# DESCRIPTION: This function checks that 'paths' is a valid directory,
#  and try to locate the file 'name' inside it. If it does not find it,
#  then it tryes to look inside the common directory paths specified
#  inside the array LIBDIR_ALL
function check_if_lib_is_there {
  local IND0=$IND
  local IND1=$IND"  "
  IND=$IND1
  STR_ERROR="'$2' NOT FOUND"

  echo >>$LOGFILE
  echo "$IND0""SEARCHING '$2'" >>$LOGFILE
  check_if_lib_is_here "$1" "$2"
  if [ $? -eq 0 ]; then IND=$IND0; return 0; fi

  if [ $ALL_NUM -gt 0 ]; then
    echo "$IND""Checking inside the common directory paths..." >>$LOGFILE
    IND="    "
    for (( i=0; i<$ALL_NUM; i++ )); do
      check_if_lib_is_here "${LIBDIR_ALL[i]}" "$2"
      if [ $? -eq 0 ]; then IND=$IND0; return 0; fi
    done
  fi

  echo $STR_ERROR >>$LOGFILE
  echo $STR_ERROR
  IND=$IND0
  return 1

  if [ "$3" == "yes" ]; then
    echo $IND"Using locate to find the path where '$2' is..." >>$LOGFILE
  fi
}

# USAGE: make_shared_lib name "-lone -ltwo -lthree ..."
# DESCRIPTION: This command generates the library 'libname.dylib'
#  linking it with libone.dylib (or libone.a, if the first is not found),
#  libtwo.dylib (or libtwo.a ...), libthree.dylib (or libthree.a ...).
function make_shared_lib {
  NAME=$1
  DEPENDENCES="$2"
  LIB_NAME="lib$NAME"
  STATIC_LIB="$LIB_NAME.a"
  SHARED_LIB="$LIB_NAME.dylib"
  INSTALL_LIB="$LIB_DEST/$SHARED_LIB"

  echo "Creating '"$SHARED_LIB"' from '"$STATIC_LIB"'..."
  CURDIR=$(pwd)
  mkdir $NAME && {
    cd $NAME

    cp $LIB_DEST/$STATIC_LIB ./ && ar x $STATIC_LIB \
    && cc $SH_OPTIONS -install_name $INSTALL_LIB -o $SHARED_LIB $LIB_DIRS $DEPENDENCES *.o \
    && cp $SHARED_LIB $LIB_DEST/
  }
  if [ $? == 0 ]; then
    echo "Dynamic library was succesfully created and installed!"
    cd $CURDIR
    return 0
  fi

  cd $CURDIR
  echo "Error during the creation of the library!"
  return 1
}

for arg in "$@" ; do
    case $arg in
        -c=*) LIBDIR_ALL[$ALL_NUM]=`echo A$arg | sed -e 's/A-c=//g'`
          ALL_NUM=$[ $ALL_NUM + 1 ]
        ;;
        --common-dir=*) LIBDIR_PETSC=`echo A$arg | sed -e 's/A--common-dir=//g'`
        ;;
        -p=*) LIBDIR_PETSC=`echo A$arg | sed -e 's/A-p=//g'`
        ;;
        --petsc-lib-dir=*) LIBDIR_PETSC=`echo A$arg | sed -e 's/A--petsc-lib-dir=//g'`
        ;;
        -d=*) LIBDIR_DEST=`echo A$arg | sed -e 's/A-d=//g'`
        ;;
        --dest-lib-dir=*) LIBDIR_DEST=`echo A$arg | sed -e 's/A--dest-lib-dir=//g'`
        ;;
        -m=*) LIBDIR_MPICH=`echo A$arg | sed -e 's/A-m=//g'`
        ;;
        --mpich-lib-dir=*) LIBDIR_MPICH=`echo A$arg | sed -e 's/A--mpich-lib-dir=//g'`
        ;;
        -v=*) LIBDIR_VEC=`echo A$arg | sed -e 's/A-v=//g'`
        ;;
        --vec-lib-dir=*) LIBDIR_VEC=`echo A$arg | sed -e 's/A--vec-lib-dir=//g'`
        ;;
        -b=*) LIBDIR_BLAS=`echo A$arg | sed -e 's/A-b=//g'`
        ;;
        --blas-lib-dir=*) LIBDIR_BLAS=`echo A$arg | sed -e 's/A--blas-lib-dir=//g'`
        ;;
        -l=*) LIBDIR_LAPACK=`echo A$arg | sed -e 's/A-l=//g'`
        ;;
        --lapack-lib-dir=*) LIBDIR_LAPACK=`echo A$arg | sed -e 's/A--lapack-lib-dir=//g'`
        ;;
        -x=*) LIBDIR_X=`echo A$arg | sed -e 's/A-x=//g'`
        ;;
        --x-lib-dir=*) LIBDIR_X=`echo A$arg | sed -e 's/A--x-lib-dir=//g'`
        ;;
        -help | --help | -? | -h ) SHOW_HELP_AND_EXIT="yes"
        ;;
        -a | --auto-detect | --auto) AUTO="yes"
        ;;
        *)
          echo "ERROR: Unrecognized option '$arg'"
          echo "Use '$0 -h' for help"
          exit 1
        ;;
    esac
    NO_ARGS="no"
done

if [ "$NO_ARGS" = "yes" ] ; then
  echo "WARNING: you gave no option (use '$0 -h' for help)"
fi

if [ "$SHOW_HELP_AND_EXIT" = "yes" ] ; then
  echo "make_petsc_dylib, written by Matteo Franchin, Feb 2006"
  echo
  echo "This command was written to build shared libraries for petsc under Mac OS X."
  echo "It takes the static libraries ('libpetsc*.a') as input and produces"
  echo "the corresponding dynamic libraries ('libpetsc*.dylib') as output."
  echo "You should find the directory where 'lipetsc.a', 'libpetscvec.a', etc. live and"
  echo "give this paths to this script with the option '--petsc-lib-dir petsc_path'."
  echo "You should also find the path for the following libraries:"
  echo "  1) libmpich.dylib libpmpich.dylib"
  echo "  2) libX11.dylib"
  echo "  3) libgfortran.dylib, ... (depending on how mpich was compiled)"
  echo "  4) libBLAS.dylib"
  echo "  5) libLAPACK.dylib"
  echo "Run this command as root: it will install the created libs under petsc_path."
  echo "Command line options (don't use spaces: '-c=/usr/lib', not '-c = /usr/lib'):"
  echo " -help, --help, -?, -h     show these help notes"
#  echo " -a, --auto-detect      tries to auto-detect the paths of the required libraries"
  echo " -c, --common-dir=path     adds path to find all libraries"
  echo " -p, --petsc-lib-dir=path  uses path to find 'libpetsc.a' and its mates"
  echo " -m, --mpich-lib-dir=path  uses path to find 'libmpich.dylib', 'libpmpich.dylib'"
  echo " -v, --vec-lib-dir=path    uses path to find 'libBLAS.dylib', 'libLAPACK.dylib'"
  echo " -b, --blas-lib-dir=path   uses path to find 'libBLAS.dylib'"
  echo " -l, --lapack-lib-dir=path uses path to find 'libLAPACK.dylib'"
  echo " -x, --x-lib-dir=path      uses path to find 'libX11.dylib'"
  exit 1
fi

LIB_DEST="$LIBDIR_PETSC"
LIB_DIRS=""
if [ -n "$LIBDIR_PETSC" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_PETSC"; fi
if [ -n "$LIBDIR_MPICH" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_MPICH"; fi
if [ -n "$LIBDIR_X" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_X"; fi
if [ -n "$LIBDIR_VEC" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_VEC"; fi
if [ -n "$LIBDIR_BLAS" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_BLAS"; fi
if [ -n "$LIBDIR_LAPACK" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_LAPACK"; fi
if [ -n "$LIBDIR_FORTRAN" ]; then LIB_DIRS="$LIB_DIRS -L$LIBDIR_FORTRAN"; fi
for (( i=0; i<$ALL_NUM; i++ )); do
  if [ -n "${LIBDIR_ALL[i]}" ]; then LIB_DIRS="$LIB_DIRS -L${LIBDIR_ALL[i]}"; fi
done

echo "Checking the system (see '$LOGFILE')..."
rm -rf $LOGFILE

STATUS=0
check_if_lib_is_there "$LIBDIR_PETSC" libpetsc.a "$AUTO" && STATUS=$[ $STATUS + 1 ]
check_if_lib_is_there "$LIBDIR_MPICH" libmpich.dylib "$AUTO" && STATUS=$[ $STATUS + 2 ]
check_if_lib_is_there "$LIBDIR_MPICH" libpmpich.dylib "$AUTO" && STATUS=$[ $STATUS + 4 ]
check_if_lib_is_there "$LIBDIR_X" libX11.dylib "$AUTO" && STATUS=$[ $STATUS + 8 ]
check_if_lib_is_there "$LIBDIR_VEC" libBLAS.dylib "$AUTO" \
|| check_if_lib_is_there "$LIBDIR_BLAS" libBLAS.dylib "$AUTO" && STATUS=$[ $STATUS + 16 ]
check_if_lib_is_there "$LIBDIR_VEC" libLAPACK.dylib "$AUTO" \
|| check_if_lib_is_there "$LIBDIR_LAPACK" libLAPACK.dylib "$AUTO" && STATUS=$[ $STATUS + 32 ]
check_if_lib_is_there "$LIBDIR_FORTRAN" lib$LIB_FORTRAN.dylib "$AUTO" && STATUS=$[ $STATUS + 64 ]

if [ $STATUS == 127 ]; then
  echo "All libraries were found."

else
  echo "Not all libraries were found."
  echo "You can exit and specify new directory paths (try: '$0 -h')."
fi

LINE=""
while true; do
  echo -n "Do you want to continue and create the dynamic libraries (y/n)? "
  read LINE
  if [ "$LINE" == "y" ]; then
    break
  elif [ "$LINE" == "n" ]; then
    exit 1
  fi
  echo "You should press either 'y' or 'n'!"
done

echo "Creating the directory '$TMPDIR'"
INITIAL_DIR=$(pwd)
mkdir $TMPDIR && cd $TMPDIR && {
  make_shared_lib petsc "-lmpich -lpmpich -lX11 -l$LIB_FORTRAN -lBLAS"
  make_shared_lib petscvec "-lmpich -lpmpich -lBLAS -lpetsc"
  make_shared_lib petscmat "-lBLAS -lmpich -lpmpich -lpetsc -lpetscvec"
  make_shared_lib petscdm "-lmpich -lpmpich -lpetsc -lpetscvec -lpetscmat"
  make_shared_lib petscksp "-lLAPACK -lmpich -lpmpich -lpetsc -lpetscvec -lpetscmat -lpetscdm"
  make_shared_lib petscsnes "-lpmpich -lpetsc -lpetscvec -lpetscksp -lpetscmat -lpetscdm"
  make_shared_lib petscts "-lpmpich -lpetsc -lpetscvec -lpetscksp -lpetscmat -lpetscsnes"
  make_shared_lib mpiuni ""
#make_shared_lib petscfortran ""
echo "Deleting the directory '$TMPDIR'"
cd $INITIAL_DIR && rm -r -f $TMPDIR
}
