# NOT WELL TESTED!!!
# This script generates shared libraries from static ones for petsc 2.2.0
# Change the following variables and run this script as root.

LIB_DEST="/usr/local/petsc/2.2.0/lib/libg/darwin8.4.0"

LIB_DIRS="-L/sw/lib -L/usr/local/mpich2/1.0.3/lib -L/usr/X11R6/lib -L/sw/lib/gcc4/lib -L/System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A -L/usr/local/petsc/2.2.0/lib/libg/darwin8.4.0"

SH_OPTIONS="-dynamiclib -fno-common"

LIB_FORTRAN="f2c"

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

TMPDIR="make_shared_lib_dir_29071979"
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
