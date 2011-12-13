#!/bin/bash

TARBALL=$1

UNTAR="tar xzvf"
TAR="tar czvf"
MKDIR_P="mkdir -p"
CREATETEMPDIR="mktemp -d nmagcore-XXXXXXXX"

function exit_with_usage {
  echo "USAGE: bash build-core.bash pathtotarball"
  echo "  pathtotarball should be the path to an existing nmag tarball."
  echo "  It should have a name like nmag-0.2.0.tar.gz"
  exit 1
}

function exit_with_msg {
  echo $*
  exit 1
}

echo $TARBALL | grep -q nmag || exit_with_usage
echo $TARBALL | grep -q ".tar.gz" || exit_with_usage

OUTDIR=`echo $TARBALL | sed -e 's/.tar.gz//'`

CORETARBALL=`echo $TARBALL | sed -e 's/nmag/nmag-core/'`
COREOUTDIR=`echo $OUTDIR | sed -e 's/nmag/nmag-core/'`

echo "SUMMARY: building tarball $CORETARBALL from $TARBALL"
echo "Press return to continue..."
read

echo "Creating temporary directory"
TEMPDIR=`$CREATETEMPDIR` || exit_with_msg "Cannot create temporary directory"
echo "Created directory: $TEMPDIR"

echo "Untar-ing tarball $TARBALL in $TEMPDIR"
$UNTAR $TARBALL -C $TEMPDIR && UNTARRED=yes
if test "$UNTARRED" = "yes"; then
  
  SUBDIR="$TEMPDIR/$OUTDIR/nsim"
  if [ -d $SUBDIR ]; then
    mv $SUBDIR "$TEMPDIR/$COREOUTDIR"
    (STARTINGDIR=`pwd` && \
     cd $TEMPDIR && \
     $TAR "$STARTINGDIR/$COREOUTDIR.tar.gz" "$COREOUTDIR/") \
    || echo "Cannot create the tarball $COREOUTDIR.tar.gz"

  else
    echo "Failure: cannot find subdirectory $TEMPDIR/$OUTDIR/nsim"
  fi

else
  echo "Failure while untarring the archive"
fi

echo "Removing temporary directory $TEMPDIR"
rm -rf $TEMPDIR
