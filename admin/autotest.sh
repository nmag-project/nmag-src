#!/bin/bash

TARBALL="$1"
TESTDIR="temporary-test-directory"
REPORTFILE="$2"

function msg {
  echo $*
}

function usage_and_exit {
  echo "Usage: autotest.sh nmagtarball.tar.gz outputfile.txt"
  exit 1
}

if [ x"$TARBALL" == x ]; then
  usage_and_exit
fi

if [ ! -f "$TARBALL" ]; then
  echo "Cannot find tarball \"$TARBALL\"."
  usage_and_exit
fi

if [ x"$REPORTFILE" == x ]; then
  echo "Need output report file."
  usage_and_exit
fi

if [ -d "$TESTDIR" ]; then
  echo "Test directory $TESTDIR alredy exists: cannot proceed!"
  echo "Remove the directory and try again."
  exit 1
fi

msg "Creating new directory $TESTDIR"
mkdir -p $TESTDIR
ln $TARBALL $TESTDIR/tarball.tar.gz
touch $REPORTFILE
ln $REPORTFILE $TESTDIR/report.txt

msg "Entering $TESTDIR"
pushd $TESTDIR

msg "Untarring and compiling..."
(tar xzvf tarball.tar.gz >compilation.log && cd nmag* && make) >compilation.log 2>&1
MAKE_EXST=$?

if [ $MAKE_EXST == 0 ]; then
  msg "Running 'make checkall'..."
  (cd nmag* && make checkall) >checkall.log 2>&1
  MAKE_CHECKALL_EXST=$?
fi

msg "Packing test results"
echo "Compilation: $MAKE_EXST" >report.txt
echo "Tests: $MAKE_CHECKALL_EXST" >>report.txt

for LOGFILE in *.log; do
  echo                     >>report.txt
  echo "-----------------" >>report.txt
  echo "$LOGFILE"          >>report.txt
  echo "-----------------" >>report.txt
  cat $LOGFILE             >>report.txt
done

msg "Exiting temporary directory"
popd

msg "Removing temporary directory"
rm -rf $TESTDIR

msg "Compressing log file"
gzip $REPORTFILE

