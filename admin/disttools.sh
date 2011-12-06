#!/bin/bash

. settings.conf

function msg {
  echo $*
}

function fatalerr {
  echo "FATAL ERROR: $*."
  exit 1
}

function get_version {
  (cd "$1" && cd interface/nsim && $PYTHON version.py)
}

function untar_pkg_file {
  LOG_FILE="$1"
  if [ -f $PKGS_FILE ]; then
    echo "Found package archive in $PKGS_FILE: using this!"
    tar xvf $PKG_FILE &> $LOG_FILE

  else
    echo "Couldn't find $PKGS_FILE, retrieving it from $REMOTE_MACHINE"
    echo "I may need you to enter the password for the remote machine."
    ssh $REMOTE_MACHINE "cat $PKGS_FILE" | tar xv
  fi
}

function allsrc_dev_compose {
  # This function is used to put together the all-sources nsim package
  # (it does just the minimum which is required in order to start the
  # compilation)
  # USAGE: allsrc_dev_compose nmag-0.1 branch
  #   will create a directory nmag-0.1 with a checkout of the nsim all-sources
  #   build system and include a directory nmag-0.1/nsim with the checkout of
  #   the specified branch of nsim.
  # NOTE: this script is intended to be used by the Nsim core developers
  #  to produce a standalone directory which still can be used to commit
  #  with SVN (the .svn directory are not removed!)
  ALLSRC_DIR_NAME=$1
  MAIN_TAG=${2:-tip}
  DEV_TAG=${3:-tip}

  # File where to put version control info
  INFOFILE=nsim/interface/nsim/info.py

  STARTING_DIR=`pwd`

  msg "NOTE: output of each command is redirected to $LOG_FILE"
  rm -rf $TEMPDIR $ALLSRC_DIR_NAME

  mkdir $TEMPDIR && cd $TEMPDIR && \
  msg "Checking out the all-source build system..." && \
  $VC_CHECKOUT $REPOS_NSIM_DIST --rev="$DEV_TAG" nmag >> $LOG_FILE && \
  (cd nmag && make hierarchy) && \
  msg "Checking out the nsim repository..." && \
  $VC_CHECKOUT $REPOS_NSIM_MAIN --rev="$MAIN_TAG" nsim >> $LOG_FILE && \
  msg "Marking repositories with versions" && \
  echo "dist_mode = 'all-source'" >> $INFOFILE && \
  echo "dist_date = '`date`'" >> $INFOFILE && \
  echo 'vcinfo = """' >> $INFOFILE && \
  (cd nsim && $VC_INFO) >> $INFOFILE && \
  echo '"""' >> $INFOFILE && \
  echo 'dist_vcinfo = """' >> $INFOFILE && \
  (cd nmag && $VC_INFO) >> $INFOFILE && \
  echo '"""' >> $INFOFILE && \
  msg "Unpacking required packages in pkg directory" && \
  mv nsim nmag/ &&
  untar_pkg_file $LOG_FILE
  SUCCESS=$?
  mv $LOG_FILE $STARTING_DIR
  if [ $SUCCESS != 0 ]; then
    msg "FAILED: see $LOG_FILE"

    cd $STARTING_DIR
    false

  else
    msg "SUCCESSFUL: cleaning up, now..."
    mv nmag $ALLSRC_DIR_NAME && mv $ALLSRC_DIR_NAME $STARTING_DIR && \
    cd $STARTING_DIR && rmdir $TEMPDIR
  fi
}

function add_doc {
  DOC_TAG=${2:-tip}
  echo "Adding documentation"
  $VC_CHECKOUT "$REPOS_NSIM_DOC" --rev="$DOC_TAG" "$1/doc/manual" >>$LOG_FILE \
  || fatalerr "Cannot add the documentation to the distribution"
}

function add_test {
  TEST_TAG=${2:-tip}
  echo "Adding test suite"
  $VC_CHECKOUT "$REPOS_NSIM_TEST" --rev="$TEST_TAG" "$1/nsim/tests" >>$LOG_FILE \
  || fatalerr "Cannot add the test suite to the distribution"
}

function build_doc {
  # This function builds the documentation for a given tarball and adds the
  # result to it.

  TARBALL_DIR="$1"
  TMP_DOC_DIR=${2:-doc-build}
  PYTHON_EXEC=${3:-python}

  echo "Building documentation: this requires a number of packages to be" \
       "installed in your Linux distribution."
  echo "Creating temporary directory: $TMP_DOC_DIR"
  mkdir -p "$TMP_DOC_DIR" \
  || fatalerr "Cannot create temporary directory to store documentation"

  echo "Copying the docs..."
  cp -r "$TARBALL_DIR/doc/manual" "$TMP_DOC_DIR/" \
  || fatalerr "Cannot find the documentation sources inside the tarball!"

  echo "Copying nsim sources..."
  cp -r "$TARBALL_DIR/nsim" "$TMP_DOC_DIR/" \
  || fatalerr "Cannot copy nsim sources, which are necessary to build the docs"

  OUTPUT="configure.log"
  echo "Building nsim..."
  echo "Running $PYTHON_EXEC configure.py (output to $OUTPUT)"
  (cd "$TMP_DOC_DIR/nsim" \
   && $PYTHON_EXEC configure.py >$OUTPUT 2>&1) \
  || fatalerr "Failed to configure nsim."

  OUTPUT="make-install.log"
  echo "Making nsim (output to $OUTPUT)"
  (cd "$TMP_DOC_DIR/nsim" \
   && make clean install >$OUTPUT 2>&1) \
  || fatalerr "Failed to make nsim."

  OUTPUT="make-singlehtml.log"
  echo "Bulding documentation: single-page HTML (output to $OUTPUT)"
  (cd "$TMP_DOC_DIR/manual/nmag" \
   && make NSIM_ROOT=../../nsim singlehtml >$OUTPUT 2>&1) \
  || fatalerr "Cannot build the single-page HTML documentation." \
              "Try to do it manually under $TMP_DOC_DIR."

  OUTPUT="make-latexpdf.log"
  echo "Building documentation: PDF (output to $OUTPUT)"
  (cd "$TMP_DOC_DIR/manual/nmag" \
   && make NSIM_ROOT=../../nsim latexpdf >$OUTPUT 2>&1) \
  || fatalerr "Cannot build the PDF documentation." \
              "Try to do it manually under $TMP_DOC_DIR."

  echo "Copying documentation back into tarball..."
  cp "$TMP_DOC_DIR/manual/nmag/_build/latex/NMAGUserManual.pdf" \
     "$TARBALL_DIR/doc/" \
  || fatalerr "Cannot copy PDF of manual (there may have been a problem in" \
              "generating the documentation!"
  rm -rf "$TARBALL_DIR/doc/html"
  cp -r "$TMP_DOC_DIR/manual/nmag/_build/singlehtml" \
     "$TARBALL_DIR/doc/html" \
  || fatalerr "Cannot copy HTML version of the manual (there may have been" \
              "a problem in generating the documentation!"

  echo "Done. Directory $TMP_DOC_DIR is has been kept (to make the " \
       "compilation of the documentation faster in the future)."
}

function remove_hg {
  echo "Removing version control in directory $1"
  rm -r "$1/.hg" \
  || fatalerr "cannot remove version control from $1"
}

function remove_dir {
  MAIN_DIR=$1
  shift 1
  echo "Removing directories $* from $1"
  (cd $MAIN_DIR && for DIR in $*; do rm -r $DIR; done) \
  || fatalerr "cannot remove directories"
}

function remove_file {
  MAIN_DIR=$1
  shift 1
  echo "Removing files $* from $1"
  (cd $MAIN_DIR && for FILE in $*; do rm $FILE; done) \
  || fatalerr "cannot remove files"
}

function gen_tarball {
  echo "Generating tarball for distribution"
  tar czvf "$1.tar.gz" $1/ >> $LOG_FILE \
  || fatalerr "Cannot generate the tarball."
}

function remove_directory {
  echo "Removing distribution directory"
  rm -r "$1" || fatalerr "Cannot remove the distribution directory."
}

function remove_hg_stuff {
  TARGET="$1"
  echo "Entering $TARGET and removing .hg directories..."
  (cd $TARGET && FILES_TO_REMOVE=`find . -type d -name ".hg"` && \
   rm -rf $FILES_TO_REMOVE)
}

function cleanup_allsrc_for_dist {
  (cd $1 && \
   rm -rf nsim/obsolete nsim/interface/nmesh/doc \
          nsim/interface/nmeshlj/doc nsim/devel)
}

function allsrc_compose {
  (allsrc_dev_compose "$1" "$2" && remove_hg_stuff "$1" && \
   cleanup_allsrc_for_dist "$1")
}
