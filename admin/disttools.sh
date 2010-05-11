#!/bin/bash

. settings.conf
. repositories.conf

function msg {
  echo $*
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
  ALLSRC_NSIM_BRANCH=$2

  STARTING_DIR=`pwd`

  msg "NOTE: output of each command is redirected to $LOG_FILE"
  rm -rf $TEMPDIR $ALLSRC_DIR_NAME

  mkdir $TEMPDIR && cd $TEMPDIR && \
  msg "Checking out the all-source build system..." && \
  $VC_CHECKOUT $REPOS_NSIM_DIST nmag >> $LOG_FILE && \
  (cd nmag && make hierarchy) && \
  msg "Checking out the nsim repository..." && \
  $VC_CHECKOUT $REPOS_NSIM_MAIN nsim >> $LOG_FILE && \
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
  echo "Adding documentation"
}

function add_test {
  echo "Adding test suite"

}

function remove_hg {
  echo "Removing version control in directory $1"
}

function gen_tarball {
  echo "Generating tarball for distribution"
}

function remove_directory {
  echo "Removing distribution directory"
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



#remove_svn_stuff 'nmag-0.1'
# allsrc_dev_compose 'nmag-0.1' 'trunk'
#allsrc_compose 'nmag-0.1' 'trunk'

# Create links for nmag manual into toplevel 'doc' dir

#         mkdir -p output/$(NSIM_VERSION)/download
#
#         rm -rf tmp/$(SRCDIR)/nmag/doc/nmag
#         mkdir -p tmp/$(SRCDIR)/nmag/doc/nmag
#         ln -s nsim/interface/nmag/manual tmp/$(SRCDIR)/nmag/doc/nmag
#
#
# echo " *** REMOVING OLD TARBALLS *** "
#
#         rm -f output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)*.tar*
#
# echo " *** CREATING CORE NMAG BUILD ARCHIVE *** "
#
#         tar --directory $(INST_SYSTEMDIR) $(TAR_SVNEXCLUDE) -cvf output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-core.tar nmag
#
# echo " *** ADDING NSIM SOURCES ARCHIVE *** "
#
# echo "tarball" >tmp/$(SRCDIR)/nmag/nsim/interface/nmag/DISTMODE
#
#         tar --directory tmp/$(SRCDIR) $(TAR_SVNEXCLUDE) --exclude=nmag/nsim/info -rf output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-core.tar nmag/nsim nmag/doc
#
#         rm tmp/$(SRCDIR)/nmag/nsim/interface/nmag/DISTMODE
#
#         # This should not be necessary, as we built that manual anyway when we made installation-system:
#         #tar --directory $(INST_SYSTEMDIR) -rf output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-all.tar nmag/INSTALL.pdf nmag/INSTALL.html
#
# echo " *** ADDING MANUAL TO ARCHIVE *** "
#
#         tar --directory tmp/nsim-build $(TAR_SVNEXCLUDE) -rf output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-core.tar nmag/nsim/interface/nmag/manual
#
#         cp output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-core.tar \
#            output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-all.tar
#
# echo " *** ADDING PACKAGE SOURCES TO BIG ARCHIVE *** "
#
#         tar --directory tmp -Af output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-all.tar $(LIBSOURCE_FILE)
#
# echo " *** ZIPPING TARBALLS *** "
#
#         # Gzip final tarballs:
#         gzip -9 output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-core.tar
#         gzip -9 output/$(NSIM_VERSION)/download/nmag-$(NSIM_VERSION)-all.tar
#
