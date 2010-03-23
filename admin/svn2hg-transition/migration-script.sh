ECHO=

SOURCES=src
DOC=doc
TESTS=test

SIMULATE=$1
if [ x$SIMULATE = xyes ]; then
  HG_MV=mv
  HG_RM="rm -rf"
  HG_CLONE="cp -r"
  HG_COMMIT="echo Not doing hg commit"
  HG_REPOS_FLATTEN="echo Not doing hg_repos_flatten"
  ORIG=orig

else
  HG_MV="hg mv"
  HG_RM="hg rm"
  HG_CLONE="hg clone"
  HG_COMMIT="hg commit"
  HG_REPOS_FLATTEN=hg_repos_flatten
  ORIG=hg-orig
fi

function hg_repos_flatten {
  SRC=$1
  DST=$2
  TMP=$DST-tmp
  hg clone $SRC $TMP
  hg -R $TMP update 0
  hg -R $TMP revert -r tip --all
  hg -R $TMP commit -m "Creating a history-free repository" 
  hg clone -r tip $TMP $DST
  rm -rf $TMP
  hg -R $SRC pull $DST
  hg -R $SRC merge
  hg -R $SRC commit -m "Original repository ready to accept changes from flat one." 
}

function gen_parent_repos {
  SVNROOT=$1
  DEST=$2

  if [ x$SIMULATE = xyes ]; then
    if [ ! -d $ORIG ]; then
      echo "Checking out the latest SVN revision..."
      svn co $SVNROOT $ORIG
    fi

  else
    # First let's convert the SVN repository into one single HG repository
    # containing everything (all the history)
    if [ ! -d $ORIG ]; then
      hg convert --datesort --config convert.svn.startrev=0 \
         --config convert.svn.tags=tags/release $SVNROOT $ORIG
    fi
  fi

  rm -rf $DEST
  $HG_CLONE $ORIG $DEST || exit 1

  if [ x$SIMULATE = xyes ]; then
    # We remove the subversion directories
    (cd $REST && find . -name ".svn" -exec rm -rf {} \;)
  fi

  # Now, we remove some EPS files: these files were simply moved out of the
  # trunk in the SVN repository and this confuses the mercurial conversion
  # extension
  cd $DEST
  $HG_RM interface/nmesh/doc/visualfigures/*.eps

  # Create a directory rest and put everything into it
  EVERYTHING=*
  mkdir rest
  $HG_MV $EVERYTHING rest/

  # Now split the manual and other documentation
  mkdir $DOC
  $HG_MV rest/interface/nmesh/doc $DOC/nmesh
  $HG_MV rest/interface/nmag/manual $DOC/nmag
  $HG_MV rest/interface/nfem/doc $DOC/nfem
  $HG_MV rest/info $DOC/devel

  # Now split the tests
  $HG_MV rest/tests $TESTS

  # Now split the sources
  MANUAL="config/ac site-lib bin hlib/hlibpatch.diff.gz sundials_sp/doc.txt \
          pycaml/tests mpi_petsc/petsc.log fastfields/fastfields.load \
          fastfields/fastfields.test interface/doc \
          interface/nmesh/tests/speed/performance.gnumeric \
          interface/nmeshlib/nmesh.config obsolete/diffop_parser/HACK_TYPES"

  for FILE in $MANUAL; do
    SRC=rest/$FILE
    DST=$SOURCES/$FILE
    echo "Moving (manually) $FILE to dest"
    $ECHO mkdir -p `dirname $DEST`
    $ECHO $HG_MV $SRC $DST
  done

  FILES=`cd rest && find . | grep -E '\.(c|h|ml|mli|mll|mly|py|pl|geo|gnp|in|inc|sh|bash|conf|bz2|tex|lisp)$|(Makefile|makefile|META|NOTES|NOTE|README|STYLE|TODO|changelog.txt)$'`
  for FILE in $FILES; do
    SRC=rest/$FILE
    DST=$SOURCES/$FILE
    echo "Moving $FILE to dest"
    $ECHO mkdir -p `dirname $DEST`
    $ECHO $HG_MV $SRC $DST
  done

  $HG_COMMIT -m "Rearranging the HG repository after converting from SVN."
  cd ..
}

function gen_children_repos {
  # Now create the three repositories
  DEST=$1
  REPDIR=$2
  rm -rf $REPDIR
  mkdir $REPDIR

  # Create the source repository
  ($HG_CLONE $DEST $REPDIR/$SOURCES && \
     cd $REPDIR/$SOURCES && \
     $HG_RM $DOC $TESTS rest && \
     $HG_MV $SOURCES/* . && \
     $HG_COMMIT -m "Creating the source repository from the common repository.")

  # Create the test repository
  ($HG_CLONE $DEST $REPDIR/$TESTS && \
     cd $REPDIR/$TESTS && \
     $HG_RM $DOC $SOURCES rest && \
     $HG_MV $TESTS/* . && \
     $HG_COMMIT -m "Creating the test repository from the common repository.")

  # Create the doc repository
  ($HG_CLONE $DEST $REPDIR/$DOC && \
     cd $REPDIR/$DOC && \
     $HG_RM $TESTS $SOURCES rest && \
     $HG_MV $DOC/* . && \
     $HG_COMMIT -m "Creating the doc repository from the common repository.")
}

function gen_flat_repos {
  REPDIR=$1
  FREPDIR=$2
  rm -rf $FREPDIR
  mkdir $FREPDIR

  for r in $SOURCES $DOC $TESTS; do
    hg_repos_flatten $REPDIR/$r $FREPDIR/$r
  done
}

gen_parent_repos svn+ssh://alpha.kk.soton.ac.uk/var/local/svn/nsim dest
gen_children_repos dest hg-repos
gen_flat_repos hg-repos hg-repos-flat

