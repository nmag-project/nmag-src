
# The dist repository
SVNROOT=svn+ssh://gamma.kk.soton.ac.uk/var/local/svn/nsim/dist
rm -rf nsim-dist
hg convert --datesort --config convert.svn.startrev=0 $SVNROOT nsim-dist

# The www repository (Nmag webpages)
SVNROOT=svn+ssh://gamma.kk.soton.ac.uk/var/local/svn/nsim/www
rm -rf nsim-www
hg convert --datesort --config convert.svn.startrev=0 $SVNROOT nsim-www

