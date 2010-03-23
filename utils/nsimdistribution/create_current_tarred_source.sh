#This script runs at one o clock in the morning on nmagtest@eta (in ~/tarredsource).
#
#This is just a backup (in case eta's drive fails).
#
#Hans 25/06/07



name=`date +nmag-allsource-%Y-%m-%d-T%H-%M-%S`


svn co svn+ssh://alpha.kk.soton.ac.uk/var/local/svn/nsimdist/trunk nsimdist
svn update nsimdist

cd nsimdist

#get pkgs.tar.gz from somewhere and place it in nsimdist

#for nigthly build, this is done already


#also need to checkout/update nsim
svn co svn+ssh://alpha.kk.soton.ac.uk/var/local/svn/nsim/trunk nsim
svn update nsim

#untar packages
tar xfv ../pkgs.tar

#then tar the lot
cd ..
mv nsimdist $name

filename=${name}.tar.gz 

tar cfvz $filename $name
mv $name nsimdist

mv $filename /tmp
cp /tmp/$filename /tmp/nmag-source-current.tgz

scp /tmp/nmag-source-current.tgz alpha.kk.soton.ac.uk:/tmp



