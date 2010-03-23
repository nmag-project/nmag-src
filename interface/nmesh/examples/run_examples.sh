for x in *py; do 
    echo "working on $x" 
<<<<<<< run_examples.sh
    ../../../bin/nmesh2 $x > $x.log.stdout; 
=======
    ../../../bin/nsim $x > $x.log.stdout; 
>>>>>>> 1.4
done