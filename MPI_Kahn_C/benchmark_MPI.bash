for nb_iter in $(seq 5 10)
do    
    N=$(python -c "print(10**$nb_iter)");
    printf "N = $N\n" ;
    for w in $(seq 1 4)
    do
	    printf "\tnb processus = $w\n" ;
	    /usr/bin/time -f "\t\t%E" ./test_pi -np $w $N;
    done
done