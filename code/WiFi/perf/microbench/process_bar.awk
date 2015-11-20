/N/ {
    n = substr($1, 3, length($1) - 2);
    #print $0;
    #print n;
  }
/__pcnt_elaps_BLOCK/ {
    block = substr($1, 19, 1);
    thread = substr($1, 27, 1);
    e = $3;
    #print $0;
    #print block, " ", thread, " ", e;
  }

/__pcnt_barrier_elaps_BLOCK/ {
    block1 = substr($1, 27, 1);
    thread1 = substr($1, 35, 1);
    b = $3;
    #print $0;
    if (e > 1000) {
	print "B"block"/T"thread, ": ", b, " ", e, " ", b/(b+e);
    }
  }

/Time Elapsed:/ {time = $3;
    #print $0;
    print n, " ", time;
  }



