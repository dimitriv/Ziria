/--input=dummy/ {
    filename = substr($1, 3, length($1) - 6);
    println filename;
  }
/--dummy-samples/ {
    x=index($1, "="); 
    nosamples=substr($1, x+1, length($1));
  }

/Time Elapsed:/ {time = $3;
   print filename " " nosamples / time #" " nosamples " " time
  }



