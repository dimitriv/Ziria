#!/bin/bash

# TravisCI requires the output to be not too verbose, and at the same
# time the build cannot stall for over 10 minutes. This script
# essentially gets around that restriction. The correct thing to
# do in future would be to allow the verbosity of the output to be
# adjusted via flags.

# Abort on Error
set -e

export PING_SLEEP=30s
export WORKDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export BUILD_OUTPUT=$WORKDIR/build.out

touch $BUILD_OUTPUT

dump_output() {
   echo Tailing the last 500 lines of output:
   tail -500 $BUILD_OUTPUT  
}
error_handler() {
  echo ERROR: An error was encountered with the build.
  dump_output
  exit 1
}
# If an error occurs, run our error handler to output a tail of the build
trap 'error_handler' ERR

# Set up a repeating loop to send some output to Travis.

bash -c "while true; do echo \$(date) - building ...; sleep $PING_SLEEP; done" &
PING_LOOP_PID=$!

# Actual commands to be run
if [ $TRAVIS_TEST = "normal" ]
  then make test >> $BUILD_OUTPUT 2>&1
elif [ $TRAVIS_TEST = "pedantic" ]
  then make test-WiFi-pedantic >> $BUILD_OUTPUT 2>&1
else
  echo "TRAVIS_TEST not set !"
  exit 1
fi

# The build finished without returning an error so dump a tail of the output
dump_output

# nicely terminate the ping output loop
kill $PING_LOOP_PID
