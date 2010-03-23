#!/bin/sh
# This script is used by the main makefile to detect compilation conflicts

BLACKLIST="make-it.sh"
TOUCH="/tmp/nmag_make_process_ID"

# The idea is simple: check if there are running processes
# having the name 'make-it.sh'
for f in $BLACKLIST; do
  NUM_COMPILATIONS=$(ps aux | grep $f | grep -v "grep $f" | wc -l)
  if [ $NUM_COMPILATIONS -ge 1 ]; then
    echo "Conflict with $f. The conflicting process is:"
    ps aux | grep $f | grep -v "grep $f"
    exit 1
  fi
done

# The idea is the following: the file $TOUCH contains the ID of the last
# make process which executed our main Makefile.
# We read this file and check if this process is still running.
# If it is not running we go on: we find the ID of the process which
# launched this script and we save it into $TOUCH, giving reading priviledges
# to all users. Then we start to build the program!
if [ -r $TOUCH ]; then
  MAKE_PROCESS_ID=$(cat $TOUCH)
  NUM_COMPILATIONS=$(ps -e -o pid | grep $MAKE_PROCESS_ID | wc -l)
  if [ $NUM_COMPILATIONS -ge 1 ]; then
    echo "Compilation conflict! The conflicting process is:"
    ps aux | grep $MAKE_PROCESS_ID | grep -v "grep $MAKE_PROCESS_ID"
    exit 1
  fi
fi

# Here we try to detect the PID of the make process which launched this script
# This is a very dirty trick: if there is a more elegant solution, let me know!
ME=$$
PARENT_CHILD_LIST=$(ps -e -o ppid,pid  | grep $$ | sed -r -e 's/[ ]+/,/g')
for PARENT_CHILD in $PARENT_CHILD_LIST; do
  PARENT=$(echo $PARENT_CHILD | cut -d , -f 1)
  CHILD=$(echo $PARENT_CHILD | cut -d , -f 2)
  if [ $CHILD -eq $ME ]; then
    echo $PARENT > $TOUCH
    chmod g+rw $TOUCH
  fi
done
exit 0
