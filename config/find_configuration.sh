#!/bin/sh
# This is another trick to deal with the problem of configuring
# the Nsim sources. Obviously we should do it better, but for now...

USER=$(whoami)
HOST=$(hostname -s)
PLATFORM=$(uname)
CONFIGURATIONS="configuration configuration_$USER""_$HOST""_$PLATFORM configuration_$HOST""_$PLATFORM configuration_$PLATFORM"
CONFIGURATION_FILE="configuration.inc"

for CONFIGURATION in $CONFIGURATIONS; do
  if [ -f $CONFIGURATION ]; then
    echo "Selecting configuration file: $CONFIGURATION"
    rm -f $CONFIGURATION_FILE
    ln -s $CONFIGURATION $CONFIGURATION_FILE
    exit 0
  fi
done

echo "Configuration file not found!"
exit 1
