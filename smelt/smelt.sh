#!/bin/sh

# Smelt wrapper script.
dn="`dirname $0`"

if [ -e "$dn/smelt" ]
then
  exec "$dn/smelt" $*
fi

sml @SMLcmdname=$0 "@SMLload=$dn/heap" $*
