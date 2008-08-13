#!/bin/sh

# Squall wrapper script.
dn="`dirname $0`"

if [ -e "$dn/squall" ]
then
  exec "$dn/squall" $*
fi

sml @SMLcmdname=$0 "@SMLload=$dn/heap" $*
