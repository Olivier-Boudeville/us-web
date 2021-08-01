#!/bin/sh

# Of course using stop-us-web-{native-build,release}.sh shall be preferred:
echo " Killing brutally (not stopping gracefully) any US-Web instance found, and EPMD as well..."

kill $(ps -edf | grep us_web | grep -v run_erl | grep -v grep | awk '{ print $2 }') 2>/dev/null
killall epmd

sleep 1

echo "Resulting US-Web found: $(ps -edf | grep us_web | grep -v grep)"
