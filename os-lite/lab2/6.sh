#!/bin/bash

out=`find /proc -maxdepth 1 -type d | awk '{ \
				match($0, "^/proc/[0-9]+"); \
				if (RSTART > 0) { \
					printf "%s/status\n", $0
				}}'`

pid=1
mmax=0

for status in $out
do
  mem=`cat $status 2>/dev/null | awk '{match($0, "VmSize"); if (RSTART > 0) {match($0, "[0-9]+"); print substr($0, RSTART, RLENGTH);}}'`
  if [[ $mem -gt $mmax ]]
  then
    mmax=$mem
    pid=`echo $status | awk '{match($0, "[0-9]+"); print substr($0, RSTART, RLENGTH);}'`
  fi
  #echo $status $mem
done

echo $pid $mmax
