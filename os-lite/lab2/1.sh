#!/bin/bash

out=`ps -x`
n=`echo "$out" | wc -l`
let lines=$n-1

echo $lines > ps.out
echo "$out" | awk '{if ($1 != "PID") {printf "%s:", $1; for(i=5;i<=NF;i++) {printf "%s ", $i}; print ""}}' >> ps.out

exit 0
# USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND

out=`ps -ux`
n=`echo "$out" | wc -l`
let lines=$n-1

echo $lines > ps.out
echo "$out" | awk '{if ($1 != "USER") {printf "%s:", $2; for(i=11;i<=NF;i++) {printf "%s ", $i}; print ""}}' >> ps.out
