#!/bin/bash

#N=200000
N=150000
K=$1
i=0

while true
do
  let i=$i+1
  if [[ $i -gt $K ]]
  then
    break
  fi
  exec "./newmem.bash" "$N" &
  sleep 1
done
