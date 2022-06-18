#!/bin/bash

N=$1
i=0
array=()

while true
do
  let i=$i+1
  array+=(1 2 3 4 5 6 7 8 9 10)
  
  if [[ $i -gt $N ]]
  then
    break
  fi
done
