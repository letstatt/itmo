#!/bin/bash

> report2.log

i=0
array=()

while true
do
  let i=i+1
  let mod=(i%100000)
  array+=(1 2 3 4 5 6 7 8 9 10)
  
  if [[ $mod -eq 0 ]]
  then
    let cnt=$i*10
    echo "$cnt elements" >> report2.log
  fi
done
