#! /bin/bash

line=""

while true
do
  read tmp
  if [[ "$tmp" == "q" ]]
  then
    break
  fi
  echo $tmp
  line=$line$tmp
done

echo $line
