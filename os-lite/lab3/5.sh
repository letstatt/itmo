#!/bin/bash

if [[ -n "$1" ]]
then
  echo "writer started"
  while true
  do
    read line
    echo "$line" > my_pipe
  done
  exit
fi

echo "reader started"
val=1
op='+'

tail -f my_pipe | while true
do
  read line
  if [[ "$line" == "QUIT" ]]
  then
    killall "5.sh"
    exit
  elif [[ "$line" == "+" || "$line" == "*" ]]
  then
    op="$line"
    echo "operator $line"
  elif [[ "$line" =~ ^[0-9]+$ ]]
  then
    let tmp=$val$op$line
    echo "do math: val $op operand = $val $op $line = $tmp"
    val=$tmp
  else
    echo "not a number! shutdown..."
    killall "5.sh"
  fi
done
