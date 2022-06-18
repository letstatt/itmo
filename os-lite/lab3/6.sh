#!/bin/bash

if [[ -n "$1" ]]
then
  echo "writer started, reader pid=$1"
  while true
  do
    read line
    if [[ "$line" == "+" ]]
    then
      kill -USR1 $1
    elif [[ "$line" == "*" ]]
    then
      kill -USR2 $1
    elif [[ "$line" == "TERM" ]]
    then
      kill -SIGTERM $1
      exit
    else
      echo "incorrect command"
    fi
  done
fi

echo "reader started, pid=$$"
echo "1 [waiting]"
val=1
const=2

op_plus()
{
  op="+"
}

op_mul()
{
  op="*"
}

term()
{
  op="term"
}

trap 'op_plus' USR1
trap 'op_mul' USR2
trap 'term' SIGTERM

while true
do
  if [[ "$op" == "+" || "$op" == "*" ]]
  then
    let val=$val$op$const
    echo $val
  elif [[ "$op" == "term" ]]
  then
    echo "shutdown by SIGTERM..."
    exit
  fi
  sleep 1
done
