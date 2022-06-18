#!/bin/bash

emitter()
{
  echo "emitter started, reader pid=$1"
  echo "SIGTERM/SIGQUIT/SIGKILL"
  while true
  do
    read line
    if [[ "$line" == "SIGTERM" ]]
    then
      kill -SIGTERM $1
	  return
    elif [[ "$line" == "SIGQUIT" ]]
    then
      kill -SIGQUIT $1
	  return
    elif [[ "$line" == "SIGKILL" ]]
    then
      kill -SIGKILL $1
      return
    else
      echo "incorrect command"
    fi
  done
}

if [[ -n "$1" ]]
then
  pid=$1
  while true
  do
    emitter $pid
	echo "enter pid:"
	read pid
  done
fi

echo "handler started, pid=$$"
echo "[waiting]"
status="waiting"

sigterm()
{
  status="SIGTERM(15)"
}

sigquit()
{
  status="SIGQUIT(3)"
}

sigkill()
{
  status="SIGKILL(9)"
}

trap 'sigterm' SIGTERM
trap 'sigquit' SIGQUIT
trap 'sigkill' SIGKILL

while true
do
  if [[ "$status" == "waiting" ]]
  then
	sleep 1
  else
    echo $status
	exit
  fi
done
