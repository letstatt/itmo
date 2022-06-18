#!/bin/bash

if [[ "$1" == "1" || "$1" == "2" || "$1" == "3" ]]
then
  a=1
  b=1
  for ((;;))
  do
    c=b
    b=a+b
    a=c
  done
fi

if [[ "$1" == "renice" ]]
then
  pid=`pgrep "4.sh" | awk -v my_pid="$$" '{if ($0 != $my_pid) print $0}' | head -n1`
  echo "PID target to limit: $pid"
  cpulimit -p $pid -l 10 2>/dev/null 1>/dev/null &
  # cpulimit inside uses SIGSTOP and SIGCONT to control CPU usage.
  # if the process exceeded its limit, it will be immediately stopped, and then continued after a while.
  exit
fi

./4.sh 1 &
./4.sh 2 &
./4.sh 3 &
