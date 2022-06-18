#!/bin/bash

IFS=$'\n'

pids=`ps -aux | awk '{if ($1 != "USER") {print $2;}}'`
record=""

for pid in $pids
do
  # check this ->                                               not rchar, but read_bytes
  read_bytes=`cat /proc/$pid/io 2>/dev/null | awk '{match($0, "^rchar"); if (RSTART > 0) {match($0, "[0-9]+"); print substr($0, RSTART, RLENGTH);}}'`
  if [[ -n "$read_bytes" ]]
  then
    record="$record$pid $read_bytes\n"
  fi
done

# TODO: should be 60 seconds
sleep 5

pids=`ps -aux | awk '{if ($1 != "USER") {printf "%d ", $2; for(i=11;i<=NF;i++) {printf "%s ", $i;} print "";}}'`

pid1=0
bytes1=0
cmd1=""
pid2=0
bytes2=0
cmd2=""
pid3=0
bytes3=0
cmd3=""

for line in $pids
do
  pid=`echo $line | awk '{print($1)}'`
  cmd=`echo $line | awk '{for(i=2;i<=NF;i++) {printf "%s ", $i}}'`
  # check this ->                                               not rchar, but read_bytes
  read_bytes=`cat /proc/$pid/io 2>/dev/null | awk '{match($0, "^rchar"); if (RSTART > 0) {match($0, "[0-9]+"); print substr($0, RSTART, RLENGTH);}}'`
  if [[ -n "$read_bytes" ]]
  then
    difference=`echo -e "$record" | awk -v read_bytes=$read_bytes -v pid=$pid 'BEGIN{diff=read_bytes;} {if ($1 == pid) {diff=diff-$2;}} END{print diff}'`
    if [[ $difference -gt $bytes1 ]]
    then
      pid3=$pid2
      bytes3=$bytes2
      cmd3=$cmd2
      pid2=$pid1
      bytes2=$bytes1
      cmd2=$cmd1
      pid1=$pid
      bytes1=$difference
      cmd1=$cmd
    elif [[ $difference -gt $bytes2 ]]
    then
      pid3=$pid2
      bytes3=$bytes2
      cmd3=$cmd2
      pid2=$pid
      bytes2=$difference
      cmd2=$cmd
    elif [[ $difference -gt $bytes3 ]]
    then
      pid3=$pid
      bytes3=$difference
      cmd3=$cmd
    fi
  fi
done

echo "$pid1:$cmd1:$bytes1"
echo "$pid2:$cmd2:$bytes2"
echo "$pid3:$cmd3:$bytes3"
