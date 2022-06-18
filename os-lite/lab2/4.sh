#!/bin/bash

out=`ps -aux | awk '{if ($1 != "USER") {print $2;}}'`
list=""

for pid in $out
do
  status="/proc/$pid/status"
  sched="/proc/$pid/schedstat" # https://unix.stackexchange.com/q/418773
  ppid=`cat $status 2>/dev/null | awk '{match($0, "PPid"); if (RSTART > 0) {match($0, "[0-9]+"); print substr($0, RSTART, RLENGTH);}}'`
  art=`cat $sched 2>/dev/null | awk '{print ($1/1000000000);}'`

  if [[ -n "$ppid" ]]
  then
    list="$list$pid $ppid $art\n"
  fi
  #echo $status $pid $ppid
done

list=`echo -e "$list" | sort -nk2`

echo "$list" | awk '{if (length $0 > 0) {printf "ProcessID=%d : Parent_ProcessID=%d : Average_Running_Time=%f\n", $1, $2, $3}}' > ps.out
