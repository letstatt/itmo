#!/bin/bash

last_ppid=-1
count=0
sum=0
IFS=$'\n'

out=`cat ps.out`
echo "" > ps.out

for line in $out
do
  ppid=`echo $line | awk -F ':' '{match($2, "[0-9]+"); print substr($2,RSTART,RLENGTH);}'`
  art=`echo $line | awk -F ':' '{match($3, "[0-9.]+"); print substr($3,RSTART,RLENGTH);}'`
  if [[ $ppid -ne $last_ppid ]]
  then
    if [[ $last_ppid != -1 ]]
    then
      #echo $sum $count $art
      avg=$(bc<<<"scale=6;$sum/$count")
      echo "Average_Running_Children_of_ParentID=$last_ppid is $avg" >> ps.out
    fi
    last_ppid=$ppid
    count=1
    sum=$art
  else
    let count=$count+1
    sum=$(bc<<<"scale=6;$sum+$art")
  fi
  echo $line >> ps.out
done

if [[ $last_ppid != -1 ]]
then
  avg=$(bc<<<"scale=6;$sum/$count")
  echo "Average_Running_Children_of_ParentID=$last_ppid is $avg" >> ps.out
fi
