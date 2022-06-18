#!/bin/bash

> current_process_params
> system_memory_params
> top5_processes

exec "./mem.bash" &
sleep 0.5

while true
do
top=`top -b -n1`
script_info=`echo "$top" | grep mem.bash`

if [[ "$script_info" == "" ]]
then
break
fi

echo "$script_info"
echo "$top" | grep MiB >> system_memory_params
echo "$top" | grep mem.bash >> current_process_params
echo "$top" | head -12 | tail -5 >> top5_processes
sleep 1

done

dmesg | grep "mem.bash" | tail -n 2 >> current_process_params
