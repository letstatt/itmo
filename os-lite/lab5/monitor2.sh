#!/bin/bash

> current_process_params
> system_memory_params
> top5_processes

exec "./mem.bash" &
exec "./mem2.bash" &
sleep 0.5

while true
do
top=`top -b -n1`
script_info1=`echo "$top" | grep mem.bash`
script_info2=`echo "$top" | grep mem2.bash`

if [[ "$script_info1" == "" && "$script_info2" == "" ]]
then
break
fi

echo "$script_info1"
echo "$script_info2"
echo "----------------"
echo "$top" | grep MiB >> system_memory_params
echo "$top" | grep mem.bash >> current_process_params1
echo "$top" | grep mem2.bash >> current_process_params2
echo "$top" | head -12 | tail -5 >> top5_processes
sleep 1

done

dmesg | grep "mem.bash" | tail -n 2 >> current_process_params
