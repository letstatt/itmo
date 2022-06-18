#!/bin/bash

let a=`find /var/log -type f -name "*.log" -exec awk '{printf "%s+", NR}' {} \;`"0"
echo $a
