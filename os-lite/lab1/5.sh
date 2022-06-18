#!/bin/bash

cat /var/log/anaconda/syslog | awk '{ if($2 == "INFO") print $0 }' > info.log
