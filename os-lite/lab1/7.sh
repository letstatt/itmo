#!/bin/bash

tmp=`find /etc -type f -exec awk -b ' \
                        {match($0, "([a-zA-Z0-9_-]+\\\.?)+@([a-zA-Z0-9_-]+\\\.)+[a-zA-Z]+"); \
                        if (RSTART != 0) \
                            printf "%s ", substr($0, RSTART, RLENGTH)}' {} \;`
echo $tmp | awk '{for(i=1;i<NF;i++) {printf "%s, ", $i}; print $NF}' > emails.lst
