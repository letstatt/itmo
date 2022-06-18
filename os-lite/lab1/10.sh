#!/bin/bash

man bash | awk '{for(i=1;i<=NF;i++) { \
                     match($i, "[A-Za-z]+");
                     if (RLENGTH >= 4 && RSTART != 0) \
                         print tolower(substr($i, RSTART, RLENGTH)) \
                 }}' | sort | uniq -dc | sort -rnk1 | head -n3 | awk '{print $2}'
