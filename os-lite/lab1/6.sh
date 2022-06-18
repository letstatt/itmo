#!/bin/bash

awk '{match($0, "\\[\\s*[0-9]+\\.[0-9]+\\] \\((I|W){2}\\) .*");\
     if (RSTART > 0) {\
         match($0, "\\[\\s*[0-9]+\\.[0-9]+\\] \\(II\\) .*");\
         if (RSTART > 0) {\
             sub("II", "Information");\
         } else {\
             sub("WW", "Warning");\
         }\
         print RSTART, $0;\
     }}' /var/log/anaconda/X.log | sort -nk1 | awk '{print substr($0, 3)}' > full.log
cat full.log
