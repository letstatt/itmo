#!/bin/bash

d=`date +%F_%H-%M-%S`
mkdir ~/test && echo "catalog test was created successfully" > ~/report && echo "" > ~/test/$d

ping -c 1 http://www.net_nikogo.ru/ 2>>/dev/null || echo "$(date +%F_%H-%M-%S) error mesg" >> ~/report
