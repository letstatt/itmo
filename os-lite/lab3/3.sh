#!/bin/bash

# minutes, hours, day of month, months, day of week
echo "*/5 * * * Tue ~/lab3/1.sh" | crontab
