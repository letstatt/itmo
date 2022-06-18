#!/bin/bash

ps -ax | awk '{match($5, "^/sbin/.*"); if (RSTART > 0) {print $1}}' > ps.out
