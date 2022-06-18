#!/bin/bash

ps -aux --sort=-start_time | awk '{if (NR == 2) {print $2;}}'
