#!/bin/bash
# task: remove comments from bash script

if ! [[ -f $1 ]]
then
  echo "File expected"
  exit 1
fi

sed " s/#[^!].*//g; s/#!!.*//g" $1
