#!/bin/bash

if [[ $PWD == $HOME ]]
then
  echo "$HOME"
else
  exit 1
fi
