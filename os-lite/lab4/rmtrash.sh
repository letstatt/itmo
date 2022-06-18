#!/bin/bash

if ! [[ -f "$1" ]]
then
  echo "file expected"
  exit 1
fi

if ! [[ -d "$HOME/.trash" ]]
then
  mkdir "$HOME/.trash" 2>/dev/null
  if [[ $? -eq 1 ]]
  then
    echo "unable to create trash folder"
	exit
  fi
fi

if ! [[ -f "$HOME/.trash.log" ]]
then
  echo -n "" > "$HOME/.trash.log" 2>/dev/null
  if [[ $? -eq 1 ]]
  then
    echo "unable to create trash journal"
	exit
  fi
fi

uuid=`uuidgen`
while [[ -f "$HOME/.trash/$uuid" ]]
do
  uuid=`uuidgen`
done

ln "$1" "$HOME/.trash/$uuid"

if [[ $? -ne 0 ]]
then
  echo "unable to create hard link"
  exit
fi

path=`realpath "$1"`
echo "$uuid:$path" >> "$HOME/.trash.log" && rm "$1" || rm "$HOME/.trash/$uuid"