#!/bin/bash

IFS=$'\n'

if [[ -z "$1" ]]
then
  echo "filename expected"
  exit
fi

if ! [[ -d "$HOME/.trash" ]]
then
  echo "unable to find trash folder"
  exit
fi

if ! [[ -f "$HOME/.trash.log" ]]
then
  echo "unable to find trash journal"
  exit
fi

matches=`cat "$HOME/.trash.log" | awk -F ":" -v "pattern=$1$" '{match($2, pattern); if (RSTART > 0) print $0}'`

cnt=`echo "$matches" | wc -l`

if [[ $cnt -eq 0 ]] || [[ -z "$matches" ]]
then
  echo "0 matches found"
  exit
fi

echo "$cnt matches found"

skipped=0

for i in $matches
do
  #uuidlen_with_separator=37
  filepath=`echo "$i" | awk '{print substr($0,38)}'`
  uuid=`echo "$i" | awk '{print substr($0,1,36)}'`
  echo "$filepath: recovery? [y/n] "
  read choice
  if [[ "$choice" == "y" ]]
  then
    filedir=`echo "$filepath" | awk -F '/' 'BEGIN{OFS="/"}{$NF=""; print $0}'`
	if ! [[ -d "$filedir" ]]
	then
	  "Directory $filedir deleted, restore into $HOME"
	  filedir="$HOME"
	fi
	
    new_filepath="$filedir""$1"
	while [[ -f "$new_filepath" ]]
	do
	  echo "path to restore already exist, try new filename "
	  read choice
	  new_filepath="$filedir""$choice"
	done
	
	ln "$HOME/.trash/$uuid" "$new_filepath"
	
	if [[ $? -ne 0 ]]
	then
	  echo "unable to restore, abort"
	  exit
	fi
	
	rm "$HOME/.trash/$uuid"
	break
  else
    let skipped=$skipped+1
  fi
done

if [[ $cnt -eq $skipped ]]
then
  echo "nothing restored"
else
  cat "$HOME/.trash.log" | awk -F ":" -v "skipped=$skipped" -v "pattern=$1$" 'BEGIN{matched=0}{match($2, pattern); if (RSTART == 0) {print $0} else {if (skipped != matched) print $0; matched+=1}}' >> "$HOME/.trash2.log"
  rm "$HOME/.trash.log"
  mv "$HOME/.trash2.log" "$HOME/.trash.log"
fi
