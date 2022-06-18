#!/bin/bash

IFS=$'\n'

if ! [[ -d "$HOME/source" ]]
then
  mkdir "$HOME/source"
  if [[ $? -ne 0 ]]
  then
    echo "unable to create source folder"
	exit 1
  fi
fi

if ! [[ -f "$HOME/backup-report" ]]
then
  echo "" > "$HOME/backup-report"
  if [[ $? -ne 0 ]]
  then
    echo "unable to create backup log"
	exit 1
  fi
fi

startup_date=`date +'%Y-%m-%d'`
startup_date_in_sec=`date +%s`
seven_days_in_sec=604800

max_date=`find ~ -maxdepth 1 -type d -regextype sed -regex ".*/Backup-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}" | awk -F '/' 'BEGIN{res=""};{date=substr($NF,8); if (date > res) res=date};END{print res}'`

max_date_in_sec=`date -d "$max_date" +%s`

let diff_date=($startup_date_in_sec-$max_date_in_sec)

if [[ -z "$max_date" ]] || [[ "$diff_date" -ge $seven_days_in_sec ]]
then
  mkdir "$HOME/Backup-$startup_date"
  if [[ $? -ne 0 ]]
  then
    echo "unable to backup"
	exit 1
  fi
  echo "----- new backup -----" >> "$HOME/backup-report"
  echo "created Backup-$startup_date" >> "$HOME/backup-report"
  for i in $(find "$HOME/source" -maxdepth 1 -type f)
  do
    filename=`echo $i | awk -F '/' '{print $NF}'`
	cp -p "$HOME/source/$filename" "$HOME/Backup-$startup_date/$filename"
	echo "$filename" >> "$HOME/backup-report"
  done
else
  echo "----- continue backup at $startup_date -----" >> "$HOME/backup-report"
  echo "append to Backup-$max_date" >> "$HOME/backup-report"
  for i in $(find "$HOME/source" -maxdepth 1 -type f)
  do
    filename=`echo $i | awk -F '/' '{print $NF}'`
	if ! [[ -f "$HOME/Backup-$max_date/$filename" ]]
	then
	  cp -p "$HOME/source/$filename" "$HOME/Backup-$max_date/$filename"
	  echo "$filename" >> "$HOME/backup-report"
	else
	  #some strings commented in case of modification
	  #size_new=`wc -c "$i" | awk '{print $1}'`
	  #size_orig=`wc -c "$HOME/Backup-$max_date/$filename" | awk '{print $1}'`
	  
	  cmp -s "$i" "$HOME/Backup-$max_date/$filename"
	  
	  #if [[ "$size_new" != "$size_orig" ]]
	  if [[ $? -eq 1 ]] # then, files aren't identical
	  then
	    uuid=`uuidgen`
		new_date_extension=`date +'%Y-%m-%d'`
		mv "$HOME/Backup-$max_date/$filename" "$HOME/Backup-$max_date/$filename.$new_date_extension.$uuid" # to avoid collision with user's files
		echo "[renaming]: $HOME/Backup-$max_date/$filename --> $HOME/Backup-$max_date/$filename.$new_date_extension.$uuid" >> "$HOME/backup-report"
	    cp -p "$HOME/source/$filename" "$HOME/Backup-$max_date/$filename"
	    echo "$filename" >> "$HOME/backup-report"
	  fi
	fi
  done
fi
