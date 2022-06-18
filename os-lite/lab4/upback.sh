#!/bin/bash

IFS=$'\n'

if ! [[ -d "$HOME/restore" ]]
then
  mkdir "$HOME/restore"
  if [[ $? -ne 0 ]]
  then
    echo "unable to create destination folder"
	exit 1
  fi
fi

max_date=`find ~ -maxdepth 1 -type d -regextype sed -regex ".*/Backup-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}" | awk -F '/' 'BEGIN{res=""};{date=substr($NF,8); if (date > res) res=date};END{print res}'`

# typical uuid
# 8b54fd3b-c617-48e7-a4e7-4562c037eaca
uuid_pattern="[0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}"

if [[ -z "$max_date" ]]
then
  echo "no backups"
else
  for i in $(find "$HOME/Backup-$max_date" -maxdepth 1 -type f)
  do
    filename=`echo $i | awk -F '/' '{print $NF}'`
	primary_revision=`echo $filename | awk -v "p=[0-9]{4}-[0-9]{2}-[0-9]{2}.$uuid_pattern$" '{match($0,p); if (RSTART == 0) print "1"}'`
	if [[ "$primary_revision" == "1" ]]
	then
	  cp -p "$i" "$HOME/restore/$filename"
	fi
  done
fi
