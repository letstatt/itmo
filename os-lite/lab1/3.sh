#! /bin/bash

echo " ~ choose a number ~ "
echo "1. open nano"
echo "2. open vi"
echo "3. open links"
echo "4. quit"

while true
do
  read c
  case $c in
  "1" )
    nano
  ;;
  "2" )
    vi
  ;;
  "3" )
    links
  ;;
  "4" )
  break
  ;;
  esac
done

echo "bye-bye"
