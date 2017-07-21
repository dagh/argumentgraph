#!/bin/bash


echo
echo "this file deletes tables in the database - do you really want to do this?"
echo
echo

#exit

tables="authorbib authorpb authors bib blocks pubmed refs text docs"

echo "delete tables in mgh"

for t in $tables
do
  echo "delete table: $t"
  mysql mgh -e "drop table $t;"
done

