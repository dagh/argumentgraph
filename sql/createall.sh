#!/bin/bash


echo
echo "this file creates new tables in the database"
echo

#tables="authorbib authorpb authors bib blocks pubmed refs text docs"

tables="docs text blocks authors authdoc"

echo "create tables in mgh"

for t in $tables
do
  echo "create table: $t"
  mysql < ${t}.sql 
done


mysql mgh -e "show tables ;"
