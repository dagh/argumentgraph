#!/bin/bash

tables="docs authors authdoc"
revtables="authdoc authors docs"


echo "delete tables in mgh"

for t in $revtables
do
  echo "delete table: $t"
  mysql mgh -e "drop table $t;"
done

echo "create tables in mgh"

for t in $tables
do
  echo "create table: ${t}"
  mysql < ${t}.sql 
done


