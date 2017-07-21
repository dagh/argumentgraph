#!/bin/bash

olddatabase=mgh
newdatabase=mgh_july11


mysql -e "show databases;"
mysql -e "create database $newdatabase;"
mysql -e "show databases;"

mysql $newdatabase -e "show tables;"

mysqldump -v $olddatabase | mysql -D $newdatabase

mysql $newdatabase -e "show tables;"
