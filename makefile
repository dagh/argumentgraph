# make file for 404 Melanoma project

rootDir=/home_ssd/dag/0-Annat/0-R/customers/mgh

codeDir=$(rootDir)/code
sqlDir=$(codeDir)/sql
#sqlDir=$(rootDir)/sql
imageDir=$(rootDir)/images
textDir=$(rootDir)/text
dbname=mgh
neoname=arg3
neoname_massCats=arg2

$(info )
$(info -------------------------------------------------)
$(info rootDir: $(rootDir))
$(info codeDir: $(codeDir))
$(info sqlDir:  $(sqlDir))
$(info -------------------------------------------------)
#$(warning this is a warning)
#$(error this is an error)

# .ONESHELL ensures that we can cd to a different directory and run the command there.
.ONESHELL:

all: docauthors findblocks references images graph
clean_all: clean_claims clean_refs clean_text_blocks clean_auth_docs clean_images clean_graph_argument clean_graph_massCats

#-----------------------------------------------------
all1:
	cd $(codeDir)
	./docauthors.r all
	./findblocks.r all
	./references.r all
	./images.r all
	./graph.r all

#-----------------------------------------------------
# this is for the Haggerty documents. Process:
# 1. readdocx.r for the Haggerty document - make note of new "docid" for Haggerty document 
# 2. do insert_filenames - add the new docid first
# 3. do connectref.r - add the new docid first
# 4. in the shiny files, add the new docid first
#    shiny/dataarg1.r shiny/grapharg1.r

#-----------------------------------------------------
# a little program that inserts filenames into the docs table for massCATS type documents
insert_filenames:
	cd $(codeDir)
	./insert_filenames_massCats.r

#-----------------------------------------------------
graph:
	cd $(codeDir)
	./graph.r
	./db -summary

#-----------------------------------------------------
images:
	$(info )
	cd $(codeDir)
	./images.r

#-----------------------------------------------------
# create authors and docs, store in docs, authors and authdoc tables
docauthors:
	$(info )
	cd $(codeDir)
	./docauthors.r all
	./db countmgh

#-----------------------------------------------------
# create text and rhetorical blocks 
findblocks:
	$(info )
	cd $(codeDir)
	./findblocks.r all
	./db countmgh

#-----------------------------------------------------
# create references and store in refs table 
references:
	$(info )
	cd $(codeDir)
	./references.r find_references 
	./references.r parse_reference_list 
	./references.r find_bibentry 
	./db countmgh

#-----------------------------------------------------
# NOTE - this will delete all database tables, and then recreate the tables
# Note also that because "docs" and "authors" tables include docid and auid, 
# which are foreign keys to all other tables, we have to treat those
# tables special
clean_auth_docs:	
	$(info )
	cd $(sqlDir)
	mysql $(dbname) -e "drop table claims;"
	mysql $(dbname) -e "drop table imagerefs;"
	mysql $(dbname) -e "drop table images;"
	mysql $(dbname) -e "drop table blocks;"
	mysql $(dbname) -e "drop table text;"
	mysql $(dbname) -e "drop table refs;"
	mysql $(dbname) -e "drop table authdoc;"
	mysql $(dbname) -e "drop table authors;"
	mysql $(dbname) -e "drop table docs;"
	mysql < docs.sql
	mysql < authors.sql
	mysql < authdoc.sql
	mysql < refs.sql
	mysql < text.sql
	mysql < blocks.sql
	mysql < images.sql
	mysql < imagerefs.sql
	mysql < claims.sql

#-----------------------------------------------------
# NOTE - this will delete text and blocks database tables, and then recreate the tables
clean_text_blocks:	
	$(info )
	cd $(sqlDir)
	mysql < text.sql
	mysql < blocks.sql
	mysql $(dbname) -e "select count(*) as 'text records' from text;"
	mysql $(dbname) -e "select count(*) as 'blocks records' from blocks;"

#-----------------------------------------------------
clean_refs:	
	$(info )
	cd $(sqlDir)
	mysql < refs.sql
	mysql $(dbname) -e "select count(*) as 'refs records' from refs;"

#-----------------------------------------------------
clean_images:	
	$(info )
	cd $(sqlDir)
	mysql $(dbname) -e "drop table imagerefs;"
	mysql $(dbname) -e "drop table images;"
	mysql < images.sql
	mysql < imagerefs.sql
	./db images
	-rm -f $(textDir)/*
	-rm -f $(imageDir)/*

#-----------------------------------------------------
clean_claims:
	cd $(sqlDir)
	mysql < claims.sql
	
#-----------------------------------------------------
clean_graph_argument:
	neo4j-shell -c "match (n:$(neoname)) detach delete n;"
	./db -summary

#-----------------------------------------------------
clean_graph_massCats:
	neo4j-shell -c "match (n:$(neoname_massCats)) detach delete n;"
	./db -summary

#-----------------------------------------------------
countmgh:
	./db countmgh

#-----------------------------------------------------
# cypher-shell "drop index on :doc(docid);"
indexes:
	cypher-shell "create index on :doc(docid);"
	cypher-shell "create index on :rtxt(linenumber);"
	cypher-shell "create index on :ref(refid);"
	cypher-shell "create index on :aut(auid);"
	cypher-shell "create index on :itxt(linenumber);"
	cypher-shell "create index on :cap(figid);"
	cypher-shell "create index on :img(imageid);"

