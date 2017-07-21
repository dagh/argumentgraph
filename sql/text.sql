use mgh;

drop table if exists `text` ;

CREATE TABLE text
(
  docid			int unsigned not null,
  type			varchar(16), 	# type: line or sentence
  linenumber		integer,        # need the line number to build a primary key
  text			varchar(4096),
  constraint docs_pk primary key (docid, type, linenumber),
  foreign key (docid) references docs(docid)
);

#show tables ;
