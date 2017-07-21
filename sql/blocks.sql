use mgh;

drop table if exists `blocks` ;

CREATE TABLE blocks
(
  docid			int unsigned not null,
  type			varchar(16), 	# type: line or sentence
  abstract		integer,
  intro			integer,
  method		integer,
  result		integer,
  discussion		integer,
  conclusion		integer,
  acknowledgment	integer,
  reference		integer,
  constraint docs_pk primary key (docid, type, abstract, intro, method, result, discussion, conclusion, acknowledgment, reference),
  foreign key (docid) references docs(docid)
);

#show tables ;


