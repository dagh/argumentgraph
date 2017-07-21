use mgh;

drop table if exists `claims` ;

# if parentlinenumber == 0, that means that this is claim 0 (null claim)

CREATE TABLE claims
(
  docid			int unsigned not null,
  type			varchar(16), 	# type: line or sentence
  linenumber		integer,        # need the line number to build a primary key
  parentdocid		int unsigned not null, # parent docid
  parenttype		varchar(16), 	# type: line or sentence
  parentlinenumber	integer,        # need the line number to build a primary key
  foreign key (docid) references docs(docid),
  foreign key (parentdocid) references docs(docid),
  constraint claims_pk primary key (docid, type, linenumber) 
);

show tables ;
  #constraint docs_pk primary key (docid, sentencenum, refid),
  #constraint docs_pk primary key (doc, type),


