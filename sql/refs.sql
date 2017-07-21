use mgh;

drop table if exists `refs` ;

# it is all right to have duplicates here because a sentence might have duplicate 
# references in different parts of the sentence

CREATE TABLE refs
(
  docid		int unsigned not null,
  sentencenum	integer,       # sentence number 
  type		varchar(16),   # type: line or sentence
  ref1		varchar(512),  # the references in the text; eg 1,3 or 1-3
  refid		varchar(512),  # the actual reference number; eg 1 or 2 or 3
  bibentry	varchar(1024), # bibliographic entry pointing to the referenced doc
  refdocid	int unsigned,  # docid of the referenced document if we can find one
  refpmid	varchar(32),   # pmid of the referenced document
  refdoi	varchar(128),  # doi of the referenced document       
  constraint docs_pk primary key (docid, sentencenum, refid),
  foreign key (docid) references docs(docid)
);

show tables ;
#constraint docs_pk primary key (doc, type),


