use mgh;

# this table stores all the references to images. In a sense, this 
# means that this file is the bridge between a sentence in a document
# and the actual image file

drop table if exists `imagerefs` ;

CREATE TABLE imagerefs
(
  irefid	int unsigned not null auto_increment,
  capid		integer, # caption id
  docid		int unsigned,  # name of document that we found the image
  type		varchar(16), 	# type: line or sentence
  linenumber	integer, # line number; works for both lines and sentences
  actual_figs	varchar(900),	
  constraint 	imagerefs_pk primary key (irefid),
  foreign key 	(docid) references docs(docid)
) ;

  #imageid	int unsigned not null,
  #foreign key 	(imageid) references images(imageid)
#alter table imagerefs add unique index imagerefs_index (docid, type, linenumber, capid)  ;
#alter table imagerefs add unique index imagerefs_index (capid, docid, type, linenumber, imageid, actual_figs) ;

alter table imagerefs add unique index imagerefs_index (capid, docid, type, linenumber, actual_figs) ;

# to drop the index, use:
#   alter table imagerefs drop index imagerefs_index ;

#show tables ;


