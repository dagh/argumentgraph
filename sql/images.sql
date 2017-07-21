use mgh;

# this table talks to the image; where we found the image (doc, page, line),
#   the caption and the name of the file.
# References to the images point to this table

drop table if exists `imagerefs` ;
drop table if exists `images` ;

CREATE TABLE images
(
  imageid		int unsigned not null auto_increment,
  capid			integer, # figure id, or caption id of the image
  imagefile		varchar(512), # name of the image file 
  docid			int unsigned not null,  # name of document that we found the image
  page			int unsigned, # page number of where we found the image
  type			varchar(16), 	# type: line or sentence
  linenumber		integer, # line number; works for both lines and sentences
  caption		varchar(2048), # text of caption
  checksum		varchar(128), # checksum of file, normally using the md5sum
  constraint image1_pk primary key (imageid),
  foreign key (docid) references docs(docid)
) ;

#show tables ;

alter table images add unique index (imagefile) 

