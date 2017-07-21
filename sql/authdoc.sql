use mgh;

drop table if exists `authdoc` ;

CREATE TABLE authdoc
(
  docid			int unsigned not null,
  auid			int unsigned not null,
  constraint		authors_pk primary key (docid, auid),
  foreign key (docid) 	references docs(docid),
  foreign key (auid) 	references authors(auid)
);
show tables ;
