use mgh;

drop table if exists `authors` ;

CREATE TABLE authors
(
  auid			int unsigned not null auto_increment,
  last			varchar(128),    
  first			varchar(128),    
  middle		varchar(128),    
  role			varchar(128),    
  institution		varchar(128),
  email			varchar(128),    
  comment		varchar(128),    
  orcid			varchar(64),
  orcscore		varchar(64),   # orcid relevancy score
  orcfamily		varchar(128),  # last name 
  orcgiven		varchar(128),  # first name
  orccredit		varchar(128),  # combo of last and first and middle initial 
  orcdoiverified	varchar(16),   # true or false, if the the data has been verified using doi to orcid
  constraint authors_pk primary key (auid)
);

alter table authors add unique index (last, first, middle)  ;

show tables ;
