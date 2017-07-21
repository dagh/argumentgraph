use mgh;

drop table if exists `docs` ;

CREATE TABLE docs
(
  docid			int unsigned not null auto_increment,
  type			varchar(16), # set to either 'standard' or 'massCATS' depending on type
  pmid			varchar(32),
  pmcid			varchar(32),
  pmcid_ver		varchar(32),  # current version number of the PMCID
  filename		varchar(128),
  bibtype		varchar(128),
  title			varchar(512),
  subtitle		varchar(512),
  year			varchar(16),
  month			varchar(16),
  date			varchar(16),
  publisher		varchar(256),
  journal		varchar(256),
  volume		varchar(128),    
  number		varchar(128),    
  pages			varchar(128),     
  url			varchar(128),     
  doi			varchar(128),       
  eprint		varchar(16),
  eprinttype		varchar(64),
  language		varchar(128),  
  issn			varchar(128),      
  abstract		varchar(4096),  
  score			varchar(16),
  journal_title		varchar(128),      
  target		varchar(128),  # massCATS entry 
  nominator		varchar(128),  # massCATS entry
  priorityscore		varchar(128),  # massCATS entry
  targetrational	varchar(256),  # massCATS entry
  status		varchar(128),  # massCATS entry
  projectID		varchar(16),   # massCATS entry
  summary		varchar(1024), # massCATS entry
  constraint docs1_pk primary key (docid)
);

alter table docs add unique index (pmid, doi, title) 

#constraint docs2_pk primary key (pmid, title, doi)

#alter table docs auto_increment=99000001

