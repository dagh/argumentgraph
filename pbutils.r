#!/usr/bin/env Rscript 


#####################################################################################
convert_pmid <- function(pmid)
{
  r1 <- NULL
  tryCatch( {
    r1 <- id_converter(pmid, "pmid")
  }, error = function( e ) {
    logdata("convert_pmid() - Cought an error:", conditionMessage(e)) 
  } ) 
    
  if(!is.null(r1) & is.null(r1$records$live)) {
    d1 <- r1$records
    d2 <- d1$versions[[1]]
    #logdata("Found data - pmid:", d1$pmid, " pmcid:", d1$pmcid, " doi:", d1$doi, " v-pmcid:", d2$pmcid, " v-mid:", d2$mid, " v-current:", d2$current)
    pmidx     <- ifelse(!is.null(d1$pmid),  d1$pmid,  '')
    pmcid     <- ifelse(!is.null(d1$pmcid), d1$pmcid, '')
    pmcid_ver <- ifelse(!is.null(d2$pmcid), d2$pmcid, '')
    d3 <- data.frame(pmid=pmidx, pmcid=pmcid, pmcid_ver=pmcid_ver, stringsAsFactors=F)
  } else {
    #logdata("did not retrieve data for the pmid:", pmid)
    d3 <- NULL
  }
  return(d3)
}

#####################################################################################
person_to_dataframe <- function(p1)
{
  d3 <- data.frame()
  for(i in seq_len(length(p1))) {
    last <- ifelse(length(p1[i]$family) > 0, p1[i]$family, "")
    if(length(p1[i]$given) > 1) {
      first <- ifelse(length(p1[i]$given[1]) > 0, p1[i]$given[1], "")
      middle <- ifelse(length(p1[i]$given[2]) > 0, p1[i]$given[2], "")
    } else {
      first <- ifelse(length(p1[i]$given[1]) > 0, p1[i]$given[1], "")
      middle <- ''
    }
    role    <- ifelse(length(p1[i]$role) > 0, p1[i]$role, "")
    email   <- ifelse(length(p1[i]$email) > 0, p1[i]$email, "")
    comment <- ifelse(length(p1[i]$comment) > 0, p1[i]$comment, "")
    
    # parse out first name if more than two and if middle name is empty
    if(str_count(first, "\\S+") > 1 & middle == '' ) {
      s3 <- strsplit(first, " ") %>% unlist() %>% as.character()
      middle <- substring(s3[length(s3)], 1,1) %>% toupper()
      first <- paste(s3[1:(length(s3)-1)])
    }
    d3 <- rbind(d3, data.frame(last=last, first=first, middle=middle, role=role, email=email, comment=comment, stringsAsFactors=F))
  }
  
  # filter out all blank lines
  d3 <- d3 %>% filter(last != '' | first != '' | middle != '')
  
  if(nrow(d3) != 0) {
    d3$middle[is.na(d3$middle)] <- ''
    d3$orcid <- ''
    d3$orcscore <- ''
    d3$orcgiven <- ''
    d3$orcfamily <- ''
    d3$orccredit <- ''
    d3$orcdoiverified <- ''
  } else {
    d3 <- NULL
  }
  return(d3)
}

#####################################################################################
parse_name_last_first_middle <- function(s1)
{
  #s1 <- "Richard Petersen"
  #s1 <- "Richard C. Petersen"
  #s1 <- "Richard Charlie Petersen"
  #s1 <- "Richard Charlie asdf asf asdf asl;kjaweopiwe  F. Petersen"
  s2 <- gsub("[[:punct:]]", "", s1)
  s2 <- gsub("[[:digit:]]", "", s2)
  s3 <- strsplit(s2, " ")[[1]] %>% as.character()
  
  last <- first <- firsti <- middle <- middlei <- '' 
  first  <- s3[1]
  firsti <- substring(first,1,1)
  last <- s3[length(s3)]
  if(length(s3) > 2) {
    middle  <- paste(s3[2:(length(s3)-1)], collapse=" ") 
    middlei <- substring(middle,1,1)
  }
  #print(paste(last, first, firsti, middle, middlei, sep="  :  "))
  
  data1 <- data.frame(s1=s1, last=last, first=first, firsti=firsti, middle=middle, middlei=middlei, stringsAsFactors = F)
  data1[is.na(data1)] <- ''
  return(data1)
}

#####################################################################################
if(0) {
  dauth <- authors ; doi <- df$doi
}
find_orcid <- function(dauth, doi)
{
  
  # 1. use the doi to retrieve a list of authors
  if(!is.null(doi)) {
    s1 <- sprintf("digital-object-ids:\"%s\"", doi)
    
    res <- my.orcid(query=s1)
    
    if(nrow(res) != 0) {
      # we found an author - now parse them out
      names(res) <- gsub('[-.]', '_', names(res))

      if(!('relevancy_score_value' %in% names(res)))              res$relevancy_score_value <- ''
      if(!('orcid_identifier_uri' %in% names(res)))               res$orcid_identifier_uri <- ''
      if(!('orcid_identifier_path' %in% names(res)))             res$orcid_identifier_path <- ''
      if(!('personal_details_given_names_value' %in% names(res))) res$personal_details_given_names_value <- ''
      if(!('personal_details_family_name_value' %in% names(res))) res$personal_details_family_name_value <- ''
      if(!('personal_details_credit_name_value' %in% names(res))) res$personal_details_credit_name_value <- ''

      res <- res %>% select(orcscoreres=relevancy_score_value, url=orcid_identifier_uri,
        orcidres=orcid_identifier_path, orcgivenres=personal_details_given_names_value,        
        orcfamilyres=personal_details_family_name_value, orccreditres=personal_details_credit_name_value)
      
      res$orcdoiverifiedres <- 'true'
      
      dauth$combo <- paste0(tolower(dauth$last), tolower(substring(dauth$first,1,1)))
      res$combo <- paste0(tolower(res$orcfamilyres), tolower(substring(res$orcgivenres,1,1)))
      
      #d4 <- full_join(dauth, res, by="combo")
      d4 <- left_join(dauth, res, by="combo")
      d4[is.na(d4)] <- ''
      d5 <- d4 %>% mutate(first=ifelse(!is.na(orcgiven) & (nchar(orcgiven) > nchar(first)), orcgiven, first),
                    orcid=ifelse(!is.na(orcidres), orcidres, ''),
                    orcgiven=ifelse(!is.na(orcgivenres), orcgivenres, ''),
                    orcfamily=ifelse(!is.na(orcfamilyres), orcfamilyres, ''),
                    orcscore=ifelse(!is.na(orcscoreres), orcscoreres, ''),
                    orccredit=ifelse(!is.null(orccreditres) & !is.na(orccreditres), orccreditres, ''),
                    orcdoiverified=ifelse(!is.null(orcdoiverifiedres) & !is.na(orcdoiverifiedres), orcdoiverifiedres, ''))
      
      dauth <- d5 %>% select(last, first, middle, role, email, comment, orcid, orcscore, orcgiven, orcfamily, orccredit, orcdoiverified)
    }
  }
  
  res1 <- d2 <- data.frame()
  for(i in seq_len(nrow(dauth))) {
    logdata(sprintf("%d of %d  last: %s  first: %s  middle: %s", i, nrow(dauth), dauth$last[i], dauth$first[i], dauth$middle[i]))
    if(dauth$orcid[i] != '') next ;
    
    if(dauth$middle[i] == '') {
      (a1 <- paste(trim(dauth$last[i]), trim(dauth$first[i]), sep="+"))
    } else {
      (a1 <- paste(trim(dauth$last[i]), trim(dauth$first[i]), trim(dauth$middle[i]), sep="+"))
    }
    
    res <- my.orcid(query=a1)
    if(nrow(res) == 0) next ;
    
    names(res) <- gsub('[-.]', '_', names(res))
    
    if(!('relevancy_score_value' %in% names(res)))              res$relevancy_score_value <- ''
    if(!('orcid_identifier_uri' %in% names(res)))               res$orcid_identifier_uri <- ''
    if(!('orcid_identifier_path' %in% names(res)))             res$orcid_identifier_path <- ''
    if(!('personal_details_given_names_value' %in% names(res))) res$personal_details_given_names_value <- ''
    if(!('personal_details_family_name_value' %in% names(res))) res$personal_details_family_name_value <- ''
    if(!('personal_details_credit_name_value' %in% names(res))) res$personal_details_credit_name_value <- ''
    
    res <- res %>% select(orcscoreres=relevancy_score_value, url=orcid_identifier_uri,
      orcidres=orcid_identifier_path, orcgivenres=personal_details_given_names_value,        
      orcfamilyres=personal_details_family_name_value, orccreditres=personal_details_credit_name_value)
    
    res$orcdoiverifiedres <- 'false'
    
    if(nrow(res) == 0) next ;
    
    res1 <- res %>% filter(orcscoreres > 0.6) %>% 
      filter(tolower(dauth$last[i]) == tolower(orcfamilyres)) %>% 
      filter(tolower(substring(dauth$first[i],1,1)) == tolower(substring(orcgivenres,1,1))) %>% 
      arrange(desc(orcscoreres))
    
    if(nrow(res1) > 0) {
      dauth$orcid[i]          <- res1$orcidres[1]
      dauth$orcscore[i]       <- res1$orcscoreres[1]
      dauth$orcgiven[i]       <- res1$orcgivenres[1]
      dauth$orcfamily[i]      <- res1$orcfamilyres[1]
      dauth$orccredit[i]      <- res1$orccreditres[1]
      dauth$orcdoiverified[i] <- res1$orcdoiverifiedres[1]
    }
  }
  return(dauth)
}

#####################################################################################
if(0) {
  newname <- dauth[4,]
  dbnames <- p1[1,]
}
are_names_close_enough <- function(newname, dbnames)
{
  #print(newname)
  #print(dbnames)
  
  # 1. we know the following:
  # 1a. the last names are the same
  # 1b. the first initial of the first names are the same
  
  # 2. return False if the length of  both first names is greater than 1, and 
  #    if they are different
  if((nchar(newname$first) > 1 & nchar(dbnames$first) > 1) & (newname$first != dbnames$first)) {
    #logdata("1. records are not the same - returning false")
    return(FALSE)
  }

  # 3. return false if newname first is longer than 1 character, and dbname first is
  #     only one character (eg initial) AND 
  #     (if the middle characters are different OR if both middle characters are blank)

  if((nchar(newname$first) > 1 & nchar(dbnames$first) == 1) &
     ((newname$middle != dbnames$middle) | (newname$middle == '' & dbnames$middle == ''))) {
    #logdata("2. records are not the same - returning false")
    return(FALSE)
  }
  
  # 4. check middle names, even though that might not matter
  #if(substring(newname$middle,1,1) == substring(dbnames$middle,1,1)) { logdata("middle initials are the same")
  #} else { logdata("middle initials are not the same")}
  
  # OK, the names are the same!
  
  # 5. if newname$first is longer than the dbnames$first, then update the database with the longer name
  if(nchar(newname$first) > nchar(dbnames$first)) {
    logdata("5. update database with new first name")
    q1 <- paste0("update authors set first = '", newname$first, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
  }
  
  # 6. if newname$middle is longer than the dbnames$middle, then update the database with the longer name
  if(nchar(newname$middle) > nchar(dbnames$middle)) {
    logdata("6. update database with new middle name")
    q1 <- paste0("update authors set middle = '", newname$middle, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
  }
  
  # 7. if new names have an ORCID entry AND dbnames do not have an ORCID, then update the database with the ORCID entry
  if(!is.null(newname$orcid) & is.null(dbnames$orcid)) {
    logdata("update ORCID")
    q1 <- paste0("7a. update authors set orcid = '", newname$orcid, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
    q1 <- paste0("7b. update authors set orcscore = '", newname$orcscore, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
    q1 <- paste0("7c. update authors set orcfamily = '", newname$family, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
    q1 <- paste0("7d. update authors set given = '", newname$given, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
    q1 <- paste0("7e. update authors set orccredit = '", newname$orccredit, "' where auid = ", dbnames$auid, ";")
    calldb(q1)
  }
  
  return(TRUE)
}

#####################################################################################
if(0) {
  dauth <- authors
}
deduping_authors <- function(dauth, verbose=F)
{
  index_not_to_add <- integer()
  found_auid <- integer()
  
  #refs <- lapply(dauth, function(x) gsub("'", "''", x))  %>% as.data.frame(stringsAsFactors=F)
  
  for(i in seq_len(nrow(dauth))) {
    if(verbose) logdata(sprintf("dauth:%d  last: %s  first: %s  middle: %s", i, dauth$last[i], dauth$first[i], dauth$middle[i]))
    p1 <- calldb(paste0("select * from authors where last = '", gsub("'", "''", dauth$last[i]), "' and substring(first,1,1) = '", substring(dauth$first[i],1,1), "';"))
    
    for(j in seq_len(nrow(p1))) {
      if(verbose) logdata(sprintf("p1   :%d  last: %s  first: %s  middle: %s", j, p1$last[j], p1$first[j], p1$middle[j]))
      if(are_names_close_enough(dauth[i,], p1[j,])) {
        #logdata("names are in the database already")
        index_not_to_add <- c(index_not_to_add, i)
        found_auid <- c(found_auid, p1$auid[j])
        break ;
      }
    }
  }

  dauth$found_auid <- NA
  if(length(index_not_to_add) > 0) {
    dauth$found_auid[index_not_to_add] <- found_auid
  }
  return(dauth)
}

#####################################################################################
process_authors <- function(docs1, b1, doi)
{
  authors <- person_to_dataframe(b1$author) 
  
  if(is.null(authors)) return(NULL) ;
  if(nrow(authors) == 0) return(NULL) ;
    
  d4 <- find_orcid(authors, doi)
  authors1 <- deduping_authors(d4)
  
  authors_already_in_database <- authors1 %>% filter(!is.na(authors1$found_auid))
  authors_not_in_database     <- authors1 %>% filter(is.na(authors1$found_auid))
  
  if(nrow(authors_not_in_database) > 0)
  {
    # add these authors to database
    authors_not_in_database$found_auid <- NULL
    authors_not_in_database <- insert_into_authors(authors_not_in_database)
  }
  # combine both list and insert into authdoc table
  authors_already_in_database$found_auid
  authors_not_in_database$auid
  auid <- c(authors_already_in_database$found_auid, authors_not_in_database$auid)
  
  if(length(auid) > 0 & nrow(docs1)) {
    # insert into authdoc table
    d1 <- data.frame(docid=rep(docs1$docid[1], each=length(auid)),  auid=auid,  stringsAsFactors = F)
    insert_into_authdoc(d1)
  }
}

#####################################################################################
if(0) {
  pmid=docname=NULL
  pmid = pbentry$eprint
  bib2=bib1
  
  pmid = reference_list$pmid[i]
  bib2 <- docname <- NULL
}
find_store_in_pubmed_pmid <- function(docid, pmid, bib2 = NULL, docname = NULL)
{
  if(!is.null(pmid)) {
    b1 <- GetPubMedByID(pmid)
    df <- as.data.frame(b1)
    pmid_convert <- convert_pmid(pmid)
  } else {
    #pmid is NULL, which means that we have to build our own df data.frame
    pmid_convert <- NULL
    b1 <- bib2
    return_now <- FALSE
    tryCatch( {
      df <- as.data.frame(b1)
    }, error = function( e ) {
      logdata("find_store_in_pubmed() - Cought an error:", conditionMessage(e), "return NULL") 
      logdata("                       - Seems as though there is a problem with a bib entry") 
      logdata("                       - pmid is NULL - docid:", docid) 
      return_now <<- TRUE
    } ) 
    if(return_now) return(NULL);
  }
  
  doi1 <- bib2$doi 
  if(is.null(df$doi))   doi1 <- bib2$doi
  if(is.null(bib2$doi)) doi1 <- df$doi

  # note: date, url and score comes from bib2, which is a bib entry
  
  names(df)
  d1 <- data.frame(
    type          = 'standard',
    pmid          = ifelse(!is.null(df$eprint), df$eprint, ''),
    pmcid         = ifelse(!is.null(pmid_convert), pmid_convert$pmcid, ''),
    pmcid_ver     = ifelse(!is.null(pmid_convert), pmid_convert$pmcid_ver, ''),
    filename      = ifelse(!is.null(docname), docname, ''),
    bibtype       = ifelse(!is.null(df$bibtype), df$bibtype, ''),
    title         = ifelse(!is.null(df$title), df$title, ''),
    subtitle      = ifelse(!is.null(df$subtitle), df$subtitle, ''),
    year          = ifelse(!is.null(df$year), df$year, ''),
    month         = ifelse(!is.null(df$month), df$month, ''),
    date          = ifelse(!is.null(bib2$date), bib2$date, ''),
    publisher     = ifelse(!is.null(df$publisher), df$publisher, ''),
    journal       = ifelse(!is.null(df$journal), df$journal, ''),
    volume        = ifelse(!is.null(df$volume), df$volume, ''),
    number        = ifelse(!is.null(df$number), df$number, ''),
    pages         = ifelse(!is.null(df$pages), df$pages, ''),
    url           = ifelse(!is.null(bib2$url), bib2$url, ''),
    doi           = ifelse(!is.null(doi1), doi1, ''),
    eprint        = ifelse(!is.null(df$eprint), df$eprint, ''),
    eprinttype    = ifelse(!is.null(df$eprinttype), df$eprinttype, ''),
    language      = ifelse(!is.null(df$language), df$language, ''),
    issn          = ifelse(!is.null(df$issn), df$issn, ''),
    abstract      = ifelse(!is.null(df$abstract), df$abstract, ''),
    score         = ifelse(!is.null(bib2$score), bib2$score, ''),
    journal_title = ifelse(!is.null(bib2$journaltitle), bib2$journaltitle, ''),
    stringsAsFactors=F)

  docs1 <- insert_into_docs(d1)
  
  if(is.null(b1$author)) {
    logdata("find_store_in_pubmed - docid:", docid, " d1$doi:", d1$doi, " has no authors, so returning NULL")
    return(NULL)
  }

  process_authors(docs1, b1, df$doi)
    
  return(docs1$docid[1])
}
  
