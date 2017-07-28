#!/usr/bin/env Rscript 

################################################################################
logdata <- function(...)
{
  dt <- format(Sys.time(), format="%d|%H:%M:%S")
  cat(sprintf("%s\n", paste(unlist(list(...)), collapse=" ")))
  cat(sprintf("%s %s\n", dt, paste(unlist(list(...)), collapse=" ")), file=file.path(codeDir, logfilename), append=T)
}

#######################################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#######################################################################
determine_most_likely_bib <- function(s5, d1)
{
  # we add weights here but by putting year and title at 90%, we actually are giving 
  #  deference to the score from CrossRef
  year1_weight  <- 0.9
  title1_weight <- 0.9
  
  # get year from s5
  year1 <- NA
  
  # 1. try to find the year inside two parans
  x1 <- str_extract_all(s5, "\\([0-9)]{4}\\)") %>% unlist()
  if(length(x1) > 0) {
    x1 <- gsub("\\(|\\)", "", x1)
    if(nchar(x1) == 4) {
      x2 <- as.integer(x1)
      if(!is.na(x2) & x2 > 1950 & x2 < 2030) {
        year1 <- x2
      }
    }
  }
  
  if(is.na(year1)) {
    year1 <- str_extract_all(s5, "[^0-9][0-9]{4}[^0-9]") %>% unlist()
    year1 <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", year1)
  
    if(length(year1) > 1) {
      logdata("determine_most_likely_bib - more than one year - pick the best year")
      for(j in seq_len(length(year1))) {
        if(year1[j] > 1950 & year1[j] < 2030) {
          year1 <- year1[j]
          break
        }
      } # for j...
    } # if(length...
  }
  
  if(length(year1) == 0) year1 <- NA
    
  if(is.na(year1)) {
    logdata("year1 length is zero")
    year1 <- ''
    year1_weight <- 0.5
  }
 
  
  # find title
  title1 <- NA
  
  # 1. try to be smart by extracting into sentences, then pick the sentence with the 
  #    most stopwords. The idea is that a title contains more stopwords than names.
  s6 <- gsub("\\.", " . ", s5)
  s7 <- convert_to_sentence(s6)
  
  if(length(s7) > 1) {
    # if there are more than one potential strings inside s7, then do this
  
    m1 <- sapply(stopwords(), function(x) {
      q1 <- paste0("^", x, " | ", x, " | ", x, "$") 
      grepl(q1, s7, ignore.case=T)
    })
    index <- which(rowSums(m1) == max(rowSums(m1)))
    
    # if we found two titles with equal number of stop words, then pick the first 
    #    title because normally, there are first names, then title, then other stuff, 
    #    and normally, there are not many stopwords in names, so therefore, the 
    #    first sentence with more stopwords is the title
    if(length(index) > 0) title1 <- s7[index[1]]
  }
  
  #######################3 
  
  if(is.na(title1)) {
    title1 <- str_extract_all(s5, "\\.[^.]*\\.") %>% unlist() %>% as.character()
    title1 <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", title1)
    
    if(length(title1) > 1) {
      logdata("determine_most_likely_bib - more than one title. Taking the first title:", title1[1])
      title1 <- title1[1]
    }
  }
  
  if(length(title1) == 0) title1 <- NA
  
  if(is.na(title1)) {
    logdata("title1 length is zero")
    title1 <- ''
    year1_weight  <- 0.5
    title1_weight <- 0.1
  }
  
  # clean up the title
  title1 <- gsub("^[[:punct:][:space:]]*|[[:punct:][:space:]]*$", "", title1)
  
  if(is.null(d1$date) ) d1$date <- NA
  if(is.null(d1$score)) d1$score <- NA
  if(is.null(d1$title)) d1$title <- NA
  
  d1 <- d1 %>% mutate(
    rankscore   = rank(score),
    titlecosine = ifelse(is.na(title), 1, 1 - stringdist(tolower(title1), tolower(d1$title), method="cosine")),
    ranktitle   = rank(titlecosine) * title1_weight,
    yeargrep    = ifelse(grepl(year1, d1$date), 1, 0),
    rankyear    = rank(yeargrep) * year1_weight,
    totalrank   = rankscore+ranktitle+rankyear)
  
  # index is the index into the dataframe, pointing the highest ranked entry
  index <- which(d1$totalrank == max(d1$totalrank))
  index <- index[1] # if there are two winners, just take the top one.
  return(list(index, d1))
}


#######################################################################
#pbentry <- LookupPubMedID(bib1)
my.LookupPubMedID <- function(bib1)
{
  sleep_time <- 0.25 #seconds

  sleep_time_on_error = 2 #seconds

  max_number_of_tries = 4
  number_of_tries = 1

  success = FALSE
  while( success == FALSE & number_of_tries <= max_number_of_tries ) {
    tryCatch( {
      logdata(paste0("my.LookupPubMedID() try: ", number_of_tries, " of ", max_number_of_tries)) 

      pbentry <- LookupPubMedID(bib1)
      success = TRUE

    }, warning = function( w ) {
      logdata(paste0("my.LookupPubMedID() Warning in LookupPubMedID() message: \"", conditionMessage(w),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("returned no matches", conditionMessage(w)) | grepl("Bad request", conditionMessage(w))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(warning) my.LookupPubMedID() done sleeping"))
    }, error = function( e ) {
      logdata(paste0("my.LookupPubMedID() Error in LookupPubMedID() message: \"", conditionMessage(e),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("Request-URI Too Long", conditionMessage(e)) | grepl("Bad request", conditionMessage(e))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(error) my.LookupPubMedID() done sleeping"))
    } ) # tryCatch ...
  } # while NOT success

  if( number_of_tries > max_number_of_tries ) {
    logdata(paste("my.LookupPubMedID() Reached MAX number of tries.  Return NULL"))
    pbentry <- NULL
  }

  return(pbentry)
}




#######################################################################
#  bib1 <- my.ReadCrossRef(s5)
my.ReadCrossRef <- function(s5)
{
  sleep_time <- 0.25 #seconds

  # in general, sleep a little bit before each call to meetup
  #logdata(paste("my.ReadCrossRef() sleep before calling CrossRef: ",sleep_time, "seconds"))
  #Sys.sleep(sleep_time)

  sleep_time_on_error = 2 #seconds

  max_number_of_tries = 4
  number_of_tries = 1

  success = FALSE
  while( success == FALSE & number_of_tries <= max_number_of_tries ) {
    tryCatch( {
      logdata(paste0("my.ReadCrossRef() try: ", number_of_tries, " of ", max_number_of_tries)) 

      bib1 <- ReadCrossRef(s5)
      
      if(any(class(bib1) == c("BibEntry", "bibentry"))) {
         success = TRUE
      } else {
        logdata("Not a valid response")
        Sys.sleep(sleep_time_on_error)
        number_of_tries <- number_of_tries + 1
        logdata(paste("my.ReadCrossRef() done sleeping"))
      } 
      
    }, warning = function( w ) {
      logdata(paste0("my.ReadCrossRef() Warning in ReadCrossRef() message: \"", conditionMessage(w),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("returned no matches", conditionMessage(w)) | grepl("Bad request", conditionMessage(w))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(warning) my.ReadCrossRef() done sleeping"))
    }, error = function( e ) {
      logdata(paste0("my.ReadCrossRef() Error in ReadCrossRef() message: \"", conditionMessage(e),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("Request-URI Too Long", conditionMessage(e)) | grepl("Bad request", conditionMessage(e))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(error) my.ReadCrossRef() done sleeping"))
    } ) # tryCatch ...
  } # while NOT success

  if( number_of_tries > max_number_of_tries ) {
    logdata(paste("my.ReadCrossRef() Reached MAX number of tries.  Return NULL"))
    bib1 <- NULL
  }

  return(bib1)
}

#######################################################################
my.orcid <- function(query='', rows=10)
{
  sleep_time <- 0.25 #seconds

  sleep_time_on_error = 2 #seconds
  max_number_of_tries = 7
  number_of_tries = 1

  success = FALSE
  while( success == FALSE & number_of_tries <= max_number_of_tries ) {
    tryCatch( {
    	if(number_of_tries != 1) logdata(paste0("my.orcid() try: ", number_of_tries, " of ", max_number_of_tries)) 

      res <- orcid(query = query, rows=rows)
      success = TRUE
      
    }, warning = function( w ) {
      logdata(paste0("my.orcid() Warning in orcid() message: \"", conditionMessage(w),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("returned no matches", conditionMessage(w)) | grepl("Bad request", conditionMessage(w))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(warning) my.orcid() done sleeping"))
    }, error = function( e ) {
      logdata(paste0("my.orcid() Error in orcid() message: \"", conditionMessage(e),
        "\" - sleeping ", sleep_time_on_error, " seconds"))
      if(grepl("Request-URI Too Long", conditionMessage(e)) | grepl("Bad request", conditionMessage(e))) {
        logdata("Exiting")
        number_of_tries <- max_number_of_tries + 1
      }
      Sys.sleep(sleep_time_on_error)
      number_of_tries <<- number_of_tries + 1
      logdata(paste("(error) my.orcid() done sleeping"))
    } ) # tryCatch ...
  } # while NOT success

  if( number_of_tries > max_number_of_tries ) {
    logdata(paste("my.orcid() Reached MAX number of tries.  Return NULL"))
    res <- NULL
  }

  return(res)
}


#############################################################################
# if this function return an error such as the following, then try the following
#   1. Try restart  RStudio.  It might clear things up.
#   2. It seems to depend on ggplot. So therefore, make sure the call 
#      to "library(ggplot2)" happens after the call this function
#
# Error in as.data.frame.default(x[[i]], optional = TRUE) : 
# cannot coerce class "c("Simple_Sent_Token_Annotator", "Annotator")" to a data.frame
# Use the following call to remove ggplot2:
#   if(length(grep(paste("^package:", "ggplot2", "$", sep=""), search()))) {detach(package:ggplot2)}
#
convert_to_sentence <- function(s)
{
  s <- as.String(s)
  a <- annotate(s, Maxent_Sent_Token_Annotator(language="en"))
  return(s[a])
}

############################################################################
simpleCap <- function(x) {
  s1 <- strsplit(x, " ")[[1]]
  s2 <- paste(toupper(substring(s1, 1,1)), substring(s1, 2), sep="", collapse=" ")
  return(s2)
}

############################################################################
clean_all_data = function( s_clean, newline_to_space=TRUE )
{
	# http://unicodelookup.com/#latin/1
	
  # translate new lines to spaces
  if(newline_to_space) s_clean <- gsub("\n", " ", s_clean)
  s_clean = gsub( "[\x01-\x1f]", "", s_clean )
  s_clean <- gsub("[[:cntrl:]]", "", s_clean)
  s_clean <- gsub("\f", "", s_clean)
  s_clean <- gsub("–", "-", s_clean)  # en dash
  s_clean <- gsub("`", "'", s_clean)  # grave accent \x60
  
  #s_clean = gsub( "[\x7f-\xff]", "", s_clean )
  #s_clean = gsub( "\x60",        "", s_clean )
  #s_clean = gsub( "\x27",        "", s_clean )
  #s_clean = gsub( "\x22",        "", s_clean )

  return( s_clean )
}

