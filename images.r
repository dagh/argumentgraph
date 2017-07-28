#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh"

setwd( rootDir ) 
codeDir   <- file.path(rootDir, "code")
dataDir   <- file.path(rootDir, "data")
outputDir <- file.path(rootDir, "output")
claimDir  <- file.path(rootDir, "find_claim")

imageDir  <- file.path(rootDir, "images")
textDir   <- file.path(rootDir, "text")
dir.create(imageDir, showWarnings=F)
dir.create(textDir, showWarnings=F)

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "db.r"))
source(file.path(codeDir, "pbutils.r"))

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(tm)
})

# need to call this such that we can access correct database
SQL_DATABASE_NAME <- 'mgh'

logfilename <- "images.log"

#####################################################################################
get_captions <- function(p1)
{
  #p1 <- read.csv(file=file.path(codeDir, "figure.csv"), stringsAsFactors = F)
  #names(p1)[4] <- 'text'
  p1$text <- gsub(" ", "", p1$text)
  
  p1$number <- seq_len(nrow(p1))
  captions <- data.frame()
  
  # first pass - very confident that these are captions
  (search_pattern <- "(^Fig|^FIG)(ure|URE|\\.)[ ]+[0-9A-Z]{1,3}[ ]*(\\.|\\||:)")
  index <- grep(search_pattern, p1$text, value=F)
  captions <- rbind(captions, p1[index,])
  p2 <- p1[-index,]
  
  # second pass
  (search_pattern <- "(^Fig|^FIG)(ure|URE|\\.)[ ]+[0-9A-Z]{1,3} [A-Z][^ ]")
  grep(search_pattern, p2$text, value=T)
  index2 <- grep(search_pattern, p2$text, value=F)
  captions <- rbind(captions, p2[index2,])
  p3 <- p2[-index2,]
  p3$text
  
  # third pass
  (search_pattern <- "(^Fig|^FIG)(ure|URE|\\.)[ ]+[0-9A-Z]{1,3}[ ]*$")
  grep(search_pattern, p3$text, value=T)
  index3 <- grep(search_pattern, p3$text, value=F)
  captions <- rbind(captions, p3[index3,])
  p4 <- p3[-index3,]
  p4$text
  
  # fourth pass
  (search_pattern <- "FIGURE:")
  grep(search_pattern, p4$text, value=T)
  index4 <- grep(search_pattern, p4$text, value=F)
  captions <- rbind(captions, p4[index4,])
  p5 <- p4[-index4,]
  p5$text
  
  captions <- arrange(captions, number)
  
  return(captions)  
}

#####################################################################################
get_file_checksums <- function(docid)
{
  doc_filename <- paste0("doc", docid, "_*")
  # set up call
  sumCall <- paste0("md5sum ", file.path(imageDir, doc_filename))
  # get checksums
  s1 <- suppressWarnings(system(sumCall, intern=T, ignore.stderr=T))
  # if nothing, just return
  if(length(s1) == 0) return(NULL) ;
  # remove extra spaces
  s1 <- gsub("[ ]{2,}", " ", s1)
  # create data structure 
  m1 <- do.call("rbind", lapply(strsplit(s1, " "), matrix, ncol=2))
  d8 <- as.data.frame(m1, stringsAsFactors=F)
  names(d8) <- c('checksum', 'path')
  d8$filename <- basename(d8$path)
  return(d8)
}  

#####################################################################################
remove_duplicate_files <- function(docid)
{
  d8 <- get_file_checksums(docid)
  if(is.null(d8)) return(NULL)
  
  # remove duplicate files
  file2 <- d8$path[duplicated(d8$checksum) | duplicated(d8$checksum, fromLast=TRUE)]
  if(length(file2) > 0) {
    logdata("Removing", length(file2), "duplicate files (based on md5sum())")
    quiet <- file.remove(file2)
  }
}

#####################################################################################
remove_known_files <- function(docid)
{
  # these are checksums for known files (logos etc) that we can safely delete
  # When you want to add to the checksums, use the Linux checksum utility: "md5sum"
  #   and add to the list - note: one long nubmer and one short number
  checksums <- c(
    '12aabba718230509c2270907c386fedc', '19cf3d83be6fb5eb3531b185d2f58a32',
    '257f20c91696d5823270a5991c7080c8', '46805467fac4b5d10c3c266b6b1b2886',
    '572164a4138f3a8122b33cebe227b9d4', '911ddb0520f429102e9f004e59a8717e',
    'a37f9eb9662e5785b0d5fd268fe143c1', 'bab70260c97221c6f1a569f4cdd2efef',
    'bdb1083442992734c87599fae79474b7', 'de4b069eef84a20acaf7dc979161c5bb',
    'e848a3c8735f86b33fd0f1797ef977f9', '2c16a47fdd7f979891db43571ffe311e'
    ) 


  d8 <- get_file_checksums(docid)
  if(is.null(d8)) return(NULL)
  
  # remove known files
  for(i in seq_len(nrow(d8))) {
    if(any(grep(d8$checksum[i], checksums))) {
      logdata("removing known file:", d8$path[i])
      file.remove(d8$path[i])
    }
  }
}  

#####################################################################################
extract_images <- function(docid, filename, remove_old_files=F)
{
  pdfDir <- file.path(dataDir, 'top50alz')
  #filename <- calldb(paste("select filename from docs where docid=", docid))$filename
  doc_pattern <- paste0("doc", docid, "_*")

  # 1. remove all temporary files in the temp directory
  if(remove_old_files) do.call(file.remove, list(list.files(imageDir, full.names=T)))
  
  # 2. extract images
  filepath <- file.path(pdfDir, filename)
  image_root <- file.path(imageDir, paste0("doc", docid))
  imageCall <- paste0("pdfimages -p -png ", filepath, " ", image_root)
  
  # note that name are of the following format doc<docid>-<pagenumber>-<order>.png
  # These three units should make each file 100% unique

  output <- system(imageCall, wait = T, intern=T, ignore.stdout=F, ignore.stderr=T)
  
  # 3. rename files: substitute out the dash and in with and underscore 
  quiet <- sapply(list.files(imageDir, pattern=doc_pattern, full.names=F), function(xold) {
    xnew <- gsub("[- ]", "_", xold)
    file.rename(file.path(imageDir, xold), file.path(imageDir, xnew)) } )
  
  # 4. remove duplicates
  remove_duplicate_files(docid)
  
  # 5. remove known files such as logos etc.
  remove_known_files(docid)
  
  # 5. list files and return a dataframe
  
  s1 <- list.files(imageDir, pattern=doc_pattern)
  if(length(s1) == 0) return(NULL)
  
  dimages <- data.frame(filename=s1, file2=s1, stringsAsFactors = F) %>% 
    separate(col=file2, into=c('file3', 'ext'), sep="\\.") %>% 
    separate(col=file3, into=c('doc', 'page1', 'cnt'), sep="_") %>% 
    mutate(page = as.integer(page1))
  
  # filter only those that are for this particular docid
  doc_pattern <- paste0("doc", docid)
  dimages <- dimages %>% filter(doc_pattern == doc)
  
  if(nrow(dimages) == 0) return(NULL)
  
  # get checksums for files
  d8 <- get_file_checksums(docid)
  dimages <- full_join(dimages, d8, by="filename") %>% select(-path)
  
  # insert into database
  dimages.tmp <- dimages %>% 
    mutate(capid=-1, type='NA', docid=docid, linenumber=-1, caption='NA') %>%
    select(capid, imagefile=filename, docid, page, type, linenumber, caption, checksum)
  
  insert_into_images(docid, dimages.tmp, insert=T)
  
  return(dimages)
}

#####################################################################################
test2 <- function()
{
  dimage <- data.frame(image=c(3,4,6,8,9), page=c(3,3,4,5,5), stringsAsFactors=F)  # image
  dcap  <- data.frame(cap =c(1,2,3,5,4), page=c(3,3,4,5,5), stringsAsFactors=F)  # captions
  
  dimage <- data.frame(image=c(0,1,2,3,4,5,6,7,8,9,10), page=c(1,14,15,16,16,17,18,18,19,20,20), stringsAsFactors=F)  # image
  dcap  <- data.frame(cap =c(99,1,2,3,4), page=c(6,15,17,19,20), stringsAsFactors=F)  # captions
  
  dimage ; dcap
  dx <- full_join(dimage,  dcap, by="page") %>% select(cap, image, page) %>%
    filter(!is.na(cap) & !is.na(image))
  dx
  
  d7 <- data.frame()
  for(i in seq_len(nrow(dx))) {
    if(i == 1) {
      d7 <- rbind(d7, dx[i,])
      next
    }
    if(dx$cap[i] %in% d7$cap | (dx$image[i] %in% d7$image & dx$page[i] %in% d7$page)) {
      next
    }
    d7 <- rbind(d7, dx[i,])
  }
  d7
}

#####################################################################################
extract_pages_and_text <- function(docid)
{
  dimages <- calldb(paste("select * from images where docid=", docid, "; "))
  
  if(nrow(dimages) == 0) {
    logdata("extract_pages_and_text - no images to work from - docid:", docid)
    return(NULL)
  }
  
  pdfDir <- file.path(dataDir, 'top50alz')
  filename <- calldb(paste("select filename from docs where docid=", docid))$filename
  
  # 1. remove all temporary files in the temp directory
  do.call(file.remove, list(list.files(textDir, full.names=T)))
  
  # 2. create the text file
  (filepath <- file.path(pdfDir, filename))
  (tmpFile <- file.path(textDir, "tmp.txt"))
  (textCall <- paste0("pdftotext -raw ", filepath, " ", tmpFile))
  system(textCall, wait = T, intern=T, ignore.stdout=F, ignore.stderr=T)
  
  # 3. read in the file and parse out page numbers
  txt1 <- suppressWarnings(readLines(tmpFile)) # don't mind warning about "incomplete final line..."
  length(txt1)
  (pageline <- grep("\f", txt1))
  (pageline <- c(1, pageline))
  dtext <- data.frame(page=seq_len(length(pageline)-1), start=pageline[1:(length(pageline)-1)], end=(pageline[2:length(pageline)]-1))
  
  # remove formfeed
  txt1 <- gsub("\f", "", txt1)
  
  #####
  # NOTE 
  # There is a function on top of this file named 'get_caption()'. We might want
  # to use that function here (May 22, 2017)
  #####
  
  # 4. get caption and line numbers
  search_pattern <- "(^FIG|^Fig|^Extended Data Fig)"
  (lineno  <- grep(search_pattern, txt1))
  caption <- grep(search_pattern, txt1, value=T)
  #(dcaption <- data.frame(lineno=grep(search_pattern, txt1), caption=grep("^Fig", txt1, value=T), stringsAsFactors = F))
  dcaption <- data.frame(lineno=lineno, caption=caption, stringsAsFactors = F)
  
  # remove captions that don't seem to be captions. Instead, they seem to be the start of a regular sentence.
  # Therefore, we try to match on a strings that starts with "F or E", then has some words  
  #search_pattern <- "(^FIG|^Fig|^Extended Data Fig).* [a-z]+"
  #search_pattern <- "(^FIG|^Fig|^Extended Data Fig)(ure|URE|\\.|) [0-9A-z]+[[:punct:]]*$"
  # try this search pattern
  #search_pattern <- "(^FIG|^Fig|^Extended Data Fig)(ure|URE|\\.|) [0-9A-z]+[[:punct:]]"
  # we're now saying that a caption can end with a numeric and possibly a 'space' and an uppper case
  #search_pattern <- "(^FIG|^Fig|^Extended Data Fig)(ure|URE|\\.|) [0-9A-z]+([[:punct:]]| [A-Z]|[ ]+)"
  dcaption$caption <- gsub(" ", "", dcaption$caption)
  search_pattern <- "(^FIG|^Fig|^Extended Data Fig)(ure|URE|\\.|) [0-9A-z]+([[:punct:]]| [A-Z]|$| \\|)"
  #(index <- grep(search_pattern, dcaption$caption, invert=F))
  (index <- grep(search_pattern, dcaption$caption, invert=T))
  dcaption$caption[index]
  if(length(index) > 0) dcaption <- dcaption[-index,]
  
  ############ start play 5
  if(0) {
  dx <- dcaption
  #search_pattern <- "(^FIG|^Fig|^Extended Data Fig)(ure|URE|\\.|) [0-9A-z]+[[:punct:]]*$"
  grep(search_pattern, dx$caption, invert=F, value=T)
  
  index <- grep(search_pattern, dx$caption, invert=F)
  if(length(index) > 0) dx <- dx[-index,]
  }
  ############ end play 5
  
  # 5. get caption page numbers
  dcaption$captpage <- sapply(dcaption$lineno, function(x) {
    for(i in seq_len(nrow(dtext))) { if(x >= dtext$start[i] & x <= dtext$end[i]) {return(dtext$page[i])} }
  })
  
  # if the data frame is empty, we need to cast the variable as integer, otherwise, we get yelled at later on
  if(nrow(dcaption) == 0) dcaption$captpage <- as.integer(dcaption$captpage)
  
  # 6. merge text, images and captions
  
  #dimages.save
  #dimages
  d8 <- dimages %>% select(filename=imagefile, page)
  (d1 <- full_join(dtext, d8, by="page") %>%  filter(!is.na(filename)))
  
  #d1; dcaption 
  #str(d1); str(dcaption)
  #dcaption9 <- dcaption
  #d19 <- d1
  #dcaption3 <- dcaption
  #d13 <- d1
  if(0) {
    #save(dcaption9, d19, dcaption3,d13, file=file.path(codeDir, "save_doc9_doc3.rdata"))
    #load(file=file.path(codeDir, "save_doc9_doc3.rdata"))
    dcaption <- dcaption9
    d1 <- d19
    dcaption <- dcaption3
    d1 <- d13
  }

  
  ######### start of play 6
  d1 
  dcaption
  if(nrow(d1) == nrow(dcaption)) {
    d1       <- arrange(d1, filename, page)
    dcaption <- arrange(dcaption, lineno, captpage)
    (djoin <- cbind(d1, dcaption) %>% select(-captpage))
    names(djoin)
  } else {
    (djoin <- full_join(d1, dcaption, by=c("page"="captpage")) %>%  filter(!is.na(filename)))
    names(djoin)
  }
  
  ######### end of play 6
  
  if(1) {
    # The idea here is that if an image "001" does not have a caption, then the next page's caption belongs
    # to the image "001". The reason is that many times, a caption is a caption for multiple images
    d7 <- data.frame()
    # start from the bottom and work upwards
    for(i in rev(seq_len(nrow(djoin)))) {
      if(is.na(djoin$caption[i])) {
        # if current line is NA, skip the rest
        next
      } else {
        d7 <- rbind(d7, djoin[i,])
        if(i != 1 && is.na(djoin$caption[i-1])) {
          # make sure it is on the same page, or one page above only
          if((djoin$page[i] - djoin$page[i-1]) <= 1) {
            djoin$caption[i-1] <- djoin$caption[i]
            djoin$lineno[i-1] <- djoin$lineno[i]
          }
        }
      }
    }
    (d7 <- arrange(d7, start))
  } else {
    d7 <- data.frame()
    for(i in seq_len(nrow(djoin))) {
      if(is.na(djoin$caption[i])) next
      if(i == 1) {
        d7 <- rbind(d7, djoin[i,])
        next
      }
      #if(djoin$cap[i] %in% d7$cap | (djoin$filename[i] %in% d7$filename & djoin$page[i] %in% d7$page)) next
      if(djoin$caption[i] %in% d7$caption | (djoin$filename[i] %in% d7$filename & djoin$page[i] %in% d7$page)) next
      d7 <- rbind(d7, djoin[i,])
    }
  }
  d7
  image_meta <- d7
  
  # 7. extract figure ID - this should be the first set of numbers 
  image_meta <- image_meta %>% mutate(capid = str_extract(caption, "[0-9]+"))
  
  # insert into database
  if(nrow(image_meta) == 0) {
    logdata("No image meta data for docid", docid, " so no insert into the database")
  } else {
    
    df <- image_meta %>% mutate(type="lines", docid=docid) %>%
      select(capid, imagefile=filename, docid, page, type, linenumber=lineno, caption) %>%
      mutate(capid  = ifelse(is.na(capid), -1, capid)) %>%
      mutate(linenumber = ifelse(is.na(linenumber), -1, linenumber)) %>%
      mutate(caption = ifelse(is.na(caption), 'NA', caption)) %>%
      arrange(capid)
    df <- df %>% arrange(desc(imagefile))
    # remove the doubles check
    #index <- which(duplicated(df$capid))
    #df$capid[index] <- df$linenumber[index] <- df$caption[index] <- -1
    df <- df %>% arrange(imagefile)
    image_meta <- df
    
    if(any(duplicated(image_meta$imagefile))) 
      logdata("images.r: WARNING - image_meta$imagefile is not unique - will not be able to write to database - docid:", docid)
    update_images(docid, image_meta)
  }
  
  return(image_meta)
}

#####################################################################################
test1 <- function()
{
  parans <- parans.save
  
  # search pattern is to find "(Fig 3)" and "Fig (3)"
  (base_pattern <- "[Ff]ig(ure)?[.s ]*")
  (search_pattern1 <- paste0("\\(", base_pattern, ".*?\\)"))
  (search_pattern2 <- paste0(base_pattern, "\\(.*?\\)"))
  (search_pattern <- paste0("(", search_pattern1, "|", search_pattern2, ")"))
  
  grep(search_pattern, "xx (Fig 3) yy", value=T)
  grep(search_pattern, "xx Fig (3) yy", value=T)
  grep(search_pattern, "xx Fig 3) yy", value=T)
  grep(search_pattern, "xx (Fig 3 yy", value=T)
  
  (parans <- data.frame(linenumber=1, text="xx (Figure 1, stage 2) yy", stringsAsFactors=F))
  (parans <- data.frame(linenumber=1, text="xx (Fig 1, Fig 2) yy", stringsAsFactors=F))
  (parans <- data.frame(linenumber=c(1,2,3), text=c("xx (Fig 1, Fig 2) yy", "xx (Fig 3, Fig 4) yy", "xx"), stringsAsFactors=F))
  (parans <- data.frame(linenumber=c(1,2,3), text=c("1", "2, 3", "x"), stringsAsFactors=F))
  (parans <- parans.save)
  
  parans
  (d1 <- parans %>%  mutate(actual_figs = sapply(str_extract_all(text, search_pattern), toString)))
  (d1 <- d1 %>% mutate(capid = sapply(str_extract_all(actual_figs, "[0-9]+"), toString)))
  (d1 <- d1 %>% mutate(tmp1 = paste(actual_figs)))
  (d1 <- d1 %>%  mutate(tmp2 = trim(tmp1)))
  (d1 <- d1 %>%  filter(capid != ''))
  (d1 <- d1 %>%  select(linenumber, text, actual_figs=tmp2, capid))
  if(nrow(d1) > 0) d1 <- separate_rows(d1,capid, sep=",") %>% mutate(capid = trim(capid))
  
  # ------------ current good one - start
  df.parans <- parans %>% 
    mutate(actual_figs = str_extract_all(text, search_pattern)) %>%  # extract all real figures matches 
    mutate(list1 = str_extract_all(actual_figs, "[0-9]+")) %>% # get all numerics
    #mutate(capid = paste(sapply(list1, function(x) x[x != ""]), collapse=",")) %>%  # remove blanks
    mutate(capid = sapply(list1, function(x) x[x != ""])) %>%  # remove blanks
    #mutate(capidx = toString(capid)) %>%
    mutate(tmp1 = paste(actual_figs)) %>%   # make into characters
    mutate(tmp2 = trim(tmp1)) %>%   # trim fluff 
    #mutate(capid = ifelse(capid==0, '', capid)) %>%   # remove zeros
    filter(capid != '') %>%
    select(linenumber, text, actual_figs=tmp2, capid)
  df.parans
  (df.parans$capid <- sapply(df.parans$capid, function(x) paste(x, collapse=",")))
  
  if(nrow(df.parans) > 0) df.parans <- separate_rows(df.parans,capid, sep=",")
  df.parans
  # ------------ current good one - end
    
}
  
#####################################################################################
find_and_pair_image_references <- function(docid)
{
  # --------------------------------------------------
  # get data from database
  sent1 <- calldb(paste0("select linenumber, text from text where type='sent' and docid=", docid, ";"))
  
  # note that we can't filter out all sentences in the references section. The reason is that in some documents,
  # the images are stored (as references) below the reference section

  ############ start of play 1
  #names(sent1)
  #grep("Figure", sent1$text, value=T)
  #grep("Figure", sent1$text, value=F)
  ############ end of play 1
  
  # --------------------------------------------------
  base_pattern <- "[Ff]ig(ure)?[.s ]*"
  
  # first thing first - through out all sentences that starts with a Fig. The assumption
  # is that these "Fig..." are Captions so therefore we don't want them here
  index1 <- grep(paste0("^", base_pattern), sent1$text)
  if(length(index1) > 0) sent1 <- sent1[-index1,]
  
  # --------------------------------------------------
  # 0. Figure out which has open parans and which do not
  if(0) {
  search_pattern1 <- "[Ff]ig(ure)?(s)?(\\.)? \\("  # when open paran is at the end
  search_pattern2 <- "\\([Ff]ig(ure)?(s)?(\\.)?"   # when open paran is at the start
  search_pattern <- paste0("(", search_pattern1, "|", search_pattern2, ")")
  index2 <- grep(search_pattern, sent1$text, value=F)
  parans <- sent1[index2,] # as open parans
  } else {
  search_pattern1 <- "[Ff]ig(ure)?(s)?(\\.)? \\("  # when open paran is at the end
  search_pattern2 <- "\\([Ff]ig(ure)?(s)?(\\.)?"   # when open paran is at the start
  search_pattern3 <- "\\([Ss]ee [Ff]ig(ure)?(s)?(\\.)?"   # when open paran is at the start
  search_pattern <- paste0("(", search_pattern1, "|", search_pattern2, "|", search_pattern3, ")")
  index2 <- grep(search_pattern, sent1$text, value=F)
  #sent1[index2,]
  parans <- sent1[index2,] # as open parans
  }
  
  # search for base pattern in all the text 
  search_pattern <- base_pattern
  search_pattern <- "[Ff]ig(ure)?[.s ]*"
  #index3 <- grep(search_pattern, sent1$text[-index2], value=F)
  index3 <- grep(search_pattern, sent1$text, value=F)
  
  index3 <- setdiff(index3, index2)
  noparans <- sent1[index3,] # does not have open params
  
  # --------------------------------------------------
  # 1. find with open parans    - end char are )
  
  search_pattern1 <- paste0("\\(", base_pattern, ".*?\\)")
  search_pattern2 <- paste0(base_pattern, "\\(.*?\\)")
  search_pattern <- paste0("(", search_pattern1, "|", search_pattern2, ")")
  
  (d1 <- parans %>%  mutate(actual_figs = sapply(str_extract_all(text, search_pattern), toString)))
  (d1 <- d1 %>% mutate(capid = sapply(str_extract_all(actual_figs, "[0-9]+"), toString)))
  (d1 <- d1 %>% mutate(tmp1 = paste(actual_figs)))
  (d1 <- d1 %>% mutate(tmp2 = trim(tmp1)))
  (d1 <- d1 %>% filter(capid != ''))
  (d1 <- d1 %>% select(linenumber, text, actual_figs=tmp2, capid))
  if(nrow(d1) > 0) d1 <- separate_rows(d1,capid, sep=",") %>% mutate(capid = trim(capid))
    
  df.parans <- d1
  
  # --------------------------------------------------
  # 2. find without open parans - end chars are .,
  search_pattern <- paste0(base_pattern, "[A-z]*[0-9]+[A-z]*[ .,]+")
  
  noparans
  (d1 <- noparans %>%  mutate(actual_figs = sapply(str_extract_all(text, search_pattern), toString)))
  (d1 <- d1 %>% mutate(capid = sapply(str_extract_all(actual_figs, "[0-9]+"), toString)))
  (d1 <- d1 %>% mutate(tmp1 = paste(actual_figs)))
  (d1 <- d1 %>% mutate(tmp2 = trim(tmp1)))
  (d1 <- d1 %>% filter(capid != ''))
  (d1 <- d1 %>% select(linenumber, text, actual_figs=tmp2, capid))
  if(nrow(d1) > 0) d1 <- separate_rows(d1,capid, sep=",") %>% mutate(capid = trim(capid))
    
  df.noparans <- d1
  
  if(nrow(df.noparans) > 0) df.noparans <- separate_rows(df.noparans,capid, sep=",")
  
  df1 <- bind_rows(df.parans, df.noparans)
  df1$capid <- as.numeric(df1$capid)
  
  #### do some heuristic clean up as we see some non-valid sentences
  df1 <- df1 %>% filter(!grepl("Author manuscript; available in", text))
  
  if(0) {
    dfim <- calldb(paste0("select capid, imageid from images where docid=", docid, ";"))
    
    #df <- full_join(dfim, df1, by="capid") %>%
    df <- left_join(df1, dfim, by="capid") %>%
      mutate(type="sent", docid=docid) %>%
      select(capid, docid, type, linenumber, imageid, actual_figs) %>%
      mutate(linenumber = ifelse(is.na(linenumber), -1, linenumber)) %>%
      filter(!is.na(imageid)) %>% 
      filter(capid != -1) %>%
      arrange(capid, linenumber)
  } else {
    #df1.save <- df1
    #df1 <- df1.save
    #names(df1)
    #df <- left_join(df1, dfim, by="capid") %>%
    df <- df1 %>% mutate(type="sent", docid=docid) %>%
      select(capid, docid, type, linenumber, actual_figs) %>%
      mutate(linenumber = ifelse(is.na(linenumber), -1, linenumber)) %>%
      filter(capid != -1) %>%
      arrange(capid, linenumber)
    df
  }
  
  (df <- unique(df))
  
  # if df is empty, just return
  if(nrow(df) == 0) return(NULL)
  
  #(df <- df %>% filter(linenumber %in% c(31, 177, 92,161,230)))
  
  # - the irefs table is a relationship table between the images table and the imagerefs table.
  # - the images table stores the images, while the imagerefs table stores the sentences that 
  #   point to the images.
  # - because there might be many sentences pointing to the same image, and there might
  #   might be many images pointed to by one sentence (many-to-many relationship), we 
  #   need a relationship table 'irefs' to sit in the middle and handle the relationships 
  # May 18, 2017
  
  # normalize to unique sentence (line) numbers
  (df7 <- df)
  (df7 <- df7 %>% select(-actual_figs))
  (df7 <- unique(df7))
  (x <- row.names(df7))
  (df7 <- df[x,])
  
  #(index <- which(duplicated(df7$linenumber)))
  #(if(length(index) > 0) df7 <- df7[-index,])
  
  # insert into imagerefs table, and then extract the data to get the irefid (unique reference IDs)
  insert_into_imagerefs(docid, df7)
  (df8 <- calldb(paste0("select irefid, capid, docid, type, linenumber from imagerefs where docid='", docid, "' ;")))
  
  ## join the irefid and the imageids based on line numbers. Then insert into the irefs table
  #(df9 <- full_join(df7, df8, by="linenumber") %>% select(irefid, imageid))
  #insert_into_irefs(df9)
}

#####################################################################################
total_clean_the_system_for_database_and_directories <- function(DELETE_ALL=F)
{
  if(DELETE_ALL) {
    logdata("deleting files and content of database tables")
    calldb("delete from imagerefs;") ; calldb("delete from images;")
    calldb("select count(*) from imagerefs;") ; calldb("select count(*) from images;")
    unlink(file.path(imageDir, "*.png"))
  }
}

#####################################################################################
#  DELETE_IMAGES_TABLES=T
do_images <- function(pdfDocs, pdfDir, DELETE_IMAGES_TABLES=F)
{
  if(DELETE_IMAGES_TABLES) total_clean_the_system_for_database_and_directories(DELETE_ALL=T)

  if(!is.null(pdfDocs)) {
    # pdfDocs is not NULL, then make sure that the file exists in the database before we proceed
    x1 <- calldb(paste0("select filename from docs where filename = '", pdfDocs, "';"))
    if(nrow(x1) == 0) {
      logdata("Error: file ", pdfDocs, "does not exists in the database - exiting")
      return(NULL)
    }
  }

  if(is.null(pdfDir))  pdfDir  <- file.path(dataDir, 'top50alz')
  if(is.null(pdfDocs)) pdfDocs <- calldb("select filename from docs where filename <> ''")$filename 

  for(i in seq_len(length(pdfDocs))) {
    docid <- calldb(paste0("select docid from docs where filename = '", pdfDocs[i], "';"))$docid
    if(length(docid) == 0) {
      logdata("Warning - could not find file '", pdfDocs[i], "' in table docs")
      next
    }
    docid <- as.integer(docid)
    
    logdata("----------------------------------------------------")
    logdata(paste(i, "running process on docid:", docid, " filename: ", pdfDocs[i]))
    retval <- extract_images(docid=docid, filename=pdfDocs[i], remove_old_files=F)
    if(is.null(retval)) next
    extract_pages_and_text(docid=docid)
    find_and_pair_image_references(docid=docid)
  }
}

#####################################################################################
# MAIN main Main
#
#  pdfDocs <- '20186853_Morris_APOE_predicts_amyloid_beta.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '20616000_Miller_Divergence_of_human_and_mouse.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '21436112_Fodero-Tavoletti_18F-ThK523_a_novel_in_vivo_tau_imaging_ligand.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '21532579_Bero_Neuronal_activity_regulates.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '22476197_Talbot_Demonstrated_brain_insulin_resistance.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '22801501_Johnson_A_mutation_in_APP.pdf' ; i <- 1 ; pdfDir <- NULL
#  pdfDocs <- '24162737_Lambert_Meta-analysis_of_74046_individuals.pdf' ; i <- 1 ; pdfDir <- NULL

logdata("==================== START OF RUN =================")
t1 <- proc.time()


process_file <- FALSE
if( length( commandArgs() ) == 6 ) {
  filenamex <- commandArgs()[6]
  if(tolower(filenamex) == 'all') {
    logdata("Do all files")
    pdfDocs <- pdfDir <- NULL
    process_file <- TRUE
  } else if(file.exists(filenamex))  {
    logdata("Process file:", filenamex)
    if(0){filenamex <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/data/top50alz/20042704_Petersen_ADNI_Clinical_characterization.pdf"}
    pdfDocs <- basename(filenamex)
    pdfDir <- dirname(filenamex)
    process_file <- TRUE
  } else if(!file.exists(filenamex))  {
    logdata("Error: file", filenamex, "does not exists - exiting")
    logdata("NOTE - we need absolute addresses to files")
    process_file <- FALSE
  }
} else {
  logdata("Input must be either 'all' for all files or a filename with an absolute file path") 
  process_file <- FALSE
}

if(process_file) do_images(pdfDocs, pdfDir, DELETE_IMAGES_TABLES=F)


s <- proc.time()[3] - t1[3]
s <- as.integer(s)
h <- s %/% 3600 ; s <- s - (h*3600) ; 
m <- s %/% 60   ; s <- s - (m*60)   ; 
logdata("")
logdata(sprintf("process time: %d hours %d minutes %d seconds", h, m, s))
logdata("==================== END OF RUN =================")

