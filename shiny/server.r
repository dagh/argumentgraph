#!/usr/bin/env Rscript 

#####################################################################################
rm(list=ls())

if(Sys.info()['nodename'] == 'hadoop') {
  rootDir <- "/home_ssd/dag/0-Annat/0-R/customers/mgh/code/shiny"
} else {
  rootDir <- "/home/ubuntu/mgh/code/shiny"
}

codeDir <- rootDir

source(file.path(codeDir, "utils.r"))
source(file.path(codeDir, "dataarg1.r"))
source(file.path(codeDir, "dataarg3.r"))

library(shiny)
library(visNetwork)
library(RNeo4j)
# NOTE - dplyr must be declared before igraph, otherwise 'as_data_frame()' bombs out on class 'igraph'
library(dplyr)
library(igraph)
#library(tidyr)

#####################################################################################

shinyServer(function(input, output) {
  source("./grapharg1.r", local = TRUE, encoding = "UTF-8")
  source("./grapharg3.r", local = TRUE, encoding = "UTF-8")
})
