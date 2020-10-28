library(here)
library(tidyverse)
library(readxl)


data <- read.csv(here::here("data/raw data_anonymized.csv")) #read data

library(readxl)
key<-read_excel(here::here("data/key.xlsx"), col_names = c("domain", "facet", "abbrev")) #read key facet-domains

source(here::here("data/facets.R")) #lists with facets and domains
source(here::here("data/domains.R"))

data[,10:375] <- as.data.frame(lapply(data[,10:375], as.numeric)) #items to numeric

data$agecat <- cut(data$age_years, c(0,13,99),c("0","1")) #Categorize age. Cut point 13 y'o

##5 participants have impossible ratings in item *empa_5m* (ratings of 6). 
#Their score in that item is going to be transformed to NA 

data$empa_5m[which(data$empa_5m > 5)]<-NA

#Two data frames: One with all the items (big). Another with the short version

big <- data[,which(colnames(data)%in% unlist(DIPSI_facets))]
small <- read.csv(here::here("data/small 98 i.csv"), stringsAsFactors = F) #this has been extracted from the cfa development.R script



DIPSI_small <- DIPSI_facets

for(i in 1:length(DIPSI_facets)){
  DIPSI_small[i][[1]] <- DIPSI_facets[i][[1]][which(as.character(unlist(DIPSI_facets[i]))%in%colnames(small))]
}

item.key <- read.csv(here::here("data/item key.csv"))
item.key <- item.key[,-1]

