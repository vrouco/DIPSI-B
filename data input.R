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

item.key<-data.frame(
  item=character(length=length(as.character(names(attr(data, "variable.labels"))))),
  item.label=character(length=length(as.character(names(attr(data, "variable.labels")))))
)

oddity.key<- readxl::read_excel(here("data/DIPSI oddity items.xlsx"), col_names = T)

#add items 4 domain version 
item.key$item=as.character(names(attr(data, "variable.labels")))
item.key$item.label=as.character(attr(data, "variable.labels"))

#add items oddity
item.key$item.label[which(item.key$item%in%oddity.key$label)]<-
  oddity.key$item.thirdPerson.victor[which(oddity.key$label%in%item.key$item)]



item.key<-item.key[-which(item.key$item.label==""),]

for(i in 1:length(item.key$item)){
  item.key$facet[i]<-names(DIPSI_facets)[grep(item.key$item[i],DIPSI_facets)]
}

prekey<-key

for(i in 1:length(item.key$item)){
  item.key$facet[i]<-names(DIPSI_facets)[grep(item.key$item[i],DIPSI_facets)]
  item.key$facet.label[i]<-prekey$facet[grep(item.key$facet[i], prekey$abbrev)]
  item.key$domain.label[i]<-prekey$domain[grep(item.key$facet[i], prekey$abbrev)]
}

group<-list(Extreme_Achievement =c(1:4),
            Extreme_Order = c(5:10),
            Perfectionism = c(11:15),
            Affective_Lability = c(16:21),
            Disorderliness = c(22:29),
            Distraction = c(30:36),
            Dominance_Egocentrism = c(37:44),
            Hyperactiveness = c(45:51),
            Hyperexpressiveness = c(52:59),
            Impulsivity = c(60:63),
            Inflexibility = c(64:72),
            Irritable_Aggressive = c(65:73),
            Narcissistic = c(74:81),
            Resistance = c(82:86),
            Risk_Taking = c(87:92),
            Anxious_Traits = c(93:99),
            Dependency = c(100:105),
            Ineffective_Coping = c(106:113),
            Insecure_Attachment = c(114:117),
            Lack_Empathy = c(118:127),
            Lack_Self_Confidence = c(128:131),
            Separation_Anxiety = c(132:134),
            Submissiveness = c(135:142),
            Paranoid_Traits = c(143:147),
            Shyness = c(148:155),
            Withdrawn = c(156:161))

key<-item.key
key<-key[order(key$domain.label, key$facet.label),]
key$in.brief<-key$item%in%colnames(small)
key$facet<-sub("_m","_rm", key$facet)

setwd(here())
