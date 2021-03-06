library(mice)
library(here)
library(VIM)

source(here("data input.R"))


#####without oddity



s1 <- read.csv(here::here("data/raw data_anonymized_withoutODD.csv"))
s1 <- s1[,-1]

s2 <- read.csv(here::here("data/raw data_anonymized_withODD.csv"))
s2 <- s2[,-1]

s1.orig<-s1
s2.orig<-s2

s1<-s1[,c(1,3,4,6,7,8,which(names(s1)%in%names(big)))]  #numbers are socio vars (id, age, gender, status, therapy), then get only informant ratings
s1<-s1[,-which(names(s1) %in% unlist(DIPSI_facets[which(names(DIPSI_facets) %in% DIPSI_domains$ODD)]))]  #remove empty oddity vars


s1.2<-s2[,c(1,3,4,6,7,8,which(names(s2)%in%names(big)))]  #take the no oddity items from the sample containing oddity
s1.2<-s1.2[,-which(names(s1.2) %in% unlist(DIPSI_facets[which(names(DIPSI_facets) %in% DIPSI_domains$ODD)]))]


s1<-rbind(s1, s1.2)

s1 <- s1[,c(-1,-5,-6)]
#####ONLY ODDITY = S2 (without rest of DIPSI)



s2<-s2[,c(1,3,4,6,7,8,which(names(s2)%in%names(big)))]  #get only informant ratings
s2<-s2[,c(1:6,which(names(s2) %in% unlist(DIPSI_facets[which(names(DIPSI_facets) %in% DIPSI_domains$ODD)])))]

s2 <- s2[,c(-1,-5,-6)]

#### the following lines of code can not be reproduced with the anonymized data as id is needed for grouping


#total missing

# s1.missing<-sum(is.na(s1))/sum(sum(is.na(s1)),sum(!is.na(s1)))
# s2.missing<-sum(is.na(s2))/sum(sum(is.na(s2)),sum(!is.na(s2)))
# 
# total.missing<-(s1.missing+s2.missing)/2
# 
# 
# 
# #### MAR MCAR MNAR?
# 
# #pattern at the variable level
# 
# p_missing <- unlist(lapply(s1, function(x) sum(is.na(x))))/nrow(s1)
# #sort(p_missing[p_missing > 0], decreasing = TRUE)
# 
# 
# p_missing <- unlist(lapply(s2, function(x) sum(is.na(x))))/nrow(s2)
# #sort(p_missing[p_missing > 0], decreasing = TRUE)
# 
# #pattern per subject
# 
# p_row_missing <- s1 %>% 
#   group_by(idnr) %>% 
#   gather(variable, value, c(-idnr,-age_child,-age_years,-gender,-status,-therapy)) %>% # 
#   summarise(missing_n = sum(is.na(value)))
# 
# p_row_missing<-as.data.frame(p_row_missing[order(p_row_missing$missing_n, decreasing = T),])
# 
# 
# n.half.miss.s1<-length(which(p_row_missing$missing_n>86))
# 
# 
# 
# full.missings.s1<-p_row_missing[p_row_missing$missing_n>86,"idnr"] #discard people with more than 50% of missings
# 
# 
# 
# p_row_missing <- s2 %>% 
#   group_by(idnr) %>% 
#   gather(variable, value, c(-idnr,-age_child,-age_years,-gender,-status,-therapy)) %>% # 
#   summarise(missing_n = sum(is.na(value)))
# 
# p_row_missing<-as.data.frame(p_row_missing[order(p_row_missing$missing_n, decreasing = T),])
# 
# 
# n.half.miss.s2<-length(which(p_row_missing$missing_n>10))
# 
# 
# 
# full.missings.s2<-p_row_missing[p_row_missing$missing_n>10,"idnr"] #discard people with more than 50% of missings
# 
# 
# unique(c(full.missings.s1, full.missings.s2))#unique ids of the 65 subjects who had more than 50% missing
# 
# 
# s1<-s1[-which(s1$idnr%in%full.missings.s1),]
# s2<-s2[-which(s2$idnr%in%full.missings.s2),]
# 
# 
# 
# 
# #####################after removal
# 
# 
# 
# #total
# 
# s1.missing.after<-sum(is.na(s1))/sum(sum(is.na(s1)),sum(!is.na(s1)))
# s2.missing.after<-sum(is.na(s2))/sum(sum(is.na(s2)),sum(!is.na(s2)))
# 
# total.missing.after<-(s1.missing.after+s2.missing.after)/2
# 
# 
# 
# 
# #pattern at the variable level
# 
# p_missing <- unlist(lapply(s1, function(x) sum(is.na(x))))/nrow(s1)
# sort(p_missing[p_missing > 0], decreasing = TRUE)
# 
# 
# p_missing <- unlist(lapply(s2, function(x) sum(is.na(x))))/nrow(s2)
# sort(p_missing[p_missing > 0], decreasing = TRUE)
# 
