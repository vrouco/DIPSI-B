library(groupdata2)


library(here)
source(here("scripts/missing data.R"))
#rm(list=ls()[-c(5,6,10,11, 23)])


#############################
#there can be a ptoblem with the partition algorithm.
#first I split the data in those who have the oddity items and those who dont have
#then I split these two datasets in the train and test data
# the problem is that thre can be a subject who is in the train and test data at the same time
#this was not a problem when I was cross validating facet models
# but it becomes a problem when a need to test the model at the dimensional level
# next there is a beginning on how I could solve it, but if I proceed it may change ALL results!

# trial <- merge(s1, s2, by=c("idnr", "age_years", "age_child", "gender", "status", "therapy"))
# 
# trial %>% 
#   mutate(gender_mis=forcats::fct_explicit_na(trial[,4])) %>% 
#   partition(p=0.5, cat_col = "gender_mis", num_col = "age_years")
##############################

set.seed(57)
part<-s1 %>% 
  mutate(gender_mis=forcats::fct_explicit_na(s1[,4])) %>% 
  partition(p=0.5, cat_col = "gender_mis", num_col = "age_years")


train1<-as.data.frame(part[1])
test1<-as.data.frame(part[2])

part<-s2 %>% 
  mutate(gender_mis=forcats::fct_explicit_na(s2[,4])) %>% 
  partition(p=0.5, cat_col = "gender_mis", num_col = "age_years")

train2<-as.data.frame(part[1])
test2<-as.data.frame(part[2])
#save(train1,test1, train2, test2, file="cross-validated data.Rdata")

rm(part)

############### check partition balance

t1<-t.test(test1$age_years, train1$age_years)
t2<-t.test(test2$age_years, train2$age_years)
library(effsize)
d1<-effsize::cohen.d(na.omit(test1$narc_1m), na.omit(train1$narc_1m))
d2<-effsize::cohen.d(na.omit(test1$sepa_4m), na.omit(train1$sepa_4m))

#prop of females
prop1<-prop.test(x=c(table(train1$gender)[2], table(test1$gender)[2]), n=c(sum(table(train1$gender)[2],table(train1$gender)[1]),
                                                                    sum(table(test1$gender)[2],table(test1$gender)[1])))


prop2<-prop.test(x=c(table(train2$gender)[2], table(test2$gender)[2]), n=c(sum(table(train2$gender)[2],table(train2$gender)[1]),
                                                                           sum(table(test2$gender)[2],table(test2$gender)[1])))





############### plot of significant item differences

plot.df<-data.frame(item=names(big),
           lower.bound=NA,
           upper.bound=NA)


for(i in 1:172){
  plot.df$item[i]<-names(train1)[6+i]
  t.test.here<-t.test(train1[,6+i],test1[,6+i], conf.level=0.95)
  plot.df$lower.bound[i]<-t.test.here$conf.int[1]
  plot.df$upper.bound[i]<-t.test.here$conf.int[2]
}
for(i in 1:22){
  plot.df$item[i+172]<-names(train2)[6+i]
  t.test.here<-t.test(train2[,6+i],test2[,6+i], conf.level=0.95)
  plot.df$lower.bound[i+172]<-t.test.here$conf.int[1]
  plot.df$upper.bound[i+172]<-t.test.here$conf.int[2]
}


cohensd <- data.frame(item = as.character(plot.df[which(plot.df$lower.bound>0 | plot.df$upper.bound<0),"item"]))

plot.df[which(plot.df$lower.bound>0 | plot.df$upper.bound<0),]

for(i in 1:length(cohensd$item)){
  tryCatch({
  cohensd$d[i] <- effsize::cohen.d(na.omit(test1[,paste(cohensd$item[i])]), na.omit(train1[,paste(cohensd$item[i])]))$estimate
  }, error = function(e){
    cohensd$d[i] <- effsize::cohen.d(na.omit(test2[,paste(cohensd$item[i])]), na.omit(train2[,paste(cohensd$item[i])]))$estimate
  })
  
}

