library(groupdata2)


library(here)
source(here("missing data.R"))



##########################
##############################
# 
# set.seed(57)
# part<-s1 %>% 
#   mutate(gender_mis=forcats::fct_explicit_na(s1[,"gender"])) %>% 
#   partition(p=0.5, cat_col = "gender_mis", num_col = "age_years")
# 
# 
# train1<-as.data.frame(part[1])
# test1<-as.data.frame(part[2])
# 
# part<-s2 %>% 
#   mutate(gender_mis=forcats::fct_explicit_na(s2[,"gender"])) %>% 
#   partition(p=0.5, cat_col = "gender_mis", num_col = "age_years")
# 
# train2<-as.data.frame(part[1])
# test2<-as.data.frame(part[2])
#save(train1,test1, train2, test2, file="cross-validated data.Rdata")

test1 <- read.csv(here::here("data/test1_anonymized.csv"))
test2 <- read.csv(here::here("data/test2_anonymized.csv"))
train1 <- read.csv(here::here("data/train1_anonymized.csv"))
train2 <- read.csv(here::here("data/train2_anonymized.csv"))

test1 <- test1[,c(-1, -5,-6)]
############### check partition balance

t1<-t.test(test1$age_years, train1$age_years)
t2<-t.test(test2$age_years, train2$age_years)
library(effsize)


#prop of females
prop1<-prop.test(x=c(table(train1$gender)[2], table(test1$gender)[2]), n=c(sum(table(train1$gender)[2],table(train1$gender)[1]),
                                                                    sum(table(test1$gender)[2],table(test1$gender)[1])))


prop2<-prop.test(x=c(table(train2$gender)[2], table(test2$gender)[2]), n=c(sum(table(train2$gender)[2],table(train2$gender)[1]),
                                                                           sum(table(test2$gender)[2],table(test2$gender)[1])))





############### significant item differences

plot.df<-data.frame(item=names(big),
           lower.bound=NA,
           upper.bound=NA)


for(i in 1:172){
  plot.df$item[i]<-names(train1)[3+i]
  t.test.here<-t.test(train1[,3+i],test1[,3+i], conf.level=0.95)
  plot.df$lower.bound[i]<-t.test.here$conf.int[1]
  plot.df$upper.bound[i]<-t.test.here$conf.int[2]
}
for(i in 1:22){
  plot.df$item[i+172]<-names(train2)[3+i]
  t.test.here<-t.test(train2[,3+i],test2[,3+i], conf.level=0.95)
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

