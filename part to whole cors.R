library(here)
library(psych)
library(lavaan)

source(here::here("data input.R"))
load(here::here("tools/final models cfa.Rdata"))
source(here::here("cross-validation.R"))

facets <- unique(key$facet)
big.df <-as.data.frame(matrix(ncol=length(facets), nrow = dim(big)[1]))
small.df <-as.data.frame(matrix(ncol=length(facets), nrow = dim(small)[1]))

this.data <- test1[complete.cases(test1), ]
my.cors<-numeric(length=27)

for(i in 1:31){
  if(i < 28){
    this.data <- test1[complete.cases(test1), ]
small_vect<- predict(cfa(models_lambda_rich[i][[1]], test1), this.data) #theta scores for DIPSI brief
big_vect <- rowSums(this.data[,names(big)[which(names(big) %in% item.key$item[item.key$facet==names(models_lambda_rich[i])])]]) #sumscores for original DIPSI
  } else {
    this.data <- test2[complete.cases(test2[,c(-1:-3, -26)]), ]
    small_vect<- predict(cfa(models_lambda_rich[i][[1]], this.data), this.data)
    big_vect <- rowSums(this.data[,names(big)[which(names(big) %in% item.key$item[item.key$facet==names(models_lambda_rich[i])])]])
}

my.cors[i]<-cor(small_vect, big_vect, method = "spearman")}

names(my.cors)<-names(models_lambda_rich)

mean(my.cors)
range(my.cors)



