
library(here)
library(psych)
library(lavaan)

source(here::here("data input.R"))
load(here::here("tools/final models cfa.Rdata"))
source(here::here("cross-validation.R"))

facets <- unique(item.key$facet)
theta.df <-as.data.frame(matrix(ncol=27, nrow = dim(test1[complete.cases(test1), ])[1]))
sumscore.df <-as.data.frame(matrix(ncol=27, nrow = dim(test1[complete.cases(test1), ])[1]))

my.cors<-numeric(length=27)

for(i in 1:31){
  if(i < 28){
    this.data <- test1[complete.cases(test1), ]
    small_vect<- lavPredict(cfa(models_lambda_rich[i][[1]], test1), this.data, method="EBM", type="lv") #theta scores for DIPSI brief
    big_vect <- rowSums(this.data[,names(small)[which(names(small) %in% item.key$item[item.key$facet==names(models_lambda_rich[i])])]]) #sumscores
  } else {
    this.data <- test2[complete.cases(test2[,c(-1:-3, -26)]), ]
    small_vect<- lavPredict(cfa(models_lambda_rich[i][[1]], this.data), this.data, method="EBM", type="lv")
    big_vect <- rowSums(this.data[,names(small)[which(names(small) %in% item.key$item[item.key$facet==names(models_lambda_rich[i])])]])
  }
  
  my.cors[i]<-cor(small_vect, big_vect, method = "spearman")
  
  if(i < 28){
  theta.df[,i]<-small_vect 
  sumscore.df[,i]<-big_vect 
  }
}


range(my.cors)
mean(my.cors)


names(my.cors) <- as.character(facets)
