
library(psych)
library(here)
library(lavaan)
library(userfriendlyscience)#for ordinal alpha and omega

source(here("data input.R"))

load(here::here("tools/final models cfa.Rdata"))
source(here("tools/functions.R"))#for the compareplot function
source(here("cfa development.R"))


val_table <- data.frame(facet=NA,
                        alpha=NA,
                        omega=NA,
                        chisq=NA,
                  
                        pval=NA,
                        
                        CFI=NA,
                        RMSEA=NA,
                        SRMR=NA)



for(i in 1:(length(DIPSI_facets))){
  
  if(i < 28){
  val_table[i,1]<-as.character(unique(item.key$facet.label[item.key$facet==names(models_lambda_rich)[i]]))
  val_table[i,2]<-round(psych::omega(test1[,DIPSI_small[i][[1]]], plot = F)$alpha, 3)
  val_table[i,3]<-round(psych::omega(test1[,DIPSI_small[i][[1]]], plot=F)$omega.tot, 3)
  #val_table[i,2]<-round(alpha(test1[,DIPSI_small[i][[1]]], check.keys=T)$total$std.alpha,2)
  #val_table[i,3]<-round(omega(test1[,DIPSI_small[i][[1]]], plot = F)$omega.tot,2)
  trash<-cfa(model=models_lambda_rich[i][[1]], data=test1, ordered=names(test1))
  val_table[i,4]<-paste(round(fitMeasures(trash)["chisq"],2), "(",fitMeasures(trash)["df"], ")",sep="")
  val_table[i,5]<-round(fitMeasures(trash)["pvalue"],3)
  val_table[i,7]<-round(fitMeasures(trash)["rmsea"],3)
  val_table[i,6]<-round(fitMeasures(trash)["cfi"],3)
  val_table[i,8]<-round(fitMeasures(trash)["srmr"],3)
  } else if(i > 27){
    val_table[i,1]<-as.character(unique(item.key$facet.label[item.key$facet==names(models_lambda_rich)[i]]))
    val_table[i,2]<-round(psych::omega(test2[,DIPSI_small[i][[1]]], plot = F)$alpha, 3)
    val_table[i,3]<-round(psych::omega(test2[,DIPSI_small[i][[1]]], plot=F)$omega.tot, 3)
    #val_table[i,2]<-round(alpha(test1[,DIPSI_small[i][[1]]], check.keys=T)$total$std.alpha,2)
    #val_table[i,3]<-round(omega(test1[,DIPSI_small[i][[1]]], plot = F)$omega.tot,2)
    trash<-cfa(model=models_lambda_rich[i][[1]], data=test2, ordered=names(test2))
    val_table[i,4]<-paste(round(fitMeasures(trash)["chisq"],2), "(",fitMeasures(trash)["df"], ")",sep="")
    val_table[i,5]<-round(fitMeasures(trash)["pvalue"],3)
    val_table[i,7]<-round(fitMeasures(trash)["rmsea"],3)
    val_table[i,6]<-round(fitMeasures(trash)["cfi"],3)
    val_table[i,8]<-round(fitMeasures(trash)["srmr"],3)
  }
  
}

#save(val_table, file="validation table.Rdata")


