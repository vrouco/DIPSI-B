library(here)
library(lavaan)
load(here::here("tools/final models cfa.Rdata"))
source(here::here("cross-validation.R"))
source(here::here("tools/functions.R"))
options(scipen=999)


set.seed(999)


key<-item.key


test1$agecat <- cut(test1$age_years, c(0,13,99),c("0","1"))
test2$agecat <- cut(test2$age_years, c(0,13,99),c("0","1"))




mi<-data.frame(facet=NA,
               conf.chi=NA,
               conf.df=NA,
               conf.p=NA,
               conf.cfi=NA,
               conf.rmsea=NA,
               met.chi=NA,
               met.df=NA,
               met.p=NA,
               met.cfi=NA,
               met.rmsea=NA,
               sca.chi=NA,
               sca.df=NA,
               sca.p=NA,
               sca.cfi=NA,
               sca.rmsea=NA)



for(i in 1:31){
  
  if(i <= 27){
    this.data <- test1
  } else {this.data <- test2}
  conf<-tryCatch({
    cfa(models_lambda_rich[i][[1]], this.data, ordered=names(this.data),
        group="agecat") 
  }, error=function(e){
    conf<-cfa(models_lambda_rich[i][[1]], this.data, estimator="MLR",
              group="agecat") 
    return(conf)})
  
  
  metric <- tryCatch({cfa(models_lambda_rich[i][[1]], this.data,  ordered=names(this.data), 
                          group="agecat",group.equal="loadings")}, error=function(e){
                            metric<-cfa(models_lambda_rich[i][[1]], this.data,  estimator="MLR", 
                                        group="agecat",group.equal="loadings")
                            return(metric)})
  
  scalar <- tryCatch({cfa(models_lambda_rich[i][[1]], this.data,  ordered=names(this.data), 
                          group="agecat",group.equal=c("loadings", "thresholds"))}, error=function(e){
                            scalar<-cfa(models_lambda_rich[i][[1]], this.data,  estimator="MLR", 
                                        group="agecat",group.equal=c("loadings", "intercepts"))
                            return(scalar)})
  
  if(conf@Options$estimator=="ML"){
    mi[i,2:6] <- round(fitMeasures(conf)[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled", "rmsea.scaled")], 3)
    
    fit.metric <- round(fitMeasures(metric)[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled", "rmsea.scaled")], 3)
    fit.scalar <- round(fitMeasures(scalar)[c("chisq.scaled","df.scaled","pvalue.scaled","cfi.scaled", "rmsea.scaled")], 3)
  } else {
    mi[i,2:6] <- round(fitMeasures(conf)[c("chisq","df","pvalue","cfi", "rmsea")], 3)
    
    fit.metric <- round(fitMeasures(metric)[c("chisq","df","pvalue","cfi", "rmsea")], 3)
    fit.scalar <- round(fitMeasures(scalar)[c("chisq","df","pvalue","cfi", "rmsea")], 3)}
  
  first<-lavTestLRT(conf, metric)
  second<-lavTestLRT(metric, scalar)
  
  mi[i,c(7,8,9)] <- round(as.numeric(first[2,c(5,6,7)]), 3)
  mi[i,c(10,11)] <- round(fit.metric[c(4,5)] - mi[i,c(5,6)], 3)
  if(mi[i,7] == 0){
    mi[i,c(7,8,9,10,11)] <- rep("-",5)}
  
  
  mi[i,c(12,13,14)] <- round(as.numeric(second[2,c(5,6,7)]), 3)
  
  mi[i,c(15,16)] <- round(fit.scalar[c(4,5)]-fit.metric[c(4,5)], 3)
  mi[i,1] <- as.character(unique(key$facet.label[which(key$facet %in% names(models_lambda_rich)[i])]))
  mi$estimator[i]<-conf@Options$estimator
}


#write.csv(mi, "mi age table.csv")

