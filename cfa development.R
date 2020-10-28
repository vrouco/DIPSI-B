#https://www.jamesuanhoro.com/post/testing-covariance-based-sems/  about the procedure of Saris-Satorra in lavaan
library(here)
source(here("cross-validation.R"))
library(lavaan)
options(scipen=999)

models<-list(
  EXPR_rm =' EXPR_rm =~ 1*expr_2m + 1*narc_6m + 1*expr_6m',
  ACTI_rm = 'ACTI_rm =~ 1*acti_3m + 1*acti_5m + 1*acti_6m',
  DOMI_rm = 'DOMI_rm =~ 1*domi_3m + 1*mani_4m + 1*domi_5m',
  IMPU_rm = 'IMPU_rm =~ 1*impu_1m + 1*impu_4m + 1*impu_5m',
  IRRI_rm = 'IRRI_rm =~ 1*irri_3m + 1*aggr_3m + 1*aggr_5m',
  DISO_rm = 'DISO_rm =~ 1*diso_2m + 1*diso_3m + 1*diso_4m',
  DIST_rm = 'DIST_rm =~ 1*pers_4m + 1*dist_3m + 1*dist_6m',
  RISK_rm = 'RISK_rm =~ 1*risk_3m + 1*risk_4m + 1*risk_5m',
  NARC_rm = 'NARC_rm =~ 1*gran_5m + 1*gran_4m + 1*narc_5m',
  LABI_rm = 'LABI_rm =~ 1*labi_2m + 1*labi_3m + 1*labi_5m',
  RESI_rm = 'RESI_rm =~ 1*resi_2m + 1*resi_4m + 1*mani_1m',
  EMPA_rm = 'EMPA_rm =~ 1*empa_4m + 1*empa_3m + 1*empa_5m',
  DEPE_rm = 'DEPE_rm =~ 1*depe_3m + 1*depe_4m + 1*depe_5m',
  ANXI_rm = 'ANXI_rm =~ 1*anxi_2m + 1*anxi_4m + 1*rumi_5m',
  SELF_rm = 'SELF_rm =~ 1*self_1m + 1*self_4m + 1*self_5m',
  ATTA_rm = 'ATTA_rm =~ 1*atta_1m + 1*atta_3m + 1*atta_6m',
  SUBM_rm = 'SUBM_rm =~ 1*naiv_3m + 1*subm_3m + 1*subm_1m',
  STRE_rm = 'STRE_rm =~ 1*stre_2m + 1*stre_4m + 1*stre_6m',
  SEPA_rm = 'SEPA_rm =~ 1*sepa_1m + 1*sepa_2m + 1*sepa_4m',
  DEPR_rm = 'DEPR_rm =~ 1*depr_3m + 1*rumi_4m + 1*depr_5m',
  FLEX_rm = 'FLEX_rm =~ 1*flex_4m + 1*obse_4m + 1*flex_6m',
  SHYN_rm = 'SHYN_rm =~ 1*shyn_2m + 1*avoi_4m + 1*shyn_5m',
  PARA_rm = 'PARA_rm =~ 1*para_4m + 1*para_5m + 1*para_6m',
  WITH_rm = 'WITH_rm =~ 1*with_6m + 1*with_3m + 1*with_4m',
  PERF_rm = 'PERF_rm =~ 1*perf_1m + 1*perf_4m + 1*perf_6m',
  ACHI_rm = 'ACHI_rm =~ 1*achi_1m + 1*achi_3m + 1*achi_5m',
  ORDE_rm = 'ORDE_rm =~ 1*orde_1m + 1*orde_4m + 1*orde_6m',
  SENS_rm = 'SENS_rm =~ 1*sens_2m + 1*sens_3m + 1*sens_4m',
  FANT_rm = 'FANT_rm =~ 1*fant_1m + 1*fant_3m + 1*fant_5m',
  DAYD_rm = 'DAYD_rm =~ 1*dayd_1m + 1*dayd_2m + 1*dayd_4m',
  ODDT_rm = 'ODDT_rm =~ 1*oddt_2m + 1*oddt_3m + 1*oddt_6m'
)

models_lambda_rich<-list(
  EXPR_rm =' EXPR_rm =~ 1*narc_6m + 1*expr_6m + 1*expr_2m 
  narc_6m  ~~ expr_6m',
  ACTI_rm = 'ACTI_rm =~ acti_6m + 1*acti_3m + 1*acti_5m 
 acti_6m  ~~ acti_5m',
  DOMI_rm = 'DOMI_rm =~ 1*domi_3m + mani_4m + 1*domi_5m',
  IMPU_rm = 'IMPU_rm =~ 1*impu_1m + 1*impu_4m + impu_5m',
  IRRI_rm = 'IRRI_rm =~ 1*aggr_3m + 1*aggr_5m + irri_3m',
  DISO_rm = 'DISO_rm =~ 1*diso_2m + 1*diso_3m + diso_4m',
  DIST_rm = 'DIST_rm =~ 1*dist_3m + 1*dist_6m + pers_4m + pers_1m 
 dist_3m  ~~ dist_6m',
  RISK_rm = 'RISK_rm =~ 1*risk_3m + 1*risk_4m + 1*risk_5m',
  NARC_rm = 'NARC_rm =~ 1*gran_5m + 1*gran_4m + narc_5m',
  LABI_rm = 'LABI_rm =~ 1*labi_2m + 1*labi_3m + 1*labi_5m 
  labi_2m  ~~ labi_3m ',
  RESI_rm = 'RESI_rm =~ 1*resi_2m + 1*resi_4m + 1*mani_1m',
  EMPA_rm = 'EMPA_rm =~ 1*empa_4m + 1*empa_3m + 1*empa_5m',
  DEPE_rm = 'DEPE_rm =~ 1*depe_3m + 1*depe_4m + 1*depe_5m',
  ANXI_rm = 'ANXI_rm =~ 1*anxi_4m + 1*rumi_5m + anxi_2m', 
  SELF_rm = 'SELF_rm =~ 1*self_4m + self_5m + 1*self_2m + self_1m 
 self_2m~~self_1m',
  ATTA_rm = 'ATTA_rm =~ 1*atta_1m + atta_3m + 1*atta_6m',
  SUBM_rm = 'SUBM_rm =~ 1*naiv_3m + subm_3m + subm_1m + 1*naiv_4m 
 naiv_3m  ~~ naiv_4m',
  STRE_rm = 'STRE_rm =~ 1*stre_2m + 1*stre_4m + 1*stre_6m',
  SEPA_rm = 'SEPA_rm =~ 1*sepa_1m + sepa_2m + 1*sepa_4m',#only close fit
  DEPR_rm = 'DEPR_rm =~ 1*depr_3m + rumi_4m + 1*depr_5m',
  FLEX_rm = 'FLEX_rm =~ 1*flex_4m + 1*obse_4m + flex_6m',
  SHYN_rm = 'SHYN_rm =~ 1*shyn_2m + avoi_4m + 1*shyn_5m',
  PARA_rm = 'PARA_rm =~ 1*para_5m + 1*para_6m + para_4m',
  WITH_rm = 'WITH_rm =~ 1*with_3m + 1*with_4m + with_6m + with_2m',
  PERF_rm = 'PERF_rm =~ 1*perf_4m + 1*perf_6m + perf_1m',
  ACHI_rm = 'ACHI_rm =~ 1*achi_5m + 1*achi_1m + achi_3m + 1*achi_2m',
  ORDE_rm = 'ORDE_rm =~ 1*orde_1m + orde_4m + 1*orde_6m',
  SENS_rm = 'SENS_rm =~ 1*sens_2m + 1*sens_3m + 1*sens_4m',
  FANT_rm = 'FANT_rm =~ 1*fant_3m + 1*fant_5m + fant_1m',
  DAYD_rm = 'DAYD_rm =~ 1*dayd_1m + 1*dayd_2m + 1*dayd_4m',
  ODDT_rm = 'ODDT_rm =~ 1*oddt_2m + 1*oddt_3m + 1*oddt_6m'
)

# derivation of the best structure

#save(models_lambda_rich, file=here::here("final models cfa.Rdata"))
#lines below were used to manually derive models


 x<-cfa(model=models[19][[1]], data=train1, ordered=names(train1))
 model2<-'SEPA_rm =~ 1*sepa_1m + sepa_2m + 1*sepa_4m'
 x<-cfa(model=model2, data=train1, ordered=names(train1))
 fitMeasures(x)[c("chisq","df","pvalue.scaled", "rmsea.scaled")]
 modindices(x, power=T, sort=T, delta=0.1)
 x2<-cfa(model=model2, data=train1, ordered=names(train1))
#  
#  
  SUBM_model = 'LABI_rm =~ 1*labi_2m + 1*labi_3m + 1*labi_6m
  labi_2m  ~~ labi_3m '
# 
# x<-cfa(model=SUBM_model, data=train1, ordered=names(train1))
# fitMeasures(x)[c("chisq","df","pvalue", "rmsea")]
# modindices(x, power=T, sort=T)

##############

model_comparison<-data.frame(facet=NA,
                             chisq.1=NA,
                           
                             pvalue.1=NA,
                             rmsea.1=NA,
                             
                             chisq.2=NA,
                            
                             pvalue.2=NA,
                             rmsea.2=NA
)

  
  
  for(i in 1:27){
    x<-cfa(model=models[i][[1]], data=train1, ordered=names(train1))
    model_comparison[i,3:4]<-round(fitMeasures(x)[c("pvalue.scaled","rmsea.scaled")],2)
    model_comparison[i,2]<-paste(round(fitMeasures(x)["chisq.scaled"],2), "(",as.character(fitMeasures(x)["df.scaled"]),")",sep="")
    y<-cfa(model=models_lambda_rich[i][[1]], data=train1, ordered=names(train1))
    if(fitMeasures(x)[3]==fitMeasures(y)[3]){model_comparison[i,5:7]<-"-"
    }else{
      model_comparison[i,6:7]<-round(fitMeasures(y)[c("pvalue.scaled","rmsea.scaled")],2)
      model_comparison[i,5]<-paste(round(fitMeasures(y)["chisq.scaled"],2), "(",as.character(fitMeasures(y)["df.scaled"]),")",sep="")
    }
    model_comparison[i,1]<-key$facet[match(names(models)[i],key$facet)]
  }
  
  for(i in 28:31){
    x<-cfa(model=models[i][[1]], data=train2, ordered=names(train2))
    model_comparison[i,3:4]<-round(fitMeasures(x)[c("pvalue.scaled","rmsea.scaled")],2)
    model_comparison[i,2]<-paste(round(fitMeasures(x)["chisq.scaled"],2), "(",as.character(fitMeasures(x)["df.scaled"]),")",sep="")
    y<-cfa(model=models_lambda_rich[i][[1]], data=train2, ordered=names(train2))
    if(fitMeasures(x)[3]==fitMeasures(y)[3]){model_comparison[i,5:7]<-"-"}
    else{
      model_comparison[i,6:7]<-round(fitMeasures(y)[c("pvalue.scaled","rmsea.scaled")],2)
      model_comparison[i,5]<-paste(round(fitMeasures(y)["chisq.scaled"],2), "(",as.character(fitMeasures(y)["df.scaled"]),")",sep="")
    }
    model_comparison[i,1]<-key$facet[match(names(models)[i],key$facet)]
  }
  


#save(model_comparison, file="cfa development.Rdata")
  
  
  #############save small items
  
  small <- unlist(models_lambda_rich)
  small <- strsplit(small, "=~ ")
  small<- unlist(small)
  is.even <- function(x) x %% 2 == 0
  small<-small[which(is.even(1:length(small)))]
  small<-sub("1\\*", "", small)
  small<-sub("1\\*", "", small)
  small<-sub("1\\*", "", small)
  small<-strsplit(small, " \\+ ")
  small<-unlist(small)
  small<-strsplit(small, " ")
  small<-unlist(small)
  small<-unlist(strsplit(unlist(small), "~~"))
  small<-small[-grep("\n", small)]
  small<-as.character(small)
  small<- small[-which(small=="")]
  small<-unique(small)#these are the 98 items that are going to be saved
  
  #write.csv(big[, which(colnames(big) %in% small)], here::here("data/small 98 i.csv"))
  
  items_small<-small
  