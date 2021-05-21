library(here)
library(readxl)

source(here::here("data input.R"))#read norm data

case <- read_excel(here::here("single case/DIPSI_B_test cases.xlsx")) #read case data
case <- case[,-grep("instr", colnames(case))]

################
case <- case[2,]
###################

items_brief <- item.key[item.key$in.brief==T,] #load key

items_brief$item.label[87] <- "His/her thoughts tend to stray at times" #fix a typo here
items_brief$item.label[88] <- "Is often completely unaware of what is happening around him/her" #fixing non converging labels
items_brief$item.label[93] <- "At times, says things that others find odd or strange"
items_brief$item.label[97] <- "Is often absorbed by intense emotions"


instrument <- read_excel(here::here("single case/DIPSI tool.xlsx"), col_names = c("domain.label", "item.label", "item.number"))#load instrument that has been administered
for(i in 1:length(instrument$domain.label)){
  if(is.na(instrument$domain.label[i])){
    instrument$domain.label[i] <- instrument$domain.label[i-1]
  }
}
instrument <- instrument[order(instrument$item.number, decreasing = F),]

#attach item labels to ordered instrument
instrument$item <- items_brief$item[match(instrument$item.label, items_brief$item.label)] #some NAs due to empty spaces I believe

my.nas <- which(is.na(instrument$item))

for(i in 1:length(my.nas)){
  instrument$item[my.nas][i] <- items_brief$item[grep(paste0(strsplit(instrument$item.label[my.nas][i], " ")[[1]][1:3], collapse = " "), items_brief$item.label)]
}

colnames(case)[5:length(colnames(case))] <- as.character(instrument$item) #assign column names to the cases 

load(here::here("tools/final models cfa.Rdata"))#load models


#######

library(semTools)
library(lavaan)



 data2 <- data[,c(2,6,10:375, 412)]
 items_small <- colnames(data2)[3:368][which(colnames(data2)[3:368] %in% item.key$item[item.key$in.brief==T])]
 data2<- data2[,c(1:2, which(colnames(data2) %in% items_small), 369)]
#data <- data[,c(-376:-411)]#remove domain scores
data2$group <- interaction(data2$gender, data2$agecat)

data2$sample <- "norm sample"

###make "case" have the same columns as data2

case$sample <- "new cases"

case$gender <- ifelse(case$geslacht=="V", "girl", "boy")

case$age <- floor(as.numeric(difftime(Sys.Date(), as.Date(case$geboortedatum), 
                                   unit="weeks"))/52.25)

case$agecat <- cut(case$age, c(0,13,99),c("0","1"))
case$group <- interaction(case$gender, case$agecat)

case <- case[, which(colnames(case) %in% colnames(data2))]
case$id <- 1:length(case$achi_1m)


data2$id <- paste0("0", seq(length(data2$sample)))


data2 <- rbind(case,data2)#merged norm sample and cases




##
fitted <- list()
for(i in 1:31){
  if(i < 28){
    
    fitted[i] <- cfa(model=models_lambda_rich[i][[1]], data2, estimator="MLR", group="group", 
                     group.equal=c("loadings", "intercepts"))
  }
  else if(i>=28){
    fitted[i] <- cfa(model=models_lambda_rich[i][[1]], data2, estimator="MLR", group="gender",
                     group.equal=c("loadings", "intercepts"))
  }
}

#############
library(dplyr)

population <- list()
onecase <- list()

for(i in 1:31){
  fs1 <- plausibleValues(fitted[i][[1]], nDraws = 20,append.data=T)
  
  this.population <- bind_rows(fs1)#merge all lists with plausib.values
  this.population <- this.population[this.population$case.idx!="1" ,]#remove the case
  
  this.groups <- names(table(this.population[,2]))#get the groups in this facet
  
  this.population.trait <- list()
  for(j in 1:length(this.groups)){
    this.population.trait[j][[1]] <- this.population[this.population[,2]==this.groups[j],][,dim(this.population)[2]]
    names(this.population.trait)[j] <- this.groups[j]
  }
  
  
  this.onecase <- bind_rows(fs1)
  this.onecase <- this.onecase[this.onecase$case.idx=="1",][,dim(this.onecase)[2]]
  
  population[i][[1]] <- this.population.trait
  onecase[i][[1]] <- this.onecase

  names(population)[i]<-names(models_lambda_rich[i])
  names(onecase)[i]<-names(models_lambda_rich[i])
}


onecase <- as.data.frame(onecase)


#the population values that matter
my.population <- list()
for(i in 1:31){
  if(i < 28){
    my.population[i] <- population[i][[1]][case$group[1]]#remove the 1 when doing this function 1 by 1. Taking only 1 case each time
    names(my.population)[i] <- names(models_lambda_rich[i])
  } else {
    my.population[i] <- population[i][[1]][case$gender[1]]
    names(my.population)[i] <- names(models_lambda_rich[i])
  }
}

pred.case <- data.frame(matrix(nrow=2, ncol=31))
colnames(pred.case)<- names(models_lambda_rich)
for(i in 1:31){
  pred.case[1,i] <- tail(ntile(append(my.population[i][[1]], mean(onecase[,i])), 10), 1)
  
  
  pred.t <- scale(append(my.population[i][[1]], mean(onecase[,i])))
  pred.t <- 50+(10*pred.t)
  pred.case[2,i] <- as.numeric(tail(pred.t, 1))
  
}

rownames(pred.case) <- c("deciles", "t.score")

write.csv(pred.case, file=here::here("case 2 predicted scores DIPSI-B.csv"))
