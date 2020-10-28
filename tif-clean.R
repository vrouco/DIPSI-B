
set.seed(13) #for random short item selection

library(here)
library(ltm)#for grm()
#source(here("cfa development.R"))#to get the items of the short version
source(here("tools/functions.R"))#for the compareplot function
source(here::here("cross-validation.R"))


load("tools/final models cfa.Rdata")
#big <- test1[,which(colnames(test1)%in% unlist(DIPSI_facets))]#get the items of the standard version


#############get the items of the brief version. ONLY non-oddity


small<-test1[,as.character(item.key$item[item.key$in.brief==T][1:86])]
small <- small[,order(names(small))]
big <- test1[,4:175]
big <- big[,order(names(big))]


#the folowwing three lines are inetnse for your computer. Consider loading the Rdata below
irtbig<-grm(big)
irtsmall<-grm(small)


#load("tools/test information function.Rdata") #with irtbig and irtsmall
#save(irtbig, irtsmall, file="test information function.Rdata")

###tif one facet

for(i in 1:31){
  
  if(i<28){
  onefacettif<- test1[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i] & item.key$in.brief==T])]
  } else { 
    onefacettif<- test2[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i] & item.key$in.brief==T])]}
onefacettif<-grm(onefacettif, start.val="random")

if(i<28){
onefacetbig<-test1[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i]])]
} else {
  onefacetbig<-test2[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i]])]
}
onefacetbig<-grm(onefacetbig, start.val="random")

this.plot <- compareplot(onefacetbig,onefacettif) 
this.plot <- this.plot+ggtitle(unique(item.key$facet.label)[i])
pdf(file=here::here(paste0("facet tif/", unique(item.key$facet.label)[i], ".pdf")))
print(this.plot)
dev.off()
}


### TIF overall DIPSI, DIPSI-B, and random selection ###

#####draw random selections of items



set<-as.list(NA)

trial<-as.list(NA)


for (j in 1:30) {
  for (i in 1:(length(DIPSI_facets))){ #take 3 random variables in each facet, except facets from ODDITY
    this.set<-sample(unlist(DIPSI_facets[i]),3)
    set[j][[1]]<-c(set[j][[1]], this.set)
  }
  extra<-sample(unlist(DIPSI_facets[1:27])[-which(unlist(DIPSI_facets)%in%set[j][[1]])],5)#add extra 5 variables from the remaining items
  set[j][[1]]<-c(set[j][[1]], extra)

  trial[j][[1]]<-as.data.frame(test1[,which(colnames(test1)%in%unlist(set[j][[1]]))])
  }

random_sel <- trial

grm.others<-as.list(NA)
# for(i in 1:30){  grm.others[i][[1]]<-grm(random_sel[i][[1]]) }  #this takes some timee, about 20 mins.
#save(grm.others, file=here::here("30 random selection items grm.Rdata"))
#load(here::here("30 random selection items grm.Rdata"))



grm.others2<-list(NA)
for(i in 1:30){
grm.others2[i][[1]]<-as.data.frame(plot(grm.others[i][[1]], "IIC", items=0, plot=F))
}#the 1st and 18th random selection is formed by some items with one category less, 
#then it creates a list instead of a data frame and it only plots the first item
load("TIF random selection.Rdata")


this.plot <- compareplot(irtbig, irtsmall)  


for(i in 1:length(grm.others2)){
  this.plot + geom_line(data = grm.others2[i][[1]], aes(x=z, y=test.info), color="grey", cex=0.5,
                                     alpha=0.7)
}



grm.others3<-grm.others2[c(-1,-18)] 
#save(grm.others2, file="TIF random selection.Rdata")
 compareplot <- function(){
 
   library(ggplot2)
   grmbig.plot <-as.data.frame(plot(irtbig, "IIC", items=0))
   trashpercentage <-grmbig.plot
   percentage <- length(irtsmall$X)/length(irtbig$X)
   trashpercentage[,2]<-trashpercentage[,2]*percentage
   trashsmall<-as.data.frame(plot(irtsmall, "IIC", items=0))
   return(ggplot(data = grmbig.plot, aes(x=grmbig.plot[,1], y=grmbig.plot[,2]))+
            geom_line(cex=1.2)+
            geom_line(data = trashsmall, aes(x=trashsmall[,1], y=trashsmall[,2]), linetype="dashed", cex=1.2)+
 
            geom_line(data = plot.irt.others.1, aes(x=plot.irt.others.1[,1], y=plot.irt.others.1[,2]), linetype="dashed", cex=0.5)+
            geom_line(data = plot.irt.others.2, aes(x=plot.irt.others.2[,1], y=plot.irt.others.2[,2]), linetype="dashed", cex=0.5)+
            geom_line(data = plot.irt.others.3, aes(x=plot.irt.others.3[,1], y=plot.irt.others.3[,2]), cex=0.5)+
            xlab("Ability")+
            ylab("Information")
          )
 }


 compareplot()


for(i in 1:length(grm.others2)){
  this.plot <- this.plot + geom_line(data = grm.others2[i][[1]], aes(x=z, y=test.info), color="grey", cex=0.5,
                                     alpha=0.7)
}

trashbig <-as.data.frame(plot(irtbig, "IIC", items=0, plot=F))
trashpercentage <-trashbig
percentage <- length(irtsmall$X)/length(irtbig$X)
trashpercentage[,2]<-trashpercentage[,2]*percentage
this.plot <- this.plot + 
  geom_line(data = trashpercentage, aes(x=trashpercentage[,1], y=trashpercentage[,2]), linetype="dashed",cex=1.5)


#save(this.plot, trashpercentage, file="TIF plot.Rdata")

