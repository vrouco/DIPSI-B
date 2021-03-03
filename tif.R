
set.seed(13) #for random short item selection

library(here)
library(ltm)#for grm()
#source(here("cfa development.R"))#to get the items of the short version
source(here("tools/functions.R"))#for the compareplot function
source(here::here("cross-validation.R"))


load("tools/final models cfa.Rdata")


#### tif one facet ####

for(i in 1:31){
  
  if(i<28){
  onefacettif<- test1[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i] & item.key$in.brief==T])]
  } else { 
    onefacettif<- test2[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i] & item.key$in.brief==T])]}
  
  onefacettif<-tryCatch(grm(onefacettif), error = function(e){
    grm(onefacettif, start.val="random")
  })


if(i<28){
onefacetbig<-test1[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i]])]
} else {
  onefacetbig<-test2[,as.character(item.key$item[item.key$facet==unique(item.key$facet)[i]])]
}
  onefacetbig<-tryCatch(grm(onefacetbig), error = function(e){
  grm(onefacetbig, start.val="random")
})


this.plot <- compareplot(onefacetbig,onefacettif) 
this.plot <- this.plot+ggtitle(unique(item.key$facet.label)[i])
#pdf(file=here::here(paste0("facet tif/", unique(item.key$facet.label)[i], ".pdf")))
print(this.plot)
#dev.off()
}


#### TIF dimensional DIPSI, DIPSI-B, and random selection ####

need_extra <- c("ACHI_rm", "DIST_rm", "SELF_rm", "SUB_rm", "WITH_rm")#these facets are not formed by 3 but by 4 items in the short version
#per dimension

for(d in 1:5){
facets.here <- as.character(unique(item.key$facet[item.key$domain.label == unique(item.key$domain.label)[d]]))

if(d != 5){
  big2 <- test1[,unlist(DIPSI_facets[facets.here])]
  small2 <- test1[,names(big2[which(names(big2) %in% item.key$item[item.key$in.brief==T])])]
} else {
  big2 <- test2[,unlist(DIPSI_facets[facets.here])]
  small2 <- test2[,names(big2[which(names(big2) %in% item.key$item[item.key$in.brief==T])])]
}


irtbig2 <- grm(big2)
irtsmall2 <- grm(small2)

set<-list()

random_sel<-list()

for (j in 1:10) {
  for(i in 1:length(facets.here)){
    this.set<-sample(unlist(DIPSI_facets[facets.here[i]]),3)
    set[j][[1]]<-c(set[j][[1]], this.set)
  }
    
  
    if(any(which(facets.here %in% need_extra))){
      number <- length(which(facets.here %in% need_extra))
      extra<-sample(unlist(DIPSI_facets[facets.here[which(facets.here %in% need_extra)]]),number)
      set[j][[1]]<-c(set[j][[1]], extra)
    }
  
    random_sel[j][[1]]<-as.data.frame(big2[,which(colnames(big2)%in%unlist(set[j][[1]]))])
    random_sel[j][[1]] <- random_sel[j][[1]][,order(names(random_sel[j][[1]]))]
}

grm.others<-list()
for(i in 1:10){  grm.others[i][[1]]<-grm(random_sel[i][[1]]) }

grm.others2<-list()
for(i in 1:10){
  grm.others2[i][[1]]<-as.data.frame(plot(grm.others[i][[1]], "IIC", items=0, plot=F))
}

this.plot <- compareplot(irtbig2, irtsmall2)  

for(i in 1:length(grm.others2)){
  this.plot <- this.plot + geom_line(data = grm.others2[i][[1]], aes(x=z, y=test.info), color="grey", cex=0.5,
                                     alpha=0.7)
}

this.plot <- this.plot + ggtitle(unique(item.key$domain.label)[d])
pdf(file=here::here(paste0("facet tif/", unique(item.key$domain.label)[d], ".pdf")))
print(this.plot)
dev.off()
}
