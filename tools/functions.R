
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


iteminfo <- function(x, this.data, plot=F){
  
  options(warn=-1)
  library(ltm)
  library(difR)
  agecat <- cut(this.data[,"age_years"], c(0,13,99),c("0","1"))
  
  empty <- as.data.frame(matrix(NA,nrow=dim(x)[2], ncol=9))
  trash <- try(grm(x), silent=T)
  
  {if (class(trash) == "try-error") {
    grmobject <- grm(x, start.val = "random") #in the documentation of grm says if error try random initial values
    grmcoef <- coefficients(grmobject)
    if(is.numeric(grmcoef)){  #if its not numeric its a list, different structure
      for(i in 1:dim(grmcoef)[1]){
        empty[i,3]<-ifelse(is.na(grmcoef[i,"Extrmt1"]), NA, grmcoef[i,"Extrmt1"])#may be that either in the first or in the last there are empty cells
        empty[i,4]<-grmcoef[i,"Extrmt2"] #less likely empty cells in middle categories
        empty[i,5]<-grmcoef[i,"Extrmt3"]
        empty[i,6]<-ifelse(is.na(grmcoef[i,"Extrmt4"]), NA, grmcoef[i,"Extrmt4"])
        empty[i,7]<-grmcoef[i,"Dscrmn"]
      }
      
    } else {
      for(i in 1:length(grmcoef)){
        empty[i,3]<-ifelse(is.na(grmcoef[[i]]["Extrmt1"]), NA, grmcoef[[i]]["Extrmt1"])
        empty[i,4]<-grmcoef[[i]]["Extrmt2"]
        empty[i,5]<-grmcoef[[i]]["Extrmt3"]
        empty[i,6]<-ifelse(is.na(grmcoef[[i]]["Extrmt4"]), NA, grmcoef[[i]]["Extrmt4"])
        empty[i,7]<-grmcoef[[i]]["Dscrmn"]}
    } }
    else {
      grmobject <- grm(x)
      grmcoef <- coefficients(grmobject)
      if(is.numeric(grmcoef)){
        for(i in 1:dim(grmcoef)[1]){
          empty[i,3]<-ifelse(is.na(grmcoef[i,"Extrmt1"]), NA, grmcoef[i,"Extrmt1"])
          empty[i,4]<-grmcoef[i,"Extrmt2"]
          empty[i,5]<-grmcoef[i,"Extrmt3"]
          empty[i,6]<-ifelse(is.na(grmcoef[i,"Extrmt4"]), NA, grmcoef[i,"Extrmt4"])
          empty[i,7]<-grmcoef[i,"Dscrmn"]
        }
        
      } else {
        for(i in 1:length(grmcoef)){
          empty[i,3]<-ifelse(is.na(grmcoef[[i]]["Extrmt1"]), NA, grmcoef[[i]]["Extrmt1"])
          empty[i,4]<-grmcoef[[i]]["Extrmt2"]
          empty[i,5]<-grmcoef[[i]]["Extrmt3"]
          empty[i,6]<-ifelse(is.na(grmcoef[[i]]["Extrmt4"]), NA, grmcoef[[i]]["Extrmt4"])
          empty[i,7]<-grmcoef[[i]]["Dscrmn"]}
      } 
    }
    
    #rownames(empty) <- rownames(coefficients(grmobject))
    if(plot==TRUE){
      this.plot <- plot(grmobject, plot=T)} else { 
        this.plot <- plot(grmobject, plot=F)}
  }
  
  DIF<-function(...){
    
    library(lordif)
    agecat2 <- cut(this.data[,"age_years"], c(0,10,14,99),c("0","medio","1"), inlude.lowest=T)
    x<-cbind(x,agecat2)
    x<-x[-which(x$agecat2=="medio"),]
    x$agecat2 <- droplevels(x$agecat)
    difinfo <- lordif(x[,-which(names(x)=="agecat2")], x$agecat2, criterion = "Chisqr", alpha=0.01, minCell = 5)
    return(difinfo)
  }
  
  trash <- quiet(try(DIF(x), silent=T))
  #trash<-difinfo
  
  {if (class(trash) != "try-error") {
    empty[,8] <- quiet(ifelse(DIF(x)$flag==TRUE, "*", paste("")))
    empty[,9] <- quiet(ifelse(DIF(x)$flag==TRUE, apply(round(DIF(x)$stats[10:12], 3), 1, max), ""))
    
  } 
    else {
      
      empty[,8] <- print(NA)
      empty[,9] <- print(NA)}
  }
  
  colnames(empty)[7:9] <- c("Discrimination","DIF suspicious", "pseudo.R2")
  empty[,2] <- key$item.label[which(key$item%in%colnames(grmobject$X))]
  empty[,1] <- colnames(x)
  colnames(empty) <- c("item", "label", "b1","b2","b3","b4","a","DIF susp","DIF")
  empty<-empty[,-8]
  empty[,3:7]<-round(empty[,3:7],2)
  if(empty$a[1] <0){ #when initial value srandom, it may be that the direction of theta changes. This to ensure always positive
    empty$b1<-(-1)*empty$b1
    empty$b2<-(-1)*empty$b2
    empty$b3<-(-1)*empty$b3
    empty$b4<-(-1)*empty$b4
    empty$a<-(-1)*empty$a
  }
  #empty[,7]<-round(empty[,7],2)
  empty$Dec<-" "
  empty$EV<-" "
  
  
  empty<-rbind(c(" ",
                 " ",
                 " "," ","","","","","",""),
               c(" ",
                 paste0("**",key$facet.label[which(key$facet%in%names(DIPSI_facets)[grep(empty$item[empty$item!=" "][1], DIPSI_facets)])][1],"**"),
                 " "," ","","","","","",""),empty)
  return(empty)
  
}





compareplot <- function(irtbig=irtbig,irtsmall=irtsmall){
  
  library(ggplot2)
  trashbig <-as.data.frame(plot(irtbig, "IIC", items=0, plot=F))
  trashpercentage <-trashbig
  percentage <- length(irtsmall$X)/length(irtbig$X)
  trashpercentage[,2]<-trashpercentage[,2]*percentage
  trashsmall<-as.data.frame(plot(irtsmall, "IIC", items=0, plot=F))
  this.plot<-ggplot(data = trashbig, aes(x=trashbig[,1], y=trashbig[,2]))+
    geom_line(cex=1.5,linetype="dotted")+
    geom_line(data = trashsmall, aes(x=trashsmall[,1], y=trashsmall[,2]), cex=1.5)+
    geom_line(data = trashpercentage, aes(x=trashpercentage[,1], y=trashpercentage[,2]), cex=1.5,linetype="dashed")+
    xlab("Ability")+
    ylab("Information")+
    theme_light()
  
  return(this.plot)       
}


rmit <- function(x,y=NULL,z=NULL,w=NULL,e=NULL,v=NULL,b=NULL,plot=F,...){
  assign("small",small[, !names(small) %in% c(x, y, z, w, e, v, b)], .GlobalEnv)
  dropitems <- names(big)[which(!names(big) %in% names(small))]
  if(plot==T){
    assign("irtsmall", grm(small), .GlobalEnv)
    this.plot <- compareplot()
    print(this.plot)
  }
  return(dropitems)

}


DIFcheck <- function(x){

list(children=
paste(round(mean(as.numeric(x)[data$agecat=="0"], na.rm=T) +
  (sd(as.numeric(x)[data$agecat=="0"], na.rm=T) / sqrt(length(na.omit(as.numeric(x)[data$agecat=="0"])))), 4),
  " - ",

      round(mean(as.numeric(x)[data$agecat=="0"], na.rm=T) -
  (sd(as.numeric(x)[data$agecat=="0"], na.rm=T) / sqrt(length(na.omit(as.numeric(x)[data$agecat=="0"])))), 4)),

adolescents=paste(round(mean(as.numeric(x)[data$agecat=="1"], na.rm=T) +
              (sd(as.numeric(x)[data$agecat=="1"], na.rm=T) / sqrt(length(na.omit(as.numeric(x)[data$agecat=="1"])))), 4),
      " - ",
      
      round(mean(as.numeric(x)[data$agecat=="1"], na.rm=T) -
              (sd(as.numeric(x)[data$agecat=="1"], na.rm=T) / sqrt(length(na.omit(as.numeric(x)[data$agecat=="1"])))), 4)))
}
