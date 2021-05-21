library(here)
library(readxl)

case <- read_excel(here::here("single case/DIPSI_B_test cases.xlsx"))
case <- case[,-grep("instr", colnames(case))]

item.key <- read_excel(here::here("data/item key.xls"))
item.key <- item.key[,-8:-20]
items_brief <- item.key[item.key$in.brief==T,]

items_brief$item.label[87] <- "His/her thoughts tend to stray at times" #fix a typo here
items_brief$item.label[88] <- "Is often completely unaware of what is happening around him/her" #fixing non converging labels
items_brief$item.label[93] <- "At times, says things that others find odd or strange"


#Is often absorbed by intense emotions          95
#96                  Daydreaming                    Misses parts of conversations because of daydreaming

instrument <- read_excel(here::here("single case/DIPSI tool.xlsx"), col_names = c("domain.label", "item.label", "item.number"))
for(i in 1:length(instrument$domain.label)){
  if(is.na(instrument$domain.label[i])){
    instrument$domain.label[i] <- instrument$domain.label[i-1]
  }
}
instrument <- instrument[order(instrument$item.number, decreasing = F),]

#attach item labels to ordered instrument
instrument$item <- items_brief$item.label[match(instrument$item.label, items_brief$item.label)] #some NAs due to empty spaces I believe

my.nas <- which(is.na(instrument$item))

for(i in 1:length(my.nas)){
  instrument$item[my.nas][i] <- items_brief$item.label[grep(paste0(strsplit(instrument$item.label[my.nas][i], " ")[[1]][1:3], collapse = " "), items_brief$item.label)]
}

#3, 4, 
colnames(case)[5:length(colnames(case))] <- as.character(items_brief$item) #I dont really know if the items in the single case excel do follow this order or the order in colnames(small)

load(here::here("tools/final models cfa.Rdata"))

