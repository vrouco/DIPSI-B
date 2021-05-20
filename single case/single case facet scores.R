library(here)
library(readxl)

case <- read_excel(here::here("single case/DIPSI_B_test cases.xlsx"))
case <- case[,-grep("instr", colnames(case))]

items_brief <- item.key[item.key$in.brief==T,]

colnames(case)[5:length(colnames(case))] <- as.character(items_brief$item) #I dont really know if the items in the single case excel do follow this order or the order in colnames(small)

load(here::here("tools/final models cfa.Rdata"))

