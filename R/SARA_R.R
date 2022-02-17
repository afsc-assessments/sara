# *******************************************************
# R code for entering SARA model report files into oracle 
# refm/stocks/assess/data2016/sara2016/SARA_read_report.r
# *******************************************************

#require(RODBC);
#channel <- odbcConnect("AFSC", uid="AGREIG", pwd="");

# Read in last year's files
  sara_series<- data.frame(read.csv("data/saraseries.csv", as.is=T))[,-1]
  sara_stock <- data.frame(read.csv("data/sarastock.csv",as.is=T))[,-1]
  mod_stock  <- data.frame(read.csv("data/modstock.csv",as.is=T))[,-1]
  mod_stats  <- data.frame(read.csv("data/modstats.csv",as.is=T))[,-1]
  sara_names <- data.frame(read.csv("data/sarastocknames.csv",as.is=T))
# Now append new shit onto it
load_oracle=F
source("R/SARA_readwrite.R")
#str(sara_stock)
#SARA_readwrite()
#summary(sara_stock)
source("R/SARA_readwrite_form.R")
#tail(sara_series[,1:4])
#tail(sara_stock[,1:4])
#ls()
  write.csv(sara_series,"../saraseries.csv")
  write.csv(sara_stock,"../sarastock.csv")
  write.csv(mod_stock,"../modstock.csv")
  write.csv(mod_stats,"../modstats.csv")
  
library(openxlsx)
wb <- createWorkbook("Fred")

## Add 3 worksheets
addWorksheet(wb, "Sara_series")
addWorksheet(wb, "Sara_stocks")
addWorksheet(wb, "modstock")
addWorksheet(wb, "modstats")

## Headers and Footers

## Need data on worksheet to see all headers and footers
writeData(wb, sheet = 1, sara_series)
writeData(wb, sheet = 2, sara_stock)
writeData(wb, sheet = 3, mod_stock)
writeData(wb, sheet = 4, mod_stats)
saveWorkbook(wb, "data/sara_2021.xlsx", overwrite = TRUE)
#-rwxr-xr-x@ 1 jim   390B Nov 28  2017 sarastockregions.csv
#-rwxr-xr-x  1 jim   251B Nov 28  2017 ts.csv
#-rwxr-xr-x@ 1 jim   2.1K Nov 28  2017 BSAI_Sum_16.csv
#-rwxr-xr-x@ 1 jim    31K Nov 28  2017 modfishery.csv
#-rwxr-xr-x@ 1 jim   104K Nov 28  2017 ageselect.csv
#-rwxr-xr-x@ 1 jim    81K Nov 28  2017 agenat.csv
#-rwxr-xr-x@ 1 jim    47K Nov 28  2017 agemature.csv
