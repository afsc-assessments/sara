#'
#' SARA report files into dataframes 
#' @export
#' @return  update from last year's files
SARA <- function(){
  sara_series<- data.frame(read.csv("data/saraseries.csv", as.is=T))[,-1]
  sara_stock <- data.frame(read.csv("data/sarastock.csv",as.is=T))[,-1]
  mod_stock  <- data.frame(read.csv("data/modstock.csv",as.is=T))[,-1]
  mod_stats  <- data.frame(read.csv("data/modstats.csv",as.is=T))[,-1]
  sara_names <- data.frame(read.csv("data/sarastocknames.csv",as.is=T))
# Now append new stuff onto it
source("R/SARA_readwrite.R")
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
}