#'
#' SARA report files into dataframes 
#' 
#' @export
#' @return  update from last year's files
SARA <- function(){
  library(here)
  sara_series<- data.frame(read.csv(here("data/saraseries.csv"), as.is=T))[,-1]
  sara_stock <- data.frame(read.csv(here("data/sarastock.csv"),as.is=T))[,-1]
  mod_stock  <- data.frame(read.csv(here("data/modstock.csv"),as.is=T))[,-1]
  mod_stats  <- data.frame(read.csv(here("data/modstats.csv"),as.is=T))[,-1]
  sara_names <- data.frame(read.csv(here("data/sarastocknames.csv"),as.is=T))
  sarastocknames<-read.csv(here("data/sarastocknames.csv"),header=T)
# Now append new stuff onto it
SARA_readwrite()
#SARA_readwrite_form()
  #write.csv(sara_series,"data/saraseries.csv")
  #write.csv(sara_stock,"data/sarastock.csv")
  #write.csv(mod_stock,"data/modstock.csv")
  #write.csv(mod_stats,"data/modstats.csv")
  
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
