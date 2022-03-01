## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(tidyverse)
library(scales)

## ----echo=FALSE---------------------------------------------------------------
#library(SARA)
#SARA()
#df<-tibble(left_join(mod_stats,sara_stock));
#names(df) <- tolower(names(df))
#glimpse(df)
#df |> filter(assessyear==2021) |> ggplot(aes(x=spawnbiomass,y=recruitment,label=fisheryyear)) + 
  #geom_point(size=.2) + geom_smooth() + theme_minimal() + facet_wrap(stock~region,scale="free")

