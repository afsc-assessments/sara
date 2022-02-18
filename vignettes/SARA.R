## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'---------------------------------------------------------
library(SARA) 
SARA() 
df<-tibble(left_join(mod_stats,sara_stock));names(df) <- tolower(names(df)) 
glimpse(df) 
p1 <- df |> filter(assessyear==2021) |> ggplot(aes(x=spawnbiomass,y=recruitment,label=fisheryyear)) + geom_point(size=.2) + geom_smooth() + theme_minimal() + 
    facet_wrap(region~stock,scale="free") + expand_limits(x = 0, y = 0)


## ---- echo=FALSE, results='asis'----------------------------------------------

