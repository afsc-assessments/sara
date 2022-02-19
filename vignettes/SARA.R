## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold'---------------------------------------------------------
library(SARA) 
SARA() 
df<-tibble(left_join(mod_stats,sara_stock));names(df) <- tolower(names(df)) 
stk_reg <- unique(paste0(df$stock,df$region))
crabstocks <- c("SNOWCRAB","BLUEKING", "REDKING", "TANNER","GOLDKING")
  df |> filter(paste0(stock,region) %in% stk_reg, 
               !stock %in% crabstocks) |> 
    group_by(paste0(stock,region)) |> summarise(max(assessyear)) |>  print(n=Inf)
p1 <- df |> filter(assessyear==2021) |> 
  ggplot(aes(x=spawnbiomass,y=recruitment,label=fisheryyear)) + geom_point(size=.2) + geom_smooth() + theme_minimal() + 
    facet_wrap(region~stock,scale="free") + expand_limits(x = 0, y = 0)


## ---- echo=FALSE, results='asis'----------------------------------------------

