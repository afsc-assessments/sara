#' Process data-raw
#'
#' Process's the Roxygen header on the csv files in data-raw into R files for each data set into
#'  data folder and R folder
#' from Eli Holmes salmon work (for example...)
#' @keywords internal
process_data_raw <- function() {
  
  # Clean up prior data files and their R and vignette files
  olddatafils <- list.files("data")
  if(length(olddatafils)!=0){
    file.remove(file.path("data", olddatafils))
    file.remove(file.path("R", stringr::str_replace(olddatafils, "[.]rda", ".R")))
    file.remove(file.path("vignettes", stringr::str_replace(olddatafils, "[.]rda", ".Rmd")))
    file.remove(file.path("vignettes", stringr::str_replace(olddatafils, "[.]rda", ".R")))
    file.remove(file.path("vignettes", stringr::str_replace(olddatafils, "[.]rda", ".html")))
  }
  
  # Go through each data file
  fils <- list.files("data-raw")
  for(fil in fils){
    filpath <- file.path("data-raw", fil)
    thetext <- readLines(filpath)
    isheader <- stringr::str_sub(thetext, 1, 2) == "#'"
    thecols <- read.csv(text=thetext[max(which(isheader))+1])
    thecols <- colnames(thecols)
    # Create the R file for the data file
    headr <- thetext[isheader]
    dataname <- stringr::str_replace_all(stringr::str_sub(headr[1], 4), " ","-")
    footr <- paste0("#' @name ", dataname, "\n",
                    "#' @docType data\n",
                    "#' @examples\n",
                    "#' data('", dataname, "')\n",
                    "#' library(ggplot2)\n",
                    "#' out$NUMBER_OF_SPAWNERS[out$NUMBER_OF_SPAWNERS==-99] <- NA\n",
                    "#' ggplot(out, aes(x=YEAR, y=NUMBER_OF_SPAWNERS)) + geom_point(na.rm = TRUE) +\n",
                    "#'   ggtitle('", dataname, "') +\n",
                    "#'   facet_wrap(~COMMON_POPULATION_NAME)\n",
                    "NULL\n")
    cat(headr, describe_text(thecols), sep="\n", footr, file=file.path("R", paste0(dataname, ".R")))
    
    # Create the vignette file
    headr <- c("---
title: ", dataname, "
output: rmarkdown::html_vignette
vignette: >
  %\\VignetteIndexEntry{", dataname, "}
  %\\VignetteEngine{knitr::rmarkdown}
  %\\VignetteEncoding{UTF-8}
---

# To load data

```{r}
library(VRData)
data('", dataname, "')
```

# Spawners plot

```{r, echo = FALSE}
out$NUMBER_OF_SPAWNERS[out$NUMBER_OF_SPAWNERS == -99] <- NA
ggplot2::ggplot(out, ggplot2::aes(x=.data$YEAR, y=.data$NUMBER_OF_SPAWNERS)) +
   ggplot2::geom_point(na.rm = TRUE) +
   ggplot2::ggtitle('Spawner Counts') +
   ggplot2::facet_wrap(~COMMON_POPULATION_NAME)
```

# Fraction wild plot

```{r, echo = FALSE}
out$FRACWILD[out$FRACWILD == -99] <- NA
ggplot2::ggplot(out, ggplot2::aes(x=.data$YEAR, y=.data$FRACWILD)) +
   ggplot2::geom_point(na.rm = TRUE) +
   ggplot2::ggtitle('Fraction Wild') +
   ggplot2::ylim(0,1) +
   ggplot2::facet_wrap(~COMMON_POPULATION_NAME)
```

# Raw data table

```{r, echo=FALSE}
knitr::kable(out)
```

")
    cat(headr, file=file.path("vignettes", paste0(dataname, ".Rmd")), sep="")
    
    # Make the data files
    out <- read.csv(filpath, skip=sum(isheader))
    save(out, file=file.path("data", paste0(dataname, ".rda")))
  }
}