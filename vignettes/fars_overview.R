## ---- echo = FALSE, include = FALSE, eval=FALSE--------------------------
#  library(readr)
#  library(magrittr)
#  library(fars)

## ----fars_read_example, eval=FALSE---------------------------------------
#  filename <- system.file("extdata/accident_2013.csv.bz2", package = "fars")
#  fars_read(filename)

## ----fars_summarize_years_example ,eval=FALSE----------------------------
#  setwd(system.file("extdata", package = "fars"))
#  fars_summarize_years(2013:2015)

## ----Example of fars_map_state ,eval=FALSE-------------------------------
#  setwd(system.file("extdata", package = "fars"))
#  fars_map_state(45, 2015)

## ----setup ,eval=FALSE---------------------------------------------------
#  library(fars)

