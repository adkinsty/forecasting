library(brms)
library(tidyverse)
library(stringr)

setwd("/Users/adkinsty/Box/side_projects/covid/Forecasting/")

rds_files <- Sys.glob("modeling/multiverse_models/*.rds")

for (f in rds_files) {
  m <- read_rds(f)
  o <- capture.output(summary(m))
  cat(o,file=str_replace(f,".rds",".txt"),sep="\n")
}