# Make all datasets needed to run the report

# Load libraries
library(tidyverse)
library(labelled)
library(eq5d)


# Make all datasets
source("src/make_raw/make_raw.R")


source("src/make_ad/make_adran.R")
source("src/make_ad/make_adsl.R")
source("src/make_ad/make_addm.R")



source("src/make_ad/make_adprom.R")




