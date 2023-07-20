##############################
# Make the subject level analysis dataset
# Input: raw adran_wide
# Output: adsl
###############################


library(tidyverse)
library(lubridate)
library(forcats)

source("src/external/functions.R")
raw <- read_rds("data/raw/raw.rds")
adran_wide <- read_rds("data/ad/adran_wide.rds")


pat <- raw %>% 
  pick("pat")

ran <- adran_wide %>% 
  select(subjectid, ran1dt, rantrt1:rantrt6, adherent, randomised)

adsl <- pat %>% 
  select(sitename, subjectid, paticdat, sex = patsex, brthdt, age = patage) %>% 
  mutate(sex = fct_recode(sex, Male = "Mann", Female = "Kvinne")) %>% 
  left_join(ran, by = "subjectid") %>% 
  mutate(across(c(adherent, randomised), ~if_else(is.na(.), FALSE, .)))


eos <-  raw %>% 
  pick("eos") %>% 
  select(sitename, subjectid, eosdato, eosavsl, eosannet) %>% 
  mutate(incl = case_when(
    sitename == "Tidligere pasienter" ~ "No, before restart",
    subjectid == "2-02" ~ "No, replaced",
    subjectid == "3-001" ~ "No, replaced", 
    subjectid == "3-005" ~ "No, replaced", 
    .default = "Yes"
  ))

adsl_all <- adsl %>% 
  left_join(eos, by = c("sitename", "subjectid"))


adsl <- adsl_all %>% 
  filter(incl == "Yes") %>% 
  select(-incl) %>% 
  mutate(fas = (randomised & ( eosdato - ran1dt) > 0)) %>% 
  set_variable_labels(fas = "Full Analysis Set?")

  
readr::write_rds(adsl, "data/ad/adsl.rds")
readr::write_rds(adsl_all, "data/ad/adsl_all.rds") 

