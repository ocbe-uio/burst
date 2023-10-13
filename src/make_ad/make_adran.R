##############################
# Make the randomisation analysis dataset
# Input: raw 
# Output: adran
###############################

set.seed(42)

library(tidyverse)
library(lubridate)
library(forcats)
library(labelled)

source("src/external/functions.R")
raw <- read_rds("data/raw/raw.rds")

sp <- raw %>% 
  pick("sp")

ran <- raw %>% 
  pick("ran")

items <- raw %>% 
  pick("items")


adran <- ran %>% 
  select(subjectid,  ran1dt = eventdate, starts_with("ran_")) %>% 
  select(-ends_with("cd")) %>% 
  rename(ran1 = ran_1_2, ran2 = ran_3_4, ran3 = ran_5_6, 
         ran4 = ran_7_8, ran5 = ran_9_10, ran6 = ran_11_12) %>% 
  set_variable_labels(ran1dt = "Date of first randomisation")

isa <- raw %>% 
  pick("isa") %>% 
  select(subjectid, eventid, eventname, eventdate) %>% 
  left_join(adran %>% select(subjectid, ran1dt), by = "subjectid")

#Check randomisation vs treatment

adherence <- adran %>% 
  select(subjectid, starts_with("ran")) %>% 
  pivot_longer(cols = ran1:ran6, names_to = "eventid", values_to = "rantrt") %>% 
  mutate(eventid = case_when(
    eventid == "ran1" ~ "V01",
    eventid == "ran2" ~ "V02",
    eventid == "ran3" ~ "V03",
    eventid == "ran4" ~ "V04",
    eventid == "ran5" ~ "V05",
    eventid == "ran6" ~ "V06"
  )) %>% 
  left_join(sp %>% select(subjectid, eventid, eventname,eventdate, ran_vis, burst_vis, sp_verdi), by = c("subjectid", "eventid")) %>% 
  mutate(ran_vis = if_else(ran_vis == "Avslått system (Sham)", "Sham", ran_vis),
         burst_vis = if_else(burst_vis == "Avslått system (Sham)", "Sham", burst_vis),
         burst_vis = if_else(burst_vis == "Burst", "Burst stimulering", burst_vis)) %>% 
  mutate(adherent = (rantrt == ran_vis & rantrt == burst_vis),
         adherent = if_else(is.na(adherent), FALSE, adherent)) %>% 
  select(subjectid, eventid, eventname, eventdate, rantrt, trt = burst_vis, trtdose = sp_verdi, adherent)


adran <- adran %>% 
  select(subjectid, ran1dt) %>% 
  left_join(adherence, by = "subjectid") %>% 
  add_case(isa) %>% 
  arrange(subjectid, eventdate) %>% 
  mutate(across(c(rantrt, trt), ~if_else(. == "Burst stimulering", "Burst stimulation", .))) %>% 
  labeliser(.,items) %>% 
  set_variable_labels(rantrt = "Randomised allocation",
                      trt = "Actual treatment",
                      trtdose = "Treatment dose (amplitude)",
                      ran1dt = "Date of first randomisation",
                      adherent = "Adherent") 


# #############################
# # Introduce pseudorandomisation
# # Remove when running final analysis
# ###############################
# 
# adran <- adran %>% 
#   mutate(eventno = str_extract(eventid, "\\d+"),
#          eventno = as.numeric(eventno)) %>% 
#   mutate(period = ceiling(eventno / 2)) %>% 
#   group_by(subjectid, period) %>% 
#   mutate(coin = sample(c(0,1), size = 1, prob = c(0.5, 0.5))) %>%
#   mutate(rantrt = if_else(coin == 0, rantrt,
#                              case_when(
#                                row_number() == 1 ~ lead(rantrt),
#                                row_number() == 2 ~ lag(rantrt)
#                              ))) %>% 
#   mutate(trt = if_else(coin == 0, trt,
#                              case_when(
#                                row_number() == 1 ~ lead(trt),
#                                row_number() == 2 ~ lag(trt)
#                              ))) %>% 
#   mutate(trtdose = if_else(coin == 0, trtdose,
#                              case_when(
#                                row_number() == 1 ~ lead(trtdose),
#                                row_number() == 2 ~ lag(trtdose)
#                              ))) %>% 
#   select(-coin) %>% 
#   ungroup()

# Remove until here
###########################################################

adran_wide <- adran %>% 
  mutate(eventno = str_extract(eventid, "\\d+"),
         eventno = as.numeric(eventno)) %>% 
  select(subjectid, eventno, eventdate, rantrt, adherent, ran1dt, trtdose) %>% 
  pivot_wider(names_from = eventno, 
              values_from = c(eventdate, rantrt, adherent, trtdose),
              names_sep ="")  %>% 
  rowwise() %>% 
  mutate(adherent = all(across(adherent1:adherent6)),
         randomised = !is.na(ran1dt)) %>% 
  ungroup() %>% 
  select(-(adherent1:adherent7))

adran <- adran %>% 
  mutate(eventno = str_extract(eventid, "\\d+"),
         eventno = as.numeric(eventno)) %>% 
  mutate(period = ceiling(eventno / 2)) %>% 
  mutate(pair = period, 
         period = eventno) %>% 
  select(-eventno) %>% 
  set_variable_labels(period = "Time during which a single treatment (Burst or Sham) is given",
                      pair = "Pair of treatments (Burst/Sham or Sham/Burst")


readr::write_rds(adran, "data/ad/adran.rds")
readr::write_rds(adran_wide, "data/ad/adran_wide.rds") 



  
  
