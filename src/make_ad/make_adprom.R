##############################
# Make the outcomes analysis dataset
# Input: raw adsl
# Output: adprom
###############################


library(tidyverse)
library(lubridate)
library(forcats)
library(eq5d)
library(labelled)

source("src/external/functions.R")
raw <- read_rds("data/raw/raw.rds")
adsl <- read_rds("data/ad/adsl.rds")
adran <- read_rds("data/ad/adran.rds")
adran_wide <- read_rds("data/ad/adran_wide.rds")

nrs_ben <- raw %>% 
  pick("nrs_ben") %>% 
  select(subjectid, eventseq, eventid, eventdate, painleg1 = nrs_sterk2cd, 
         painleg2 = nrs_svak2cd, painleg3 = nrs_dogn2cd, painleg4 = nrs_naa2cd) %>% 
  set_variable_labels(painleg1 = "Strongest pain in leg",
                      painleg2 = "Weakest pain in leg", 
                      painleg3 = "Most common pain in leg",
                      painleg4 = "Pain now in leg")


  
nrs_r <- raw %>% 
  pick("nrs_r") %>% 
  select(subjectid, eventseq, eventid, eventdate, painback = nrs_ryggcd) %>% 
  set_variable_labels(painback = "Most common pain in back")

fp <- raw %>% 
  pick("fp") %>% 
  select(subjectid, eventseq, eventid, eventdate, painunpl = fp_plagcd) %>% 
  set_variable_labels(painunpl = "Pain unpleasantness")

psfs <- raw %>% 
  pick("psfs") %>% 
  select(subjectid, eventseq, eventid, eventdate, psfs1 = pdfsnrs1cd,
         psfs2 = pdfsnrs2cd, psfs3 = pdfsnrs3cd) %>% 
  set_variable_labels(psfs1 = "Ability to do first activity (higher is better)",
                      psfs2 = "Ability to do second activity (higher is better)",
                      psfs3 = "Ability to do third activity (higher is better)"
                      )
  

      
  eq5d <- raw %>% 
    pick("eq5d") %>% 
    select(subjectid, eventseq, eventid, eventdate, MO = eq5d1cd,
           SC = eq5d2cd, UA = eq5d3cd, PD = eq5d4cd, AD = eq5d5cd, eq5dvas = eq5d_vas) 
    
eq5d <- eq5d %>%  
  mutate(eq5dui = eq5d(., country="Denmark", version="5L", type="VT", ignore.invalid = TRUE)) %>% 
  select(subjectid, eventseq, eventid, eventdate, eq5dui, eq5dvas) %>% 
  set_variable_labels(eq5dui = "EQ-5D-5L Utility index (Danish value set)",
                      eq5dvas = "EQ-5D-5L VAS")

hscl <- raw %>% 
  pick("hscl") %>% 
  mutate(hscl_score = rowMeans(select(., num_range("hscl", range = 1:25, suffix = "cd")), na.rm = TRUE)) %>% 
  mutate(hscl_nmiss = rowSums(is.na(select(., num_range("hscl", range = 1:25, suffix = "cd"))))) %>% 
  set_variable_labels(hscl_score = "Hopkins Symptom Checklist-25 mean score",
                      hscl_nmiss = "HSCL number of missing answers") %>% 
  mutate(hscltot = round(hscl_score, digits = 2)) %>% 
  select(subjectid, eventseq, eventid, eventdate, hscl_score, hscl_nmiss)

is <- raw %>% 
  pick("is") %>% 
  mutate(isi_score = rowSums(select(., num_range("is_", range = 1:5, suffix = "cd")), na.rm = TRUE)) %>% 
  mutate(isi_nmiss = rowSums(is.na(select(., num_range("is_", range = 1:25, suffix = "cd"))))) %>% 
  set_variable_labels(isi_score = "Insomnia Severity Index score",
                      isi_nmiss = "ISI number of missing answers") %>% 
  mutate(isi_score = round(isi_score, digits = 2)) %>% 
  select(subjectid, eventseq, eventid, eventdate, isi_score, isi_nmiss)
  
pgic <- raw %>% 
  pick("pgic") %>% 
  select(subjectid, eventseq, eventid, eventdate, pgic = pgic_end, pgiccd = pgic_endcd, pbq = pgic_sti, pbqcd = pgic_sticd) %>% 
  set_variable_labels(pgic = "Patient Global Impression of Change last 14 days",
                      pgiccd = "PGIC - Code",
                      pbq = "Patient blinding question",
                      pbqcd = "Patient blinding question - Code")



adprom <- adsl %>% 
  select(subjectid, fas) %>% 
  filter(fas) %>% 
  left_join(adran_wide, by = "subjectid") %>% 
  left_join(nrs_ben, by = "subjectid") %>% 
  left_join(nrs_r, by = c("subjectid", "eventseq", "eventid", "eventdate")) %>% 
  left_join(fp, by = c("subjectid", "eventseq", "eventid", "eventdate")) %>% 
  left_join(psfs, by = c("subjectid", "eventseq", "eventid", "eventdate")) %>%
  left_join(eq5d, by = c("subjectid", "eventseq", "eventid", "eventdate")) %>%
  left_join(hscl, by = c("subjectid", "eventseq", "eventid", "eventdate"))  %>%
  left_join(is, by = c("subjectid", "eventseq", "eventid", "eventdate")) %>%
  left_join(pgic, by = c("subjectid", "eventseq", "eventid", "eventdate"))  %>% 
  filter(eventid != "SS0")


adprom0 <- adprom %>% 
  mutate(rantrt = case_when(
    eventdate < eventdate1 ~ "Baseline",
    eventdate < eventdate2 ~ rantrt1,
    eventdate < eventdate3 ~ rantrt2,
    eventdate < eventdate4 ~ rantrt3,
    eventdate < eventdate5 ~ rantrt4,
    eventdate < eventdate6 ~ rantrt5,
    eventdate < eventdate7 ~ rantrt6,
    TRUE ~ "Follow-up"
  )) %>%
  mutate(trtdose = case_when(
    eventdate < eventdate1 ~ 0,
    eventdate < eventdate2 ~ trtdose1,
    eventdate < eventdate3 ~ trtdose2,
    eventdate < eventdate4 ~ trtdose3,
    eventdate < eventdate5 ~ trtdose4,
    eventdate < eventdate6 ~ trtdose5,
    eventdate < eventdate7 ~ trtdose6,
    TRUE ~ 0
  )) %>% 
  mutate(period = case_when(
    eventdate < eventdate1 ~ 0,
    eventdate < eventdate2 ~ 1,
    eventdate < eventdate3 ~ 2,
    eventdate < eventdate4 ~ 3,
    eventdate < eventdate5 ~ 4,
    eventdate < eventdate6 ~ 5,
    eventdate < eventdate7 ~ 6,
    TRUE ~ 7
  )) %>% 
  mutate(pair = paste0("Pair ", ceiling(period / 2))) %>% 
  mutate (pair = if_else(pair == "Pair 0", "Baseline", pair)) %>% 
  mutate(studyday = eventdate - ran1dt) %>% 
  group_by(subjectid) %>% 
  mutate(subject = paste0("Subject ",  stringr::str_pad(cur_group_id(), 2, pad="0"))) %>% 
  ungroup() %>% 
  select(-c(eventdate1:eventdate7, rantrt1:rantrt7, trtdose1:trtdose7)) %>% 
  select(subject, subjectid:adherent, studyday, rantrt, trtdose, period, pair, everything()) 



  adprom <- adprom0
    
  readr::write_rds(adprom, "data/ad/adprom.rds")