##############################
# Make the demographics and baseline characteristics analysis data set
# Input: raw adsl
# Output: addm
###############################

library(tidyverse)
library(labelled)

raw <- read_rds("data/raw/raw.rds")
adsl <- read_rds("data/ad/adsl.rds")

vs <- raw %>% 
  pick("vs") %>% 
  select(subjectid, vsweight:vsbmi)

cm <- raw %>% 
  pick("cm") %>% 
  select(subjectid,
         cmneuro = annevsm1,
         cmami = annevsm2, 
         cmgab = annevsm3,
         cmpre = annevsm4, 
         cmdul = annevsm5,
         cmnonop = anikkop1, 
         cmpara = anikkop2, 
         cmnsaid = anikkop3, 
         cmantid = anikkop4, 
         cmopoid = anopjn, 
         cmoptotmme = cmsum) %>% 
  mutate(across(c(cmneuro, cmnonop), ~if_else(is.na(.), "Yes", "No")),
         cmopoid = if_else(cmopoid == "Ja", "Yes", "No")) %>% 
  mutate(across(c(cmami:cmdul, cmpara:cmantid), ~if_else(is.na(.), "No", "Yes"))) %>% 
  mutate(across(cmneuro:cmopoid, factor)) %>% 
  set_variable_labels(
    cmneuro = "Treatment for neuropatic pain?",
    cmami = "Amitriptyline",
    cmgab = "Gabapentin", 
    cmpre = "Pregabalin",
    cmdul = "Duloxetine",
    cmnonop = "Treatment with other non-opoids?",
    cmpara = "Paracetamol",
    cmnsaid = "NSAIDs",
    cmantid = "Antidepressiva",
    cmopoid = "Treatmetn with opoids?",
    cmoptotmme = "Total daily morphine milligram equivalents")

bh <- raw %>% 
  pick("bh") %>% 
  select(subjectid, 
         mhprevpt = bh_tidl1,
         mhprevcp =bh_tidl2, 
         mhprevpsych = bh_tidl3, 
         mhprevtens = bh_tidl4, 
         mhprevother = bh_tidl5, 
         mhprevspec = bhtidann,
         mhcurpt = bh_paga1,
         mhcurcp =bh_paga2, 
         mhcurpsych = bh_paga3, 
         mhcurtens = bh_paga4, 
         mhcurother = bh_paga5, 
         mhcurspec = bhpagann)
  
rn <- raw %>% 
  pick("rn") %>% 
  select(subjectid, 
         mhbackpain = rn_rel)

sv <- raw %>% 
  pick("sv") %>% 
  mutate(mhpaindur = round(svlenaar + svlenmnd/12, digits = 1),
         mhcurpaindur = round(svnivaar + svnivmnd/12, digits = 2)) %>% 
  select(subjectid, mhpaindur, mhcurpaindur)

mh <- bh %>% 
  left_join(rn, by = "subjectid") %>% 
  left_join(sv, by = "subjectid") %>% 
  mutate(across(c(mhprevpt:mhprevother, mhcurpt:mhcurother), ~if_else(is.na(.), "No", "Yes"))) %>%
  mutate(mhbackpain = if_else(mhbackpain == "Ja", "Yes", "No")) %>% 
  mutate(across(c(mhprevpt:mhprevother, mhcurpt:mhcurother, mhbackpain), factor)) %>%
  set_variable_labels(
    mhprevpt = "Previous physiotherapy?",
    mhprevcp = "Previous chiropractor?",
    mhprevpsych = "Previous psychologist?",
    mhprevtens = "Previous TENS?",
    mhprevother = "Other previous treatment?",
    mhprevspec = "Other previous treatment, specify",
    mhcurpt = "Current physiotherapy?",
    mhcurcp = "Current chiropractor?",
    mhcurpsych = "Current psychologist?",
    mhcurtens = "Current TENS",
    mhcurother = "Other current treatment?",
    mhcurspec = "Other current treatment, specify",
    mhbackpain = "Current back pain?",
    mhpaindur = "Duration of pain (years)",
    mhcurpaindur = "Duration of current pain level (years)"
  )

bi <- raw %>% 
  pick("bi") %>% 
  mutate(dmmstatus = fct_recode(bistatus, 
                              Married = "Gift",  
                              Single = "Enslig",
                              Divorsed = "Skilt", 
                              Cohabitant = "Samboer"),
         dmwork = fct_recode(aslivsit,
                             No = "Ikke yrkesaktiv",
                             Yes = "Yrkesaktiv/Student/Militærtjeneste"),
         dmsupport = case_when(
           !is.na(as_ytel1) ~ as_ytel1,
           !is.na(as_ytel2) ~ as_ytel2,
           !is.na(as_ytel3) ~ as_ytel3,
           !is.na(as_ytel4) ~ as_ytel4, 
           !is.na(as_ytel5) ~ as_ytel5,
           !is.na(as_ytel6) ~ as_ytel6,
           !is.na(as_ytel7) ~ as_ytel7,
           !is.na(as_ytel8) ~ as_ytel8,
           !is.na(as_ytel9) ~ as_ytel9,
           !is.na(as_ytel10) ~ as_ytel10
         )) %>% 
  select(subjectid, dmmstatus, 
         dmchild = bibarn,
         dmwork, 
         dmworkhrs = as_pros,
         dmoutwork = as_ute, dmsupport, dmedu = bi_utdan) %>% 
  mutate(dmoutwork = if_else(dmoutwork == 2019, 2, dmoutwork)) %>% 
  mutate(across(c(dmmstatus, dmwork, dmsupport), factor)) %>% 
  set_variable_labels(
    dmmstatus = "Marital status",
    dmchild = "Number of children",
    dmwork = "Working?", 
    dmworkhrs = "Working hours per week",
    dmoutwork = "Years not working", 
    dmsupport = "Type of governmental support", 
    dmedu = "Completed education"
  )

#Pain Catastrophizing Scale

pcs <- raw %>% 
  pick("pcs") %>% 
  mutate(pcstot = rowSums(across(num_range("pcs_", range = 1:13, suffix = "cd")))) %>% 
  mutate(pcsrum = rowSums(across(num_range("pcs_", range = 8:11, suffix = "cd"))),
         pcsmag = rowSums(across(c(pcs_6cd, pcs_7cd, pcs_13cd))),
         pcshelp = rowSums(across(c(num_range("pcs_", range = 1:5, suffix = "cd"), pcs_12cd)))
  ) %>% 
  mutate(pcstot_cat = cut(pcstot, c(0, 24.5, 100), labels = c("PCS ≤ 24", "PCS > 24" ))) %>% 
  set_variable_labels(pcstot = "PCS Total score",
                      pcsrum = "PCS Rumination",
                      pcsmag = "PCS Magnification",
                      pcshelp = "PCS Helplessness",
                      pcstot_cat = "PCS Total score dichotomised at 24") %>% 
  select(subjectid, pcstot:pcshelp, pcstot_cat)
         


addm <- adsl %>% 
  select(sitename:age, fas) %>% 
  select(subjectid, everything()) %>% 
  mutate(dmage = round((paticdat - brthdt)/365.25, digits = 1),
         dmage = as.numeric(dmage)) %>%
  select(-paticdat, -brthdt, -age) %>% 
  mutate(sitename = factor(sitename)) %>% 
  set_variable_labels(sitename = "Site name") %>% 
  set_variable_labels(dmage = "Age (years)") %>% 
  left_join(bi, by = "subjectid") %>% 
  left_join(pcs, by = "subjectid") %>% 
  left_join(cm, by = "subjectid") %>% 
  left_join(mh, by = "subjectid")

readr::write_rds(addm, "data/ad/addm.rds")

