# Code to import data from Viedoc into labelled datasets
# The output is a data list (raw) where the datasets are stored in the column "data"
# To retreive a data set use the following code 
# 
# dm <- pick(raw, "dm")
# 
# This retrieves the dm dataset
# 
# Note that the numerical category-variables (ending with cd) are kept because they 
# may contain information (i.e. that the numbers have an interpretation). In R all 
# categorical variables (factors) are coded 1, 2, 3 etc regardless of the value in 
#the "cd" variable. If not needed they can be removed by adding the line
# %>% mutate(data = map(data,remove_cd))
# or do it by each dataset like
# dm <- pick(raw, "dm") %>% 
#   remove_cd()

# Both the "pick" and "remove_cd" are functions defined in "functions.R" in the
# external folder



library(tidyverse)
library(glue)
library(haven)
library(labelled)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  export_name <- "_20230919_063242" #default export
} else if (length(args) != 0) {
  export_name <- args[1]
}


export_folder <- glue("data/raw/{export_name}")
                      
source("src/external/functions.R")


cl <- read_csv(glue("{export_folder}/{export_name}_CodeLists.csv"), skip = 1) %>%
  rename_all(tolower) %>%
  group_by(formatname) %>%
  nest(value_labels = c(datatype, codevalue, codetext))


items <- read_csv(glue("{export_folder}/{export_name}_Items.csv"), skip = 1) %>%
  rename_all(tolower) %>%
  mutate(id = tolower(id)) %>%
  mutate(categorical = if_else(!is.na(formatname), 2,
                               if_else(paste0(id, "cd") == lead(id), 1, 0)
  )) %>%
  mutate(formatname = if_else(categorical == 1, lead(formatname), formatname)) %>%
  left_join(cl, by = "formatname") %>%
  rename_all(tolower) %>% 
  mutate(cols_abb = case_when(
    datatype == "date" ~ "D",
    datatype == "datetime" ~ "T",
    datatype == "double" ~ "d",
    datatype == "integer" ~ "i",
    datatype == "string" ~ "c",
    datatype == "text" ~ "c"
  ))

  


col_types <- setNames(as.list(items$cols_abb), items$id)


prefix <- str_sub(export_folder, -16)
raw <- tibble(files = list.files(export_folder)) %>%
  mutate(
    id = str_remove(files, paste0(prefix, "_")),
    id = str_remove(id, ".csv"),
    id = str_to_lower(id),
    files = paste0(export_folder, "/", files)
  ) %>%
  filter(!(id %in% c("items", "codelists", "readme.txt"))) %>%
  mutate(txt = map(files, read_csv, col_types = col_types, skip = 1)) %>%
  mutate(txt = map(txt, rename_all, tolower)) %>%
  mutate(txt = map(txt, labeliser, codelist = items)) %>%
  mutate(data = map(txt, factoriser, codelist = items)) %>%
  mutate(data = map(data, ~ mutate_if(., haven::is.labelled, as.numeric))) %>% 
  add_row(files= glue("{export_name}_CodeLists.csv"), id = "codelist", txt = list(cl), data = list(cl)) %>% 
  add_row(files= glue("{export_name}_Items.csv"), id = "items", txt = list(items), data = list(items)) %>% 
  mutate(data = map(data, labeliser, codelist = items))


write_rds(raw, "data/raw/raw.rds")
