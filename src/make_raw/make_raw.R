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


items <- read_csv(file.path(export_folder, paste0(export_name, "_Items.csv")), skip = 1, show_col_types = FALSE) %>%
  rename_all(tolower) %>%
  mutate(id_ = id,
         id = tolower(id)) %>%
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
  )) %>% 
  mutate(cols_abb = if_else(id == "designversion", "d", cols_abb))

  
my_read_csv <- function(file){
  my_cols <- read_csv(file, skip = 1, n_max = 1, show_col_types = FALSE) %>% 
    colnames() %>% 
    as_tibble_col(column_name = "id_")
  
  my_col_types <- my_cols %>% 
    left_join(items %>% select(id_, cols_abb), by = "id_") %>% 
    mutate(cols_abb = if_else(is.na(cols_abb), "?", cols_abb)) 
  
  my_col_types <- setNames(as.list(my_col_types$cols_abb), my_col_types$id_)
  
  data <- read_csv(file, col_types = my_col_types, skip = 1, show_col_types = FALSE)
  return(data)
}


raw <- tibble(files = list.files(export_folder)) %>%
  mutate(
    id = str_remove(files, paste0(prefix, "_")),
    id = str_remove(id, ".csv"),
    id = str_to_lower(id),
    files = file.path(export_folder, files)
  ) %>%
  filter(!(id %in% c("items", "codelists", "readme.txt"))) %>%
  filter(!endsWith(id, ".sas")) %>% 
  mutate(txt = map(files, my_read_csv)) %>%
  mutate(problems = map(txt, problems),
         any_problems = map_chr(problems, \(x) if_else(nrow(x) != 0, "Yes", "No"))) %>% 
  mutate(txt = map(txt, rename_all, tolower)) %>%
  mutate(txt = map(txt, labeliser, codelist = items)) %>%
  mutate(data_lbl = map(txt, factoriser, codelist = items)) %>%
  mutate(data = map(data_lbl, haven::zap_labels)) %>% 
  add_row(files = file.path(export_folder, glue("{export_name}_CodeLists.csv")), 
          id = "codelist", 
          txt = list(cl), 
          data = list(cl), 
          data_lbl = list(cl), 
          any_problems = "No") %>% 
  add_row(files = file.path(export_folder, glue("{export_name}_Items.csv")), 
          id = "items", 
          txt = list(items), 
          data = list(items), 
          data_lbl = list(cl),
          any_problems = "No") 

raw <- raw %>% 
  #mutate(data = map(data,remove_cd)) %>%  # de-comment to remove the "cd" variables
  mutate(data = map(data, labeliser, codelist = items)) %>% 
  mutate(data_lbl = map(data_lbl, remove_fct, codelist = items)) %>% 
  mutate(data_lbl = map(data_lbl, labeliser, codelist = items)) %>% 
  set_variable_labels(
    files = "Path to file",
    id = "ID",
    txt = "Raw import",
    data_lbl = "With value labels for export",
    data = "For analyses (value labels removed)",
    any_problems = "Any problems with the import?",
    problems = "Table of problems"
  ) %>% 
  select(files:id, any_problems, problems, txt, data_lbl:data) %>% 
  mutate(problems = if_else(any_problems == "Yes", problems, as.vector(NA)))

# 
# col_types <- setNames(as.list(items$cols_abb), items$id)
# 
# 
# prefix <- str_sub(export_folder, -16)
# raw <- tibble(files = list.files(export_folder)) %>%
#   mutate(
#     id = str_remove(files, paste0(prefix, "_")),
#     id = str_remove(id, ".csv"),
#     id = str_to_lower(id),
#     files = paste0(export_folder, "/", files)
#   ) %>%
#   filter(!(id %in% c("items", "codelists", "readme.txt"))) %>%
#   mutate(txt = map(files, read_csv, col_types = col_types, skip = 1)) %>%
#   mutate(txt = map(txt, rename_all, tolower)) %>%
#   mutate(txt = map(txt, labeliser, codelist = items)) %>%
#   mutate(data = map(txt, factoriser, codelist = items)) %>%
#   mutate(data = map(data, ~ mutate_if(., haven::is.labelled, as.numeric))) %>% 
#   add_row(files= glue("{export_name}_CodeLists.csv"), id = "codelist", txt = list(cl), data = list(cl)) %>% 
#   add_row(files= glue("{export_name}_Items.csv"), id = "items", txt = list(items), data = list(items)) %>% 
#   mutate(data = map(data, labeliser, codelist = items))


write_rds(raw, "data/raw/raw.rds")
