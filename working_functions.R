library(R.matlab)
library(rmatio)

library(rstudioapi)
library(tidyverse)
library(data.tree)
library(listviewer)
library(stringr)
library(purrr)


source("ConvertToRDS.R")

### determining the full path of each file
in_path <- file.path("D:", "_Rig_data", "SoloData", "Data")
file_list <- list.files(file.path("D:", "_Rig_data", "SoloData", "Data"), recursive = T) %>% as.list()
full_path <- paste0(in_path, "/", file_list) %>% as.list()


ConvertToRDS(file = full_path[[2]])


walk(full_path, ~ ConvertToRDS(file = .x))




#ReadData("data_@AthenaDelayComp_athena_AA01_190424b.mat.rds")



rds_list <- list.files(file.path("D:", "_R_WD", "git_projects", "rat_wm_training", "data", "rds_files")) %>% as.list()
walk(rds_list, ~ ReadData(file = .x) %>% TRAININGtoCSV())



TRAINING <- read_csv(file.path("D:", "_R_WD", "git_projects", "rat_wm_training", "output_data", "TRAINING.csv"))



setdiff(
  rds_list %>% unlist() %>% substr(start = 1, stop = 41) %>% head(),
  paste0(TRAINING$file, ".rds") %>% substr(start = 1, stop = 40) %>% head()
)
setdiff(
  paste0(TRAINING$file, ".rds") %>% substr(start = 1, stop = 40) %>% head(),
  rds_list %>% unlist() %>% substr(start = 1, stop = 41) %>% head()
)


setdiff(c(1,2,3,4,5,6), c(4,5,6,7,8,9))
setdiff(c(4,5,6,7,8,9), c(1,2,3,4,5,6))




intersect(c(1,2,3,4,5,6), c(4,5,6,7,8,9))
