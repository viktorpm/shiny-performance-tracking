
### loading functions ----
source("ConvertToRDS.R") ### f. to convert mat files to rds
source("ReadData.R") ### f. to read rds files to a tibble
source("TRAININGtoCSV.R") ### f. to save tibble to csv



### determining the full path of each file ----
in_path <- file.path("D:", "_Rig_data", "SoloData", "Data")
file_list <- list.files(file.path("D:", "_Rig_data", "SoloData", "Data"), recursive = T) %>% as.list()
full_path <- paste0(in_path, "/", file_list) %>% as.list()

### converting mat files to rds ----
walk(full_path, ~ ConvertToRDS(file = .x))



### reading the converted rds filenames to a list
### reading rds files to a tibble and saving them to a csv file ----
rds_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files")) %>% as.list()
walk(rds_list, ~ ReadData(rds_file = .x) %>% TRAININGtoCSV())
