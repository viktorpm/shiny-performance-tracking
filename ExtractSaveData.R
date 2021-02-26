
### loading functions ----
source("ConvertToRDS.R") ### f. to convert mat files to rds
source("ReadData.R") ### f. to read rds files to a tibble
source("TRAININGtoCSV.R") ### f. to save tibble to csv
# source("TRAININGtoFeather.R") ### f. to save tibble to csv



### determining the full path of each file ----

### listing all the mat files in the data folders
in_path <- file.path("D:", "_Rig_data", "SoloData", "Data")
file_list <- list.files(file.path("D:", "_Rig_data", "SoloData", "Data"), 
                        pattern = "\\.mat$",
                        recursive = T) %>% 
  as.list()

### removing test data
file_list <- file_list[!grepl("experimenter", file_list) & # excluding files from experimenter folder
                         !grepl("Session Settings", file_list) & 
                         !grepl("FakeSubject", file_list)]
# & !grepl("emmett", file_list)] 
# full_path <- paste0(in_path, "/", file_list) %>% as.list()

### listing files already converted to rds 
rds_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files")) %>% as.list()

### listing the not yet converted mat files (remove ".rds", remove path form character strings)
### difference between file_list and rds_list
not_yet_conv <- setdiff(
  sub(pattern = ".*\\/", "", file_list),  
  rds_list %>% unlist() %>% substr(start = 1, stop = nchar(.)-4)
) 


### adding path to the not yet converted mat files
ifelse(
  not_yet_conv %>% is_empty(),
  to_be_conv <-  character(),
  to_be_conv <- paste0(
    in_path, 
    "/",
    file_list[str_detect(file_list, pattern = paste(not_yet_conv, collapse = "|")
    )] %>% unlist()
  ) 
  )




### converting not yet converted mat files to rds ----
ifelse(
  to_be_conv %>% is_empty(),
  warning("Nothing to convert"),
  walk(to_be_conv, ~ ConvertToRDS(file = .x))
)


### reading rds files to a tibble and saving them to a csv file ----

### updated rds_list 
rds_list <- list.files(file.path("D:", "_R_WD", "git_projects", "r_codes_rat_wm", "data", "rds_files")) %>% as.list()

to_append <- setdiff(
  rds_list %>% unlist(),
    suppressMessages(
    suppressWarnings(
      read_csv(file.path("shiny_app", "TRAINING.csv")) %>% dplyr::select(file) %>% pull()))
  ) %>%
  as.list()

walk(to_append, ~ ReadData(rds_file = .x) %>% TRAININGtoCSV())
# walk(rds_list, ~ ReadData(rds_file = .x) %>% TRAININGtoFeather())



################################################
### Trial by trial data 

# walk(rds_list, ~ ReadData(rds_file = .x, trialData = T) %>% 
#        TRAININGtoCSV(filename = "TrialByTrial.csv"))


# data_@AltStatDelayComp_athena_AA08_200217a.mat.rds
# data_@AthenaDelayComp_viktor_VP12_200213a.mat.rds
# data_@SoundCategorization_athena_AA05_200217a.mat.rds
# data_@AltSoundCategorization_sharbat_SC04_200106a.mat.rds
# LT01_Gap_Detection_20191011_093950.mat.rds
# 
# 
# ReadData(rds_file = "LT01_Gap_Detection_20191011_093950.mat.rds",trialData = T) %>%
#   TRAININGtoCSV(filename = "TrialByTrial.csv")









