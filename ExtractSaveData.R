# Load necessary functions
source("ConvertToRDS.R") # Function to convert mat files to rds
source("ReadData.R") # Function to read rds files to a tibble
source("TRAININGtoCSV.R") # Function to save tibble to csv
# source("TRAININGtoFeather.R") # Function to save tibble to feather format (commented out)

# Define the path to the data folder
in_path <- file.path("/mnt", "ceph","_raw_data", "rat_training_172", "SoloData", "Data")

# List all .mat files in the data folder, excluding test data
file_list <- list.files(in_path, pattern = "\\.mat$", recursive = TRUE)
file_list <- file_list[!grepl("experimenter|Session Settings|FakeSubject", file_list)]

# List files already converted to rds
rds_list <- list.files(file.path("/mnt", "ceph","_raw_data", "rat_training_172", "rds_files"))

# Identify not yet converted mat files by comparing base filenames
not_yet_conv <- setdiff(
  sub(pattern = ".*\\/", "", file_list),  
  rds_list %>% unlist() %>% substr(start = 1, stop = nchar(.)-4)
)

# Add path to the not yet converted mat files
to_be_conv <- ifelse(
  length(not_yet_conv) == 0,
  character(),
  paste0(in_path, "/", file_list[str_detect(file_list, pattern = paste(not_yet_conv, collapse = "|"))])
)

# Convert not yet converted mat files to rds
if (length(to_be_conv) > 0) {
  walk(to_be_conv, ~ ConvertToRDS(file = .x))
} else {
  warning("Nothing to convert")
}

# Read rds files to a tibble and save them to a csv file
rds_list <- list.files(file.path("/mnt", "ceph","_raw_data", "rat_training_172", "rds_files"))
to_append <- setdiff(
  rds_list,
  suppressMessages(suppressWarnings(
    if (file.exists(file.path("shiny_app", "TRAINING.csv"))) {
      read_csv(file.path("shiny_app", "TRAINING.csv")) %>% dplyr::select(file) %>% pull()
    } else {
      character()
    }
  ))
)

walk(to_append, ~ ReadData(rds_file = .x) %>% TRAININGtoCSV())

# Trial by trial data
# walk(rds_list, ~ ReadData(rds_file = .x, trialData = TRUE) %>%
#        TRAININGtoCSV(filename = "TrialByTrial.csv"))
# 
# # Specific example of trial by trial data
# ReadData(rds_file = "LT01_Gap_Detection_20191011_093950.mat.rds", trialData = TRUE) %>%
#   TRAININGtoCSV(filename = "TrialByTrial.csv")

# Documentation:
# Variables:
# - in_path: The path to the data folder.
# - file_list: A list of all .mat files in the data folder, excluding test data.
# - rds_list: A list of files already converted to .rds.
# - not_yet_conv: A list of .mat files that have not yet been converted to .rds.
# - to_be_conv: A list of paths to .mat files that need to be converted to .rds.
# - to_append: A list of .rds files that need to be appended to the TRAINING.csv file.
