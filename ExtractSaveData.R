
# Define the path to the data folder
in_path <- path_to_mat_files

# List all .mat files in the data folder, excluding test data
file_list <- list.files(in_path, pattern = "\\.mat$", recursive = TRUE)
file_list <- file_list[!grepl("experimenter|Session Settings|FakeSubject", file_list)]

# List files already converted to rds
rds_list <- list.files(path_to_rds_files)


# Identify not yet converted mat files by comparing base file names
not_yet_conv <- setdiff(
  basename(file_list),  
  basename(rds_list) %>% str_remove(pattern = "\\.rds$")
)


# Add path to the not yet converted mat files
to_be_conv <- if (length(not_yet_conv) > 0) {
  file.path(in_path, file_list[str_detect(file_list, paste(not_yet_conv, collapse = "|"))])
} else {
  character()
}


# Convert not yet converted mat files to rds using parallel processing
if (length(to_be_conv) > 0) {
  # Set up a parallel backend using the number of cores available
  cl <- makeCluster(detectCores() - 1) # Subtract 1 to leave one core free for the main R process
  
  # Use parLapply to apply the ConvertToRDS function in parallel
  parLapply(cl, to_be_conv, ConvertToRDS)
  
  # Stop the cluster after use
  stopCluster(cl)
} else {
  warning("Nothing to convert")
}


# Read rds files to a tibble and save them to a csv file
rds_list <- list.files(path_to_rds_files)
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
