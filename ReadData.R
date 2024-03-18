# Function to read and process data from an RDS file
#' @param rds_file The name of the RDS file to process.
#' @param trialData A logical indicating whether to process trial data (TRUE) or not (FALSE).
#' @return A list containing processed data from the RDS file.

ReadData <- function(rds_file, trialData = F) {
  
  # Define the path to the data
  path <- file.path("shiny_app")
  data_source <- NA
  
  # Default mode, no trial data
  if (trialData == F) {
    # Read RDS file
    rat_data <- readRDS(paste0(file.path("/mnt", "ceph","_raw_data", "rat_training_172", "rds_files"), "/", rds_file))
    
    # Check the source of the data
    if ("SessionData" %in% names(rat_data)) {
      data_source <- "bpod"
      source("ReadBpodData.R")
      TRAINING <- ReadBpodData(rds_file = rds_file, data_source = data_source, rat_data = rat_data)
      return(TRAINING)
    } else if ("saved" %in% names(rat_data)) {
      data_source <- "bcontrol"
      source("ReadBcontrolData.R")
      TRAINING <- ReadBcontrolData(rds_file = rds_file, rat_data = rat_data, data_source = data_source)
      return(TRAINING)
    }
  }
  
  # With trial data
  if (trialData == T) {
    # Check if CSV file exists
    csv_exists <- file.exists(file.path(path, "TrialByTrial.csv"))
    
    # Check if the rds_file has been processed
    if (csv_exists) {
      read_TRIAL <- suppressMessages(suppressWarnings(read_csv(file.path(path, "TrialByTrial.csv"))))
      file_processed_test <- str_detect(read_TRIAL$file, regex(paste0(rds_file)))
    } else {
      file_processed_test <- F
    }
    
    # Process if not already processed
    if (all(file_processed_test == F)) {
      rat_data <- readRDS(paste0(file.path("/mnt", "ceph","_raw_data", "rat_training_172", "rds_files"), "/", rds_file))
      
      # Check the source of the data
      if ("SessionData" %in% names(rat_data)) {
        return(NULL)
      } else if ("saved" %in% names(rat_data)) {
        data_source <- "bcontrol"
        source("ReadTrialData.R")
        TRIAL <- ReadTrialData(rds_file = rds_file, rat_data = rat_data, data_source = data_source)
        return(TRIAL)
      }
    } else {
      warning(paste0(rds_file, " file has already been processed"))
      return(NULL)
    }
  }
}
